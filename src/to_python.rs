use crate::construct_ast::ast_structure::{Ast, AstNode};
use std::fmt::Write;
use lazy_static::lazy_static;
use regex::Regex;
use crate::{EMPTY_STR, IGNORE_ENUMS, IGNORE_FUNCS, IGNORE_STRUCTS, unwrap_enum};
use crate::mold_tokens::OperatorType;
use crate::types::{unwrap_u};
use crate::{throw, CUR_COL, CUR_LINE, CUR_PATH, LINE_DIFF, SRC_CODE};
use crate::add_types::utils::join;
use crate::construct_ast::tree_utils::update_pos_from_tree_node;

lazy_static!{
    static ref NUM_TYP_RE: Regex = Regex::new(r"[uif]").unwrap();
    // pub static ref NONE: String = String::from("_NONE");
}

#[derive(Copy, Clone, Debug)]
pub enum ToWrapVal {
    Nothing,
    GetAsValue,
    GetInnerValue,
    GetName
}

//4 probably using a string builder would be much more efficient (but less readable IMO)
pub fn to_python(
    ast: &[Ast], pos: usize, indentation: usize, add_val_wrapper: ToWrapVal
) -> String {
    update_pos_from_tree_node(&ast[pos]);

    let children = unwrap_u(&ast[pos].children);
    match &ast[pos].value {
        AstNode::Module | AstNode::Body => {
            let mut res = String::new();
            for child in children {
                write!(
                    res,
                    "\n{}{}",
                    "\t".repeat(indentation),
                    to_python(ast, *child, indentation, ToWrapVal::GetInnerValue)
                ).unwrap();
            }
            res
        },
        AstNode::Function(name)=> {
            if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
                return EMPTY_STR;
            }

            let name = if name.contains("::") {
                name.rsplit_once("::").unwrap().1.to_string()
            } else { name.clone() };
            if indentation == 1 {
                let named_args = add_value_to_named_args(ast, children);
                format!(
"def {name}({}){}:
{}self = value_(self)
{}{named_args}{}",
                    to_python(ast, children[1], indentation, ToWrapVal::Nothing), //1 param
                    to_python(ast, children[2], indentation, ToWrapVal::Nothing), //1 return
                    "\t".repeat(indentation + 1),
                    "\t".repeat(indentation + 1),
                    to_python(ast, children[3], indentation + 1, ToWrapVal::Nothing) //1 body
                )
            } else {
                normal_func_to_py(ast, indentation, children, &name)
            }
        },
        AstNode::StaticFunction(name) => {
            if indentation != 0 {
                let named_args = add_value_to_named_args(ast, children);
                format!(
                    "@staticmethod\n{}def {name}({}){}:\n\t{named_args}{}",
                    "\t".repeat(indentation),
                    to_python(ast, children[1], indentation, ToWrapVal::Nothing), // param
                    to_python(ast, children[2], indentation, ToWrapVal::Nothing), // return
                    to_python(ast, children[3], indentation + 1, ToWrapVal::Nothing) // body
                )
            } else {
                if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
                    return EMPTY_STR;
                }
                normal_func_to_py(ast, indentation, children, name)
            }
        },
        AstNode::ReturnType => { EMPTY_STR },
        AstNode::IfStatement => {
            let mut res = format!("if {}{}",
                to_python(ast, children[0], indentation, ToWrapVal::Nothing), //1 ():
                to_python(ast, children[1], indentation + 1, ToWrapVal::Nothing) //1 body
            );
            for child in children.iter().skip(2){
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        let c_children = ast[*child].ref_children();
                        write!(
                            res, "\n{}elif {}{}",
                            "\t".repeat(indentation),
                            to_python(ast, c_children[0], indentation, ToWrapVal::Nothing), //1 ():
                            to_python(ast, c_children[1], indentation + 1, ToWrapVal::Nothing) //1 body
                        ).unwrap();
                    },
                    AstNode::Body => {
                        write!(
                            res, "\n{}else:{}",
                            "\t".repeat(indentation),
                            to_python(ast, *child, indentation + 1, ToWrapVal::Nothing) //1 body
                        ).unwrap();
                    },
                    _ => throw!()
                }
            }
            res
        },
        AstNode::WhileStatement => {
            format!("while {}{}",
                to_python(ast, children[0], indentation, ToWrapVal::Nothing), //1 ():
                to_python(ast, children[1], indentation + 1, ToWrapVal::Nothing) //1 body
            )
        },
        AstNode::FirstAssignment => {
            format!("{}={}#!",
                to_python(ast, children[0], indentation, ToWrapVal::GetName),
                to_python(ast, children[1], indentation, ToWrapVal::GetAsValue)
            )
        },
        AstNode::Assignment | AstNode::OpAssignment(_) => {
            let op = if let AstNode::OpAssignment(op) = &ast[pos].value {
                op.to_string()
            } else { EMPTY_STR };
            match &ast[children[0]].value {
                AstNode::Identifier(name) => format!(
                    "{name}.v {op}= {}",
                    to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
                ),
                AstNode::Property => {
                    let prop_children = ast[children[0]].ref_children();
                    let base = to_python(ast, prop_children[0], indentation, ToWrapVal::GetName);
                    let attr = to_python(ast, prop_children[1], indentation, ToWrapVal::GetName);
                    let val = to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue);
                    if !op.is_empty() {
                        format!("{base}.setattr('{attr}', {base}.getattr('{attr}') {op} {val})")
                    } else {
                        format!("{base}.setattr('{attr}', {val})")
                    }
                }
                AstNode::Index => format!(
                    "{} {op}= {}",
                    to_python(ast, children[0], indentation, ToWrapVal::GetName),
                    to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
                ),
                AstNode::UnaryOp(OperatorType::Dereference) => format!(
                    "{}.v {op}= {}",
                    to_python(ast, children[0], indentation, ToWrapVal::GetName),
                    to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
                ),
                _ => todo!()
            }
        },
        AstNode::Identifier(name) => {
            let name = if name == "None" { throw!() } else { name };
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetName | ToWrapVal::GetAsValue => name.clone(),
                ToWrapVal::GetInnerValue =>
                    String::from(remove_unnecessary_val_creation(&format!("{}.v", name)))
            }
        },
        AstNode::Operator(op) => {
            // let c1 = format!("{}.v", to_python(ast, children[0], indentation, false));
            // let c2 = format!("{}.v", to_python(ast, children[1], indentation, false));
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!(
                    "value_({} {op} {})",
                    to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue),
                    to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
                ),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!(
                    "{} {op} {}",
                    to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue),
                    to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
                ),
            }
        },
        AstNode::UnaryOp(op) => {
            match op {
                OperatorType::MutPointer => {
                    let p = format!("pointer_({})",
                        to_python(ast, children[0], indentation, ToWrapVal::GetName)
                    );
                    match add_val_wrapper {
                        ToWrapVal::Nothing => throw!(),
                        ToWrapVal::GetAsValue => format!("value_({p})"),
                        ToWrapVal::GetName | ToWrapVal::GetInnerValue => p,
                    }
                }
                OperatorType::Pointer => {
                    let p = format!("pointer_({})",
                             to_python(ast, children[0], indentation, ToWrapVal::GetName)
                    );
                    match add_val_wrapper {
                        ToWrapVal::Nothing => throw!(),
                        ToWrapVal::GetAsValue => format!("value_({p})"),
                        ToWrapVal::GetName | ToWrapVal::GetInnerValue => p,
                    }
                }
                OperatorType::Dereference => {
                    let r = format!("{}.p",
                         to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue)
                    );
                    match add_val_wrapper {
                        ToWrapVal::Nothing => throw!(),
                        ToWrapVal::GetInnerValue => remove_unnecessary_val_creation(&format!("{r}.v")).to_string(),
                        ToWrapVal::GetAsValue => r,
                        ToWrapVal::GetName => r
                    }
                }
                _ => {
                    match add_val_wrapper {
                        ToWrapVal::Nothing => throw!(),
                        ToWrapVal::GetAsValue =>
                            format!("value_({op}{})", to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue)),
                        ToWrapVal::GetName | ToWrapVal::GetInnerValue =>
                            format!("{op}{}", to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue)),
                    }
                }
            }
        },
        AstNode::Parentheses => {
            format!("({})",
                to_python(ast, children[0], indentation, add_val_wrapper)
            )
        },
        AstNode::ColonParentheses => {
            format!(
                "{}:",
                children.iter().map(|child|
                    to_python(ast, *child, indentation, ToWrapVal::GetInnerValue)
                ).collect::<Vec<_>>().concat()
            )
        },
        AstNode::Index => {
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!(
                    "value_({}[{}])",
                    to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue),
                    to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
                ),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!(
                    "{}[{}]",
                    to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue),
                    to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
                ),
            }
        },
        AstNode::Number(num) => {
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!("value_({})", NUM_TYP_RE.split(num).next().unwrap()),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => NUM_TYP_RE.split(num).next().unwrap().to_string(),
            }
        },
        AstNode::String { val, .. } => {
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!("value_({val})"),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => val.clone(),
            }
        },
        AstNode::Char(chr) => {
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!("value_('{chr}')"),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!("'{chr}'"),
            }
        },
        AstNode::Bool(b) => {
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => String::from(if *b { "value_(True)" } else { "value_(False)" }),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => String::from(if *b { "True" } else { "False" }),
            }
        },
        AstNode::ListLiteral => {
            let elems = join(children.iter().map(
                |child| to_python(ast, *child, indentation + 1, ToWrapVal::GetInnerValue)
            ), ", ");
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!("value_(built_in_list_([{elems}]))"),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!("built_in_list_([{elems}])"),
            }
        },
        AstNode::SetLiteral => {
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => {
                    let mut res = String::from("value_({");
                    for (i, child) in children.iter().enumerate() {
                        if i != 0 {
                            write!(res, ", ").unwrap();
                        }
                        write!(res, "{}", to_python(ast, *child, indentation + 1, ToWrapVal::GetInnerValue)).unwrap();
                    }
                    write!(res, "}})").unwrap();
                    res
                },
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => {
                    let mut res = String::from("{");
                    for (i, child) in children.iter().enumerate() {
                        if i != 0 {
                            write!(res, ", ").unwrap();
                        }
                        write!(res, "{}", to_python(ast, *child, indentation + 1, ToWrapVal::GetInnerValue)).unwrap();
                    }
                    write!(res, "}}").unwrap();
                    res
                },
            }
        },
        AstNode::DictLiteral => {
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => {
                    let mut res = String::from("value_({");
                    for (i, child) in children.iter().enumerate() {
                        if i != 0 {
                            if i % 2 == 0 {
                                write!(res, ", ").unwrap();
                            } else {
                                write!(res, ": ").unwrap();
                            }
                        }
                        write!(res, "{}", to_python(ast, *child, indentation + 1, ToWrapVal::GetInnerValue)).unwrap();
                    }
                    write!(res, "}})").unwrap();
                    res
                },
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => {
                    let mut res = String::from("{");
                    for (i, child) in children.iter().enumerate() {
                        if i != 0{
                            if i % 2 == 0 {
                                write!(res, ", ").unwrap();
                            } else {
                                write!(res, ": ").unwrap();
                            }
                        }
                        write!(res, "{}", to_python(ast, *child, indentation + 1, ToWrapVal::GetInnerValue)).unwrap();
                    }
                    write!(res, "}})").unwrap();
                    res
                },
            }
        },
        AstNode::Pass => String::from("pass"),
        AstNode::FunctionCall(_) => {
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if let Some(res) = built_in_funcs(ast, name, indentation, children, false) {
                    return res;
                }
            }

            let res = format!(
                "{}({})",
                to_python(ast, children[0], indentation, ToWrapVal::GetName),
                if children.len() > 1 {
                    to_python(ast, children[1], indentation, ToWrapVal::GetAsValue)
                } else { EMPTY_STR }
            );
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!("value_({})", res),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => res,
            }
        },
        AstNode::ArgsDef => {
            if children.is_empty() {
                return EMPTY_STR;
            }
            let mut res = to_python(
                ast, children[0], indentation, ToWrapVal::GetName
            );
            for child in children.iter().skip(1) {
                write!(
                    res, ", {}", to_python(
                        ast, *child, indentation, ToWrapVal::GetName
                    )
                ).unwrap();
            }
            res
        }
        AstNode::Args => {
            if let ToWrapVal::GetName = add_val_wrapper {
                throw!();
            }
            if children.is_empty() {
                return EMPTY_STR;
            }
            let mut res = to_python(ast, children[0], indentation, add_val_wrapper);
            for child in children.iter().skip(1) {
                write!(res, ", {}",
                       to_python(ast, *child, indentation, add_val_wrapper)
                ).unwrap();
            }
            res
        },
        AstNode::Return => {
            if children.is_empty() { String::from("return") }
            else {
                format!("return {}",
                    to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue)
                )
            }
        },
        AstNode::Property => {
            if let Some(res) = built_in_methods(ast, children, false) {
                return res;
            }
            if let AstNode::FunctionCall(true) = ast[children[1]].value {
                let func_name_pos = unwrap_u(&ast[children[1]].children)[0];
                let func_name = unwrap_enum!(&ast[func_name_pos].value, AstNode::Identifier(n), n);
                let res = if func_name == "__init__" {
                    format!(
                        "{}({})",
                        to_python(
                            ast, children[0], indentation, ToWrapVal::GetName
                        ),
                        to_python(
                            ast, unwrap_u(&ast[children[1]].children)[1], indentation, ToWrapVal::GetAsValue
                        )
                    )
                } else {
                    format!(
                        "{}.{}",
                        to_python(ast, children[0], indentation, ToWrapVal::GetName),
                        to_python(ast, children[1], indentation, ToWrapVal::GetName)
                    )
                };
                return if let ToWrapVal::GetAsValue = add_val_wrapper {
                    format!("value_({res})")
                } else {
                    res
                }
            }
            let base = to_python(ast, children[0], indentation, ToWrapVal::GetAsValue);
            if let Ast { value: AstNode::FunctionCall(_), children: Some(ch), .. } = &ast[children[1]] {
                return match add_val_wrapper {
                    ToWrapVal::Nothing => throw!(),
                    ToWrapVal::GetAsValue => format!(
                        "value_({}.getattr('{}')({}))",
                        base,
                        to_python(ast, ch[0], indentation, ToWrapVal::GetName),
                        to_python(ast, ch[1], indentation, ToWrapVal::GetAsValue)
                    ),
                    ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!(
                        "{}.getattr('{}')({})",
                        base,
                        to_python(ast, ch[0], indentation, ToWrapVal::GetName),
                        to_python(ast, ch[1], indentation, ToWrapVal::GetAsValue)
                    ),
                }
            }
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!(
                    "value_({}.getattr('{}'))",
                    base,
                    to_python(ast, children[1], indentation, ToWrapVal::GetName)
                ),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!(
                    "{}.getattr('{}')",
                    base,
                    to_python(ast, children[1], indentation, ToWrapVal::GetName)
                ),
            }
        },
        AstNode::ForStatement => {
            let par = to_python(ast, children[0], indentation, ToWrapVal::Nothing);
            let vars = to_python(ast, unwrap_u(&ast[children[0]].children)[0], indentation, ToWrapVal::Nothing);
            format!("for {par}\n{}({vars},)=map(value_, ({vars},)){}",
                    "\t".repeat(indentation + 1),
                    to_python(ast, children[1], indentation + 1, ToWrapVal::Nothing),

            )
        },
        AstNode::ForVars => {
            let mut res = to_python(ast, children[0], indentation, ToWrapVal::GetName);
            for child in children.iter().skip(1) {
                write!(res, ", {}",
                    to_python(ast, *child, indentation, ToWrapVal::GetName)
                ).unwrap();
            }
            res
        },
        AstNode::ForIter => {
            format!(" in {}", to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue))
        },
        AstNode::Struct(name) => {
            if unsafe { IGNORE_STRUCTS.contains(name.as_str()) } {
                return EMPTY_STR;
            }
            format!(
                "class {name}:\n{}",
                to_python(ast, children[2], indentation + 1, ToWrapVal::Nothing)
            )
        }
        AstNode::Continue => String::from("continue"),
        AstNode::Break => String::from("break"),
        AstNode::Null => String::from("None"),
        AstNode::Enum(name) => {
            if unsafe { IGNORE_ENUMS.contains(name.as_str()) } {
                return EMPTY_STR;
            }
            let mut options = vec![];
            let module = unwrap_u(&ast[children[1]].children);
            for option in module {
                let name = unwrap_enum!(&ast[*option].value, AstNode::Identifier(n), n);
                let types = unwrap_u(&ast[*option].children);
                options.push((name, types));
            }

            let mut res = format!(
                "class {name}:\n\
                \tdef getattr(attr): return eval(f'{name}.{{attr}}')\
                {}\n\n",
                options.iter()
                    .enumerate()
                    .map(|(i, (opt_name, _))| format!("\n\t{opt_name}={i}"))
                    .collect::<Vec<_>>()
                    .concat()
            );
            for (opt_name, opt_types) in options {
                let args = (0..opt_types.len())
                    .map(|i| format!("_{i}"))
                    .collect::<Vec<_>>();
                let args_with_self = if args.is_empty() { EMPTY_STR } else {
                    format!("self.{}", args.join(", self."))
                };
                let args = args.join(",");
                writeln!(res,
"class {name}__{opt_name}({name}):
\tdef __init__(self,{args}):
\t\t({args_with_self}) = ({args})
\tdef getattr(attr): return eval(f'Color.{{attr}}')
\tdef get_enum_inner_vals_(self): return {args_with_self}
{name}.{opt_name} = {name}__{opt_name}"
                ).unwrap();
            }
            res
        }
        AstNode::Arg { name, is_arg, is_kwarg } => {
            let mut res = if *is_arg          { format!("*{name}")    }
            else if *is_kwarg   { format!("**{name}")   }
            else                { name.clone()          };
            if let Some(default_val) = &ast[pos].children {
                write!(res, "={}", to_python(
                    ast, default_val[0], indentation, ToWrapVal::GetInnerValue
                )).unwrap();
            }
            res
        }
        AstNode::NamedArg(name) => {
            format!(
                "{name}={}",
                to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue)
            )
        }
        AstNode::ListComprehension | AstNode::SetComprehension | AstNode::DictComprehension => {
            let parts = ast[children[0]].ref_children();
            let loops = ast[children[1]].children.clone().unwrap();
            format!("{}{}{}{}{}",
                /*[{*/match &ast[pos].value {
                    AstNode::ListComprehension => "[",
                    AstNode::SetComprehension | AstNode::DictComprehension => "{",
                    _ => unreachable!()
                },
                /*expression*/if let AstNode::DictComprehension = &ast[pos].value {
                    format!(
                        "{}: {}",
                        to_python(ast, parts[0], indentation, ToWrapVal::GetInnerValue),
                        to_python(ast, parts[1], indentation, ToWrapVal::GetInnerValue)
                    )
                } else {
                    to_python(ast, parts[0], indentation, ToWrapVal::GetInnerValue)
                },
                /*loops*/loops.iter().map(|r#loop| {
                    let colon_par = ast[*r#loop].ref_children()[0];
                    let loop_parts = ast[colon_par].ref_children();
                    format!(" for {} in map(value_, {})",
                            to_python(ast, loop_parts[0], indentation, ToWrapVal::Nothing),
                            to_python(ast, loop_parts[1], indentation, ToWrapVal::Nothing)
                                .strip_prefix(" in ").unwrap()
                    )
                }).collect::<Vec<_>>().concat(),
                /*condition*/if let Some(condition) = &ast[children[2]].children {
                    format!(
                        " if {}",
                        to_python(ast, condition[0], indentation, ToWrapVal::GetInnerValue)
                    )
                } else { EMPTY_STR },
                /*}]*/match &ast[pos].value {
                    AstNode::ListComprehension => "]",
                    AstNode::SetComprehension | AstNode::DictComprehension => "}",
                    _ => unreachable!()
                }
            )
        }
        AstNode::Cast => {
            to_python(ast, children[0], indentation, add_val_wrapper)
        }
        AstNode::Ternary => {
            format!(
                "{} if {} else {}",
                to_python(ast, children[0], indentation, add_val_wrapper),
                to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue),
                to_python(ast, children[2], indentation, add_val_wrapper)
            )
        }
        AstNode::Import => {
            if let AstNode::From = ast[children[0]].value {
                format!(
                    "from {} import {}",
                    join(
                        ast[children[0]].ref_children()
                            .iter()
                            .map(|i| to_python(
                                ast, *i, indentation, ToWrapVal::GetName
                            )),
                        "."
                    ),
                    join(
                        children.iter().skip(1).map(|i| to_python(
                            ast, *i, indentation, ToWrapVal::GetName
                        )),
                        ", "
                    )
                )
            } else {
                format!(
                    "import {}",
                    join(
                        children.iter().map(|i| to_python(
                            ast, *i, indentation, ToWrapVal::GetName
                        )),
                        ", "
                    )
                )
            }
        }
        AstNode::As(new_name) => {
            format!(
                "{} as {new_name}",
                to_python(ast, children[0], indentation, add_val_wrapper)
            )
        },
        AstNode::GenericsDeclaration | AstNode::Trait { .. } | AstNode::Ignore => EMPTY_STR,
        AstNode::Match => {
            format!(
                "match {}:{}",
                to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue),
                children.iter().skip(1).map(|case|
                    to_python(ast, *case, indentation + 1, ToWrapVal::Nothing)
                ).collect::<Vec<_>>().concat(),
            )
        }
        AstNode::Case => {
            let name = if children.len() == 2 {
                "_val_"
            } else {
                unwrap_enum!(&ast[children[1]].value, AstNode::Identifier(n), n)
            };
            let condition_children = ast[children[0]].ref_children();
            let empty_vec = vec![];
            let par_values = if condition_children.len() == 2 {
                unwrap_u(&ast[condition_children[1]].children)
            } else { &empty_vec };

            let par_names = join(
                par_values.iter().map(
                    |x| unwrap_enum!(&ast[*x].value, AstNode::Identifier(i), i)
                ),
                ", "
            );
            let indent = "\t".repeat(indentation);
            let case_type = to_python(ast, condition_children[0], indentation, ToWrapVal::GetName);
            let body = to_python(ast, *children.last().unwrap(), indentation + 1, ToWrapVal::Nothing);
            if case_type == "_" {
                return format!("\n{indent}case _:{body}")
            } else if case_type == "None" {
                return format!("\n{indent}case None:{body}")
            }

            format!(
                "\n{indent}case {name} if isinstance({name}, {case_type}):\
                {}\
                {body}",
                if par_values.is_empty() { EMPTY_STR } else {
                    format!("\n{indent}\t({par_names})={name}.get_enum_inner_vals_()")
                }
            )
        }
        AstNode::Tuple => {
            let elems = join(children.iter().map(
                |child| to_python(ast, *child, indentation, ToWrapVal::GetInnerValue)
            ), ", ");
            match add_val_wrapper {
                ToWrapVal::Nothing => throw!(),
                ToWrapVal::GetAsValue => format!("value_(({elems}))"),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!("({elems})"),
            }
        }
        _ => throw!("Unexpected AST {:?}", ast[pos].value)
    }
}

fn normal_func_to_py(
    ast: &[Ast], indentation: usize, children: &[usize], name: &str
) -> String {
    format!(
        "def {name}({}){}:\n\t{}{}",
        to_python(ast, children[1], indentation, ToWrapVal::Nothing), //1 param
        to_python(ast, children[2], indentation, ToWrapVal::Nothing), //1 return
        add_value_to_named_args(ast, children), //1 named_args
        to_python(ast, children[3], indentation + 1, ToWrapVal::Nothing) //1 body
    )
}

fn add_value_to_named_args(ast: &[Ast], children: &[usize]) -> String {
    let named_args: Vec<_> = unwrap_u(&ast[children[1]].children).iter().filter_map(|x| {
        if ast[*x].children.is_some() {
            Some(unwrap_enum!(&ast[*x].value, AstNode::Arg { name, .. }, name))
        } else { None }
    }
    ).collect();
    if named_args.is_empty() {
        EMPTY_STR
    } else {
        format!(
            "({},) = ({},)",
            join(named_args.iter(), ","),
            join(named_args.iter().map(|x| format!("value_({x})")), ",")
        )
    }
}

fn remove_unnecessary_val_creation(st: &str) -> &str {
    if st.starts_with("value_(") && st.ends_with(").v") {
        throw!("just checking if this is ever useful, if so- remove this panic");
        // let end = st.len() - ").v".len();
        // return &st["value_(".len()..end]
    }
    st
}

fn built_in_methods(
    ast: &[Ast], children: &[usize], _add_index: bool
) -> Option<String> {
    let AstNode::FunctionCall(_) = ast[children[1]].value else {
        return None
    };
    if let AstNode::Identifier(func_name) = &ast[unwrap_u(&ast[children[1]].children)[0]].value {
        // let arg_pos = unwrap_u(&ast[children[1]].children)[1];
        match func_name.as_str() {
            _ => { None }
        }
    } else { None }
}

fn built_in_funcs(
    ast: &[Ast], name: &str, indentation: usize,
    children: &[usize], _add_index: bool
) -> Option<String> {
    // let arg_pos = unwrap_u(&ast[children[1]].children)[1];
    match name {
        "reversed" => Some(format!(
            "reversed(list({}))", //3 pretty bad that I need to turn it into a list first
            to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
        )),
        "len" => Some(format!(
            "({}).getattr('__len__')()",
            to_python(ast, children[1], indentation, ToWrapVal::GetAsValue)
        )),
        "range" => {
            Some(format!(
                "range({})",
                to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
            ))
        }
        "iter" | "iter_imut" => Some(format!(
            "({}).getattr('_iter_')()",
            to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
        )),
        "min" | "max" | "abs" => Some(format!(
            "{name}({})",
            to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
        )),
        "sum" => Some(format!(
            "sum(x._dereference_all() if hasattr(x, '_dereference_all') else x for x in {})",
            to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
        )),
        "int" => Some(format!(
            "int(({})._dereference_all())",
            to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
        )),
        _ => { None }
    }
}
