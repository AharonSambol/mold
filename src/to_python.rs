use crate::construct_ast::ast_structure::{Ast, AstNode};
use std::fmt::Write;
use lazy_static::lazy_static;
use regex::Regex;
use crate::{EMPTY_STR, IGNORE_FUNCS, IGNORE_STRUCTS, unwrap_enum};
use crate::mold_tokens::OperatorType;
use crate::types::{unwrap_u};

lazy_static!{
    static ref NUM_TYP_RE: Regex = Regex::new(r"[uif]").unwrap();
}

pub enum ToWrapVal {
    Nothing,
    GetAsValue,
    GetInnerValue,
    GetName
}

pub fn to_python(
    ast: &[Ast], pos: usize, indentation: usize, add_val_wrapper: ToWrapVal
) -> String {
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
        AstNode::Function(name) => {
            let name = if name.contains("::") {
                name.split("::").last().unwrap().to_string()
            } else { name.clone() };
            if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
                return EMPTY_STR;
            }
            if indentation == 1 {
                format!(
"def {name}({}){}:
{}self = _value_(self){}",
                    to_python(ast, children[1], indentation, ToWrapVal::Nothing), // param
                    to_python(ast, children[2], indentation, ToWrapVal::Nothing), // return
                    "\t".repeat(indentation + 1),
                    to_python(ast, children[3], indentation + 1, ToWrapVal::Nothing) // body
                )
            } else {
                format!(
                    "def {name}({}){}:{}",
                    to_python(ast, children[1], indentation, ToWrapVal::Nothing), // param
                    to_python(ast, children[2], indentation, ToWrapVal::Nothing), // return
                    to_python(ast, children[3], indentation + 1, ToWrapVal::Nothing) // body
                )
            }
        },
        AstNode::StaticFunction(name) => {
            format!(
                "@staticmethod\n{}def {name}({}){}:{}",
                "\t".repeat(indentation),
                to_python(ast, children[1], indentation, ToWrapVal::Nothing), // param
                to_python(ast, children[2], indentation, ToWrapVal::Nothing), // return
                to_python(ast, children[3], indentation + 1, ToWrapVal::Nothing) // body
            )
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
                        let c_children = unwrap_enum!(&ast[*child].children);
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
                    _ => panic!()
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
            format!("{}={}",
                to_python(ast, children[0], indentation, ToWrapVal::GetName),
                to_python(ast, children[1], indentation, ToWrapVal::GetAsValue)
            )
        },
        AstNode::Assignment => {
            format!("{}.v = {}",
                to_python(ast, children[0], indentation, ToWrapVal::GetName),
                to_python(ast, children[1], indentation, ToWrapVal::GetInnerValue)
            )
        },
        AstNode::Identifier(name) => {
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetName | ToWrapVal::GetAsValue => String::from(name),
                ToWrapVal::GetInnerValue =>
                    String::from(remove_unnecessary_val_creation(&format!("{}.v", name)))
            }
        },
        AstNode::Operator(op) => {
            // let c1 = format!("{}.v", to_python(ast, children[0], indentation, false));
            // let c2 = format!("{}.v", to_python(ast, children[1], indentation, false));
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => format!(
                    "_value_({} {op} {})",
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
                    format!("_pointer_({})",
                        to_python(ast, children[0], indentation, ToWrapVal::GetName)
                    )
                }
                OperatorType::Pointer => {
                    format!("_pointer_({})",
                             to_python(ast, children[0], indentation, ToWrapVal::GetName)
                    )
                }
                OperatorType::Dereference => {

                    let r = format!("{}.p",
                         to_python(ast, children[0], indentation, ToWrapVal::GetName) //todo GetAsValue?
                    );
                    match add_val_wrapper {
                        ToWrapVal::Nothing => panic!(),
                        ToWrapVal::GetInnerValue => remove_unnecessary_val_creation(&format!("{r}.v")).to_string(),
                        ToWrapVal::GetAsValue => r,
                        ToWrapVal::GetName => r
                    }
                }
                _ => {
                    match add_val_wrapper {
                        ToWrapVal::Nothing => panic!(),
                        ToWrapVal::GetAsValue =>
                            format!("_value_({op}{})", to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue)),
                        ToWrapVal::GetName | ToWrapVal::GetInnerValue =>
                            format!("{op}{}", to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue)),
                    }
                }
            }
        },
        AstNode::Parentheses => {
            // todo this sometimes causes unneeded _value_() calls
            format!("({})",
                to_python(ast, children[0], indentation, add_val_wrapper)
            )
        },
        AstNode::ColonParentheses => {
            let mut res = String::new();
            for child in children {
                write!(res, "{}", to_python(ast, *child, indentation, ToWrapVal::GetInnerValue)).unwrap();
            }
            write!(res, ":").unwrap();
            res
        },
        AstNode::Index => {
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => format!(
                    "_value_({}[{}])",
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
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => format!("_value_({})", NUM_TYP_RE.split(num).next().unwrap()),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => NUM_TYP_RE.split(num).next().unwrap().to_string(),
            }
        },
        AstNode::String { val, .. } => {
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => format!("_value_({val})"),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => val.clone(),
            }
        },
        AstNode::Char(chr) => {
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => format!("_value_('{chr}')"),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!("'{chr}'"),
            }
        },
        AstNode::Bool(b) => {
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => String::from(if *b { "_value_(True)" } else { "_value_(False)" }),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => String::from(if *b { "True" } else { "False" }),
            }
        },
        AstNode::ListLiteral => {
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => {
                    let mut res = String::from("_value_([");
                    for (i, child) in children.iter().enumerate() {
                        if i != 0 {
                            write!(res, ", ").unwrap();
                        }
                        write!(res, "{}", to_python(ast, *child, indentation + 1, ToWrapVal::GetInnerValue)).unwrap();
                    }
                    write!(res, "])").unwrap();
                    res
                },
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => {
                    let mut res = String::from("[");
                    for (i, child) in children.iter().enumerate() {
                        if i != 0 {
                            write!(res, ", ").unwrap();
                        }
                        write!(res, "{}", to_python(ast, *child, indentation + 1, ToWrapVal::GetInnerValue)).unwrap();
                    }
                    write!(res, "]").unwrap();
                    res
                },
            }
        },
        AstNode::SetLiteral => {
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => {
                    let mut res = String::from("_value_({");
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
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => {
                    let mut res = String::from("_value_({");
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
        AstNode::Pass => { String::from("pass") },
        AstNode::FunctionCall(_) => {
            // TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if let Some(res) = built_in_funcs(ast, name, indentation, children, false) {
                    return res;
                }
            }

            let mut res = format!("{}(",
                to_python(ast, children[0], indentation, ToWrapVal::GetName)
            );
            if children.len() > 1 {
                write!(res, "{}", to_python(ast, children[1], indentation, ToWrapVal::Nothing)).unwrap();
            }
            write!(res, ")").unwrap();
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => format!("_value_({})", res),
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
            if children.is_empty() {
                return EMPTY_STR;
            }
            let mut res = to_python(
                ast, children[0], indentation, ToWrapVal::GetAsValue
            );
            for child in children.iter().skip(1) {
                write!(res, ", {}",
                       to_python(
                           ast, *child, indentation, ToWrapVal::GetAsValue
                       )
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
            // TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if let Some(res) = built_in_methods(ast, indentation, children, false) {
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
                            ast, unwrap_u(&ast[children[1]].children)[1], indentation, ToWrapVal::GetName
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
                    format!("_value_({res})")
                } else {
                    res
                }
            }
            let base = to_python(ast, children[0], indentation, ToWrapVal::GetInnerValue);
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => format!(
                    "_value_({}.{})",
                    base,
                    to_python(ast, children[1], indentation, ToWrapVal::GetName)
                ),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => format!(
                    "{}.{}",
                    base,
                    to_python(ast, children[1], indentation, ToWrapVal::GetName)
                ),
            }
        },
        AstNode::ForStatement => {
            // TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            let par = to_python(ast, children[0], indentation, ToWrapVal::Nothing);
            let vars = to_python(ast, unwrap_u(&ast[children[0]].children)[0], indentation, ToWrapVal::Nothing);
            format!("for {par}\n{}({vars},)=map(_value_, ({vars},)){}",
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
        AstNode::StructInit => {
            let mut res = format!("{}(",
                to_python(ast, children[0], indentation, ToWrapVal::GetName)
            );
            let arg_vals = unwrap_u(&ast[children[1]].children);
            for (i, val) in arg_vals.iter().enumerate() {
                if i == 0 {
                    write!(res, "{}",
                           to_python(ast, *val, indentation, ToWrapVal::GetAsValue)
                    ).unwrap();
                } else {
                    write!(res, ", {}",
                           to_python(ast, *val, indentation, ToWrapVal::GetAsValue)
                    ).unwrap();
                }
            }
            write!(res, ")").unwrap();
            match add_val_wrapper {
                ToWrapVal::Nothing => panic!(),
                ToWrapVal::GetAsValue => format!("_value_({res})"),
                ToWrapVal::GetName | ToWrapVal::GetInnerValue => res,
            }
        }
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
        AstNode::GenericsDeclaration | AstNode::Trait { .. } => EMPTY_STR,
        AstNode::Enum(_name) => {
            todo!()
            // TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        }
        _ => panic!("Unexpected AST {:?}", ast[pos].value)
    }
}

fn remove_unnecessary_val_creation(st: &str) -> &str {
    if st.starts_with("_value_(") && st.ends_with(").v") {
        panic!("just checking if this is ever useful, if so- remove this panic");
        // let end = st.len() - ").v".len();
        // return &st["_value_(".len()..end]
    }
    st
}

fn built_in_methods(
    ast: &[Ast], indentation: usize, children: &[usize], _add_index: bool
) -> Option<String> {
    let AstNode::FunctionCall(_) = ast[children[1]].value else {
        return None
    };
    if let AstNode::Identifier(func_name) = &ast[unwrap_u(&ast[children[1]].children)[0]].value {
        // let arg_pos = unwrap_u(&ast[children[1]].children)[1];
        match func_name.as_str() {
            "iter" => {
                //1 ignores it
                Some(to_python(ast, children[0], indentation, todo!()))
            }

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
        "reversed" => {
            Some(format!("{}.rev()",
                 to_python(ast, children[1], indentation, todo!())
            ))
        }
        _ => { None }
    }
}
