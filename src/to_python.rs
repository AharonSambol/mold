use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use lazy_static::lazy_static;
use regex::Regex;
use crate::built_in_funcs::BuiltIn;
use crate::{IGNORE_FUNCS, IGNORE_STRUCTS, unwrap_enum};
use crate::mold_tokens::OperatorType;
use crate::types::{unwrap_u};

lazy_static!{
    static ref NUM_TYP_RE: Regex = Regex::new(r"[uif]").unwrap();
}

// TODO remove all the places where it says _value_(...).v  (to just ... )
pub fn to_python(
    ast: &[Ast], pos: usize, indentation: usize,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>, add_index: bool
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
                    to_python(ast, *child, indentation, built_ins, true)
                ).unwrap();
            }
            res
        },
        AstNode::Function(name) => {
            let name = if name.contains("::") {
                name.split("::").last().unwrap().to_string()
            } else { name.clone() };
            if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
                return String::new();
            }
            if indentation == 1 {
                format!(
"def {name}({}){}:
{}self = _value_(self){}",
                    to_python(ast, children[1], indentation, built_ins, false), // param
                    to_python(ast, children[2], indentation, built_ins, false), // return
                    "\t".repeat(indentation + 1),
                    to_python(ast, children[3], indentation + 1, built_ins, true) // body
                )
            } else {
                format!(
                    "def {name}({}){}:{}",
                    to_python(ast, children[1], indentation, built_ins, false), // param
                    to_python(ast, children[2], indentation, built_ins, false), // return
                    to_python(ast, children[3], indentation + 1, built_ins, true) // body
                )
            }
        },
        AstNode::StaticFunction(name) => {
            format!(
                "@staticmethod\n{}def {name}({}){}:{}",
                "\t".repeat(indentation),
                to_python(ast, children[1], indentation, built_ins, false), // param
                to_python(ast, children[2], indentation, built_ins, false), // return
                to_python(ast, children[3], indentation + 1, built_ins, true) // body
            )
        },
        AstNode::ReturnType => { String::new() },
        AstNode::IfStatement => {
            let mut res = format!("if {}{}",
                to_python(ast, children[0], indentation, built_ins, true), //1 ():
                to_python(ast, children[1], indentation + 1, built_ins, true) //1 body
            );
            for child in children.iter().skip(2){
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        let c_children = unwrap_enum!(&ast[*child].children);
                        write!(
                            res, "\n{}elif {}{}",
                            "\t".repeat(indentation),
                            to_python(ast, c_children[0], indentation, built_ins, true), //1 ():
                            to_python(ast, c_children[1], indentation + 1, built_ins, true) //1 body
                        ).unwrap();
                    },
                    AstNode::Body => {
                        write!(
                            res, "\n{}else:{}",
                            "\t".repeat(indentation),
                            to_python(ast, *child, indentation + 1, built_ins, true) //1 body
                        ).unwrap();
                    },
                    _ => panic!()
                }
            }
            res
        },
        AstNode::WhileStatement => {
            format!("while {}{}",
                to_python(ast, children[0], indentation, built_ins, true), //1 ():
                to_python(ast, children[1], indentation + 1, built_ins, true) //1 body
            )
        },
        AstNode::FirstAssignment => {
            format!("{}={}",
                to_python(ast, children[0], indentation, built_ins, false),
                to_python(ast, children[1], indentation, built_ins, true)
            )
        },
        AstNode::Assignment => {
            let val = format!("{}.v", to_python(ast, children[1], indentation, built_ins, true));

            format!("{}.v = {}",
                to_python(ast, children[0], indentation, built_ins, false),
                remove_unnecessary_val_creation(&val)
            )
        },
        AstNode::Identifier(name) => { String::from(name) },
        AstNode::Operator(op) => {
            let c1 = format!("{}.v", to_python(ast, children[0], indentation, built_ins, true));
            let c2 = format!("{}.v", to_python(ast, children[1], indentation, built_ins, true));
            format!(
                "_value_({} {op} {})",
                remove_unnecessary_val_creation(&c1),
                remove_unnecessary_val_creation(&c2)
            )
        },
        AstNode::UnaryOp(op) => {
            match op {
                OperatorType::MutPointer => {
                    format!("_value_(_pointer_({}))",
                        to_python(ast, children[0], indentation, built_ins, false)
                    )
                }
                OperatorType::Pointer => {
                    format!("_value_(_pointer_({}))",
                             to_python(ast, children[0], indentation, built_ins, false)
                    )
                }
                OperatorType::Dereference => {
                    let r = format!("{}.v",
                             to_python(ast, children[0], indentation, built_ins, false)
                    );
                    format!("{}.p", remove_unnecessary_val_creation(&r))
                }
                _ => {
                    let r = format!("{}.v",
                        to_python(ast, children[0], indentation, built_ins, true)
                    );
                    format!("_value_({op} {})", remove_unnecessary_val_creation(&r))
                }
            }
        },
        AstNode::Parentheses => {
            // todo this sometimes causes unneeded _value_() calls
            format!("({})",
                to_python(ast, children[0], indentation, built_ins, add_index)
            )
        },
        AstNode::ColonParentheses => {
            let mut res = String::new();
            for child in children {
                write!(res, "{}", to_python(ast, *child, indentation, built_ins, true)).unwrap();
            }
            write!(res, ":").unwrap();
            res
        },
        AstNode::Index => {
            let r = format!("{}.v",
                to_python(ast, children[0], indentation, built_ins, true)
            );
            format!("_value_({}.v[{}])",
                remove_unnecessary_val_creation(&r),
                to_python(ast, children[1], indentation, built_ins, true)
            )
        },
        AstNode::Number(num) => format!("_value_({})", NUM_TYP_RE.split(num).next().unwrap()),
        AstNode::String { val, .. } => format!("_value_({val})"),
        AstNode::Char(chr) => format!("_value_('{chr}')"),
        AstNode::Bool(b) => String::from(if *b { "_value_(True)" } else { "_value_(False)" }),
        AstNode::ListLiteral => {
            let mut res = String::from("_value_([");
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                let r = format!("{}.v", to_python(ast, *child, indentation + 1, built_ins, true));
                write!(res, "{}", remove_unnecessary_val_creation(&r)).unwrap();
            }
            write!(res, "])").unwrap();
            res
        },
        AstNode::SetLiteral => {
            let mut res = String::from("_value_({{");
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                write!(res, "{}", to_python(ast, *child, indentation + 1, built_ins, true)).unwrap();
            }
            write!(res, "}})").unwrap();
            res
        },
        AstNode::DictLiteral => {
            let mut res = String::from("_value_({{");
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    if i % 2 == 0 {
                        write!(res, ", ").unwrap();
                    } else {
                        write!(res, ": ").unwrap();
                    }
                }
                write!(res, "{}", to_python(ast, *child, indentation + 1, built_ins, true)).unwrap();
            }
            write!(res, "}})").unwrap();
            res
        },
        AstNode::Pass => { String::from("pass") },
        AstNode::FunctionCall(_) => {
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if built_ins.contains_key(name.as_str()) {
                    return built_ins[name.as_str()].to_str_python(ast, children, built_ins);
                }
                if let Some(res) = built_in_funcs(ast, name, indentation, built_ins, children, add_index) {
                    return res;
                }
            }

            let mut res = format!("{}(",
                to_python(ast, children[0], indentation, built_ins, false)
            );
            if children.len() > 1 {
                write!(res, "{}", to_python(ast, children[1], indentation, built_ins, false)).unwrap();
            }
            write!(res, ")").unwrap();
            res
        },
        AstNode::ArgsDef => {
            if children.is_empty() {
                return String::new();
            }
            let mut res = to_python(ast, children[0], indentation, built_ins, false);
            // write!(res, "{}", unwrap_enum!(&ast[children[0]].value, AstNode::Identifier(name), name)).unwrap();
            for child in children.iter().skip(1) {
                // write!(res, ",{}", unwrap_enum!(&ast[*child].value, AstNode::Identifier(name), name)).unwrap();
                write!(res, "{}", to_python(ast, *child, indentation, built_ins, false)).unwrap();
            }
            res
        }
        AstNode::Args => {
            if children.is_empty() {
                return String::new();
            }
            let r = format!("{}.v", to_python(ast, children[0], indentation, built_ins, false));
            let mut res = format!("_value_(__cpy_strct({}))",
                remove_unnecessary_val_creation(&r)
            );
            for child in children.iter().skip(1) {
                let r = format!("{}.v", to_python(ast, *child, indentation, built_ins, false));
                write!(res, ", _value_(__cpy_strct({}))",
                    remove_unnecessary_val_creation(&r)
                ).unwrap();
            }
            res
        },
        AstNode::Return => {
            if children.is_empty() { String::from("return") }
            else {
                let r = format!("{}.v", to_python(ast, children[0], indentation, built_ins, false));
                format!("return _value_({})",
                    remove_unnecessary_val_creation(&r)
                )
            }
        },
        AstNode::Property => {
            if let Some(res) = built_in_methods(ast, indentation, built_ins, children, add_index) {
                return res;
            }
            let base = format!("{}.v", to_python(ast, children[0], indentation, built_ins, true));
            format!("_value_({}.{}.v)",
                remove_unnecessary_val_creation(&base),
                to_python(ast, children[1], indentation, built_ins, true)
            )
        },
        AstNode::ForStatement => {
            format!("for {}{}",
                to_python(ast, children[0], indentation, built_ins, false),
                to_python(ast, children[1], indentation + 1, built_ins, true)
            )
        },
        AstNode::ForVars => {
            let mut res = to_python(ast, children[0], indentation, built_ins, false);
            for child in children.iter().skip(1) {
                write!(res, ", {}",
                    to_python(ast, *child, indentation, built_ins, false)
                ).unwrap();
            }
            res
        },
        AstNode::ForIter => {
            let r = format!("{}.v",
                to_python(ast, children[0], indentation, built_ins, true)
            );
            format!(" in {}", remove_unnecessary_val_creation(&r))
        },
        AstNode::StructInit => {
            let mut res = format!("_value_({}(",
                to_python(ast, children[0], indentation, built_ins, false)
            );
            let arg_vals = unwrap_u(&ast[children[1]].children);
            for (i, val) in arg_vals.iter().enumerate() {
                if i == 0 {
                    write!(res, "{}",
                           to_python(ast, *val, indentation, built_ins, true)
                    ).unwrap();
                } else {
                    write!(res, ", {}",
                           to_python(ast, *val, indentation, built_ins, true)
                    ).unwrap();
                }
            }
            write!(res, "))").unwrap();
            res
        }
        AstNode::Struct(name) => {
            if unsafe { IGNORE_STRUCTS.contains(name.as_str()) } {
                return String::new();
            }
            let param = &ast[children[1]];
            let param: Vec<&String> = unwrap_u(&param.children).iter()
                .map(|&x|
                    unwrap_enum!(&ast[x].value, AstNode::Identifier(n), n)
                ).collect();
            let param_comma = join(param.iter(), ", ");
            let param_assign = join(param.iter().map(
                |&x| format!("self.{x} = {x}")
            ), "\n\t\t");
            format!(
"class {name}:
\tdef __init__(self, {param_comma}):
\t\tself._is_STRUCT__ = True
\t\t{param_assign} {}",
                to_python(ast, children[2], indentation + 1, built_ins, true)
            )
        }
        AstNode::Continue => String::from("continue"),
        AstNode::Break => String::from("break"),
        AstNode::GenericsDeclaration | AstNode::Trait { .. } => String::new(),
        _ => panic!("Unexpected AST {:?}", ast[pos].value)
    }
}

fn remove_unnecessary_val_creation(st: &str) -> &str {
    if st.starts_with("_value_(") && st.ends_with(").v") {
        let end = st.len() - ").v".len();
        return &st["_value_(".len()..end]
    }
    st
}

fn built_in_methods(
    ast: &[Ast], indentation: usize,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    children: &[usize], add_index: bool
) -> Option<String> {
    let AstNode::FunctionCall(_) = ast[children[1]].value else {
        return None
    };
    if let AstNode::Identifier(func_name) = &ast[unwrap_u(&ast[children[1]].children)[0]].value {
        // let arg_pos = unwrap_u(&ast[children[1]].children)[1];
        match func_name.as_str() {
            "iter" => {
                //1 ignores it
                Some(to_python(ast, children[0], indentation, built_ins, add_index))
            }

            _ => { None }
        }
    } else { None }
}

fn built_in_funcs(
    ast: &[Ast], name: &str, indentation: usize,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    children: &[usize], add_index: bool
) -> Option<String> {
    // let arg_pos = unwrap_u(&ast[children[1]].children)[1];
    match name {
        "reversed" => {
            Some(format!("{}.rev()",
                to_python(ast, children[1], indentation, built_ins, add_index)
            ))
        }
        _ => { None }
    }
}
