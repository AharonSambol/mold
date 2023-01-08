use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use lazy_static::lazy_static;
use regex::Regex;
use crate::built_in_funcs::BuiltIn;
use crate::{IGNORE_FUNCS, IGNORE_STRUCTS, unwrap_enum};
use crate::types::{unwrap_u};

lazy_static!{
    static ref NUM_TYP_RE: Regex = Regex::new(r"[uif]").unwrap();
}

pub fn to_python(ast: &[Ast], pos: usize, indentation: usize, res: &mut String, built_ins: &HashMap<&str, Box<dyn BuiltIn>>) {
    let children = unwrap_u(&ast[pos].children);
    match &ast[pos].value {
        AstNode::Module | AstNode::Body => {
            for child in children {
                write!(res, "\n{}", "\t".repeat(indentation)).unwrap();
                to_python(ast, *child, indentation, res, built_ins);
            }
        },
        AstNode::Function(name) => {
            let name = if name.contains("::") {
                name.split("::").last().unwrap().to_string()
            } else { name.clone() };
            if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
                return;
            }
            write!(res, "def {name}").unwrap();
            write!(res, "(").unwrap();
            to_python(ast, children[1], indentation, res, built_ins); // param
            write!(res, ")").unwrap();
            to_python(ast, children[2], indentation, res, built_ins); // return
            write!(res, ":").unwrap();
            to_python(ast, children[3], indentation + 1, res, built_ins); // body
        },
        AstNode::StaticFunction(name) => {
            write!(res, "@staticmethod\n{}def {name}", "\t".repeat(indentation)).unwrap();
            write!(res, "(").unwrap();
            to_python(ast, children[1], indentation, res, built_ins); // param
            write!(res, ")").unwrap();
            to_python(ast, children[2], indentation, res, built_ins); // return
            write!(res, ":").unwrap();
            to_python(ast, children[3], indentation + 1, res, built_ins); // body
        },
        AstNode::ReturnType => {},
        AstNode::IfStatement => {
            write!(res, "if ").unwrap();
            to_python(ast, children[0], indentation, res, built_ins);
            to_python(ast, children[1], indentation + 1, res, built_ins);
            for child in children.iter().skip(2){
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        write!(res, "\n{}elif ", "\t".repeat(indentation)).unwrap();
                        let c_children = unwrap_enum!(&ast[*child].children);
                        to_python(ast, c_children[0], indentation, res, built_ins);
                        to_python(ast, c_children[1], indentation + 1, res, built_ins);
                    },
                    AstNode::Body => {
                        write!(res, "\n{}else:", "\t".repeat(indentation)).unwrap();
                        to_python(ast, *child, indentation + 1, res, built_ins);
                    },
                    _ => panic!()
                }
            }
        },
        AstNode::WhileStatement => {
            write!(res, "while ").unwrap();
            to_python(ast, children[0], indentation, res, built_ins);
            to_python(ast, children[1], indentation + 1, res, built_ins);
        },
        AstNode::Assignment | AstNode::FirstAssignment => {
            let children = children;
            to_python(ast, children[0], indentation, res, built_ins);
            write!(res, "=").unwrap();
            to_python(ast, children[1], indentation, res, built_ins);
        },
        AstNode::Identifier(name) => {
            write!(res, "{name}").unwrap();
        },
        AstNode::Operator(op) => {
            to_python(ast, children[0], indentation, res, built_ins);
            write!(res, " {} ", op).unwrap();
            to_python(ast, children[1], indentation, res, built_ins);
        },
        AstNode::UnaryOp(op) => {
            write!(res, " {}", op).unwrap();
            to_python(ast, children[0], indentation, res, built_ins);
        },
        AstNode::Parentheses => {
            write!(res, "(").unwrap();
            to_python(ast, children[0], indentation, res, built_ins);
            write!(res, ")").unwrap();
        },
        AstNode::ColonParentheses => {
            for child in children {
                to_python(ast, *child, indentation, res, built_ins);
            }
            write!(res, ":").unwrap();
        },
        AstNode::Index => {
            to_python(ast, children[0], indentation, res, built_ins);
            write!(res, "[").unwrap();
            to_python(ast, children[1], indentation, res, built_ins);
            write!(res, "]").unwrap();
        },
        AstNode::Number(num) => write!(res, "{}", NUM_TYP_RE.split(num).next().unwrap()).unwrap(),
        AstNode::String { val, .. } => write!(res, "{val}").unwrap(),
        AstNode::Char(chr) => write!(res, "'{chr}'").unwrap(),
        AstNode::Bool(b) => write!(res, "{}", if *b { "True" } else { "False" }).unwrap(),
        AstNode::ListLiteral => {
            write!(res, "[").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                to_python(ast, *child, indentation + 1, res, built_ins);
            }
            write!(res, "]").unwrap();

        },
        AstNode::SetLiteral => {
            write!(res, "{{").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                to_python(ast, *child, indentation + 1, res, built_ins);
            }
            write!(res, "}}").unwrap();

        },
        AstNode::DictLiteral => {
            write!(res, "{{").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    if i % 2 == 0 {
                        write!(res, ", ").unwrap();
                    } else {
                        write!(res, ": ").unwrap();
                    }
                }
                to_python(ast, *child, indentation + 1, res, built_ins);
            }
            write!(res, "}}").unwrap();

        },
        AstNode::Pass => {
            write!(res, "pass").unwrap();
        },
        AstNode::FunctionCall(_) => {
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if built_ins.contains_key(name.as_str()) {
                    built_ins[name.as_str()].to_str_python(ast, res, children, built_ins);
                    return;
                }
                if built_in_funcs(ast, name, indentation, res, built_ins, children) {
                    return;
                }
            }

            to_python(ast, children[0], indentation, res, built_ins);
            write!(res, "(").unwrap();
            if children.len() > 1 {
                to_python(ast, children[1], indentation, res, built_ins);
            }
            write!(res, ")").unwrap();
        },
        AstNode::ArgsDef => {
            if children.is_empty() {
                return;
            }
            to_python(ast, children[0], indentation, res, built_ins);
            for child in children.iter().skip(1) {
                write!(res, ",").unwrap();
                to_python(ast, *child, indentation, res, built_ins);
            }
        }
        AstNode::Args => {
            if children.is_empty() {
                return;
            }
            write!(res, "__cpy_strct(").unwrap();
            to_python(ast, children[0], indentation, res, built_ins);
            write!(res, ")").unwrap();
            for child in children.iter().skip(1) {
                write!(res, ",").unwrap();
                write!(res, "__cpy_strct(").unwrap();
                to_python(ast, *child, indentation, res, built_ins);
                write!(res, ")").unwrap();
            }
        },
        AstNode::Return => {
            write!(res, "return ").unwrap();
            if !children.is_empty() {
                to_python(ast, children[0], indentation, res, built_ins);
            }
        },
        AstNode::Property => {
            if built_in_methods(ast, indentation, res, built_ins, children) {
                return;
            }
            to_python(ast, children[0], indentation, res, built_ins);
            write!(res, ".").unwrap();
            to_python(ast, children[1], indentation, res, built_ins);
        },
        AstNode::ForStatement => {
            write!(res, "for ").unwrap();
            to_python(ast, children[0], indentation, res, built_ins);
            to_python(ast, children[1], indentation + 1, res, built_ins);
        },
        AstNode::ForVars => {
            to_python(ast, children[0], indentation, res, built_ins);
            for child in children.iter().skip(1) {
                write!(res, ", ").unwrap();
                to_python(ast, *child, indentation, res, built_ins);
            }
        },
        AstNode::ForIter => {
            write!(res, " in ").unwrap();
            to_python(ast, children[0], indentation, res, built_ins);
        },
        AstNode::StructInit => {
            to_python(ast, children[0], indentation, res, built_ins);
            write!(res, "(").unwrap();
            let arg_vals = unwrap_u(&ast[children[1]].children);
            for val in arg_vals {
                to_python(ast, *val, indentation, res, built_ins);
                write!(res, ", ").unwrap();
            }
            write!(res, ")").unwrap();
        }
        AstNode::Struct(name) => {
            if unsafe { IGNORE_STRUCTS.contains(name.as_str()) } {
                return;
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
            write!(res,
"class {name}:
\tdef __init__(self, {param_comma}):
\t\tself._is_STRUCT__ = True
\t\t{param_assign}").unwrap();
            to_python(ast, children[2], indentation + 1, res, built_ins);
        }
        AstNode::Continue => write!(res, "continue").unwrap(),
        AstNode::Break => write!(res, "break").unwrap(),
        AstNode::GenericsDeclaration | AstNode::Trait(_) => {},
        _ => panic!("Unexpected AST {:?}", ast[pos].value)
    }
}


fn built_in_methods(
    ast: &[Ast], indentation: usize, res: &mut String,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    children: &[usize]
) -> bool {
    if let AstNode::Identifier(func_name) = &ast[unwrap_u(&ast[children[1]].children)[0]].value {
        // let arg_pos = unwrap_u(&ast[children[1]].children)[1];
        match func_name.as_str() {
            "iter" => {
                //1 ignores it
                to_python(ast, children[0], indentation, res, built_ins);
            }

            _ => { return false }
        }
        true
    } else { false }
}

fn built_in_funcs(
    ast: &[Ast], name: &str, indentation: usize, res: &mut String,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    children: &[usize]
) -> bool {
    // let arg_pos = unwrap_u(&ast[children[1]].children)[1];
    match name {
        "reversed" => {
            to_python(ast, children[1], indentation, res, built_ins);
            write!(res, ".rev()").unwrap();
        }
        _ => { return false }
    }
    true
}
