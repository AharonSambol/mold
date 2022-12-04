use crate::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use std::iter::zip;
use crate::to_rust::{BIFunc, is_bif};
use crate::types::{unwrap_u};


pub fn to_python(ast: &Vec<Ast>, pos: usize, indentation: usize, res: &mut String) {
    let children = unwrap_u(&ast[pos].children);

    match &ast[pos].value {
        AstNode::Module | AstNode::Body => {
            for child in children {
                write!(res, "\n{}", "\t".repeat(indentation)).unwrap();
                to_python(ast, *child, indentation, res);
            }
        },
        AstNode::Function(name) => {
            write!(res, "def {}", name).unwrap();
            write!(res, "(").unwrap();
            to_python(ast, children[0], indentation, res); // param
            write!(res, ")").unwrap();
            to_python(ast, children[1], indentation, res); // return
            write!(res, ":").unwrap();
            to_python(ast, children[2], indentation + 1, res); // body
        },
        AstNode::StaticFunction(name) => {
            write!(res, "@staticmethod\n{}def {}", "\t".repeat(indentation), name).unwrap();
            write!(res, "(").unwrap();
            to_python(ast, children[0], indentation, res); // param
            write!(res, ")").unwrap();
            to_python(ast, children[1], indentation, res); // return
            write!(res, ":").unwrap();
            to_python(ast, children[2], indentation + 1, res); // body
        },
        AstNode::ReturnType => {},
        AstNode::IfStatement => {
            write!(res, "if ").unwrap();
            to_python(ast, children[0], indentation, res);
            to_python(ast, children[1], indentation + 1, res);
            for child in children.iter().skip(2){
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        write!(res, "\n{}elif ", "\t".repeat(indentation)).unwrap();
                        let c_children = if let Some(x) = &ast[*child].children { x } else { panic!() };
                        to_python(ast, c_children[0], indentation, res);
                        to_python(ast, c_children[1], indentation + 1, res);
                    },
                    AstNode::Module => {
                        write!(res, "\n{}else:", "\t".repeat(indentation)).unwrap();
                        to_python(ast, *child, indentation + 1, res);
                    },
                    _ => panic!()
                }
            }
        },
        AstNode::WhileStatement => {
            write!(res, "while ").unwrap();
            to_python(ast, children[0], indentation, res);
            to_python(ast, children[1], indentation + 1, res);
        },
        AstNode::Assignment | AstNode::FirstAssignment => {
            let children = children;
            to_python(ast, children[0], indentation, res);
            write!(res, "=").unwrap();
            to_python(ast, children[1], indentation, res);
        },
        AstNode::Identifier(name) => {
            write!(res, "{}", name).unwrap();
        },
        AstNode::Operator(op) => {
            to_python(ast, children[0], indentation, res);
            write!(res, " {} ", op.to_string()).unwrap();
            to_python(ast, children[1], indentation, res);
        },
        AstNode::UnaryOp(op) => {
            write!(res, " {}", op.to_string()).unwrap();
            to_python(ast, children[0], indentation, res);
        },
        AstNode::Parentheses => {
            write!(res, "(").unwrap();
            to_python(ast, children[0], indentation, res);
            write!(res, ")").unwrap();
        },
        AstNode::ColonParentheses => {
            for child in children {
                to_python(ast, *child, indentation, res);
            }
            write!(res, ":").unwrap();
        },
        AstNode::Index => {
            to_python(ast, children[0], indentation, res);
            write!(res, "[").unwrap();
            to_python(ast, children[1], indentation, res);
            write!(res, "]").unwrap();
        },
        AstNode::Number(num) => write!(res, "{}", num).unwrap(),
        AstNode::String(str) => write!(res, "{}", str).unwrap(),
        AstNode::Char(chr) => write!(res, "{}", chr).unwrap(),
        AstNode::Bool(b) => write!(res, "{}", if *b { "True" } else { "False" }).unwrap(),
        AstNode::ListLiteral => {
            write!(res, "[").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                to_python(ast, *child, indentation + 1, res);
            }
            write!(res, "]").unwrap();

        },
        AstNode::Pass => {
            write!(res, "pass").unwrap();
        },
        AstNode::FunctionCall(is_static) => {
            match is_bif(&ast[children[0]].value) {
                Some(BIFunc::Rev) => write!(res, "reversed(").unwrap(),
                Some(BIFunc::DPrint) => write!(res, "print(").unwrap(),
                _ => {
                    to_python(ast, children[0], indentation, res);
                    write!(res, "(").unwrap();
                }
            }
            if children.len() > 1 {
                to_python(ast, children[1], indentation, res);
            }
            write!(res, ")").unwrap();
        },
        AstNode::Args | AstNode::ArgsDef => {
            if children.len() == 0 {
                return;
            }
            to_python(ast, children[0], indentation, res);
            for child in children.iter().skip(1) {
                write!(res, ",").unwrap();
                to_python(ast, *child, indentation, res);
            }
        },
        AstNode::Return => {
            write!(res, "return ").unwrap();
            if children.len() != 0 {
                to_python(ast, children[0], indentation, res);
            }
        },
        AstNode::Property => {
            to_python(ast, children[0], indentation, res);
            write!(res, ".").unwrap();
            to_python(ast, children[1], indentation, res);
        },
        AstNode::ForStatement => {
            write!(res, "for ").unwrap();
            to_python(ast, children[0], indentation, res);
            to_python(ast, children[1], indentation + 1, res);
        },
        AstNode::ForVars => {
            to_python(ast, children[0], indentation, res);
            for child in children.iter().skip(1) {
                write!(res, ", ").unwrap();
                to_python(ast, *child, indentation, res);
            }
        },
        AstNode::ForIter => {
            write!(res, " in ").unwrap();
            to_python(ast, children[0], indentation, res);
        },
        AstNode::StructInit => {
            to_python(ast, children[0], indentation, res);
            write!(res, "(").unwrap();
            let arg_vals = unwrap_u(&ast[children[1]].children);
            for val in arg_vals {
                to_python(ast, *val, indentation, res);
                write!(res, ", ").unwrap();
            }
            write!(res, ")").unwrap();
        }
        AstNode::Struct(name) => {
            let param = &ast[children[0]];
            let param = unwrap_u(&param.children).iter()
                .map(|&x|
                    if let AstNode::Identifier(n) = &ast[x].value { n } else { panic!() }
                ).collect();
            let param_comma = join(&param, ", ");
            let param_assign = join(&param.iter().map(|x| format!("self.{} = {}", x, x)).collect(), "\n\t\t");
            write!(res, "class {}:\n\tdef __init__(self, {}):\n\t\t{}", name, param_comma, param_assign).unwrap();
            to_python(ast, children[1], indentation + 1, res);
        }
        _ => panic!("Unexpected AST {:?}", ast[pos].value)
    }
}
