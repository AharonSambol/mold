use crate::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
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
        AstNode::Function(func) => {
            write!(res, "def {}({}):", func.name, join(&func.params, ", ")).unwrap();
            for child in children {
                write!(res, "\n{}", "\t".repeat(indentation + 1)).unwrap();
                to_python(ast, *child, indentation + 1, res);
            }
        },
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
        AstNode::Assignment => {
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
            write!(res, "{}", op.to_string()).unwrap();
            to_python(ast, children[1], indentation, res);
        },
        AstNode::UnaryOp(op) => {
            write!(res, "{}", op.to_string()).unwrap();
            to_python(ast, children[0], indentation, res);
        },
        AstNode::Parentheses => {
            write!(res, "(").unwrap();
            to_python(ast, children[0], indentation, res);
            write!(res, ")").unwrap();
        },
        AstNode::ColonParentheses => {
            to_python(ast, children[0], indentation, res);
            write!(res, ":").unwrap();
        },
        AstNode::Index => {
            to_python(ast, children[0], indentation, res);
            write!(res, "[").unwrap();
            to_python(ast, children[1], indentation, res);
            write!(res, "]").unwrap();
        },
        AstNode::Number(num) => write!(res, "{}", num).unwrap(),
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
        AstNode::Type(typ) => {
            write!(res, "{}", typ).unwrap(); //todo
        },
        AstNode::FunctionCall => {
            to_python(ast, children[0], indentation, res);
            write!(res, "(").unwrap();
            if children.len() > 1 {
                to_python(ast, children[1], indentation, res);
            }
            write!(res, ")").unwrap();
        },
        AstNode::Args => {
            if children.len() == 0 {
                return;
            }
            to_python(ast, children[0], indentation, res);
            for child in children.iter().skip(1) {
                write!(res, ",").unwrap();
                to_python(ast, *child, indentation, res);
            }
        },
        _ => panic!("Unexpected AST {:?}", ast[pos].value)
    }
}
