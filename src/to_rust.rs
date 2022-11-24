use std::collections::HashMap;
use crate::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use crate::mold_tokens::OperatorType;
use crate::types::{Type, TypeKind, unwrap, unwrap_u};


pub fn to_rust(ast: &Vec<Ast>, pos: usize, indentation: usize, res: &mut String, enums: &mut HashMap<String, String>) {
    let children = unwrap_u(&ast[pos].children);

    match &ast[pos].value {
        AstNode::Module => {
            let indent = "\t".repeat(indentation);
            write!(res, "\n{}", indent).unwrap();
            for child in children {
                write!(res, "\n{}", indent).unwrap();
                to_rust(ast, *child, indentation, res, enums);
            }
            write!(res, "\n{}", indent).unwrap();
        },
        AstNode::Body => {
            let indent = "\t".repeat(indentation);
            write!(res, "\n{}{{", indent).unwrap();
            for child in children {
                write!(res, "\n{}", indent).unwrap();
                to_rust(ast, *child, indentation, res, enums);
                write!(res, ";").unwrap();
            }
            write!(res, "\n{}}}", indent).unwrap();
        },
        AstNode::Function(func) => {
            for par in &func.params {
                make_enums(&par.typ, enums);
            }
            if let Some(return_type) = &func.return_type {
                write!(res, "fn {}({}) -> {} {{", func.name, join(&func.params, ", "), return_type).unwrap();
            } else {
                write!(res, "fn {}({}) {{", func.name, join(&func.params, ", ")).unwrap();
            }
            for child in children {
                write!(res, "\n{}", "\t".repeat(indentation + 1)).unwrap();
                to_rust(ast, *child, indentation + 1, res, enums);
                write!(res, ";").unwrap();
            }
            write!(res, "\n{}}}", "\t".repeat(indentation)).unwrap();
        },
        AstNode::IfStatement => {
            write!(res, "if ").unwrap();
            to_rust(ast, children[0], indentation, res, enums);
            to_rust(ast, children[1], indentation + 1, res, enums);
            for child in children.iter().skip(2){
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        write!(res, "\n{}else if ", "\t".repeat(indentation)).unwrap();
                        let c_children = if let Some(x) = &ast[*child].children { x } else { panic!() };
                        to_rust(ast, c_children[0], indentation, res, enums);
                        to_rust(ast, c_children[1], indentation + 1, res, enums);
                    },
                    AstNode::Module => {
                        write!(res, "\n{}else", "\t".repeat(indentation)).unwrap();
                        to_rust(ast, *child, indentation + 1, res, enums);
                    },
                    _ => panic!()
                }
            }
        },
        AstNode::WhileStatement => {
            write!(res, "while ").unwrap();
            to_rust(ast, children[0], indentation, res, enums);
            to_rust(ast, children[1], indentation + 1, res, enums);
        },
        AstNode::Assignment => {
            if let Some(c) = &ast[children[0]].children {
                write!(res, "let ").unwrap();
                to_rust(ast, children[0], indentation, res, enums);
                write!(res, ":").unwrap();
                to_rust(ast, c[0], indentation, res, enums);
            } else {
                to_rust(ast, children[0], indentation, res, enums);
            }
            write!(res, "=").unwrap();
            to_rust(ast, children[1], indentation, res, enums);
        },
        AstNode::Identifier(name) => {
            write!(res, "{}", name).unwrap();
        },
        // todo floor div
        AstNode::Operator(op) => {
            if let OperatorType::FloorDiv = op {
                panic!()
            } else if let OperatorType::FloorDivEq = op {
                panic!()
            } else if let OperatorType::Pow = op {
                to_rust(ast, children[0], indentation, res, enums);
                write!(res, ".pow(").unwrap();
                to_rust(ast, children[1], indentation, res, enums);
                write!(res, ")").unwrap();
            } else {
                to_rust(ast, children[0], indentation, res, enums);
                write!(res, "{}", op.to_string()).unwrap();
                to_rust(ast, children[1], indentation, res, enums);
            }
        },
        AstNode::UnaryOp(op) => {
            let st = if let OperatorType::BinNot = op { String::from("!") } else { op.to_string() };
            write!(res, "{}", st).unwrap();
            to_rust(ast, children[0], indentation, res, enums);
        },
        AstNode::Parentheses => {
            write!(res, "(").unwrap();
            to_rust(ast, children[0], indentation, res, enums);
            write!(res, ")").unwrap();
        },
        AstNode::ColonParentheses => {
            to_rust(ast, children[0], indentation, res, enums);
        },
        AstNode::Index => {
            to_rust(ast, children[0], indentation, res, enums);
            write!(res, "[").unwrap();
            to_rust(ast, children[1], indentation, res, enums);
            write!(res, "]").unwrap();
        },
        AstNode::Number(num) => write!(res, "{}", num).unwrap(),
        AstNode::ListLiteral => {
            write!(res, "vec![").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                to_rust(ast, *child, indentation + 1, res, enums);
            }
            write!(res, "]").unwrap();

        },
        AstNode::Pass => {
            write!(res, "()").unwrap();
        },
        // 5 !!!!!!!!!!!!!!!!!!!!
        AstNode::Type(typ) => {
            make_enums(typ, enums);
            write!(res, "{}", typ).unwrap(); //todo
        },
        AstNode::FunctionCall => {
            to_rust(ast, children[0], indentation, res, enums);
            write!(res, "(").unwrap();
            if children.len() > 1 {
                to_rust(ast, children[1], indentation, res, enums);
            }
            write!(res, ")").unwrap();
        },
        AstNode::Args => {
            if children.len() == 0 {
                return;
            }
            to_rust(ast, children[0], indentation, res, enums);
            for child in children.iter().skip(1) {
                write!(res, ",").unwrap();
                to_rust(ast, *child, indentation, res, enums);
            }
        },
        _ => panic!("Unexpected AST {:?}", ast[pos].value)
    }
}

fn make_enums(typ: &Type, enums: &mut HashMap<String, String>){
    match &typ.kind {
        TypeKind::Unknown | TypeKind::Typ(_) => (),
        TypeKind::OneOf => {
            let types = unwrap(&typ.children);
            let enm = join(types, "-or-");
            if !enums.contains_key(&enm) {
                let elems = types
                    .iter()
                    .map(|x| format!("_{}: {}", x, x))
                    .collect::<Vec<String>>();
                enums.insert(
                    enm.clone(),
                    format!("enum {} {{ {} }}", enm, join(&elems , ","))
                );
            }
        },
        TypeKind::TypWithGenerics => {
            for child in unwrap(&typ.children).iter().skip(1) {
                make_enums(child, enums)
            }
        }
    }
}