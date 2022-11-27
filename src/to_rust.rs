use std::collections::HashMap;
use crate::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use crate::mold_tokens::OperatorType;
use crate::types::{Type, TypeKind, unwrap, unwrap_u};


pub fn to_rust(
    ast: &Vec<Ast>, pos: usize, indentation: usize, res: &mut String, 
    enums: &mut HashMap<String, String>
) {
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
            write!(res, " {{").unwrap();
            for child in children {
                write!(res, "\n{}", indent).unwrap();
                to_rust(ast, *child, indentation, res, enums);
                write!(res, ";").unwrap();
            }
            write!(res, "\n{}}}", "\t".repeat(indentation - 1)).unwrap();
        },
        AstNode::Function(func) => {
            for par in &func.params {
                make_enums(&par.typ, enums);
            }
            let mut param = join(&func.params, ", mut ");
            if param != "" {
                param = format!("mut {}", param);
            }
            if let Some(return_type) = &func.return_type {
                write!(res, "fn {}({}) -> {} {{", func.name, param, return_type).unwrap();
            } else {
                write!(res, "fn {}({}) {{", func.name, param).unwrap();
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
            to_rust(ast, children[0], indentation, res, enums);
            write!(res, "=").unwrap();
            to_rust(ast, children[1], indentation, res, enums);
            // todo return type of variable
        },
        AstNode::FirstAssignment => {
            write!(res, "let mut ").unwrap();
            to_rust(ast, children[0], indentation, res, enums);
            if let Some(c) = &ast[children[0]].children {
                write!(res, ": ").unwrap();
                to_rust(ast, c[0], indentation, res, enums);
            }
            write!(res, " = ").unwrap();
            to_rust(ast, children[1], indentation, res, enums);
            // todo return type here
        }
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
                write!(res, " {} ", op.to_string()).unwrap();
                to_rust(ast, children[1], indentation, res, enums);
            }
        },
        AstNode::UnaryOp(op) => {
            let st = if let OperatorType::BinNot = op { String::from("!") } else { op.to_string() };
            write!(res, " {}", st).unwrap();
            to_rust(ast, children[0], indentation, res, enums);
        },
        AstNode::Parentheses => {
            write!(res, "(").unwrap();
            to_rust(ast, children[0], indentation, res, enums);
            write!(res, ")").unwrap();
        },
        AstNode::ColonParentheses => {
            for child in children {
                to_rust(ast, *child, indentation, res, enums);
            }
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
            let is_print: fn(&AstNode) -> bool = |x| {
                if let AstNode::Identifier(w) = x { if w == "print" {
                    return true;
                }} return false;
            };
            if is_print(&ast[children[0]].value) {
                write!(res, "println!(\"{{}}\", ").unwrap();
            } else {
                to_rust(ast, children[0], indentation, res, enums);
                write!(res, "(").unwrap();
            }
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
        AstNode::Return => {
            write!(res, "return ").unwrap();
            if children.len() != 0 {
                to_rust(ast, children[0], indentation, res, enums);
            }
        },
        AstNode::String(str) => write!(res, "{}", str).unwrap(),
        AstNode::Char(chr) => write!(res, "{}", chr).unwrap(),
        AstNode::Property => {
            to_rust(ast, children[0], indentation, res, enums);
            write!(res, ".").unwrap();
            to_rust(ast, children[1], indentation, res, enums);
        },
        AstNode::ForStatement => {
            write!(res, "for ").unwrap();
            to_rust(ast, children[0], indentation, res, enums);
            to_rust(ast, children[1], indentation + 1, res, enums);
        },
        AstNode::ForVars => {
            to_rust(ast, children[0], indentation, res, enums);
            for child in children.iter().skip(1) {
                write!(res, ", ").unwrap();
                to_rust(ast, *child, indentation, res, enums);
            }
        },
        AstNode::ForIter => {
            write!(res, " in ").unwrap();
            to_rust(ast, children[0], indentation, res, enums);
        }
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