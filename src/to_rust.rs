use std::collections::HashMap;
use crate::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use std::iter::zip;
use crate::mold_ast::StructTypes;
use crate::mold_tokens::OperatorType;
use crate::types::{Type, TypeKind, unwrap, unwrap_u};

// TODO types with |

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
        AstNode::Function(name) => {
            println!("{}", name);
            let param = &ast[children[0]];
            let return_typ = &ast[children[1]];
            // let body = &ast[children[2]];
            for par in unwrap_u(&param.children) {
                let typ = if let Some(t) = &ast[*par].typ { t } else { panic!() };
                make_enums(typ,  enums);
            }
            let mut param = join(
                &unwrap_u(&param.children).iter()
                    .map(|&x|
                        format!("{}: {}",
                                if let AstNode::Identifier(n) = &ast[x].value { n } else { panic!() },
                                if let Some(t) = &ast[x].typ { t } else { panic!() }
                        )
                    ).collect(),
                ", mut "
            );
            if param != "" {
                param = format!("mut {}", param);
            }
            if let Some(t) = &return_typ.typ {
                write!(res, "fn {}({}) -> {}", name, param, t).unwrap();
            } else {
                write!(res, "fn {}({})", name, param).unwrap();
            }
            to_rust(ast, children[2], indentation + 1, res, enums);
            // for child in children {
            //     write!(res, "\n{}", "\t".repeat(indentation + 1)).unwrap();
            //     to_rust(ast, *child, indentation + 1, res, enums);
            //     write!(res, ";").unwrap();
            // }
            // write!(res, "\n{}}}", "\t".repeat(indentation)).unwrap();
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
            write!(res, " = ").unwrap();
            to_rust(ast, children[1], indentation, res, enums);
        },
        AstNode::FirstAssignment => {
            write!(res, "let mut ").unwrap();
            to_rust(ast, children[0], indentation, res, enums);
            if let Some(c) = &ast[children[0]].typ {
                make_enums(c, enums);
                write!(res, ": {}", c).unwrap();
            }
            write!(res, " = ").unwrap();
            to_rust(ast, children[1], indentation, res, enums);
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
        AstNode::FunctionCall => {
            match is_bif(&ast[children[0]].value) {
                Some(BIFunc::Print) => {
                    write!(res, "{}",
                           format!("println!(\"{}\", ",
                                   "{} ".repeat(
                                       unwrap_u(&ast[children[1]].children).len()
                                   ).strip_suffix(' ').unwrap()
                           )
                    ).unwrap()
                },
                Some(BIFunc::DPrint) => {
                    write!(res, "{}",
                           format!("println!(\"{}\", ",
                                   "{:?} ".repeat(
                                       unwrap_u(&ast[children[1]].children).len()
                                   ).strip_suffix(' ').unwrap()
                           )
                    ).unwrap()
                }
                Some(BIFunc::Range) => {
                    let args = unwrap_u(&ast[children[1]].children);
                    match args.len() {
                        1 => {
                            write!(res, "(0..").unwrap();
                            to_rust(ast, args[0], indentation, res, enums);
                            write!(res, ")").unwrap();
                        },
                        2 => {
                            write!(res, "(").unwrap();
                            to_rust(ast, args[0], indentation, res, enums);
                            write!(res, "..").unwrap();
                            to_rust(ast, args[1], indentation, res, enums);
                            write!(res, ")").unwrap();
                        },
                        3 => {
                            write!(res, "(").unwrap();
                            to_rust(ast, args[0], indentation, res, enums);
                            write!(res, "..").unwrap();
                            to_rust(ast, args[1], indentation, res, enums);
                            write!(res, ").step_by(").unwrap();
                            to_rust(ast, args[2], indentation, res, enums);
                            write!(res, ")").unwrap();
                        },
                        _ => panic!("too many args passed to range, expected 1-3 found {}", children.len())
                    }
                    return;
                },
                Some(BIFunc::Rev) => {
                    to_rust(ast, children[1], indentation, res, enums);
                    write!(res, ".rev()").unwrap();
                    return;
                },
                Some(BIFunc::Enumerate) => {
                    to_rust(ast, children[1], indentation, res, enums);
                    write!(res, ".enumerate()").unwrap();
                    return;
                },
                _ => {
                    to_rust(ast, children[0], indentation, res, enums);
                    write!(res, "(").unwrap();
                }
            }
            if children.len() > 1 {
                to_rust(ast, children[1], indentation, res, enums);
            }
            write!(res, ")").unwrap();
        },
        AstNode::Args | AstNode::ArgsDef => {
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
        },
        AstNode::Struct(name) => {
            let param = &ast[children[0]];
            // let funcs = if let AstNode::Functions(v) = &ast[children[1]].value { v } else { panic!() };
            for par in unwrap_u(&param.children) {
                let typ = if let Some(t) = &ast[*par].typ { t } else { panic!() };
                make_enums(typ, enums);
            }
            let param = join(
                &unwrap_u(&param.children).iter()
                    .map(|&x|
                        format!("{}: {}",
                            if let AstNode::Identifier(n) = &ast[x].value { n } else { panic!() },
                            if let Some(t) = &ast[x].typ { t } else { panic!() }
                        )
                    ).collect(),
                ", "
            );
            write!(res, "struct {} {{ {} }}\nimpl {} {{", name, param, name).unwrap();
            to_rust(ast, children[2], indentation + 1, res, enums);
            write!(res, "\n{}}}", "\t".repeat(indentation)).unwrap();
        },
        AstNode::StructInit => {
            to_rust(ast, children[0], indentation, res, enums);
            write!(res, "{{ ").unwrap();
            let arg_vals = unwrap_u(&ast[children[1]].children);
            let arg_names = unwrap_u(&ast[children[2]].children);
            for (val, name) in zip(arg_vals, arg_names) {
                to_rust(ast, *name, indentation, res, enums);
                write!(res, ": ").unwrap();
                to_rust(ast, *val, indentation, res, enums);
                write!(res, ", ").unwrap();
            }
            write!(res, "}}").unwrap();
        }
        AstNode::Bool(b) => {
            write!(res, "{}", b).unwrap();
        }
        _ => panic!("Unexpected AST {:?}", ast[pos].value)
    }
}

fn make_enums(typ: &Type, enums: &mut HashMap<String, String>){
    match &typ.kind {
        TypeKind::Trait(_trt) => todo!(),
        TypeKind::Args => todo!(),
        TypeKind::Implements => todo!(),
        TypeKind::Tuple => todo!(),
        TypeKind::Generic(_gen) => todo!(),
        TypeKind::Optional => todo!(),
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
        TypeKind::TypWithSubTypes => {
            for child in unwrap(&typ.children).iter().skip(1) {
                make_enums(child, enums)
            }
        }
        TypeKind::Struct(_) => todo!(),
        TypeKind::Class(_) => todo!(),
    }
}

pub enum BIFunc {
    Print, DPrint, Range, Rev, Enumerate
}
pub fn is_bif(x: &AstNode) -> Option<BIFunc> {
    return if let AstNode::Identifier(w) = x {
        match w.as_str() {
            "print" => Some(BIFunc::Print),
            "dprint" => Some(BIFunc::DPrint),
            "range" => Some(BIFunc::Range),
            "rev" => Some(BIFunc::Rev),
            "enumerate" => Some(BIFunc::Enumerate),
            _ => None
        }
    } else { None }
}