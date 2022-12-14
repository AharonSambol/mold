use std::collections::{HashMap, HashSet};
use crate::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use std::iter::zip;
use crate::built_in_funcs::BuiltIn;
use crate::IGNORE_STRUCTS;
use crate::mold_tokens::OperatorType;
use crate::types::{unwrap_u, Type, TypeKind, TypName, STR_TYPE};




pub fn to_rust(
    ast: &Vec<Ast>, pos: usize, indentation: usize, res: &mut String,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    enums: &mut HashMap<String, String>
) {
    let children = unwrap_u(&ast[pos].children);

    match &ast[pos].value {
        AstNode::Module => {
            let indent = "\t".repeat(indentation);
            write!(res, "\n{indent}").unwrap();
            for child in children {
                write!(res, "\n{}", indent).unwrap();
                to_rust(ast, *child, indentation, res, built_ins, enums);
            }
            write!(res, "\n{indent}").unwrap();
        },
        AstNode::Body => {
            let indent = "\t".repeat(indentation);
            write!(res, " {{").unwrap();
            for child in children {
                write!(res, "\n{indent}").unwrap();
                to_rust(ast, *child, indentation, res, built_ins, enums);
                write!(res, ";").unwrap();
            }
            write!(res, "\n{}}}", "\t".repeat(indentation - 1)).unwrap();
        },
        AstNode::Function(name) | AstNode::StaticFunction(name) => {
            let param = &ast[children[0]];
            let return_typ = &ast[children[1]];
            // for par in unwrap_u(&param.children) {
                // let typ = if let Some(t) = &ast[*par].typ { t } else { panic!() };
                // make_enums(typ,  enums);
            // }
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
                param = format!("mut {param}");
            }
            if let Some(t) = &return_typ.typ {
                write!(res, "fn {name}({param}) -> {t}").unwrap();
            } else {
                write!(res, "fn {name}({param})").unwrap();
            }
            to_rust(ast, children[2], indentation + 1, res, built_ins, enums);
        },
        AstNode::IfStatement => {
            write!(res, "if ").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            to_rust(ast, children[1], indentation + 1, res, built_ins, enums);
            for child in children.iter().skip(2){
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        write!(res, "\n{}else if ", "\t".repeat(indentation)).unwrap();
                        let c_children = if let Some(x) = &ast[*child].children { x } else { panic!() };
                        to_rust(ast, c_children[0], indentation, res, built_ins, enums);
                        to_rust(ast, c_children[1], indentation + 1, res, built_ins, enums);
                    },
                    AstNode::Body => {
                        write!(res, "\n{}else", "\t".repeat(indentation)).unwrap();
                        to_rust(ast, *child, indentation + 1, res, built_ins, enums);
                    },
                    _ => panic!()
                }
            }
        },
        AstNode::WhileStatement => {
            write!(res, "while ").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            to_rust(ast, children[1], indentation + 1, res, built_ins, enums);
        },
        AstNode::Assignment => {
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            write!(res, " = ").unwrap();
            to_rust(ast, children[1], indentation, res, built_ins, enums);
        },
        AstNode::FirstAssignment => {
            write!(res, "let mut ").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            if let Some(c) = &ast[children[0]].typ {
                // make_enums(c, built_ins, enums);
                write!(res, ": {c}").unwrap();
            }
            write!(res, " = ").unwrap();
            to_rust(ast, children[1], indentation, res, built_ins, enums);
        }
        AstNode::Identifier(name) => {
            write!(res, "{name}").unwrap();
        },
        // todo floor div
        AstNode::Operator(op) => {
            if let OperatorType::FloorDiv = op {
                panic!()
            } else if let OperatorType::FloorDivEq = op {
                panic!()
            } else if let OperatorType::Pow = op {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".pow(").unwrap();
                to_rust(ast, children[1], indentation, res, built_ins, enums);
                write!(res, ")").unwrap();
            } else if is_string_addition(ast, pos, op) {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, " {} ", op.to_string()).unwrap();
                to_rust(ast, children[1], indentation, res, built_ins, enums);
                if let Some(Type { kind: TypeKind::Struct(t_name), .. }) = &ast[children[1]].typ {
                    if *t_name == TypName::Static("String") {
                        write!(res, ".as_str()").unwrap();
                    }
                }
            } else {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, " {} ", op.to_string()).unwrap();
                to_rust(ast, children[1], indentation, res, built_ins, enums);
            }
        },
        AstNode::UnaryOp(op) => {
            let st = if let OperatorType::BinNot = op { String::from("!") } else { op.to_string() };
            write!(res, " {st}").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
        },
        AstNode::Parentheses => {
            write!(res, "(").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            write!(res, ")").unwrap();
        },
        AstNode::ColonParentheses => {
            for child in children {
                to_rust(ast, *child, indentation, res, built_ins, enums);
            }
        },
        AstNode::Index => {
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            if let Some(Type{ kind: TypeKind::Struct(stct), .. }) = &ast[children[1]].typ {
                if *stct == TypName::Static("usize") {
                    write!(res, "]").unwrap();
                    to_rust(ast, children[1], indentation, res, built_ins, enums);
                    write!(res, "]").unwrap();
                    return;
                }
            }
            write!(res, "[(").unwrap();
            to_rust(ast, children[1], indentation, res, built_ins, enums);
            write!(res, ") as usize]").unwrap();
        },
        AstNode::Number(num) => write!(res, "{num}").unwrap(),
        AstNode::ListLiteral => {
            write!(res, "vec![").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                to_rust(ast, *child, indentation + 1, res, built_ins, enums);
            }
            write!(res, "]").unwrap();

        },
        AstNode::Pass => {
            write!(res, "()").unwrap();
        },
        AstNode::FunctionCall(_) => {
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if built_ins.contains_key(name.as_str()) {
                    built_ins[name.as_str()].to_str_rust(ast, res, children, built_ins, enums);
                    return;
                }
            }
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            write!(res, "(").unwrap();
            if children.len() > 1 {
                to_rust(ast, children[1], indentation, res, built_ins, enums);
            }
            write!(res, ")").unwrap();
        },
        AstNode::Args | AstNode::ArgsDef => {
            if children.len() == 0 {
                return;
            }
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            for child in children.iter().skip(1) {
                write!(res, ",").unwrap();
                to_rust(ast, *child, indentation, res, built_ins, enums);
            }
        },
        AstNode::Return => {
            write!(res, "return ").unwrap();
            if children.len() != 0 {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
            }
        },
        AstNode::String { val, mutable } => {
            if *mutable {
                write!(res, "String::from({val})").unwrap()
            } else {
                write!(res, "{val}").unwrap()
            }
        },
        AstNode::Char(chr) => write!(res, "'{chr}'").unwrap(),
        AstNode::Property => {
            if built_in_func(&ast, indentation, res, built_ins, enums, &children) {
                return;
            }
            if let AstNode::FunctionCall(true) = ast[children[1]].value {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, "::").unwrap();
                to_rust(ast, children[1], indentation, res, built_ins, enums);
            } else {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".").unwrap();
                to_rust(ast, children[1], indentation, res, built_ins, enums);
            }
        },
        AstNode::ForStatement => {
            write!(res, "for ").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            to_rust(ast, children[1], indentation + 1, res, built_ins, enums);
        },
        AstNode::ForVars => {
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            for child in children.iter().skip(1) {
                write!(res, ", ").unwrap();
                to_rust(ast, *child, indentation, res, built_ins, enums);
            }
        },
        AstNode::ForIter => {
            write!(res, " in ").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
        },
        AstNode::Struct(name) => {
            if unsafe { IGNORE_STRUCTS.contains(name.as_str()) } {
                return;
            }
            let param = &ast[children[0]];
            // let funcs = if let AstNode::Functions(v) = &ast[children[1]].value { v } else { panic!() };
            // for par in unwrap_u(&param.children) {
                // let typ = if let Some(t) = &ast[*par].typ { t } else { panic!() };
                // make_enums(typ, built_ins, enums);
            // }
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
            write!(res, "#[derive(Debug, Clone)]\nstruct {name} {{ {param} }}\nimpl {name} {{").unwrap();
            to_rust(ast, children[1], indentation + 1, res, built_ins, enums);
            write!(res, "\n{}}}", "\t".repeat(indentation)).unwrap();
        },
        AstNode::StructInit => {
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            write!(res, "{{ ").unwrap();
            let arg_vals = unwrap_u(&ast[children[1]].children);
            let arg_names = unwrap_u(&ast[children[2]].children);
            for (val, name) in zip(arg_vals, arg_names) {
                to_rust(ast, *name, indentation, res, built_ins, enums);
                write!(res, ": ").unwrap();
                to_rust(ast, *val, indentation, res, built_ins, enums);
                write!(res, ", ").unwrap();
            }
            write!(res, "}}").unwrap();
        }
        AstNode::Bool(b) => {
            write!(res, "{}", if *b { "true" } else { "false" }).unwrap();
        }
        AstNode::Continue => write!(res, "continue").unwrap(),
        AstNode::Break => write!(res, "break").unwrap(),
        _ => panic!("Unexpected AST {:?}", ast[pos].value)
    }
}

fn built_in_func(
    ast: &Vec<Ast>, indentation: usize, res: &mut String,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>, enums: &mut HashMap<String, String>,
    children: &Vec<usize>
) -> bool {
    if let Some((struct_name, func_name)) = get_struct_and_func_name(ast, children) {
        let struct_name = match struct_name {
            TypName::Str(s) => s.as_str(),
            TypName::Static(s) => s
        };
        let arg_pos = unwrap_u(&ast[children[1]].children)[1];

        match (struct_name, func_name.as_str()) {
            ("String", "split") => {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                if unwrap_u(&ast[arg_pos].children).len() != 0 {
                    write!(res, ".split(").unwrap();
                    to_rust(ast, arg_pos, indentation, res, built_ins, enums);
                    write!(res, ")").unwrap();
                } else {
                    write!(res, ".split_whitespace()").unwrap();
                }
            }
            ("String", "strip" | "lstrip" | "rstrip") => {
                let trim = match func_name.as_str() {
                    "strip" => "trim",
                    "lstrip" => "trim_start",
                    "rstrip" => "trim_end",
                    _ => unreachable!()
                };
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                if unwrap_u(&ast[arg_pos].children).len() != 0 {
                    write!(res, ".{trim}_matches(").unwrap();
                    to_rust(ast, arg_pos, indentation, res, built_ins, enums);
                    write!(res, ")").unwrap();
                } else {
                    write!(res, ".{trim}()").unwrap();
                }
            }
            ("String", "len") => {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".len()").unwrap();
            }
            ("String", "startswith" | "endswith") => {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".{}_with(", func_name.strip_suffix("with").unwrap()).unwrap();
                to_rust(ast, arg_pos, indentation, res, built_ins, enums);
                write!(res, ")").unwrap();
            }
            ("String", "find") => {
                write!(res, "if let Some(__res__) = ").unwrap();
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".find(").unwrap();
                to_rust(ast, arg_pos, indentation, res, built_ins, enums);
                write!(res, ") {{ __res__ as i32 }} else {{ -1 }}").unwrap();
            }
            ("String", "count") => {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".matches(").unwrap();
                to_rust(ast, arg_pos, indentation, res, built_ins, enums);
                write!(res, ").count()").unwrap();
            }
            ("String", "removeprefix" | "removesuffix") => {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".strip_{}(", func_name.strip_prefix("remove").unwrap()).unwrap();
                to_rust(ast, arg_pos, indentation, res, built_ins, enums);
                write!(res, ").unwrap_or(&").unwrap();
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ")").unwrap();
            }
            ("String", "lower" | "upper") => {
                write!(res, "{{").unwrap();
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".make_ascii_{}case();", func_name).unwrap();
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, "}}").unwrap();
            }

            _ => { return false }
        }
        true
    } else { false }

}

fn is_string_addition(ast: &Vec<Ast>, pos: usize, op: &OperatorType) -> bool{
    if matches!(op, OperatorType::Plus) {
        if let Some(Type { kind: TypeKind::Struct(nm), .. }) = &ast[pos].typ {
            if *nm == TypName::Static("String") {
                return true;
            }
        }
    }
    false
}

fn get_struct_and_func_name<'a>(ast: &'a Vec<Ast>, children: &Vec<usize>) -> Option<(&'a TypName, &'a String)> {
    if let Some(Type{ kind: TypeKind::Struct(struct_name), .. }) = &ast[children[0]].typ {
        if let AstNode::Identifier(func_name) = &ast[unwrap_u(&ast[children[1]].children)[0]].value {
            return Some((struct_name, func_name))
        }
    }
    None
}


fn _make_enums(){
// fn make_enums(typ: &Type, enums: &mut HashMap<String, String>){
//     match &typ.kind {
//         TypeKind::Trait(_trt) => todo!(),
//         TypeKind::Args => todo!(),
//         TypeKind::Implements => todo!(),
//         TypeKind::Tuple => todo!(),
//         TypeKind::Generic(_gen) => todo!(),
//         TypeKind::Optional => todo!(),
//         TypeKind::Unknown | TypeKind::Typ(_) => (),
//         TypeKind::OneOf => {
//             let types = unwrap(&typ.children);
//             let enm = join(types, "-or-");
//             if !enums.contains_key(&enm) {
//                 let elems = types
//                     .iter()
//                     .map(|x| format!("_{x}: {x}"))
//                     .collect::<Vec<String>>();
//                 enums.insert(
//                     enm.clone(),
//                     format!("enum {enm} {{ {} }}", join(&elems , ","))
//                 );
//             }
//         },
//         TypeKind::TypWithSubTypes => {
//             for child in unwrap(&typ.children).iter().skip(1) {
//                 make_enums(child, built_ins, enums)
//             }
//         }
//         TypeKind::Struct(_) => {
//             // todo for typ in arg-types and func-types
//         },
//         TypeKind::Function(_) => {
//             // todo for typ in arg-types and func-types
//         },
//         TypeKind::Class(_) => todo!(),
//         TypeKind::Pointer => (),
//     }
// }
}
