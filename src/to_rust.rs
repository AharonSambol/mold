use std::collections::hash_map::Entry;
use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use std::iter::zip;
use crate::built_in_funcs::BuiltIn;
use crate::{IGNORE_FUNCS, IGNORE_STRUCTS, unwrap_enum};
use crate::mold_tokens::OperatorType;
use crate::types::{unwrap_u, Type, TypeKind, TypName, GenericType};




pub fn to_rust(
    ast: &[Ast], pos: usize, indentation: usize, res: &mut String,
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
            if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
                return;
            }

            print_function_rust(name, ast, indentation, res, built_ins, enums, children);
        },
        AstNode::IfStatement => {
            write!(res, "if ").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            to_rust(ast, children[1], indentation + 1, res, built_ins, enums);
            for child in children.iter().skip(2){
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        write!(res, "\n{}else if ", "\t".repeat(indentation)).unwrap();
                        let c_children = unwrap_enum!(&ast[*child].children);
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
            if ast[pos].is_mut {
                write!(res, "let mut ").unwrap();
            } else {
                write!(res, "let ").unwrap();
            }
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            if let Some(c) = &ast[children[0]].typ {
                // print_type(&ast[children[0]].typ);
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
                todo!()
            } else if let OperatorType::FloorDivEq = op {
                todo!()
            } else if let OperatorType::Pow = op {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".pow(").unwrap();
                to_rust(ast, children[1], indentation, res, built_ins, enums);
                write!(res, ")").unwrap();
            } else if is_string_addition(ast, pos, op) {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, " {} ", op).unwrap();
                to_rust(ast, children[1], indentation, res, built_ins, enums);
                if let Some(Type { kind: TypeKind::Struct(t_name), .. }) = &ast[children[1]].typ {
                    if *t_name == TypName::Static("String") {
                        write!(res, ".as_str()").unwrap();
                    }
                }
            } else {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, " {} ", op).unwrap();
                to_rust(ast, children[1], indentation, res, built_ins, enums);
            }
        },
        AstNode::UnaryOp(op) => {
            write!(res, "{op}").unwrap();
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
        AstNode::SetLiteral => {
            write!(res, "HashSet::from([").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                to_rust(ast, *child, indentation + 1, res, built_ins, enums);
            }
            write!(res, "])").unwrap();

        },
        AstNode::DictLiteral => {
            write!(res, "HashMap::from([").unwrap();
            for (i, children) in children.windows(2).step_by(2).enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                write!(res, "(").unwrap();
                to_rust(ast, children[0], indentation + 1, res, built_ins, enums);
                write!(res, ", ").unwrap();
                to_rust(ast, children[1], indentation + 1, res, built_ins, enums);
                write!(res, ")").unwrap();
            }
            write!(res, "])").unwrap();

        },
        AstNode::Pass => {
            write!(res, "()").unwrap();
        },
        AstNode::FunctionCall(_) => {
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                // TODO !!!!!!!!!!!1
                if built_ins.contains_key(name.as_str()) {
                    built_ins[name.as_str()].to_str_rust(ast, res, children, built_ins, enums);
                    return;
                }
                if built_in_funcs(ast, name, indentation, res, built_ins, enums, children) {
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
            if children.is_empty() {
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
            if !children.is_empty() {
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
            if let AstNode::FunctionCall(_) = ast[children[1]].value {
                if built_in_methods(ast, indentation, res, built_ins, enums, children) {
                    return;
                }
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
            write!(res, "mut ").unwrap();
            to_rust(ast, children[0], indentation, res, built_ins, enums);
            for child in children.iter().skip(1) {
                write!(res, ", mut ").unwrap();
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
            let generics_ast = &ast[children[0]];
            let param = &ast[children[1]];
            let generic = format_generics(generics_ast);
            let param = join(
                unwrap_u(&param.children).iter()
                    .map(|&x|
                        format!("{}: {}",
                            unwrap_enum!(&ast[x].value, AstNode::Identifier(n), n),
                            unwrap_enum!(&ast[x].typ)
                        )
                    ),
                ", "
            );
            write!(res,
                   "#[derive(Debug, Clone)]\nstruct {name}{generic} {{ {param} }}\n\
                    impl{generic} {name}{generic} {{"
            ).unwrap();
            let mut trait_functions = vec![];
            for func in unwrap_u(&ast[children[2]].children) {
                let func_name = unwrap_enum!(&ast[*func].value, AstNode::Function(n) | AstNode::StaticFunction(n), n);
                if func_name.contains("::"){
                    trait_functions.push((*func, func_name));
                    continue
                }
                write!(res, "\n{}", "\t".repeat(indentation + 1)).unwrap();
                to_rust(ast, *func, indentation + 1, res, built_ins, enums);

            }
            let indent = "\t".repeat(indentation);
            write!(res, "\n{indent}}}").unwrap();
            let mut trait_to_funcs: HashMap<&str, Vec<(usize, &str)>> = HashMap::new();
            for (func_pos, func_name) in trait_functions {
                let mut func_name = func_name.split("::");
                let trait_name = func_name.next().unwrap();
                let func_name = func_name.next().unwrap();
                match trait_to_funcs.entry(trait_name) {
                    Entry::Vacant(e) => { e.insert(vec![(func_pos, func_name)]); },
                    Entry::Occupied(mut e) => { e.get_mut().push((func_pos, func_name)); }
                }
            }
            for (trait_name, funcs) in trait_to_funcs.iter() {
                write!(res, "\n{indent}impl {trait_name} for {name} {{").unwrap();
                for (func_pos, func_name) in funcs {
                    write!(res, "\n{indent}    ").unwrap();
                    print_function_rust(
                        func_name, ast, indentation + 1, res, built_ins, enums,
                        unwrap_u(&ast[*func_pos].children)
                    );
                }
                write!(res, "\n{indent}}}").unwrap();
            }
        },
        AstNode::StructInit => {
            write!(res, "{}{{ ", ast[pos].typ.clone().unwrap()).unwrap();
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
        AstNode::ReturnType => { unreachable!() },
        AstNode::Trait { name, .. } => {
            let generics_ast = &ast[children[0]];
            let generic = format_generics(generics_ast);
            let functions = unwrap_u(&ast[children[1]].children);

            write!(res, "trait {name}{generic} {{").unwrap();
            for func in functions {
                let func = &ast[*func];
                let func_name = unwrap_enum!(&func.value, AstNode::Function(name), name);
                let func_children = unwrap_u(&func.children);
                let func_generics = format_generics(&ast[func_children[0]]);
                let args = unwrap_u(&ast[func_children[1]].children).iter().map(|x| {
                    let arg = &ast[*x];
                    let name = unwrap_enum!(&arg.value, AstNode::Identifier(name), name);
                    let typ = unwrap_enum!(&arg.typ);
                    format!("{name}: {typ}")
                });
                let args = join(args, ", ");
                let return_typ = &ast[func_children[2]].typ;
                if let Some(rt) = return_typ {
                    write!(res, "\n\tfn {func_name}{func_generics}({args}) -> {rt};").unwrap();
                } else {
                    write!(res, "\n\tfn {func_name}{func_generics}({args});").unwrap();
                }
            }
            write!(res, "\n}}").unwrap();
        }
        _ => panic!("Unexpected AST `{:?}`", ast[pos].value)
    }
}

fn print_function_rust(
    name: &str,
    ast: &[Ast], indentation: usize, res: &mut String,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    enums: &mut HashMap<String, String>,
    children: &[usize]
) {
    let generics_ast = &ast[children[0]];
    let param = &ast[children[1]];
    let return_typ = &ast[children[2]];
    let generic = format_generics(generics_ast);
    let param = join(
        unwrap_u(&param.children).iter()
            .map(|&x|
                format!("{}{}: {}",
                        if ast[x].is_mut { "mut " } else { "" },
                        unwrap_enum!(&ast[x].value, AstNode::Identifier(n), n),
                        unwrap_enum!(&ast[x].typ),
                )
            ),
        ", "
    );
    if let Some(t) = &return_typ.typ {
        write!(res, "fn {name}{generic}({param}) -> {t}").unwrap();
    } else {
        write!(res, "fn {name}{generic}({param})").unwrap();
    }

    to_rust(ast, children[3], indentation + 1, res, built_ins, enums);
}

fn built_in_methods(
    ast: &[Ast], indentation: usize, res: &mut String,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>, enums: &mut HashMap<String, String>,
    children: &[usize]
) -> bool {
    if let Some((struct_name, func_name)) = get_struct_and_func_name(ast, children) {
        let arg_pos = unwrap_u(&ast[children[1]].children)[1];

        match (struct_name.get_str(), func_name.as_str()) {
            ("String", "split") => {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                if !unwrap_u(&ast[arg_pos].children).is_empty() {
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
                if !unwrap_u(&ast[arg_pos].children).is_empty() {
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
            ("Vec", "append") => {
                to_rust(ast, children[0], indentation, res, built_ins, enums);
                write!(res, ".push(").unwrap();
                to_rust(ast, arg_pos, indentation, res, built_ins, enums);
                write!(res, ")").unwrap();
            }

            _ => { return false }
        }
        true
    } else { false }
}

fn built_in_funcs(
    ast: &[Ast], name: &str, indentation: usize, res: &mut String,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>, enums: &mut HashMap<String, String>,
    children: &[usize]
) -> bool {
    // let arg_pos = unwrap_u(&ast[children[1]].children)[1];
    match name {
        "reversed" => {
            to_rust(ast, children[1], indentation, res, built_ins, enums);
            write!(res, ".rev()").unwrap();
        }
        _ => { return false }
    }
    true
}

fn is_string_addition(ast: &[Ast], pos: usize, op: &OperatorType) -> bool{
    if matches!(op, OperatorType::Plus) {
        if let Some(Type { kind: TypeKind::Struct(nm), .. }) = &ast[pos].typ {
            if *nm == TypName::Static("String") {
                return true;
            }
        }
    }
    false
}

pub fn get_struct_and_func_name<'a>(ast: &'a [Ast], children: &[usize]) -> Option<(&'a TypName, &'a String)> {
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
//         TypeKind::Trait(_trt) => ,
//         TypeKind::Args => ,
//         TypeKind::Implements => ,
//         TypeKind::Tuple => ,
//         TypeKind::Generic(_gen) => ,
//         TypeKind::Optional => ,
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
//             // for typ in arg-types and func-types
//         },
//         TypeKind::Function(_) => {
//             // for typ in arg-types and func-types
//         },
//         TypeKind::Class(_) => ,
//         TypeKind::Pointer => (),
//     }
// }
}

fn format_generics(generics_ast: &Ast) -> String {
    if let Some(Type{ children: Some(generics), .. }) = &generics_ast.typ {
        format!("<{}>", join(generics.iter().map(|x|
            unwrap_enum!(&x.kind, TypeKind::Generic(GenericType::Declaration(name)), name)
        ), ","))
    } else {
        String::new()
    }
}

