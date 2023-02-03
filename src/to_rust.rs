use std::collections::hash_map::Entry;
use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode, join};
use std::fmt::Write;
use std::iter::zip;
use crate::{EMPTY_STR, IGNORE_ENUMS, IGNORE_FUNCS, IGNORE_STRUCTS, IGNORE_TRAITS, unwrap_enum};
use crate::add_types::ast_add_types::{SPECIFIED_NUM_TYPE_RE};
use crate::construct_ast::mold_ast::Info;
use crate::construct_ast::tree_utils::print_tree;
use crate::mold_tokens::OperatorType;
use crate::types::{unwrap_u, Type, TypeKind, TypName, GenericType, unwrap};


pub fn to_rust(
    ast: &[Ast], pos: usize, indentation: usize, res: &mut String,
    enums: &mut HashMap<String, String>, info: &Info
) {
    let children = unwrap_u(&ast[pos].children);

    match &ast[pos].value {
        AstNode::Module => {
            let indent = "\t".repeat(indentation);
            write!(res, "\n{indent}").unwrap();
            for child in children {
                write!(res, "\n{}", indent).unwrap();
                to_rust(ast, *child, indentation, res, enums, info);
            }
            write!(res, "\n{indent}").unwrap();
        },
        AstNode::Body => {
            let indent = "\t".repeat(indentation);
            write!(res, " {{").unwrap();
            for child in children {
                write!(res, "\n{indent}").unwrap();
                to_rust(ast, *child, indentation, res, enums, info);
                write!(res, ";").unwrap();
            }
            write!(res, "\n{}}}", "\t".repeat(indentation - 1)).unwrap();
        },
        AstNode::Function(name) | AstNode::StaticFunction(name) => {
            if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
                return;
            }
            print_function_rust(name, ast, indentation, res, enums, children, info);
        },
        AstNode::IfStatement => {
            write!(res, "if ").unwrap();
            to_rust(ast, children[0], indentation, res, enums, info);
            to_rust(ast, children[1], indentation + 1, res, enums, info);
            for child in children.iter().skip(2){
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        write!(res, "\n{}else if ", "\t".repeat(indentation)).unwrap();
                        let c_children = ast[*child].children.as_ref().unwrap();
                        to_rust(ast, c_children[0], indentation, res, enums, info);
                        to_rust(ast, c_children[1], indentation + 1, res, enums, info);
                    },
                    AstNode::Body => {
                        write!(res, "\n{}else", "\t".repeat(indentation)).unwrap();
                        to_rust(ast, *child, indentation + 1, res, enums, info);
                    },
                    _ => panic!()
                }
            }
        },
        AstNode::WhileStatement => {
            write!(res, "while ").unwrap();
            to_rust(ast, children[0], indentation, res, enums, info);
            to_rust(ast, children[1], indentation + 1, res, enums, info);
        },
        AstNode::Assignment => {
            if let AstNode::Index = &ast[children[0]].value {
                let ch = ast[children[0]].children.as_ref().unwrap();
                let mut list = String::new();
                to_rust(ast, ch[0], indentation, &mut list, enums, info);
                let mut idx = String::new();
                to_rust(ast, ch[1], indentation, &mut idx, enums, info);
                let list = if let AstNode::Index = &ast[ch[1]].value {
                    list
                } else {
                    format!("&mut {list}")
                };
                print_tree(&ast.to_vec(), children[0]);
                write!(
                    res,
                    "unsafe {{\n\t let mut list: *mut {} = {list};\n\t\
                    let len = (*list).len();\n\tlet val=",
                    ast[ch[0]].typ.as_ref().unwrap()
                ).unwrap();
                to_rust(ast, children[1], indentation, res, enums, info);
                write!(res, ";\n\t(*list)[{{let pos={idx}; if pos >= 0 {{ pos as usize }} else \
                {{ (pos + len as i32) as usize }} }}] = val\n}}").unwrap();
            } else {
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, " = ").unwrap();
                to_rust(ast, children[1], indentation, res, enums, info);
            }
        },
        AstNode::FirstAssignment => {
            if ast[pos].is_mut {
                write!(res, "let mut ").unwrap();
            } else {
                write!(res, "let ").unwrap();
            }
            to_rust(ast, children[0], indentation, res, enums, info);
            if let Some(c) = &ast[children[0]].typ {
                // print_type(&ast[children[0]].typ);
                write!(res, ": {c}").unwrap();
            }
            write!(res, " = ").unwrap();
            to_rust(ast, children[1], indentation, res, enums, info);
        }
        AstNode::Identifier(name) => {
            write!(res, "{name}").unwrap();
        },
        AstNode::Operator(op) => {
            match op {
                OperatorType::FloorDiv => {
                    #[inline] fn div(
                        ast: &[Ast], children: &Vec<usize>, indentation: usize,
                        res: &mut String, enums: &mut HashMap<String, String>, info: &Info
                    ) {
                        to_rust(ast, children[0], indentation, res, enums, info);
                        write!(res, " / ").unwrap();
                        to_rust(ast, children[1], indentation, res, enums, info);
                    }
                    if matches!(
                        &ast[children[0]].typ.as_ref().unwrap().kind,
                        TypeKind::Struct(name) if name == "f32" || name == "f64"
                    ) {
                        write!(res, "(").unwrap();
                        div(ast, children, indentation, res, enums, info);
                        write!(res, ").floor()").unwrap();
                    } else {
                        div(ast, children, indentation, res, enums, info);
                    }
                }
                OperatorType::FloorDivEq => {
                    todo!()
                }
                OperatorType::Div => {
                    for (i, child) in children.iter().enumerate() {
                        if matches!(
                            &ast[*child].typ.as_ref().unwrap().kind,
                            TypeKind::Struct(name) if name == "f32" || name == "f64"
                        ) {
                            to_rust(ast, *child, indentation, res, enums, info);
                        } else {
                            write!(res, "(").unwrap();
                            to_rust(ast, *child, indentation, res, enums, info);
                            write!(res, " as f32)").unwrap();
                        }
                        if i == 0 {
                            write!(res, "/").unwrap();
                        }
                    }
                }
                OperatorType::DivEq => {
                    todo!()
                }
                OperatorType::Pow => {
                    to_rust(ast, children[0], indentation, res, enums, info);
                    write!(res, ".pow(").unwrap();
                    to_rust(ast, children[1], indentation, res, enums, info);
                    write!(res, ")").unwrap();
                }
                _ if is_string_addition(ast, pos, op) => {
                    to_rust(ast, children[0], indentation, res, enums, info);
                    write!(res, " {} ", op).unwrap();
                    to_rust(ast, children[1], indentation, res, enums, info);
                    if let Some(Type { kind: TypeKind::Struct(t_name), .. }) = &ast[children[1]].typ {
                        if *t_name == TypName::Static("String") {
                            write!(res, ".as_str()").unwrap();
                        }
                    }
                }
                _ => {
                    to_rust(ast, children[0], indentation, res, enums, info);
                    write!(res, " {} ", op).unwrap();
                    to_rust(ast, children[1], indentation, res, enums, info);
                }
            }
        },
        AstNode::UnaryOp(op) => {
            if matches!(op, OperatorType::MutPointer | OperatorType::Pointer)
                && matches!(&ast[children[0]].value, AstNode::Index) {
                //1 the index will already put a &mut\&
            } else {
                write!(res, "{op}").unwrap();
            }
            to_rust(ast, children[0], indentation, res, enums, info);
        },
        AstNode::Parentheses => {
            write!(res, "(").unwrap();
            to_rust(ast, children[0], indentation, res, enums, info);
            write!(res, ")").unwrap();
        },
        AstNode::ColonParentheses => {
            for child in children {
                to_rust(ast, *child, indentation, res, enums, info);
            }
        },
        AstNode::Index => {
            let as_pointer = should_be_mut_index(ast, pos);
            write!(res, "unsafe {{").unwrap();
            let mut list = String::new();
            to_rust(ast, children[0], indentation, &mut list, enums, info);
            let list = if let AstNode::Index = ast[children[0]].value {
                format!("{list} as *mut")
            } else if let IndexTyp::Mut = as_pointer {
                format!("&mut {list} as *mut")
            } else {
                format!("&{list} as *const")
            };
            write!(
                res,
                "let list = {list} {};let len = (*list).len();",
                ast[children[0]].typ.as_ref().unwrap()
            ).unwrap();

            match as_pointer {
                IndexTyp::Mut => write!(res, "&mut ").unwrap(),
                IndexTyp::Ref => write!(res, "&").unwrap(),
                IndexTyp::Val => {}
            }
            write!(res, "(*list)[{{ let pos =").unwrap();
            to_rust(ast, children[1], indentation, res, enums, info);
            write!(
                res,
                "; if pos >= 0 {{ pos as usize }} else {{ (pos + len as i32) as usize }} }}]}}"
            ).unwrap();
        },
        AstNode::Number(num) => {
            if !SPECIFIED_NUM_TYPE_RE.is_match(num) {
                if num.contains('.') {
                    write!(res, "{num}f32").unwrap()
                } else {
                    write!(res, "{num}i32").unwrap()
                }
            } else {
                write!(res, "{num}").unwrap()
            }
        },
        AstNode::ListLiteral => {
            write!(res, "vec![").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                to_rust(ast, *child, indentation + 1, res, enums, info);
            }
            write!(res, "]").unwrap();

        },
        AstNode::SetLiteral => {
            write!(res, "HashSet::from([").unwrap();
            for (i, child) in children.iter().enumerate() {
                if i != 0{
                    write!(res, ", ").unwrap();
                }
                to_rust(ast, *child, indentation + 1, res, enums, info);
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
                to_rust(ast, children[0], indentation + 1, res, enums, info);
                write!(res, ", ").unwrap();
                to_rust(ast, children[1], indentation + 1, res, enums, info);
                write!(res, ")").unwrap();
            }
            write!(res, "])").unwrap();

        },
        AstNode::Pass => {
            write!(res, "()").unwrap();
        },
        AstNode::FunctionCall(_) => {
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if built_in_funcs(ast, name, indentation, res, enums, children, info) {
                    return;
                }
            }
            to_rust(ast, children[0], indentation, res, enums, info);
            write!(res, "(").unwrap();
            if children.len() > 1 {
                to_rust(ast, children[1], indentation, res, enums, info);
            }
            write!(res, ")").unwrap();
        },
        AstNode::Args | AstNode::ArgsDef => {
            if children.is_empty() {
                return;
            }
            to_rust(ast, children[0], indentation, res, enums, info);
            for child in children.iter().skip(1) {
                write!(res, ", ").unwrap();
                to_rust(ast, *child, indentation, res, enums, info);
            }
        },
        AstNode::Return => {
            write!(res, "return ").unwrap();
            if !children.is_empty() {
                to_rust(ast, children[0], indentation, res, enums, info);
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
                if built_in_methods(ast, indentation, res, enums, children, info) {
                    return;
                }
            }

            if matches!(ast[children[1]].value, AstNode::FunctionCall(true))
                || matches!(ast[children[0]].typ, Some(Type { kind: TypeKind::Enum(_), .. }))
            {
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, "::").unwrap();
                to_rust(ast, children[1], indentation, res, enums, info);
            } else {
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ".").unwrap();
                to_rust(ast, children[1], indentation, res, enums, info);
            }
        },
        AstNode::ForStatement => {
            write!(res, "for ").unwrap();
            to_rust(ast, children[0], indentation, res, enums, info);
            to_rust(ast, children[1], indentation + 1, res, enums, info);
        },
        AstNode::ForVars => {
            write!(res, "mut ").unwrap();
            to_rust(ast, children[0], indentation, res, enums, info);
            for child in children.iter().skip(1) {
                write!(res, ", mut ").unwrap();
                to_rust(ast, *child, indentation, res, enums, info);
            }
        },
        AstNode::ForIter => {
            write!(res, " in ").unwrap();
            to_rust(ast, children[0], indentation, res, enums, info);
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
                            ast[x].typ.as_ref().unwrap()
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
                #[allow(unused_parens)]
                let func_name = unwrap_enum!(
                    &ast[*func].value, (AstNode::Function(n) | AstNode::StaticFunction(n)), n
                );
                if func_name.contains("::"){
                    trait_functions.push((*func, func_name));
                    continue
                }
                write!(res, "\n{}", "\t".repeat(indentation + 1)).unwrap();
                to_rust(ast, *func, indentation + 1, res, enums, info);

            }
            let indent = "\t".repeat(indentation);
            write!(res, "\n{indent}}}").unwrap();
            let mut trait_to_funcs: HashMap<&str, Vec<(usize, &str)>> = HashMap::new();
            for (func_pos, func_name) in trait_functions {
                let mut func_name = func_name.split("::");
                let trait_name = func_name.next().unwrap();
                let func_name = func_name.next().unwrap();
                match trait_to_funcs.entry(trait_name) {
                    Entry::Vacant(e) =>
                        { e.insert(vec![(func_pos, func_name)]); },
                    Entry::Occupied(mut e) =>
                        { e.get_mut().push((func_pos, func_name)); }
                }
            }
            for (trait_name, funcs) in trait_to_funcs.iter() {
                write!(res, "\n{indent}impl {trait_name} for {name} {{").unwrap();
                let mut type_defs = HashMap::new();
                for (func_pos, func_name) in funcs {
                    write!(res, "\n{indent}    ").unwrap();
                    let func_children = unwrap_u(&ast[*func_pos].children);
                    if func_children.len() == 5 {
                        let types_pos = unwrap_u(&ast[func_children[4]].children);
                        for i in types_pos {
                            type_defs.insert(
                                unwrap_enum!(&ast[*i].value, AstNode::Type(n), n),
                                ast[*i].typ.as_ref().unwrap()
                            );
                        }

                    }
                    print_function_rust(
                        func_name, ast, indentation + 1, res, enums,
                        func_children, info
                    );
                }
                for (name, typ) in type_defs {
                    write!(res, "\n\ttype {name} = {typ};").unwrap();
                }
                write!(res, "\n{indent}}}").unwrap();
            }
        },
        AstNode::StructInit => {
            write!(res, "{}{{ ", ast[pos].typ.clone().unwrap()).unwrap();
            let arg_vals = unwrap_u(&ast[children[1]].children);
            let arg_names = unwrap_u(&ast[children[2]].children);
            for (val, name) in zip(arg_vals, arg_names) {
                to_rust(ast, *name, indentation, res, enums, info);
                write!(res, ": ").unwrap();
                to_rust(ast, *val, indentation, res, enums, info);
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
            if unsafe { IGNORE_TRAITS.contains(name.as_str()) } {
                return;
            }
            let generics_ast = &ast[children[0]];
            let generic = format_generics(generics_ast);
            let functions = unwrap_u(&ast[children[1]].children);

            write!(res, "trait {name}{generic} {{").unwrap();
            for func in functions {
                // todo type
                let func = &ast[*func];
                if let AstNode::Function(func_name) = &func.value {
                    let func_children = unwrap_u(&func.children);
                    let func_generics = format_generics(&ast[func_children[0]]);
                    let args = unwrap_u(&ast[func_children[1]].children)
                        .iter()
                        .map(|x| {
                            let arg = &ast[*x];
                            let name = unwrap_enum!(&arg.value, AstNode::Arg { name, is_arg, is_kwarg }, name);
                            // TODO if is_arg \ is_kwarg
                            let typ = arg.typ.as_ref().unwrap();
                            format!("{name}: {typ}")
                        });
                    let args = join(args, ", ");
                    let return_typ = &ast[func_children[2]].typ;
                    if let Some(rt) = return_typ {
                        write!(res, "\n\tfn {func_name}{func_generics}({args}) -> {rt};").unwrap();
                    } else {
                        write!(res, "\n\tfn {func_name}{func_generics}({args});").unwrap();
                    }
                } else {
                    write!(res, "\n\ttype {};", unwrap_enum!(&func.value, AstNode::Type(t), t)).unwrap();
                }
            }
            write!(res, "\n}}").unwrap();
        }
        AstNode::Enum(name) => {
            if unsafe { IGNORE_ENUMS.contains(name.as_str()) } {
                return;
            }
            let generics_ast = &ast[children[0]];
            let generic = format_generics(generics_ast);
            writeln!(res, "enum {name}{generic}{{").unwrap();
            let module = unwrap_u(&ast[children[1]].children);
            for option in module {
                let name = unwrap_enum!(&ast[*option].value, AstNode::Identifier(n), n);
                write!(res, "\t{name}").unwrap();
                let types = unwrap_u(&ast[*option].children);
                if !types.is_empty() {
                    write!(res, "(").unwrap();
                    for (i, typ) in types.iter().enumerate() {
                        if i != 0 {
                            write!(res, ", ").unwrap();
                        }
                        write!(res, "{}", ast[*typ].typ.as_ref().unwrap()).unwrap();
                    }
                    write!(res, ")").unwrap();
                }
                writeln!(res, ",").unwrap();
            }
            writeln!(res, "}}").unwrap();
        }
        AstNode::NamedArg(_) => {
            to_rust(ast, unwrap_u(&ast[pos].children)[0], indentation, res, enums, info);
        }
        AstNode::ListComprehension | AstNode::SetComprehension | AstNode::DictComprehension => {
            match &ast[pos].value {
                AstNode::ListComprehension => write!(res, "{{let mut res = vec![];"),
                AstNode::SetComprehension => write!(res, "{{let mut res = HashSet::new();"),
                AstNode::DictComprehension => write!(res, "{{let mut res = HashMap::new();"),
                _ => unreachable!()
            }.unwrap();
            let loops = ast[children[1]].children.clone().unwrap();
            for r#loop in loops.iter() {
                let colon_par = ast[*r#loop].children.as_ref().unwrap();
                write!(res, "for ").unwrap();
                to_rust(ast, colon_par[0], indentation, res, enums, info);
                write!(res, "{{").unwrap();
            }
            let close = if let Some(condition) = &ast[children[2]].children {
                write!(res, "if ").unwrap();
                to_rust(ast, condition[0], indentation, res, enums, info);
                write!(res, "{{").unwrap();
                1
            } else {
                0
            };
            if let AstNode::ListComprehension = &ast[pos].value {
                write!(res, "res.push(").unwrap();
            } else {
                write!(res, "res.insert(").unwrap();
            }
            if let AstNode::DictComprehension = &ast[pos].value {
                let parts = ast[children[0]].children.as_ref().unwrap();
                to_rust(ast, parts[0], indentation, res, enums, info);
                write!(res, ",").unwrap();
                to_rust(ast, parts[1], indentation, res, enums, info);
            } else {
                to_rust(ast, ast[children[0]].children.as_ref().unwrap()[0], indentation, res, enums, info);
            }
            write!(res, ");").unwrap();
            write!(res, "{}", "}".repeat(loops.len() + close)).unwrap();
            write!(res, "res}}").unwrap();
        }
        _ => panic!("Unexpected AST `{:?}`", ast[pos].value)
    }
}

fn print_function_rust(
    name: &str,
    ast: &[Ast], indentation: usize, res: &mut String,
    enums: &mut HashMap<String, String>,
    children: &[usize], info: &Info
) {
    let generics_ast = &ast[children[0]];
    let param = &ast[children[1]];
    let return_typ = &ast[children[2]];
    let generic = format_generics(generics_ast);
    let param = join(
        unwrap_u(&param.children).iter()
            .map(|&x|
                {
                    let name = unwrap_enum!(&ast[x].value, AstNode::Arg { name, .. }, name);
                    let typ = ast[x].typ.as_ref().unwrap();
                    format!("{}{name}: {typ}", if ast[x].is_mut { "mut " } else { "" })
                }
            ),
        ", "
    );
    if let Some(t) = &return_typ.typ {
        write!(res, "fn {name}{generic}({param}) -> {t}").unwrap();
    } else {
        write!(res, "fn {name}{generic}({param})").unwrap();
    }

    to_rust(ast, children[3], indentation + 1, res, enums, info);
}

fn built_in_methods(
    ast: &[Ast], indentation: usize, res: &mut String,
    enums: &mut HashMap<String, String>, children: &[usize], info: &Info
) -> bool {
    if let Some((struct_name, func_name)) = get_struct_and_func_name(ast, children) {
        let arg_pos = unwrap_u(&ast[children[1]].children)[1];

        match (struct_name.get_str(), func_name.as_str()) {
            ("String", "split") => {
                to_rust(ast, children[0], indentation, res, enums, info);
                if !unwrap_u(&ast[arg_pos].children).is_empty() {
                    write!(res, ".split(").unwrap();
                    to_rust(ast, arg_pos, indentation, res, enums, info);
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
                to_rust(ast, children[0], indentation, res, enums, info);
                if !unwrap_u(&ast[arg_pos].children).is_empty() {
                    write!(res, ".{trim}_matches(").unwrap();
                    to_rust(ast, arg_pos, indentation, res, enums, info);
                    write!(res, ")").unwrap();
                } else {
                    write!(res, ".{trim}()").unwrap();
                }
            }
            ("String", "startswith" | "endswith") => {
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ".{}_with(", func_name.strip_suffix("with").unwrap()).unwrap();
                to_rust(ast, arg_pos, indentation, res, enums, info);
                write!(res, ")").unwrap();
            }
            ("String", "find") => {
                write!(res, "if let Some(__res__) = ").unwrap();
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ".find(").unwrap();
                to_rust(ast, arg_pos, indentation, res, enums, info);
                write!(res, ") {{ __res__ as i32 }} else {{ -1 }}").unwrap();
            }
            ("String", "count") => {
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ".matches(").unwrap();
                to_rust(ast, arg_pos, indentation, res, enums, info);
                write!(res, ").count()").unwrap();
            }
            ("String", "removeprefix" | "removesuffix") => {
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ".strip_{}(", func_name.strip_prefix("remove").unwrap()).unwrap();
                to_rust(ast, arg_pos, indentation, res, enums, info);
                write!(res, ").unwrap_or(&").unwrap();
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ")").unwrap();
            }
            ("String", "lower" | "upper") => {
                write!(res, "{{").unwrap();
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ".make_ascii_{}case();", func_name).unwrap();
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, "}}").unwrap();
            }
            ("Vec", "append") => {
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ".push(").unwrap();
                to_rust(ast, arg_pos, indentation, res, enums, info);
                write!(res, ")").unwrap();
            }
            (_, "len") => {
                write!(res, "(").unwrap();
                to_rust(ast, children[0], indentation, res, enums, info);
                write!(res, ".len() as i32)").unwrap();
            }
            _ => { return false }
        }
        true
    } else { false }
}

fn built_in_funcs(
    ast: &[Ast], name: &str, indentation: usize, res: &mut String,
    enums: &mut HashMap<String, String>, children: &[usize], info: &Info
) -> bool { // todo get rid of unnecessary boxes
    match name {
        "reversed" => {
            write!(res, "Box::new(").unwrap();
            to_rust(ast, children[1], indentation, res, enums, info);
            write!(res, ".rev())").unwrap();
        }
        "range" => { // 3 ugly code
            let pos = ast[children[0]].parent.unwrap();
            let parent = ast[pos].parent.unwrap();
            let is_in_for = matches!(&ast[parent].value, AstNode::ForIter);

            let args = unwrap_u(&ast[children[1]].children);
            if !is_in_for {
                write!(res, "Box::new(").unwrap();
            }
            if args.is_empty() {
                panic!("not enough args passed to `range` expected at least 1")
            }
            let mut optional_args = vec![];
            for i in args.iter().skip(1) {
                if let Some(Type { kind: TypeKind::Enum(name), .. }) = &ast[*i].typ {
                    if name == "i32__or__bool" {
                        let func_call = ast[*i].children.as_ref().unwrap()[1];
                        let func_children = ast[func_call].children.as_ref().unwrap();
                        let opt_name = &ast[func_children[0]];
                        if matches!(&opt_name.value, AstNode::Identifier(n) if n == "_i32") {
                            let args_pos = &ast[func_children[1]];
                            optional_args.push(args_pos.children.as_ref().unwrap()[0]);
                            continue
                        }
                    }
                }
                break
            }
            match optional_args.len() {
                0 => {
                    write!(res, "(0..").unwrap();
                    to_rust(ast, args[0], 0, res, enums, info);
                    write!(res, ")").unwrap();
                }
                1 => {
                    write!(res, "(").unwrap();
                    to_rust(ast, args[0], 0, res, enums, info);
                    write!(res, "..").unwrap();
                    to_rust(ast, optional_args[0], 0, res, enums, info);
                    write!(res, ")").unwrap();
                }
                2 => {
                    write!(res, "(").unwrap();
                    to_rust(ast, args[0], 0, res, enums, info);
                    write!(res, "..").unwrap();
                    to_rust(ast, optional_args[0], 0, res, enums, info);
                    write!(res, ").step_by((").unwrap();
                    to_rust(ast, optional_args[1], 0, res, enums, info);
                    write!(res, ") as usize)").unwrap();
                }
                _ => panic!("too many args passed to `range` expected <=3 found {}", args.len())
            }
            if !is_in_for {
                write!(res, ")").unwrap();
            }
        },
        "print" => {
            //1 the first arg will be a vec cuz its *args
            let args = unwrap_u(&ast[children[1]].children);
            let args = unwrap_u(&ast[args[0]].children);
            let mut formats = String::new();
            for arg in args {
                let mut typ = ast[*arg].typ.as_ref().unwrap();
                let mut pointers = String::new();
                while let Type{ kind: k@ (TypeKind::Pointer | TypeKind::MutPointer), children } = typ {
                    pointers = match k {
                        TypeKind::Pointer => { format!("&{pointers}") }
                        _ => { format!("&mut {pointers}") }
                    };
                    typ = &children.as_ref().unwrap()[0];
                }
                if implements_trait(typ, "Display", ast, info) {
                    write!(formats, "{pointers}{{}} ").unwrap();
                } else if implements_trait(typ, "Debug", ast, info) {
                    write!(formats, "{pointers}{{:?}} ").unwrap();
                } else {
                    todo!()
                }
            }
            write!(res, "println!(\"{}\"", formats.trim_end()).unwrap();
            for arg in args {
                write!(res, ",").unwrap();
                to_rust(ast, *arg, 0, res, enums, info);
            }
            write!(res, ")").unwrap();
        }
        "min" => {
            let args = unwrap_u(&ast[children[1]].children);
            if args.len() == 1 {
                to_rust(ast, args[0], 0, res, enums, info);
                write!(res, ".min().expect(\"min on empty iter\")").unwrap();
            } else {
                todo!()
            }
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
    let mut typ = if let Some(t) = &ast[children[0]].typ { t } else { return  None };
    while let Type{ kind: TypeKind::MutPointer | TypeKind::Pointer, children: ch} = typ {
        typ = &unwrap(ch)[0];
    }
    if let Type{ kind: TypeKind::Struct(struct_name), .. } = typ {
        if let AstNode::Identifier(func_name) =
            &ast[unwrap_u(&ast[children[1]].children)[0]].value
        {
            return Some((struct_name, func_name))
        }
    }
    None
}

fn format_generics(generics_ast: &Ast) -> String {
    if let Some(Type{ children: Some(generics), .. }) = &generics_ast.typ {
        format!("<{}>", join(generics.iter().map(|x|
            unwrap_enum!(&x.kind, TypeKind::Generic(GenericType::Declaration(name)), name)
        ), ","))
    } else {
        EMPTY_STR
    }
}

pub fn implements_trait(mut typ: &Type, expected_trait: &str, ast: &[Ast], info: &Info) -> bool {
    if let TypeKind::Generic(GenericType::Of(_)) = &typ.kind {
        typ = &unwrap(&typ.children)[0];
    }
    println!("TYP: {typ}");
    match &typ.kind {
        TypeKind::Trait(name) => name == expected_trait,
        TypeKind::Struct(struct_name) => {
            let struct_def = &ast[info.structs[struct_name.get_str()].pos];
            let traits = &ast[unwrap_u(&struct_def.children)[3]];

            unwrap_u(&traits.children).iter().any(|trt|
                matches!(&ast[*trt].value, AstNode::Identifier(name) if expected_trait == name)
            )
        }
        _ => false
    }
}

enum IndexTyp {
    Mut, Ref, Val
}
fn should_be_mut_index(ast: &[Ast], pos: usize) -> IndexTyp {
    let parent = ast[pos].parent.unwrap();
    match &ast[parent].value {
        AstNode::Parentheses => should_be_mut_index(ast, parent),
        AstNode::Assignment | AstNode::Index =>
            if ast[parent].children.as_ref().unwrap()[0] == pos {
                IndexTyp::Mut
            } else {
                IndexTyp::Val
            }, // && should_be_mut_index(ast, parent),
        AstNode::ForStatement | AstNode::ArgsDef | AstNode::Bool(_) | AstNode::Char(_)
        | AstNode::ForVars | AstNode::Function(_) | AstNode::GenericsDeclaration
        | AstNode::Identifier(_) | AstNode::Arg { .. } | AstNode::IfStatement | AstNode::Enum(_)
        | AstNode::Number(_) | AstNode::Pass | AstNode::Continue | AstNode::Break
        | AstNode::ReturnType | AstNode::StaticFunction(_) | AstNode::String { .. }
        | AstNode::Struct(_) | AstNode::StructInit | AstNode::Trait { .. } | AstNode::Traits
        | AstNode::Type(_) | AstNode::Types | AstNode::WhileStatement | AstNode::ListComprehension
        | AstNode::SetComprehension | AstNode::DictComprehension
        => unreachable!(),
        AstNode::Args | AstNode::Body | AstNode::ColonParentheses | AstNode::DictLiteral
        | AstNode::FirstAssignment | AstNode::ForIter
        | AstNode::ListLiteral | AstNode::Module | AstNode::Operator(_) | AstNode::Return
        | AstNode::SetLiteral | AstNode::NamedArg(_)
        => IndexTyp::Val,
        AstNode::UnaryOp(op) => match op {
            OperatorType::MutPointer => IndexTyp::Mut,
            OperatorType::Pointer => IndexTyp::Ref,
            _ => IndexTyp::Val
        }
        AstNode::FunctionCall(_)  | AstNode::Property => IndexTyp::Mut,
    }
}
