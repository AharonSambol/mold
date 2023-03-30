use std::collections::hash_map::Entry;
use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode};
use std::fmt::Write;
use std::iter::zip;
use crate::{EMPTY_STR, IGNORE_ENUMS, IGNORE_FUNCS, IGNORE_STRUCTS, IGNORE_TRAITS, typ_with_child, unwrap_enum, some_vec};
use crate::add_types::ast_add_types::{get_associated_type, NUM_TYPES, SPECIFIED_NUM_TYPE_RE};
use crate::add_types::generics::apply_generics_from_base;
use crate::add_types::utils::{get_pointer_complete_inner, get_pointer_inner, is_float, join};
use crate::construct_ast::mold_ast::{Info};
use crate::construct_ast::tree_utils::{print_tree, update_pos_from_tree_node};
use crate::mold_tokens::OperatorType;
use crate::types::{unwrap_u, Type, TypeKind, TypName, GenericType, implements_trait, print_type};
use crate::{throw, CUR_COL, CUR_LINE, CUR_PATH, LINE_DIFF, SRC_CODE};

//4 probably using a string builder would be much more efficient (but less readable IMO)
pub fn to_rust(
    ast: &[Ast], pos: usize, indentation: usize, info: &Info
) -> String {
    update_pos_from_tree_node(&ast[pos]);

    let children = unwrap_u(&ast[pos].children);
    match &ast[pos].value {
        AstNode::Body => {
            let indent = "\t".repeat(indentation);
            format!(
                " {{{}\n{}}}",
                children.iter().map(|child|
                    format!("\n{indent}{};", to_rust(ast, *child, indentation, info))
                ).collect::<Vec<_>>().concat(),
                "\t".repeat(indentation - 1)
            )
        },
        AstNode::Module => {
            let indent = "\t".repeat(indentation);
            format!(
                "\n{indent}{}\n{indent}",
                children.iter().map(|child|
                    format!(
                        "\n{indent}{}",
                        to_rust(ast, *child, indentation, info)
                    )
                ).collect::<Vec<_>>().concat()
            )
        },
        AstNode::Function(name) | AstNode::StaticFunction(name) => {
            if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
                return EMPTY_STR
            }
            function_to_rust_str(name, ast, indentation, children, info)
        },
        AstNode::IfStatement => {
            let mut res = format!(
                "if {}{}",
                to_rust(ast, children[0], indentation, info),
                to_rust(ast, children[1], indentation + 1, info)
            );
            for child in children.iter().skip(2) {
                match &ast[*child].value {
                    AstNode::IfStatement => {
                        let c_children = ast[*child].ref_children();
                        write!(
                            res, "\n{}else if {}{}",
                            "\t".repeat(indentation),
                            to_rust(ast, c_children[0], indentation, info),
                            to_rust(ast, c_children[1], indentation + 1, info),
                        ).unwrap();
                    },
                    AstNode::Body => {
                        write!(
                            res, "\n{}else {}",
                            "\t".repeat(indentation),
                            to_rust(ast, *child, indentation + 1, info)
                        ).unwrap();
                    },
                    _ => throw!()
                }
            }
            res
        },
        AstNode::WhileStatement => {
            format!(
                "while {}{}",
                to_rust(ast, children[0], indentation, info),
                to_rust(ast, children[1], indentation + 1, info)
            )
        },
        AstNode::Assignment => {
            format!(
                "{} = {}",
                to_rust(ast, children[0], indentation, info),
                to_rust(ast, children[1], indentation, info)
            )
        },
        AstNode::FirstAssignment => {
            format!(
                "let {}{}{} = {}",
                if ast[pos].is_mut { "mut " } else { "" },
                to_rust(ast, children[0], indentation, info),
                if let Some(c) = &ast[children[0]].typ {
                    format!(": {c}")
                } else { EMPTY_STR },
                to_rust(ast, children[1], indentation, info)
            )
        }
        AstNode::OpAssignment(op) => {
            if let OperatorType::FloorDiv = op {
                if is_float(&ast[children[0]].typ.as_ref().unwrap().kind) {
                    let first = to_rust(ast, children[0], indentation, info);
                    return format!(
                        "{first} = ({first} / {}).floor()",
                        to_rust(ast, children[1], indentation, info)
                    )
                }
            }
            if is_string_addition(ast, children[0], children[1], op) {
                format!(
                    "{} += &{}",
                    to_rust(ast, children[0], indentation, info),
                    to_rust(ast, children[1], indentation, info),
                )
            } else {
                format!(
                    "{} {op}= {}",
                    to_rust(ast, children[0], indentation, info),
                    to_rust(ast, children[1], indentation, info)
                )
            }
        }
        AstNode::Identifier(name) => { name.clone() },
        AstNode::Operator(op) => {
            match op {
                OperatorType::FloorDiv => {
                    #[inline] fn div(
                        ast: &[Ast], children: &[usize], indentation: usize,
                        info: &Info
                    ) -> String {
                        format!(
                            "{} / {}",
                            to_rust(ast, children[0], indentation, info),
                            to_rust(ast, children[1], indentation, info)
                        )
                    }
                    if is_float(&ast[children[0]].typ.as_ref().unwrap().kind) {
                        format!("({}).floor()", div(ast, children, indentation, info))
                    } else {
                        div(ast, children, indentation, info)
                    }
                }
                OperatorType::Div => {
                    //1 casts both nums to f32 unless their f32/64 and joins with `/`
                    join(
                        children.iter().map(|child|
                            if is_float(&ast[*child].typ.as_ref().unwrap().kind) {
                                to_rust(ast, *child, indentation, info)
                            } else {
                                format!(
                                    "(({}) as f32)",
                                    to_rust(ast, *child, indentation, info)
                                )
                            }
                        ),
                        " / "
                    )
                }
                OperatorType::Pow => {
                    format!(
                        "({}).pow({})",
                        to_rust(ast, children[0], indentation, info),
                        to_rust(ast, children[1], indentation, info)
                    )
                }
                OperatorType::Is =>
                    is_to_rust(ast, indentation, info, children),
                OperatorType::IsNot =>
                    format!("!({})", is_to_rust(ast, indentation, info, children)),
                OperatorType::In =>
                    in_to_rust(ast, indentation, info, children),
                OperatorType::NotIn =>
                    format!("!({})", in_to_rust(ast, indentation, info, children)),
                _ if is_string_addition(ast, children[0], children[1], op) => {
                    format!(
                        "{} + &{}",
                        to_rust(ast, children[0], indentation, info),
                        to_rust(ast, children[1], indentation, info),
                    )
                }
                _ => {
                    format!(
                        "{} {op} {}",
                        to_rust(ast, children[0], indentation, info),
                        to_rust(ast, children[1], indentation, info),
                    )
                }
            }
        },
        AstNode::UnaryOp(op) =>
            format!("{op}{}", to_rust(ast, children[0], indentation, info)),
        AstNode::Parentheses =>
            format!("({})", to_rust(ast, children[0], indentation, info)),
        AstNode::ColonParentheses => {
            children.iter().map(|child|
                to_rust(ast, *child, indentation, info)
            ).collect::<Vec<_>>().concat()
        },
        AstNode::Index => { // TODO pointer to tuple? or pointer to pointer?
            let base = to_rust(ast, children[0], indentation, info);
            let index = to_rust(ast, children[1], indentation, info);
            let inner = get_pointer_complete_inner(ast[children[0]].typ.as_ref().unwrap());
            if let TypeKind::Tuple = inner.kind {
                if let AstNode::Number(num) = &ast[children[1]].value {
                    return format!("{base}.{num}")
                }
                let enum_typ = ast[pos].typ.as_ref().unwrap().to_string();
                let tuple_types = inner.children.as_ref().unwrap();
                let tuple_len = tuple_types.len();
                if tuple_types.iter().skip(1).all(|t| *t == tuple_types[0]) {
                    return format!(
                        "{{ \
                    let _idx_ = {index};\
                    match _idx_ {{ {}, _ => panic!(\"index `{{_idx_}}` out of range, tuple of len `{tuple_len}`\") }}\
                    }}",
                        join((0..tuple_len).map(
                            |i| format!(
                                "{i} | {} => {base}.{i}",
                                (i as isize) - (tuple_len as isize),
                            )
                        ), ", ")
                    )
                }
                return format!(
                    "{{ \
                    let _idx_ = {index};\
                    match _idx_ {{ {}, _ => panic!(\"index `{{_idx_}}` out of range, tuple of len `{tuple_len}`\") }}\
                    }}",
                    join((0..tuple_len).map(
                        |i| format!(
                            "{i} | {} => {enum_typ}::_{}({base}.{i})",
                            (i as isize) - (tuple_len as isize),
                            tuple_types[i]
                        )
                    ), ", ")
                )
            }
            format!(
                "({}{base}, {index}))",
                match ast[children[0]].typ.as_ref().unwrap().kind {
                    TypeKind::Pointer => "*_index(",
                    TypeKind::MutPointer => "*_index_mut(",
                    _ if ast[pos].is_mut => "*_index_mut(&mut ",
                    _ => "*_index(&"
                }
            )
            /*
            to_rust(ast, children[0], indentation, info);
            write!(res, "[(").unwrap();
            to_rust(ast, children[1], indentation, info);
            write!(res, ") as usize]").unwrap();
            */
        },
        AstNode::Number(num) => {
            if !SPECIFIED_NUM_TYPE_RE.is_match(num) {
                if num.contains('.') { format!("{num}f32") } else { format!("{num}i32") }
            } else { num.clone() }
        },
        AstNode::ListLiteral => {
            format!(
                "vec![{}]",
                join(children.iter().map(|child|
                    to_rust(ast, *child, indentation + 1, info)
                ), ", ")
            )
        },
        AstNode::SetLiteral => {
            format!(
                "HashSet::from([{}])",
                join(children.iter().map(|child|
                    to_rust(ast, *child, indentation + 1, info)
                ), ", ")
            )
        },
        AstNode::DictLiteral => {
            format!(
                "HashMap::from([{}])",
                join(children.windows(2).step_by(2).map(
                    |children| format!(
                        "({}, {})",
                        to_rust(ast, children[0], indentation + 1, info),
                        to_rust(ast, children[1], indentation + 1, info)
                    )
                ), ", ")
            )
        },
        AstNode::Pass => String::from("()"),
        AstNode::FunctionCall(_) => {
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if let Some(res) = built_in_funcs(ast, name, indentation, children, info) {
                    return res
                }
            }
            format!(
                "{}({})",
                to_rust(ast, children[0], indentation, info),
                if children.len() > 1 {
                    to_rust(ast, children[1], indentation, info)
                } else { EMPTY_STR }
            )
        },
        AstNode::Args | AstNode::ArgsDef => {
            if children.is_empty() { return EMPTY_STR }
            join(
                children.iter().map(|child|
                    to_rust(ast, *child, indentation, info)
                ),", "
            )
        },
        AstNode::Return => {
            format!(
                "return {}",
                if children.is_empty() { EMPTY_STR } else {
                    to_rust(ast, children[0], indentation, info)
                }
            )
        },
        AstNode::String { val, mutable } => {
            if *mutable { format!("String::from({val})") }
            else { val.clone() }
        },
        AstNode::Char(chr) => format!("'{chr}'"),
        AstNode::Property => {
            if let AstNode::FunctionCall(_) = ast[children[1]].value {
                if let Some(res) = built_in_methods(ast, indentation, children, info){
                    return res
                }
            }

            if matches!(ast[children[1]].value, AstNode::FunctionCall(true))
                || matches!(ast[children[0]].typ, Some(Type { kind: TypeKind::Enum(_), .. }))
            {
                format!(
                    "{}::{}",
                    to_rust(ast, children[0], indentation, info),
                    to_rust(ast, children[1], indentation, info)
                )
            } else {
                format!(
                    "{}.{}",
                    to_rust(ast, children[0], indentation, info),
                    to_rust(ast, children[1], indentation, info)
                )
            }
        },
        AstNode::ForStatement => {
            format!(
                "for {} {}",
                to_rust(ast, children[0], indentation, info),
                to_rust(ast, children[1], indentation + 1, info)
            )
        },
        AstNode::ForVars => {
            join(children.iter().map(|child|
                format!("mut {}", to_rust(ast, *child, indentation, info))
            ), ", ")
        },
        AstNode::ForIter => {
            format!(" in {}", to_rust(ast, children[0], indentation, info))
        },
        AstNode::Struct(name) => { //3 warning long code...
            if unsafe { IGNORE_STRUCTS.contains(name.as_str()) } {
                return EMPTY_STR
            }
            let generics_ast = &ast[children[0]];
            let param = &ast[children[1]];
            let generic = format_generics(generics_ast);
            let param = join(
                unwrap_u(&param.children).iter()
                    .map(|&x|
                        format!("{}: {}",
                            unwrap_enum!(&ast[x].value, AstNode::Arg { name, .. }, name),
                            ast[x].typ.as_ref().unwrap()
                        )
                    ),
                ", "
            );
            let mut res = format!(
                "/*#[derive(Clone, PartialEq)]*/\npub struct {name}{generic} {{ {param} }}\n\
                impl{generic} {name}{generic} {{"
            );
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
                write!(
                    &mut res,
                    "\n{}{}",
                    "\t".repeat(indentation + 1),
                    to_rust(ast, *func, indentation + 1, info)
                ).unwrap();
            }
            let indent = "\t".repeat(indentation);
            write!(&mut res, "\n{indent}}}").unwrap();
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
                write!(&mut res, "\n{indent}impl {trait_name} for {name} {{").unwrap();
                let mut type_defs = HashMap::new();
                for (func_pos, func_name) in funcs {
                    write!(&mut res, "\n{indent}    ").unwrap();
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
                    write!(&mut res, "{}", function_to_rust_str(
                        func_name, ast, indentation + 1, func_children, info
                    )).unwrap();
                }
                for (name, typ) in type_defs {
                    write!(&mut res, "\n\ttype {name} = {typ};").unwrap();
                }
                write!(&mut res, "\n{indent}}}").unwrap();
            }
            res
        },
        // AstNode::StructInit => {
        //     let arg_vals = unwrap_u(&ast[children[1]].children);
        //     format!(
        //         "{}::new({})",
        //         ast[pos].typ.clone().unwrap(),
        //         join(
        //             arg_vals.iter().map(|x|
        //                 to_rust(ast, *x, indentation, info),
        //             ),
        //             ", "
        //         )
        //     )
        // }
        AstNode::RustStructInit => {
            let args = unwrap_u(&ast[children[1]].children);
            format!(
                "{}{{ {} }}",
                ast[pos].typ.clone().unwrap(),
                join(
                    args.iter().map(|x| {
                        let name = unwrap_enum!(&ast[*x].value, AstNode::Identifier(name), name);
                        format!("{name}: _self_{name}_")
                    }),
                    ", "
                )
            )
        }
        AstNode::Bool(b) => if *b { String::from("true") } else { String::from("false") },
        AstNode::Continue => String::from("continue"),
        AstNode::Break => String::from("break"),
        AstNode::ReturnType => unreachable!(),
        AstNode::Trait { name, .. } => {
            if unsafe { IGNORE_TRAITS.contains(name.as_str()) } {
                return EMPTY_STR
            }
            let generics_ast = &ast[children[0]];
            let generic = format_generics(generics_ast);
            let a_types = format_associated_types(generics_ast);
            let functions = unwrap_u(&ast[children[1]].children);

            let mut res = format!("pub trait {name}{generic} {{ \n\t{a_types}");
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
                            let name = unwrap_enum!(&arg.value, AstNode::Arg { name, .. }, name);
                            // TODO if is_arg \ is_kwarg
                            let typ = arg.typ.as_ref().unwrap();
                            format!("{name}: {typ}")
                        });
                    let args = join(args, ", ");
                    let return_typ = &ast[func_children[2]].typ;
                    if let Some(rt) = return_typ {
                        write!(&mut res, "\n\tfn {func_name}{func_generics}({args}) -> {rt};").unwrap();
                    } else {
                        write!(&mut res, "\n\tfn {func_name}{func_generics}({args});").unwrap();
                    }
                } else {
                    write!(&mut res, "\n\ttype {};", unwrap_enum!(&func.value, AstNode::Type(t), t)).unwrap();
                }
            }
            write!(res, "\n}}").unwrap();
            res
        }
        AstNode::Enum(name) => {
            if unsafe { IGNORE_ENUMS.contains(name.as_str()) } {
                return EMPTY_STR
            }
            let generics_ast = &ast[children[0]];
            let generic = format_generics(generics_ast);
            let module = unwrap_u(&ast[children[1]].children);
            format!(
                "/*#[derive(Clone, PartialEq)]*/\npub enum {name}{generic}{{ {} }}",
                join(module.iter().map(|option| {
                    let name = unwrap_enum!(&ast[*option].value, AstNode::Identifier(n), n);
                    let types = unwrap_u(&ast[*option].children);
                    format!(
                        "\n\t{name}{}",
                        if types.is_empty() { EMPTY_STR } else {
                            format!(
                                "({})",
                                join(types.iter().map(|typ|
                                    ast[*typ].typ.as_ref().unwrap()
                                ), ",")
                            )
                        }
                    )
                }), ", ")
            )
        }
        AstNode::NamedArg(_) => {
            to_rust(ast, unwrap_u(&ast[pos].children)[0], indentation, info)
        }
        AstNode::ListComprehension | AstNode::SetComprehension | AstNode::DictComprehension => {
            let loops = ast[children[1]].children.clone().unwrap();
            format!(
                "{}{}{}{}({});{}res}}",
                /*1 initialize*/ match &ast[pos].value {
                    AstNode::ListComprehension => "{let mut res = vec![];",
                    AstNode::SetComprehension => "{let mut res = HashSet::new();",
                    AstNode::DictComprehension => "{let mut res = HashMap::new();",
                    _ => unreachable!()
                },
                /*1 loops*/ loops.iter().map(|r#loop| {
                    let colon_par = ast[*r#loop].ref_children()[0];
                    format!("for {} {{", to_rust(ast, colon_par, indentation, info))
                }).collect::<Vec<_>>().concat(),
                /*1 condition*/ if let Some(condition) = &ast[children[2]].children {
                    format!("if {} {{", to_rust(ast, condition[0], indentation, info))
                } else { EMPTY_STR },
                /*1 add method*/ if let AstNode::ListComprehension = &ast[pos].value { "res.push" } else { "res.insert" },
                /*1 add element*/ if let AstNode::DictComprehension = &ast[pos].value {
                    let parts = ast[children[0]].ref_children();
                    format!(
                        "{},{}",
                        to_rust(ast, parts[0], indentation, info),
                        to_rust(ast, parts[1], indentation, info)
                    )
                } else {
                    to_rust(ast, ast[children[0]].ref_children()[0], indentation, info)
                },
                /*1 close braces*/ "}".repeat(loops.len() + (ast[children[2]].children.is_some() as usize))
            )
        }
        AstNode::Cast => {
            format!(
                "{} as {}",
                to_rust(ast, children[0], indentation, info),
                ast[pos].typ.as_ref().unwrap()
            )
        }
        AstNode::Arg { name, .. } => name.clone(),
        AstNode::Ternary => {
            format!(
                "if {} {{ {} }} else {{ {} }}",
                to_rust(ast, children[1], indentation, info),
                to_rust(ast, children[0], indentation, info),
                to_rust(ast, children[2], indentation, info),
            )
        }
        AstNode::Import => {
            if let AstNode::From = ast[children[0]].value {
                format!(
                    "use super::{}::{{ {} }};",
                    join(
                        ast[children[0]].ref_children()
                            .iter()
                            .map(|i| to_rust(
                                ast, *i, indentation, info
                            )),
                        "::"
                    ),
                    join(
                        children.iter().skip(1).map(|i| to_rust(
                            ast, *i, indentation, info
                        )),
                        ", "
                    )
                )
            } else {
                format!(
                    "use super::{{ {} }};",
                    join(
                        children.iter().map(|i| to_rust(
                            ast, *i, indentation, info
                        )),
                        ", "
                    )
                )
            }
        }
        AstNode::As(new_name) => {
            format!(
                "{} as {new_name}",
                to_rust(ast, children[0], indentation, info)
            )
        },
        AstNode::Ignore => EMPTY_STR,
        AstNode::Match => {
            let sep = format!("\n{}", "\t".repeat(indentation + 1));
            let sep_m1 = format!("\n{}", "\t".repeat(indentation));
            format!(
                "match {} {{ {sep}{}{sep_m1}}}",
                to_rust(ast, children[0], indentation, info),
                join(
                    children.iter().skip(1).map(
                        |child| to_rust(ast, *child, indentation + 1, info)
                    ),
                    sep.as_str()
                )
            )
        }
        AstNode::Case => {
            let parent_match = &ast[*ast[pos].parent.as_ref().unwrap()];
            let match_on = parent_match.ref_children()[0];
            let match_on = get_pointer_complete_inner(ast[match_on].typ.as_ref().unwrap());
            let is_one_of = matches!(&match_on.kind, TypeKind::OneOf);
            // println!("is_one_of: {is_one_of}, {:?}", &ast[match_on].typ);
            let condition_children = ast[children[0]].ref_children();
            let option_name = to_rust(ast, condition_children[0], indentation, info);
            let body = to_rust(ast, *children.last().unwrap(), indentation + 1, info);
            if option_name == "_" {
                format!("_ => {{ {body} }}")
            } else if is_one_of {
                if option_name.split_once("::").unwrap().1 == "_None" { //3 this is trash code
                    return format!("{option_name} => {{ {body} }}")
                }
                format!(
                    "{option_name}({}) => {{ {body} }}",
                    if children.len() == 3 {
                        unwrap_enum!(&ast[children[1]].value, AstNode::Identifier(n), n)
                    } else { "_" }
                )
            } else if condition_children.len() == 1 {
                format!("{option_name} => {{ {body} }}")
            } else {
                format!(
                    "{option_name}({}) => {{ {body} }}",
                    join(
                        ast[condition_children[1]].ref_children().iter().map(
                            |x| to_rust(ast, *x, indentation, info),
                        ),
                        ", "
                    )
                )
            }
        }
        AstNode::Null => throw!("unexpected `None`, can only assign `None` to variable which is of `union` type"),
        AstNode::Tuple => {
            format!(
                "({})",
                join(children.iter().map(
                    |child| to_rust(ast, *child, indentation, info)
                ), ", ")
            )
        }
        AstNode::VArgs => {
            format!(
                "vec![{}]",
                join(children.iter().map(
                    |child| to_rust(ast, *child, indentation, info)
                ), ", ")
            )
        }
        _ => throw!("Unexpected AST `{:?}`", ast[pos].value)
    }
}

fn in_to_rust(
    ast: &[Ast], indentation: usize, info: &Info, children: &[usize]
) -> String {
    #[inline] fn is_hm(node: &Ast) -> bool {
        let node_typ = node.typ.as_ref().unwrap();
        let node_typ = get_pointer_complete_inner(node_typ);
        if let TypeKind::Struct(name) = &node_typ.kind {
            if name == "HashMap" {
                return true
            }
        }
        false
    }
    format!(
        "{}.{}({}{})",
        to_rust(ast, children[1], indentation, info),
        if is_hm(&ast[children[1]]) { "contains_key" } else { "contains" },
        if let TypeKind::Pointer | TypeKind::MutPointer = ast[children[0]].typ.as_ref().unwrap().kind {
            ""
        } else { "&" },
        to_rust(ast, children[0], indentation, info)
    )
}

fn is_to_rust(
    ast: &[Ast], indentation: usize, info: &Info, children: &[usize]
) -> String {
    #[inline] fn as_ptr(
        ast: &[Ast], pos: usize, indentation: usize, info: &Info
    ) -> String {
        if let TypeKind::Pointer | TypeKind::MutPointer = ast[pos].typ.as_ref().unwrap().kind {
            to_rust(ast, pos, indentation, info)
        } else {
            format!(
                "&({}) as *const _",
                to_rust(ast, pos, indentation, info)
            )
        }
    }
    format!(
        "ptr::eq({}, {})",
        as_ptr(ast, children[0], indentation, info),
        as_ptr(ast, children[1], indentation, info)
    )
}

fn function_to_rust_str(
    name: &str, ast: &[Ast], indentation: usize, children: &[usize], info: &Info
) -> String {
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
    let res = if let Some(t) = &return_typ.typ {
        format!("pub fn {name}{generic}({param}) -> {t}")
    } else {
        format!("pub fn {name}{generic}({param})")
    };
    format!(
        "{res}{}",
        to_rust(ast, children[3], indentation + 1, info)
    )
}

fn built_in_methods(
    ast: &[Ast], indentation: usize, children: &[usize], info: &Info
) -> Option<String> {
    if let Some((struct_name, func_name)) = get_struct_and_func_name(ast, children) {
        let arg_pos = unwrap_u(&ast[children[1]].children)[1];
        Some(match (struct_name.get_str(), func_name.as_str()) {
            ("String", "split") => {
                if !unwrap_u(&ast[arg_pos].children).is_empty() {
                    format!(
                        "{}.split({})",
                        to_rust(ast, children[0], indentation, info),
                        to_rust(ast, arg_pos, indentation, info)
                    )
                } else {
                    format!(
                        "{}.split_whitespace()",
                        to_rust(ast, children[0], indentation, info)
                    )
                }
            }
            ("String", "strip" | "lstrip" | "rstrip") => {
                let trim = match func_name.as_str() {
                    "strip" => "trim",
                    "lstrip" => "trim_start",
                    "rstrip" => "trim_end",
                    _ => unreachable!()
                };
                format!(
                    "{}.{trim}{}",
                    to_rust(ast, children[0], indentation, info),
                    if !unwrap_u(&ast[arg_pos].children).is_empty() {
                        format!(
                            "_matches({})",
                            to_rust(ast, arg_pos, indentation, info)
                        )
                    } else {
                        String::from("()")
                    }
                )
            }
            ("String", "startswith" | "endswith") => {
                format!(
                    "{}.{}_with({})",
                    to_rust(ast, children[0], indentation, info),
                    func_name.strip_suffix("with").unwrap(),
                    to_rust(ast, arg_pos, indentation, info)
                )
            }
            ("String", "find") => {
                format!(
                    "if let Some(__res__) = {}.find({}) {{ __res__ as i32 }} else {{ -1 }}",
                    to_rust(ast, children[0], indentation, info),
                    to_rust(ast, arg_pos, indentation, info),
                )
            }
            ("String", "count") => {
                format!(
                    "{}.matches({}).count()",
                    to_rust(ast, children[0], indentation, info),
                    to_rust(ast, arg_pos, indentation, info),
                )
            }
            ("String", "removeprefix" | "removesuffix") => {
                format!(
                    "{}.strip_{}({}).unwrap_or(&{})",
                    to_rust(ast, children[0], indentation, info),
                    func_name.strip_prefix("remove").unwrap(),
                    to_rust(ast, arg_pos, indentation, info),
                    to_rust(ast, children[0], indentation, info),
                )
            }
            ("String", "lower" | "upper") => {
                format!(
                    "{{ {}.make_ascii_{func_name}case(); {} }}",
                    to_rust(ast, children[0], indentation, info),
                    to_rust(ast, children[0], indentation, info)
                )
            }
            ("Vec", "append") => {
                format!(
                    "{}.push({})",
                    to_rust(ast, children[0], indentation, info),
                    to_rust(ast, arg_pos, indentation, info)
                )
            }
            // (_, "len") => {
            //     format!(
            //         "({}.len() as i32)",
            //         to_rust(ast, children[0], indentation, info)
            //     )
            // }
            _ => { return None }
        })
    } else { None }
}

fn built_in_funcs(
    ast: &[Ast], name: &str, indentation: usize, children: &[usize], info: &Info
) -> Option<String> { // todo get rid of unnecessary boxes
    #[inline] fn is_hm(ast: &[Ast], pos: usize) -> bool {
        matches!(
            &ast[pos].typ.as_ref().unwrap().kind,
            TypeKind::Struct(name) if name == "HashMap"
        )
    }
    fn into_iter(ast: &[Ast], pos: usize) -> &'static str {
        let is_iter = matches!(
            &ast[pos].typ.as_ref().unwrap().kind,
            TypeKind::Trait(name) if name == "Iterator"
        );
        if is_iter { "" } else if is_hm(ast, pos) { ".keys()" } else { ".iter()" }
    }
    let args = unwrap_u(&ast[children[1]].children);

    Some(match name {
        "reversed" => {
            format!(
                "Box::new({}{}.rev())",
                to_rust(ast, args[0], indentation, info),
                into_iter(ast, args[0])
            )
        }
        "range" => { // 3 ugly code
            let pos_in_ast = ast[children[0]].parent.unwrap();
            let parent = ast[pos_in_ast].parent.unwrap();
            let is_in_for = matches!(&ast[parent].value, AstNode::ForIter);

            // if args.is_empty() {
            //     throw!("not enough args passed to `range` expected at least 1")
            // }

            let mut optional_args = vec![];
            for i in args.iter().skip(1) {
                if let Some(Type { kind: TypeKind::Null, .. }) = &ast[*i].typ {
                    break
                }
                optional_args.push(*i);
            }
            format!(
                "{}{}{}",
                if !is_in_for { "Box::new(" } else { "" },
                match optional_args.len() {
                    0 => format!("(0..{})", to_rust(ast, args[0], 0, info)),
                    1 => {
                        format!(
                            "({}..{})",
                            to_rust(ast, args[0], 0, info),
                            to_rust(ast, optional_args[0], 0, info)
                        )
                    }
                    2 => {
                        format!(
                            "({}..{}).step_by(({}) as usize)",
                            to_rust(ast, args[0], 0, info),
                            to_rust(ast, optional_args[0], 0, info),
                            to_rust(ast, optional_args[1], 0, info)
                        )
                    }
                    _ => unreachable!()// throw!("too many args passed to `range` expected <=3 found {}", args.len())
                },
                if !is_in_for { ")" } else { "" }
            )
        },
        "print" => {
            //1 the first arg will be a vec cuz its *args
            let args = unwrap_u(&ast[args[0]].children);
            let mut formats = String::new();
            let mut is_debug_vc = vec![false; args.len()];
            for (i, arg) in args.iter().enumerate() {
                // assert!(matches!(&ast[*arg].value, AstNode::Property));
                // let property_children = ast[*arg].ref_children();
                // assert!(matches!(&ast[property_children[0]].value, AstNode::Identifier(idf) if idf == "_boxof_Debug_endof___or___boxof_Display_endof_"));
                // assert!(matches!(&ast[property_children[1]].value, AstNode::FunctionCall(true)));
                // let func_children = ast[property_children[1]].ref_children();
                // let is_debug = unwrap_enum!(&ast[func_children[0]].value, AstNode::Identifier(varient), varient) == "__boxof_Debug_endof_";
                // if is_debug {
                //     is_debug_vc[i] = true;
                //     write!(formats, "{{:?}} ").unwrap();
                // } else {
                //     write!(formats, "{{}} ").unwrap();
                // }
                let mut typ = ast[*arg].typ.as_ref().unwrap();
                let mut pointers = String::new();
                while let Type{ kind: k@ (TypeKind::Pointer | TypeKind::MutPointer), children } = typ {
                    pointers = match k {
                        TypeKind::Pointer => { format!("&{pointers}") }
                        _ => { format!("&mut {pointers}") }
                    };
                    typ = &children.as_ref().unwrap()[0];
                }

                let display = typ_with_child! {
                    TypeKind::Trait(TypName::Static("Display")),
                    Type {
                        kind: TypeKind::GenericsMap,
                        children: None
                    }
                };
                let debug = typ_with_child! {
                    TypeKind::Trait(TypName::Static("Debug")),
                    Type {
                        kind: TypeKind::GenericsMap,
                        children: None
                    }
                };
                if implements_trait(typ, &display, ast, info).is_some() {
                    write!(formats, "{pointers}{{}} ").unwrap();
                } else if implements_trait(typ, &debug, ast, info).is_some() {
                    write!(formats, "{pointers}{{:?}} ").unwrap();
                } else {
                    update_pos_from_tree_node(&ast[*arg]);
                    throw!("expected `Display` or `Debug` but found `{}`", typ);
                }
            }
            format!(
                "println!(\"{}\", {})",
                formats.trim_end(),
                join(args.iter().zip(is_debug_vc).map(|(arg, is_dbg)|
                    to_rust(ast, *arg, 0, info)
                ), ", ")
            )
        }
        "min" | "max" => {
            if args.len() == 1 {
                format!(
                    "{}{}.{name}().expect(\"{name} on empty iter\")",
                    to_rust(ast, args[0], 0, info),
                    into_iter(ast, args[0])
                )
            } else {
                todo!()
            }
        }
        "iter" | "iter_imut" => { // TODO this doesn't need to be boxed
            format!(
                "Box::new({}.{}())",
                to_rust(ast, args[0], 0, info),
                if is_hm(ast, args[0]) { "keys" }
                else if name == "iter" { "iter_mut" }
                else { "iter" }
            )
        }
        "len" => {
            format!("(({}).len() as i32)", to_rust(ast, args[0], 0, info))
        }
        "abs" => {
            format!("({}).abs()", to_rust(ast, args[0], 0, info))
        }
        "sum" => {
            let inner_typ = get_associated_type(
                ast[args[0]].typ.as_ref().unwrap(),
                vec!["IntoIterator", "Iterator"], "Item", info
            ).expect("`sum` on non `Iterator | IntoIterator`");
            let inner_typ = apply_generics_from_base(
                &Some(inner_typ), ast[args[0]].typ.as_ref().unwrap()
            ).unwrap();
            format!(
                "{}{}.sum::<{inner_typ}>()",
                to_rust(ast, args[0], 0, info),
                into_iter(ast, args[0]),
            )
        }
        "pow" => {
            //1 the first arg will be a vec cuz its *args
            // let args = unwrap_u(&ast[args[0]].children);

            if args.len() < 2 {
                throw!("not enough args passed to `pow` expected at least 2 (at most 3), got `{}`", args.len())
            }
            if args.len() > 3 {
                throw!("too many args passed to `pow` expected at most 3 (at least 2), got `{}`", args.len())
            }
            let base_typ = unwrap_enum!(
                &ast[args[0]].typ.as_ref().unwrap().kind,
                TypeKind::Struct(name), name.get_str()
            );
            let is_float = ["f32", "f64"].contains(&base_typ);
            // let cast_to = if is_float { base_typ } else { "u32" };
            // throw!("{:?}", ast[args[2]].value);

            format!(
                "({}).{}({}{})",
                to_rust(ast, args[0], 0, info),
                if is_float { "powf" } else { "pow" },
                to_rust(ast, args[1], 0, info),
                if let AstNode::Null = ast[args[2]].value {
                    EMPTY_STR
                } else {
                    format!(" % ({})", to_rust(ast, args[2], 0, info))
                }
            )
        }
        "input" => {
            format!( //1 pop removes the \n
                "{{ let mut _res_ = String::new();\
                {}\
                std::io::stdin().read_line(&mut _res_)\
                .ok().expect(\"Failed to read line\");\
                _res_.pop();\
                _res_\
                }}",
                if !args.is_empty() {
                    format!(
                        "print!(\"{{}}\", {}); \
                        std::io::Write::flush(&mut std::io::stdout()).expect(\"io flush failed\");",
                        to_rust(ast, args[0], indentation, info)
                    )
                } else { EMPTY_STR }
            )
        }
        "int" => {
            let arg_typ = ast[args[0]].typ.as_ref().unwrap();
            let base = ast[args[1]].typ.as_ref().unwrap();
            let arg = to_rust(ast, args[0], indentation, info);
            let TypeKind::Null = base.kind else {
                let base = to_rust(ast, args[1], indentation, info);
                if let TypeKind::Pointer | TypeKind::MutPointer = &arg_typ.kind {
                    let arg_typ = get_pointer_inner(arg_typ);
                    if let TypeKind::Struct(name) = &arg_typ.kind {
                        if name != "String" {
                            throw!("int() can't convert non-string with explicit base");
                        }
                        return Some(format!("{{\
                        let _arg_ = {arg};\
                        let _base_ = {base};\
                        i32::from_str_radix(_arg_, _base_ as u32).unwrap_or_else(|_| panic!(\"invalid literal for int() with base {{_base_}}: `{{_arg_}}`\"))\
                        }}"))
                    }
                }
                throw!()
            };
            if let TypeKind::Struct(name) = &arg_typ.kind {
                if NUM_TYPES.contains(name.get_str()) || name == "bool" {
                    return Some(format!("({arg} as i32)"))
                }
            } else if let TypeKind::Pointer | TypeKind::MutPointer = &arg_typ.kind {
                let inner = get_pointer_inner(arg_typ);
                if let TypeKind::Struct(name) = &inner.kind {
                    if name == "String" {
                        return Some(format!("{{\
                        let _arg_ = {arg};\
                        _arg_.parse::<i32>().unwrap_or_else(|_| panic!(\"invalid literal for int() with base 10: `{{_arg_}}`\"))\
                        }}"))
                    }
                }
            }
            print_type(&Some(arg_typ.clone()));
            unreachable!()
        }
        "str" => {
            format!("({}).to_string()", to_rust(ast, args[0], indentation, info))
        }
        "exit" => {
            format!("std::process::exit({});", to_rust(ast, args[0], indentation, info))
        }
        _ => { return None }
    })
}

fn is_string_addition(ast: &[Ast], pos1: usize, pos2: usize, op: &OperatorType) -> bool{
    matches!(op, OperatorType::Plus)
    && matches!(&ast[pos1].typ, Some(Type { kind: TypeKind::Struct(nm), .. }) if nm == "String")
    && matches!(&ast[pos2].typ, Some(Type { kind: TypeKind::Struct(nm), .. }) if nm == "String")
}

pub fn get_struct_and_func_name<'a>(ast: &'a [Ast], children: &[usize]) -> Option<(&'a TypName, &'a String)> {
    let typ = if let Some(t) = &ast[children[0]].typ { t } else { return  None };
    let typ = get_pointer_complete_inner(typ);
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
        let generics: Vec<_> = generics.iter().filter_map(|x|
            if let TypeKind::Generic(GenericType::Declaration(name)) = &x.kind {
                Some(name.clone())
            } else { None }
        ).collect();
        if generics.is_empty() { EMPTY_STR } else {
            format!("<{}>", generics.join(","))
        }
    } else { EMPTY_STR }
}

fn format_associated_types(generics_ast: &Ast) -> String {
    if let Some(Type{ children: Some(generics), .. }) = &generics_ast.typ {
        let generics: Vec<_> = generics.iter().filter_map(|x|
            if let TypeKind::AssociatedType(name) = &x.kind {
                if let Some(children) = &x.children {
                    Some(format!("type {} = {};", name.clone(), children[0]))
                } else {
                    Some(format!("type {};", name.clone()))
                }
            } else { None }
        ).collect();
        if generics.is_empty() { EMPTY_STR } else {
            generics.join("\n\t")
        }
    } else { EMPTY_STR }
}


// enum IndexTyp {
//     Mut, Ref, Val
// }
// fn should_be_mut_index(ast: &[Ast], pos: usize) -> IndexTyp {
//     let parent = ast[pos].parent.unwrap();
//     match &ast[parent].value {
//         AstNode::Parentheses => should_be_mut_index(ast, parent),
//         AstNode::Assignment | AstNode::Index =>
//             if ast[parent].ref_children()[0] == pos {
//                 IndexTyp::Mut
//             } else {
//                 IndexTyp::Val
//             }, // && should_be_mut_index(ast, parent),
//         AstNode::ForStatement | AstNode::ArgsDef | AstNode::Bool(_) | AstNode::Char(_)
//         | AstNode::ForVars | AstNode::Function(_) | AstNode::GenericsDeclaration
//         | AstNode::Identifier(_) | AstNode::Arg { .. } | AstNode::IfStatement | AstNode::Enum(_)
//         | AstNode::Number(_) | AstNode::Pass | AstNode::Continue | AstNode::Break
//         | AstNode::ReturnType | AstNode::StaticFunction(_) | AstNode::String { .. }
//         | AstNode::Struct(_) | AstNode::StructInit | AstNode::Trait { .. } | AstNode::Traits
//         | AstNode::Type(_) | AstNode::Types | AstNode::WhileStatement | AstNode::ListComprehension
//         | AstNode::SetComprehension | AstNode::DictComprehension
//         => unreachable!(),
//         AstNode::Args | AstNode::Body | AstNode::ColonParentheses | AstNode::DictLiteral
//         | AstNode::FirstAssignment | AstNode::ForIter
//         | AstNode::ListLiteral | AstNode::Module | AstNode::Operator(_) | AstNode::Return
//         | AstNode::SetLiteral | AstNode::NamedArg(_)
//         => IndexTyp::Val,
//         AstNode::UnaryOp(op) => match op {
//             OperatorType::MutPointer => IndexTyp::Mut,
//             OperatorType::Pointer => IndexTyp::Ref,
//             _ => IndexTyp::Val
//         }
//         AstNode::FunctionCall(_)  | AstNode::Property => IndexTyp::Mut,
//     }
// }
