use std::collections::{HashMap, HashSet};
use std::fmt;
use fmt::Write;
use std::env::{args, vars};
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::find_generics::{find_generics_in_typ, get_generic_names, get_generics, is_generic};
use crate::construct_ast::get_typ::{get_arg_typ, get_params, try_get_arg_typ};
use crate::construct_ast::mold_ast::{FuncType, StructType, TraitType, Info, make_ast_statement, VarTypes, EnumType, add_trait_to_struct};
use crate::construct_ast::tree_utils::{add_to_tree, print_tree};
use crate::mold_tokens::{IsOpen, OperatorType, Pos, SolidToken, SolidTokenWPos};
use crate::types::{GenericType, print_type, Type, TypeKind, TypName, UNKNOWN_TYPE, unwrap, unwrap_u};
use crate::{add_trait, IMPL_TRAITS, Implementation, ImplTraitsKey, ImplTraitsVal, IS_COMPILED, PARSED_FILES, some_vec, typ_with_child, unwrap_enum};
use crate::add_types::ast_add_types::add_types;
use crate::add_types::utils::{add_to_stack, update_pos_from_token};
use crate::{throw, CUR_COL, CUR_LINE, CUR_PATH, LINE_DIFF, SRC_CODE};

pub fn make_func(
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, info: &mut Info,
) -> usize {
    let (p, body_pos, func_generics) = make_func_signature(
        tokens, pos, ast, parent, vars, info, 
    );
    pos = p;
    info.generics.push(func_generics);
    let res = make_ast_statement(
        tokens, pos, ast, body_pos, indent + 1,
        vars, info, 
    );
    info.generics.pop();
    res
}

fn make_func_signature(
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    vars: &mut VarTypes, info: &mut Info
) -> (usize, usize, HashSet<String>) {
    // let is_static = matches!(&tokens[pos], SolidToken::Static);
    pos += 1;
    let mut name =
        if let SolidToken::Word(name) = &tokens[pos].tok { name.clone() }
        else {
            update_pos_from_token(&tokens[pos]);
            throw!("Invalid name for function `{}`", tokens[pos].tok)
        };
    let src_pos = tokens[pos].pos.clone();
    pos += 1;

    //1 if is trait function
    // TODO Trt[int]::func_name()
    if matches!(tokens[pos].tok, SolidToken::Colon) && matches!(tokens[pos + 1].tok, SolidToken::Colon) {
        pos += 2; //1 skip the ::
        write!(name, "::{}", unwrap_enum!(&tokens[pos].tok, SolidToken::Word(w), w)).unwrap();
        pos += 1; //1 skip the next word
        let trait_name = name.split("::").next().unwrap();
        let parent_struct = ast[parent].parent.unwrap();
        let parent_struct_name = unwrap_enum!(&ast[parent_struct].value, AstNode::Struct(n), n);
        // let traits = unwrap_u(&ast[parent_struct].children)[3];
        let key = ImplTraitsKey {
            name: parent_struct_name.clone(),
            path: info.cur_file_path.to_str().unwrap().to_string(),
        };
        let val = ImplTraitsVal {
            trt_name: trait_name.to_string(),
            implementation: Implementation::None,
            types: None,
            generics: None,
        };
        add_trait!(key, val);
    }
    let index = add_to_tree(parent, ast, Ast::new(
        AstNode::StaticFunction(name.clone()),
        src_pos
    ));

    //1 generics
    let (generics_names, associated_types_names) = get_generics(&mut pos, tokens, index, ast, info);
    let mut struct_generics_names = vec![];
    let mut struct_a_types_names = vec![];
    let mut generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    let mut a_types_hs: HashSet<String> = HashSet::from_iter(associated_types_names.iter().cloned());
    if let Some(struct_parent) = ast[parent].parent {
        if let AstNode::Struct(_) = ast[struct_parent].value {
            let struct_generics = &ast[unwrap_u(&ast[struct_parent].children)[0]];
            let stct_g_typ = struct_generics.typ.as_ref().unwrap();
            for child in unwrap(&stct_g_typ.children) {
                match &child.kind {
                    TypeKind::Generic(GenericType::Declaration(name)) => {
                        struct_generics_names.push(name.clone());
                        generics_hs.insert(name.clone());
                    }
                    TypeKind::AssociatedType(name) => {
                        struct_a_types_names.push(name.clone());
                        a_types_hs.insert(name.clone());
                    }
                    _ => unreachable!()
                }

            }
        }
    }
    info.generics.push(generics_hs.clone());

    let SolidToken::Parenthesis(IsOpen::True) = tokens[pos].tok else {
        update_pos_from_token(&tokens[pos]);
        throw!("expected `(` or `::`, found `{}`", tokens[pos].tok)
    };
    pos += 1;
    let mut params = get_params(tokens, &mut pos, info);
    let args_pos = add_to_tree(index, ast, Ast::new_no_pos(AstNode::ArgsDef));
    let return_pos = add_to_tree(index, ast, Ast::new_no_pos(AstNode::ReturnType));
    let body_pos = add_to_tree(index, ast, Ast::new_no_pos(AstNode::Body));

    let mut input = vec![];
    for (param, default_val, src_pos) in &mut params {
        if param.name == "self" && param.typ == UNKNOWN_TYPE {
            param.typ = Type {
                kind: TypeKind::Struct(TypName::Static("Self")),
                children: None
            };
        }

        //1 checks if it is actually a generic and if so makes it a generic type
        let typ = Some(is_generic(&param.typ, &generics_hs));

        if param.name != "self" || name != "__init__" {
            input.push(Param{
                typ: typ.clone().unwrap(),
                ..param.clone()
            });
        }
        let identifier_pos = add_to_tree(args_pos, ast, Ast {
            children: None, parent: Some(args_pos),
            value: AstNode::Arg {
                name: param.name.clone(),
                is_arg: param.is_args,
                is_kwarg: param.is_kwargs,
            },
            typ,
            is_mut: param.is_mut,
            pos: Some(src_pos.clone())
        });
        if let Some(default_val) = default_val {
            let ast_len = ast.len();
            ast[identifier_pos].children = Some(vec![ast_len]);
            param.pos = ast_len;
            input.last_mut().unwrap().pos = ast_len;
            for node in default_val.iter().skip(1) {
                let mut node_clone = node.clone();
                node_clone.parent = Some(node_clone.parent.unwrap() + ast_len);
                node_clone.children = node_clone.children.map(|children|
                    children.iter().map(|i| i + ast_len).collect::<Vec<_>>()
                );
                ast.push(node_clone);
            }
        }
        add_to_stack(vars, param.name.clone(), identifier_pos);
    }
    if !params.is_empty() && params[0].0.name == "self" {
        ast[index].value = AstNode::Function(name.clone());
    }

    pos += 1;
    //1 return type
    if let SolidToken::Operator(OperatorType::Returns) = tokens[pos].tok {
        pos += 1;
        let typ = get_arg_typ(tokens, &mut pos, info);
        ast[return_pos].typ = Some(find_generics_in_typ(&typ, &generics_hs));
    } else if name == "__init__" && ast[return_pos].typ.is_none() {
        if let AstNode::Struct(name) = &ast[ast[parent].parent.unwrap()].value {
            let generic_and_types: Vec<_> = struct_generics_names.iter().map(
                |name| typ_with_child! {
                    TypeKind::Generic(GenericType::WithVal(name.clone())),
                    Type {
                        kind: TypeKind::Generic(GenericType::NoVal(name.clone())),
                        children: None
                    }
                }
            ).chain(
                struct_a_types_names.iter().map(|name| Type {
                    kind: TypeKind::AssociatedType(name.clone()),
                    children: None
                })
            ).collect();
            ast[return_pos].typ = Some(typ_with_child! {
                TypeKind::Struct(TypName::Str(name.clone())),
                Type {
                    kind: TypeKind::GenericsMap,
                    children: if generic_and_types.is_empty() { None } else { Some(generic_and_types) },
                }
            });
        }
    }
    let SolidToken::Colon = tokens[pos].tok else {
        update_pos_from_token(&tokens[pos]);
        throw!("expected `:`, found `{}`", tokens[pos].tok)
    };
    pos += 1;
    if parent == 0 { //1 top level function
        info.funcs.insert(name, FuncType {
            input: if input.is_empty() { None } else { Some(input) },
            output: ast[return_pos].typ.clone(),
        });
    }
    (pos, body_pos, info.generics.pop().unwrap())
}

pub fn make_struct(
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    info: &mut Info
) -> usize {
    // 1                                                                                                    name    generics   a_types
    #[inline] fn get_traits_in_parenthesis(tokens: &[SolidTokenWPos], pos: &mut usize, info: &mut Info) -> Vec<(String, Vec<Type>, HashMap<String, Type>)> {
        #[inline] fn add_generic_or_a_type(generics: &mut Vec<Type>, a_types: &mut HashMap<String, Type>, tokens: &[SolidTokenWPos], pos: &mut usize, info: &mut Info) {
            if let SolidToken::Operator(OperatorType::Eq) = tokens[*pos + 1].tok {
                let name = unwrap_enum!(&tokens[*pos].tok, SolidToken::Word(w), w.clone());
                *pos += 2;
                let typ = try_get_arg_typ(tokens, pos, info, true, false, &mut 0).unwrap();
                a_types.insert(name, typ);
            } else {
                generics.push(try_get_arg_typ(tokens, pos, info, true, false, &mut 0).unwrap());
            }
        }
        let mut trait_names = vec![];
        if let SolidToken::Parenthesis(IsOpen::True) = tokens[*pos].tok {
            *pos += 1;
            while let SolidToken::Word(w) = &tokens[*pos].tok {
                *pos += 1;
                let mut generics = vec![];
                let mut a_types = HashMap::new();
                if let SolidToken::Bracket(IsOpen::True) = tokens[*pos].tok { //1 generics
                    *pos += 1;
                    add_generic_or_a_type(&mut generics, &mut a_types, tokens, pos, info);
                    while let SolidToken::Comma = &tokens[*pos].tok {
                        *pos += 1;
                        add_generic_or_a_type(&mut generics, &mut a_types, tokens, pos, info);
                    }
                    unwrap_enum!(tokens[*pos].tok, SolidToken::Bracket(IsOpen::False));
                    *pos += 1;
                }
                // let generics_names = get_generic_names(pos, tokens);
                trait_names.push((w.clone(), generics, a_types));
                *pos -= 1;
                let SolidToken::Comma = tokens[*pos + 1].tok else {
                    break
                };
                *pos += 2;
            }
            *pos += 2;
        }
        trait_names
    }
    #[inline] fn make_init_and_funcs(
        tokens: &[SolidTokenWPos], ast: &mut Vec<Ast>, indent: usize, info: &mut Info, struct_name: &String,
        args_pos: usize, body_pos: usize, func_posses: &mut Vec<usize>,
        generics_names: Vec<String>, associated_types_names: Vec<String>
    ) {
        if unsafe { IS_COMPILED } {
            let init_pos = func_posses.iter().enumerate().find(|(_, &x)|
                if let SolidToken::Word(name) = &tokens[x + 1].tok {
                    name == "__init__"
                } else { false }
            ).unwrap_or_else(|| throw!("no `__init__` function found in `{}`", struct_name)).0; // todo dont need init
            let init_pos = func_posses.swap_remove(init_pos);
            let func_posses: Vec<_> = func_posses.iter().map(
                |func_pos| {
                    make_func_signature(
                        tokens, *func_pos, ast, body_pos, &mut vec![HashMap::new()],
                        info,
                    )
                }
            ).collect();

            let mut vars = vec![HashMap::new()];
            let (init_body_tok_pos, init_body_pos, init_generics) = make_func_signature(
                tokens, init_pos, ast, body_pos, &mut vars, info,
            );
            info.generics.push(init_generics);
            make_ast_statement(
                tokens, init_body_tok_pos, ast, init_body_pos, indent + 2,
                &mut vars, info,
            );
            info.generics.pop();

            let mut args = vec![];
            for (i, node) in ast.iter().enumerate().skip(init_body_pos) {
                if let AstNode::Property = node.value {
                    let children = unwrap_u(&node.children);
                    if let AstNode::Identifier(left) = &ast[children[0]].value {
                        if left == "self" {
                            if let AstNode::Identifier(right) = &ast[children[1]].value {
                                args.push((i, right.clone())); //, matches!(&ast[node.parent.unwrap()].value, AstNode::FirstAssignment)));
                            }
                        }
                    }
                }
            }
            for (idf_pos, name) in &args {
                ast[*idf_pos] = Ast {
                    value: AstNode::Identifier(format!("_self_{}_", name)),
                    children: None,
                    parent: ast[*idf_pos].parent,
                    typ: ast[*idf_pos].typ.clone(),
                    is_mut: ast[*idf_pos].is_mut,
                    pos: None
                };
            }

            add_types(ast, init_body_pos, &mut vars, info, &None);

            //1 remove self from args
            {
                let fun_pos = ast[init_body_pos].parent.unwrap();
                let args_pos = unwrap_u(&ast[fun_pos].children)[1];
                if !unwrap_u(&ast[args_pos].children).is_empty() {
                    let pos_0 = unwrap_u(&ast[args_pos].children)[0];
                    let first_arg_val = ast[pos_0].value.clone();
                    let args_children = &mut ast[args_pos].children;
                    if let AstNode::Arg { name: n, .. } = first_arg_val {
                        if n == "self" {
                            let args_children = args_children.as_mut().unwrap();
                            args_children.remove(0);
                        }
                    }
                }
            }
            let mut seen_names = HashSet::new();
            args.retain(|(_, name)| if seen_names.contains(name) {
                false
            } else {
                seen_names.insert(name.clone());
                true
            });
            for (idf_pos, name) in &args {
                add_to_tree(args_pos, ast, Ast::new_w_typ_no_pos(
                    AstNode::Arg { name: name.clone(), is_arg: false, is_kwarg: false },
                    ast[*idf_pos].typ.clone()
                ));
            }
            let return_pos = add_to_tree(init_body_pos, ast, Ast::new_no_pos(AstNode::Return));
            let struct_init = add_to_tree(return_pos, ast, Ast::new_no_pos(AstNode::RustStructInit));
            add_to_tree(struct_init, ast, Ast::new_no_pos(AstNode::Identifier(struct_name.clone())));

            let generic_types: Vec<_> = generics_names.iter().map(|x| Type {
                kind: TypeKind::Generic(GenericType::NoVal(x.clone())),
                children: None
            }).chain(associated_types_names.iter().map(|x| Type {
                kind: TypeKind::AssociatedType(x.clone()),
                children: None
            })).collect();
            ast[struct_init].typ = Some(Type {
                kind: TypeKind::Struct(TypName::Str(struct_name.clone())),
                children: Some(vec![Type {
                    kind: TypeKind::GenericsMap,
                    children: if generic_types.is_empty() { None } else { Some(generic_types) }
                }])
            });

            let args_pos = add_to_tree(
                struct_init, ast, Ast::new_no_pos(AstNode::Args)
            );
            for (_, name) in args {
                add_to_tree(args_pos, ast, Ast::new_no_pos(
                    AstNode::Identifier(name)
                ));
            }
            for (body_tok_pos, body_pos, func_generics) in func_posses {
                info.generics.push(func_generics);
                make_ast_statement(
                    tokens, body_tok_pos, ast, body_pos, indent + 2,
                    &mut vars, info,
                );
                info.generics.pop();
            }
        } else {
            for func_pos in func_posses {
                make_func(
                    tokens, *func_pos, ast, body_pos, indent + 1,
                    &mut vec![HashMap::new()], info,
                );
            }
        }
    }

    let struct_name =
        if let SolidToken::Word(name) = &tokens[pos].tok { name.clone() }
        else {
            update_pos_from_token(&tokens[pos]);
            throw!("Invalid name for struct `{}`", tokens[pos].tok)
        };
    let index = add_to_tree(
        parent, ast, Ast::new(AstNode::Struct(struct_name.clone()), tokens[pos].pos.clone())
    );

    pos += 1;
    let (generics_names, associated_types_names) = get_generics(&mut pos, tokens, index, ast, info);
    info.generics.push(HashSet::from_iter(generics_names.clone()));
    info.struct_associated_types.extend(associated_types_names.clone());

    *info.structs.get_mut(&struct_name).unwrap() = StructType {
        generics: Some(generics_names.clone()),
        associated_types: Some(associated_types_names.clone()),
        pos: index,
        parent_file: info.cur_file_path.to_str().unwrap().to_string()
    };
    info.structs.insert(String::from("Self"), info.structs[&struct_name].clone());

    //1 part1 of: traits in parentheses in struct declaration, e.g. struct A(*trait*):
    let trait_names = get_traits_in_parenthesis(tokens, &mut pos, info);
    //1 part2 of: traits in parentheses in struct declaration, e.g. struct A(*trait*):
    for (trt_name, generics_names, a_types) in trait_names {
        let key = ImplTraitsKey {
            name: struct_name.clone(),
            path: info.cur_file_path.to_str().unwrap().to_string(),
        };
        let val = ImplTraitsVal {
            trt_name: trt_name.to_string(),
            implementation: Implementation::Todo(info.cur_file_path.to_str().unwrap().to_string()),
            types: if a_types.is_empty() { None } else { Some(a_types) },
            generics: if generics_names.is_empty() { None } else { Some(generics_names) }
        };
        add_trait!(key, val);
    }

    let args_pos = add_to_tree(index, ast, Ast::new_no_pos(AstNode::ArgsDef));
    let body_pos = add_to_tree(index, ast, Ast::new_no_pos(AstNode::Module));
    // let traits_pos = add_to_tree(index, ast, Ast::new(AstNode::Traits));
    // let types_pos = add_to_tree(index, ast, Ast::new(AstNode::Types));

    update_pos_from_token(&tokens[pos]);
    unwrap_enum!(tokens[pos].tok, SolidToken::Colon, None::<bool>, "expected colon");
    pos += 2 + indent;
    if pos < tokens.len() && matches!(&tokens[pos].tok, SolidToken::Tab) { //todo what is this?
        pos += 1;
    }
    let mut func_posses = vec![];
    'whl: while pos < tokens.len() {
        match tokens[pos].tok {
            SolidToken::Def => func_posses.push(pos),
            SolidToken::NewLine => {
                if pos + indent + 2 >= tokens.len()
                    || !tokens.iter().skip(pos + 1).take(indent + 1).all(
                        |t| matches!(t.tok, SolidToken::Tab)
                    )
                { break 'whl }
            }
            SolidToken::Type => {
                let trait_name = unwrap_enum!(&tokens[pos+1].tok, SolidToken::Word(n), n);
                unwrap_enum!(&tokens[pos+2].tok, SolidToken::Period);
                let type_name = unwrap_enum!(&tokens[pos+3].tok, SolidToken::Word(n), n);
                unwrap_enum!(&tokens[pos+4].tok, SolidToken::Operator(OperatorType::Eq));
                pos += 5;
                let typ = get_arg_typ(tokens, &mut pos, info);

                let key = ImplTraitsKey{
                    name: struct_name.clone(),
                    path: info.cur_file_path.to_str().unwrap().to_string(),
                };
                if let Some(entry) = unsafe { IMPL_TRAITS.get_mut(&key) } {
                    // TODO generics and types ( like in add_trait!() )
                    if let Some(trt) = entry.iter_mut().find(|x| *trait_name == x.trt_name) {
                        if let Some(hm) = &mut trt.types {
                            hm.insert(type_name.clone(), typ);
                        } else {
                            trt.types = Some(HashMap::from([(type_name.clone(), typ)]));
                        }
                    } else {
                        entry.push(ImplTraitsVal {
                            types: Some(HashMap::from([(type_name.clone(), typ)])),
                            trt_name: trait_name.clone(),
                            implementation: Implementation::None,
                            generics: None
                        })
                    }
                } else {
                    unsafe {
                        IMPL_TRAITS.insert(
                            key,
                            vec![ImplTraitsVal {
                                types: Some(HashMap::from([(type_name.clone(), typ)])),
                                trt_name: trait_name.clone(),
                                implementation: Implementation::None,
                                generics: None
                            }]
                        );
                    }
                }
            }
            _ => (),
        }
        pos += 1;
    }

    make_init_and_funcs(
        tokens, ast, indent, info, &struct_name, args_pos, body_pos, &mut func_posses,
        generics_names, associated_types_names
    );

    info.generics.pop();
    info.struct_associated_types.clear();
    info.structs.remove("Self");
    pos
}



pub fn make_enum(
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    info: &mut Info
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos].tok { name.clone() }
        else {
            update_pos_from_token(&tokens[pos]);
            throw!("Invalid name for enum `{}`", tokens[pos].tok)
        };
    let index = add_to_tree(
        parent, ast, Ast::new(AstNode::Enum(name.clone()), tokens[pos].pos.clone())
    );

    pos += 1;
    let (generics_names, _) = get_generics(&mut pos, tokens, index, ast, info);
    info.generics.push(HashSet::from_iter(generics_names.clone()));
    // let generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    *info.enums.get_mut(&name).unwrap() = EnumType {
        generics: Some(generics_names),
        pos: index,
        parent_file: info.cur_file_path.to_str().unwrap().to_string()
    };

    let body_pos = add_to_tree(
        index, ast, Ast::new_no_pos(AstNode::Module)
    );

    update_pos_from_token(&tokens[pos]);
    unwrap_enum!(tokens[pos].tok, SolidToken::Colon, "expected colon");
    pos += 2 + indent;
    if pos < tokens.len() && matches!(&tokens[pos].tok, SolidToken::Tab) {  //todo what is this?
        pos += 1;
    }
    'whl: while pos < tokens.len() {
        match &tokens[pos].tok {
            SolidToken::Word(option) => {
                let opt_pos = add_to_tree(
                    body_pos, ast,
                    Ast::new(AstNode::Identifier(option.clone()), tokens[pos].pos.clone())
                );
                if let SolidToken::Parenthesis(IsOpen::True) = &tokens[pos + 1].tok {
                    pos += 1;
                    let mut first = true;
                    while first || matches!(&tokens[pos].tok, SolidToken::Comma) {
                        first = false;
                        let src_pos = tokens[pos].pos.clone();
                        pos += 1;
                        let typ = get_arg_typ(tokens, &mut pos, info);
                        add_to_tree(opt_pos, ast, Ast {
                            value: AstNode::Types,
                            children: None,
                            parent: None,
                            typ: Some(typ),
                            is_mut: false,
                            pos: Some(src_pos)
                        });
                    }
                }
            },
            SolidToken::NewLine => {
                if pos + indent + 2 >= tokens.len()
                    || !tokens.iter().skip(pos + 1).take(indent + 1).all(
                    |t| matches!(t.tok, SolidToken::Tab)
                )
                {
                    break 'whl
                }
            }
            _ => ()
        }
        pos += 1;
    }
    info.generics.pop();
    pos
}

pub fn make_trait(
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    info: &mut Info
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos].tok { name.clone() }
        else {
            update_pos_from_token(&tokens[pos]);
            throw!("Invalid name for trait `{}`", tokens[pos].tok)
        };

    let index = add_to_tree(parent, ast, Ast::new(
        AstNode::Trait {
            name: name.clone(),
            strict: matches!(tokens[pos-1].tok, SolidToken::StrictTrait)
        }, tokens[pos].pos.clone()
    ));

    pos += 1;
    let (generics_names, associated_types_names) = get_generics(&mut pos, tokens, index, ast, info);
    let generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    info.generics.push(generics_hs.clone());
    info.struct_associated_types.extend(associated_types_names.iter().cloned());

    *info.traits.get_mut(&name).unwrap() = TraitType {
        generics: Some(generics_names),
        associated_types: Some(associated_types_names),
        pos: index,
        parent_file: info.cur_file_path.to_str().unwrap().to_string()
    };
    let info_trt = info.traits[&name].clone();
    info.structs.insert(String::from("Self"), StructType {
        generics: info_trt.generics,
        associated_types: info_trt.associated_types,
        pos: info_trt.pos,
        parent_file: info_trt.parent_file
    });

    let body_pos = add_to_tree(index, ast, Ast::new_no_pos(AstNode::Module));

    update_pos_from_token(&tokens[pos]);
    unwrap_enum!(tokens[pos].tok, SolidToken::Colon, "expected colon");
    pos += 3 + indent;

    while let SolidToken::Def | SolidToken::Type = &tokens[pos].tok {
        if let SolidToken::Type = &tokens[pos].tok {
            todo!("Pretty sure this isnt needed any more");
            // let name = unwrap_enum!(&tokens[pos + 1].tok, SolidToken::Word(w), w);
            // info.struct_associated_types.insert(name.clone());
            //
            // add_to_tree(body_pos, ast, Ast::new(AstNode::Type(name.clone())));
            // //1 skip 4 cuz:
            // //  type, name, newline, indent + 1
            // pos += 4 + indent;
            // continue
        }
        pos += 1;
        let func_name = unwrap_enum!(&tokens[pos].tok, SolidToken::Word(n), n);
        let func_pos = add_to_tree(body_pos, ast, Ast::new(
            AstNode::Function(func_name.clone()), tokens[pos].pos.clone()
        ));
        pos += 1;
        get_generics(&mut pos, tokens, func_pos, ast, info);
        pos += 1;
        let params = get_params(tokens, &mut pos, info);
        let mut args: Vec<(_, _)> = params.iter().map(|(x, _, src_pos)|
            (Param {
                typ: is_generic(&x.typ, &generics_hs),
                name: x.name.clone(),
                ..*x
            }, src_pos)
        ).collect();
        if !args.is_empty() && args[0].0.name == "self" && args[0].0.typ == UNKNOWN_TYPE {
            args[0].0.typ = Type {
                kind: TypeKind::Struct(TypName::Static("Self")),
                children: None
            };
        }
        pos += 1;
        let return_type = if let SolidToken::Operator(OperatorType::Returns) = tokens[pos].tok {
            pos += 1;
            let return_type = get_arg_typ(tokens, &mut pos, info);
            Some(find_generics_in_typ(&return_type, &generics_hs))
        } else { None };
        //1 args
        let args_pos = add_to_tree(
            func_pos, ast, Ast::new_no_pos(AstNode::ArgsDef)
        );
        for (arg, src_pos) in args {
            add_to_tree(args_pos, ast, Ast {
                value: AstNode::Arg {
                    name: arg.name.clone(),
                    is_arg: arg.is_args,
                    is_kwarg: arg.is_kwargs
                },
                typ: Some(arg.typ),
                children: None,
                parent: None,
                is_mut: arg.is_mut,
                pos: Some(src_pos.clone())
            });
        }
        //1 return
        add_to_tree(
            func_pos, ast, Ast::new_w_typ_no_pos(AstNode::ReturnType, return_type)
        );
        pos += indent + 2;
        if pos >= tokens.len() {
            break
        }
    }
    info.structs.remove("Self");
    info.struct_associated_types.clear();
    info.generics.pop();
    pos - indent - 2
}
