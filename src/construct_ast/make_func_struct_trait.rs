use std::collections::{HashMap, HashSet};
use std::fmt;
use fmt::Write;
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::find_generics::{find_generics_in_typ, get_generic_names, get_generics, is_generic};
use crate::construct_ast::get_typ::{get_arg_typ, get_params};
use crate::construct_ast::mold_ast::{FuncType, StructType, TraitType, Info, make_ast_statement, VarTypes, EnumType};
use crate::construct_ast::tree_utils::{add_to_tree};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{GenericType, print_type, Type, TypeKind, TypName, UNKNOWN_TYPE, unwrap, unwrap_u};
use crate::{IS_COMPILED, some_vec, typ_with_child, unwrap_enum};
use crate::add_types::ast_add_types::add_types;

pub fn make_func(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
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
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    vars: &mut VarTypes, info: &mut Info
) -> (usize, usize, HashSet<String>) {
    // let is_static = matches!(&tokens[pos], SolidToken::Static);
    pos += 1;
    let mut name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function `{:?}`", tokens[pos]) };
    pos += 1;

    //1 if is trait function
    // todo can there be more than one? (if so make it while instead of if)
    if matches!(tokens[pos], SolidToken::Colon) && matches!(tokens[pos + 1], SolidToken::Colon) {
        pos += 2; //1 skip the ::
        write!(name, "::{}", unwrap_enum!(&tokens[pos], SolidToken::Word(w), w)).unwrap();
        pos += 1; //1 skip the next word
        let trait_name = name.split("::").next().unwrap();
        let parent_struct = ast[parent].parent.unwrap();
        let traits = unwrap_u(&ast[parent_struct].children)[3];
        if !unwrap_u(&ast[traits].children).iter().any(|x|
            unwrap_enum!(&ast[*x].value, AstNode::Identifier(n), n) == trait_name
        ) {
            add_to_tree(traits, ast, Ast::new(
                AstNode::Identifier(String::from(trait_name))
            ));
        }
    }
    let index = add_to_tree(parent, ast, Ast::new(
        AstNode::StaticFunction(name.clone())
    ));

    //1 generics
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    let mut generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    if let Some(struct_parent) = ast[parent].parent {
        if let AstNode::Struct(_) = ast[struct_parent].value {
            let struct_generics = &ast[unwrap_u(&ast[struct_parent].children)[0]];
            let stct_g_typ = unwrap_enum!(&struct_generics.typ);
            for child in unwrap(&stct_g_typ.children) {
                unwrap_enum!(
                    child,
                    Type { kind: TypeKind::Generic(GenericType::Declaration(name)), .. },
                    generics_hs.insert(name.clone())
                );
            }
        }
    }
    info.generics.push(generics_hs.clone());

    let SolidToken::Parenthesis(IsOpen::True) = tokens[pos] else {
        panic!("expected `(` or `::`, found {:?}", tokens[pos])
    };
    pos += 1;
    let mut params = get_params(tokens, &mut pos, info);
    let args_pos = add_to_tree(index, ast, Ast::new(AstNode::ArgsDef));
    let return_pos = add_to_tree(index, ast, Ast::new(AstNode::ReturnType));
    let body_pos = add_to_tree(index, ast, Ast::new(AstNode::Body));

    let mut input = vec![];
    for (param, default_val) in &mut params {
        if param.name == "self" && param.typ == UNKNOWN_TYPE {
            param.typ = Type {
                kind: TypeKind::Struct(TypName::Static("Self")),
                children: None
            };
        }

        //1 checks if it is actually a generic and if so makes it a generic type
        let typ = Some(
            is_generic(&param.typ, &generics_hs)
        );
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
            is_mut: param.is_mut
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
        vars.last_mut().unwrap().insert(param.name.clone(), identifier_pos);
    }
    if !params.is_empty() && params[0].0.name == "self" {
        ast[index].value = AstNode::Function(name.clone());
    }

    pos += 1;
    //1 return type
    if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
        pos += 1;
        let typ = get_arg_typ(tokens, &mut pos, info);
        ast[return_pos].typ = Some(find_generics_in_typ(&typ, &generics_hs));
    } else if name == "__init__" && ast[return_pos].typ.is_none() {
        if let AstNode::Struct(name) = &ast[ast[parent].parent.unwrap()].value {
            ast[return_pos].typ = Some(typ_with_child! {
                TypeKind::Struct(TypName::Str(name.clone())),
                Type{
                    kind: TypeKind::GenericsMap,
                    children: None,
                }
            });
        }
    }
    if let SolidToken::Colon = tokens[pos] {} else {
        panic!("expected colon, found `{:?}`", tokens[pos])
    }
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
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    info: &mut Info
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() } else { panic!("Invalid name for struct `{:?}`", tokens[pos]) };
    let index = add_to_tree(parent, ast, Ast::new(AstNode::Struct(name.clone())));

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    info.generics.push(HashSet::from_iter(generics_names.clone()));
    // let generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    *info.structs.get_mut(&name).unwrap() = StructType { generics: Some(generics_names), pos: index };
    info.structs.insert(String::from("Self"), info.structs[&name].clone());
    //1 part1 of: traits in parentheses in struct declaration, e.g. struct A(*trait*):
    let mut trait_names = vec![];
    if let SolidToken::Parenthesis(IsOpen::True) = tokens[pos] {
        pos += 1;
        while let SolidToken::Word(w) = &tokens[pos] {
            pos += 1;
            let generics_names = get_generic_names(&mut pos, tokens);
            trait_names.push((w.clone(), generics_names));
            pos -= 1;
            let SolidToken::Comma = tokens[pos + 1] else {
                break
            };
            pos += 2;
        }
        pos += 2;
    }

    let args_pos = add_to_tree(index, ast, Ast::new(AstNode::ArgsDef));
    let body_pos = add_to_tree(index, ast, Ast::new(AstNode::Module));
    let traits_pos = add_to_tree(index, ast, Ast::new(AstNode::Traits));
    // let types_pos = add_to_tree(index, ast, Ast::new(AstNode::Types));

    //1 part2 of: traits in parentheses in struct declaration, e.g. struct A(*trait*):

    for (t, generics_names) in trait_names {
        let trait_pos = add_to_tree(
            traits_pos, ast,
            Ast::new(AstNode::Identifier(t.clone()))
        );
        // add_to_tree(trait_pos, ast, Ast::new_w_typ(
        //     AstNode::GenericsDeclaration,
        //     Some(Type {
        //         kind: TypeKind::Generics,
        //         children: if generics_names.is_empty() { None } else {
        //             todo!("generic traits not yet implemented"); //1 bcs IDK how that should look
        //             // Some(generics_names.iter().map(|name|
        //             //     Type {
        //             //         kind: TypeKind::Generic(GenericType::Declaration(name.clone())),
        //             //         children: None,
        //             //     }
        //             // ).collect())
        //         }
        //     })
        // ));
    }

    unwrap_enum!(tokens[pos], SolidToken::Colon, None::<bool>, "expected colon");
    pos += 2 + indent;
    if pos < tokens.len() && matches!(&tokens[pos], SolidToken::Tab) { //todo what is this?
        pos += 1;
    }
    let mut func_posses = vec![];
    'whl: while pos < tokens.len() {
        match tokens[pos] {
            SolidToken::Def => func_posses.push(pos),
            SolidToken::NewLine => {
                if pos + indent + 2 >= tokens.len()
                    || !tokens.iter().skip(pos + 1).take(indent + 1).all(
                    |t| matches!(t, SolidToken::Tab)
                )
                {
                    break 'whl
                }
            }
            SolidToken::Type => {
                let trait_name = unwrap_enum!(&tokens[pos+1], SolidToken::Word(n), n);
                unwrap_enum!(&tokens[pos+2], SolidToken::Period);
                let type_name = unwrap_enum!(&tokens[pos+3], SolidToken::Word(n), n);
                unwrap_enum!(&tokens[pos+4], SolidToken::Operator(OperatorType::Eq));
                pos += 5;
                let typ = get_arg_typ(tokens, &mut pos, info);
                // let trait_impl_pos = unwrap_u(&ast[traits_pos].children).iter().find(
                //     |&i| unwrap_enum!(&ast[*i].value, AstNode::Identifier(i), i) == trait_name
                // );
                let pos =/* if let Some(pos) = trait_impl_pos { *pos } else {*/
                    add_to_tree(traits_pos, ast, Ast::new(
                        AstNode::Identifier(trait_name.clone())
                    ));
                add_to_tree(pos, ast, Ast {
                    value: AstNode::Type(type_name.clone()),
                    children: None,
                    parent: None,
                    typ: Some(typ),
                    is_mut: false,
                });
                // panic!("!");
            }
            _ => (),
        }
        pos += 1;
    }


    //1 __init__ and funcs
    if unsafe { IS_COMPILED } {
        let init_pos = func_posses.iter().enumerate().find(|(_, &x)|
            if let SolidToken::Word(name) = &tokens[x + 1] {
                name == "__init__"
            } else { false }
        ).unwrap_or_else(|| panic!("no __init__ function found in {name}")).0; // todo dont need init
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
            tokens, init_pos, ast, body_pos, &mut vars,
            info, 
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
                            args.push((i, right.clone(), matches!(&ast[node.parent.unwrap()].value, AstNode::FirstAssignment)));
                        }
                    }
                }
            }
        }
        for (idf_pos, name, _) in &args {
            ast[*idf_pos] = Ast {
                value: AstNode::Identifier(format!("_self_{}_", name)),
                children: None,
                parent: ast[*idf_pos].parent,
                typ: ast[*idf_pos].typ.clone(),
                is_mut: ast[*idf_pos].is_mut,
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
                if let AstNode::Identifier(name) = first_arg_val {
                    if name == "self" {
                        let args_children = unwrap_enum!(args_children);
                        args_children.remove(0);
                    }
                }
            }
        }
        args.retain(|(_, _, is_first_assign)| *is_first_assign);
        for (idf_pos, name, _) in &args {
            add_to_tree(args_pos, ast, Ast {
                value: AstNode::Identifier(name.clone()),
                typ: ast[*idf_pos].typ.clone(),
                children: None,
                parent: None,
                is_mut: false,
            });
        }
        let return_pos = add_to_tree(init_body_pos, ast, Ast::new(AstNode::Return));
        let struct_init = add_to_tree(return_pos, ast, Ast::new(AstNode::StructInit));
        add_to_tree(struct_init, ast, Ast::new(AstNode::Identifier(name)));
        let args_pos = add_to_tree(struct_init, ast, Ast::new(AstNode::Args));
        for (_, name, _) in args {
            add_to_tree(args_pos, ast, Ast::new(AstNode::Identifier(format!("_self_{}_", name))));
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
                tokens, func_pos, ast, body_pos, indent + 1,
                &mut vec![HashMap::new()], info, 
            );
        }
    }
    info.generics.pop();
    info.structs.remove("Self");
    pos
}

pub fn make_enum(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    info: &mut Info
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() } else { panic!("Invalid name for enum `{:?}`", tokens[pos]) };
    let index = add_to_tree(parent, ast, Ast::new(AstNode::Enum(name.clone())));

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    info.generics.push(HashSet::from_iter(generics_names.clone()));
    // let generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    *info.enums.get_mut(&name).unwrap() = EnumType { generics: Some(generics_names), pos: index };

    let body_pos = add_to_tree(index, ast, Ast::new(AstNode::Module));

    unwrap_enum!(tokens[pos], SolidToken::Colon, "expected colon");
    pos += 2 + indent;
    if pos < tokens.len() && matches!(&tokens[pos], SolidToken::Tab) {  //todo what is this?
        pos += 1;
    }
    'whl: while pos < tokens.len() {
        match &tokens[pos] {
            SolidToken::Word(option) => {
                let opt_pos = add_to_tree(
                    body_pos, ast,
                    Ast::new(AstNode::Identifier(option.clone()))
                );
                if let SolidToken::Parenthesis(IsOpen::True) = &tokens[pos + 1] {
                    pos += 1;
                    let mut first = true;
                    while first || matches!(&tokens[pos], SolidToken::Comma) {
                        first = false;
                        pos += 1;
                        let typ = get_arg_typ(tokens, &mut pos, info);
                        add_to_tree(opt_pos, ast, Ast {
                            value: AstNode::Types,
                            children: None,
                            parent: None,
                            typ: Some(typ),
                            is_mut: false,
                        });
                    }
                }
            },
            SolidToken::NewLine => {
                if pos + indent + 2 >= tokens.len()
                    || !tokens.iter().skip(pos + 1).take(indent + 1).all(
                    |t| matches!(t, SolidToken::Tab)
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
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    info: &mut Info
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for trait `{:?}`", tokens[pos]) };

    let index = add_to_tree(parent, ast, Ast::new(
        AstNode::Trait {
            name: name.clone(),
            strict: matches!(tokens[pos-1], SolidToken::StrictTrait)
        }
    ));

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    let generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    info.generics.push(generics_hs.clone());

    *info.traits.get_mut(&name).unwrap() = TraitType { generics: Some(generics_names), pos: index};
    info.structs.insert(String::from("Self"), StructType {
        generics: info.traits[&name].generics.clone(),
        pos: info.traits[&name].pos,
    });

    let body_pos = add_to_tree(index, ast, Ast::new(AstNode::Module));

    unwrap_enum!(tokens[pos], SolidToken::Colon, "expected colon");
    pos += 3 + indent;

    while let SolidToken::Def | SolidToken::Type = &tokens[pos] {
        if let SolidToken::Type = &tokens[pos] {
            let name = unwrap_enum!(&tokens[pos + 1], SolidToken::Word(w), w);
            info.struct_inner_types.insert(name.clone());

            add_to_tree(body_pos, ast, Ast::new(AstNode::Type(name.clone())));
            //1 skip 4 cuz:
            //  type, name, newline, indent + 1
            pos += 4 + indent;
            continue
        }
        pos += 1;
        let func_name = unwrap_enum!(&tokens[pos], SolidToken::Word(n), n);
        let func_pos = add_to_tree(body_pos, ast, Ast::new(AstNode::Function(func_name.clone())));
        pos += 1;
        get_generics(&mut pos, tokens, func_pos, ast);
        pos += 1;
        let params = get_params(tokens, &mut pos, info);
        let mut args: Vec<_> = params.iter().map(|(x, _)| Param {
            typ: is_generic(&x.typ, &generics_hs),
            name: x.name.clone(),
            ..*x
        }).collect();
        if !args.is_empty() && args[0].name == "self" && args[0].typ == UNKNOWN_TYPE {
            args[0].typ = Type {
                kind: TypeKind::Struct(TypName::Static("Self")),
                children: None
            };
        }
        // args.insert(0, Param {
        //     name: String::from("self"),
        //     typ: typ_with_child! {
        //         TypeKind::MutPointer,
        //         Type {
        //             kind: TypeKind::Struct(TypName::Static("Self")),
        //             children: None
        //         }
        //     },
        //     is_mut: true
        // });
        pos += 1;
        let return_type = if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
            pos += 1;
            let return_type = get_arg_typ(tokens, &mut pos, info);
            Some(find_generics_in_typ(&return_type, &generics_hs))
        } else { None };
        //1 args
        let args_pos = add_to_tree(
            func_pos, ast, Ast::new(AstNode::ArgsDef)
        );
        for arg in args {
            add_to_tree(args_pos, ast, Ast{
                value: AstNode::Arg {
                    name: arg.name.clone(),
                    is_arg: arg.is_args,
                    is_kwarg: arg.is_kwargs
                },
                typ: Some(arg.typ),
                children: None,
                parent: None,
                is_mut: arg.is_mut,
            });
        }
        //1 return
        add_to_tree(
            func_pos, ast, Ast::new_w_typ(AstNode::ReturnType, return_type)
        );
        pos += indent + 2;
        if pos >= tokens.len() {
            break
        }
    }
    info.structs.remove("Self");
    info.struct_inner_types.clear();
    info.generics.pop();
    pos - indent - 2
}
