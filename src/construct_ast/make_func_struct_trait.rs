use std::collections::{HashMap, HashSet};
use std::fmt;
use fmt::Write;
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::find_generics::{find_generics_in_typ, get_generic_names, get_generics, is_generic};
use crate::construct_ast::get_functions_and_types::{FuncType, FuncTypes, StructType, StructTypes, TraitType, TraitTypes};
use crate::construct_ast::get_typ::{get_arg_typ, get_params};
use crate::construct_ast::mold_ast::{make_ast_statement, VarTypes};
use crate::construct_ast::tree_utils::{add_to_tree, print_tree};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{GenericType, Type, TypeKind, TypName, unwrap, unwrap_u};
use crate::{IS_COMPILED, some_vec, typ_with_child, unwrap_enum};

pub fn make_func(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> usize {
    let is_static = matches!(&tokens[pos], SolidToken::Static);
    pos += if is_static { 2 } else { 1 };
    let mut name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };
    pos += 1;

    // todo can there be more than one? (if so make it while instead of if)
    if matches!(tokens[pos], SolidToken::Colon) && matches!(tokens[pos + 1], SolidToken::Colon) {
        // TODO
        pos += 2; //1 skip the ::
        write!(name, "::{}", unwrap_enum!(&tokens[pos], SolidToken::Word(w), w)).unwrap();
        pos += 1; //1 skip the next word
        let trait_name = name.split("::").next().unwrap();
        let parent_struct = ast[parent].parent.unwrap();
        let traits = unwrap_u(&ast[parent_struct].children)[3];
        if !unwrap_u(&ast[traits].children).iter().any(|x|
            unwrap_enum!(&ast[*x].value, AstNode::Identifier(n), n) == trait_name
        ) {
            // panic!("this struct doesnt implement the `{trait_name}` trait");
            add_to_tree(traits, ast, Ast::new(
                AstNode::Identifier(String::from(trait_name))
            ));
        }
    }
    let index = add_to_tree(parent, ast, Ast::new(
        if is_static {
            AstNode::StaticFunction(name.clone())
        } else {
            AstNode::Function(name.clone())
        }
    ));

    let generics_names = get_generics(&mut pos, tokens, index, ast);
    let mut generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    if let Some(struct_parent) = ast[parent].parent {
        if let AstNode::Struct(_) = ast[struct_parent].value {
            let struct_generics = &ast[unwrap_u(&ast[struct_parent].children)[0]];
            print_tree((ast.clone(), struct_parent));
            print_tree((ast.clone(), unwrap_u(&ast[struct_parent].children)[0]));
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

    let SolidToken::Parenthesis(IsOpen::True) = tokens[pos] else {
        panic!("expected `(` or `::`, found {:?}", tokens[pos])
    };
    pos += 1;
    let params = get_params(tokens, &mut pos, funcs, structs, traits);
    let args_pos = add_to_tree(index, ast, Ast::new(AstNode::ArgsDef));
    let return_pos = add_to_tree(index, ast, Ast::new(AstNode::ReturnType));
    let body_pos = add_to_tree(index, ast, Ast::new(AstNode::Body));

    let mut input = vec![];
    for param in &params {
        //1 checks if it is actually a generic and if so makes it a generic type
        let typ = Some(
            is_generic(&param.typ, &generics_hs)
        );
        input.push(typ.clone().unwrap());
        let identifier_pos = add_to_tree(args_pos, ast, Ast {
            children: None, parent: Some(args_pos),
            value: AstNode::Identifier(param.name.clone()),
            typ,
            is_mut: param.is_mut
        });
        vars.last_mut().unwrap().insert(param.name.clone(), identifier_pos);
    }

    pos += 1;
    if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
        pos += 1;
        let typ = get_arg_typ(tokens, &mut pos, funcs, structs, traits);
        ast[return_pos].typ = Some(find_generics_in_typ(&typ, &generics_hs));
    }
    if let SolidToken::Colon = tokens[pos] {} else {
        panic!("expected colon, found `{:?}`", tokens[pos])
    }
    pos += 1;
    funcs.insert(name, FuncType {
        input: if input.is_empty() { None } else { Some(input) },
        output: ast[return_pos].typ.clone(),
    });
    make_ast_statement(tokens, pos, ast, body_pos, indent + 1, vars, funcs, structs, traits)
}

pub fn make_struct(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };

    let index = add_to_tree(parent, ast, Ast::new(AstNode::Struct(name.clone())));

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    let generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    *structs.get_mut(&name).unwrap() = StructType { generics: Some(generics_names), pos: index};
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

    for (t, generics_names) in trait_names {
        let trait_pos = add_to_tree(
            traits_pos, ast,
            Ast::new(AstNode::Identifier(t.clone()))
        );
        add_to_tree(trait_pos, ast, Ast::new_w_typ(
            AstNode::GenericsDeclaration,
            Some(Type {
                kind: TypeKind::Generics,
                children: if generics_names.is_empty() { None } else {
                    todo!("generic traits not yet implemented"); //1 bcs IDK how that should look
                    // Some(generics_names.iter().map(|name|
                    //     Type {
                    //         kind: TypeKind::Generic(GenericType::Declaration(name.clone())),
                    //         children: None,
                    //     }
                    // ).collect())
                }
            })
        ));
    }

    unwrap_enum!(tokens[pos], SolidToken::Colon, "expected colon");
    pos += 2 + indent;
    if pos < tokens.len() && matches!(&tokens[pos], SolidToken::Tab) {
        pos += 1;
        while let SolidToken::Word(word) = &tokens[pos] {
            pos += 1;
            if let SolidToken::Colon = &tokens[pos] {
                pos += 1;
                let typ = is_generic(&get_arg_typ(tokens, &mut pos, funcs, structs, traits), &generics_hs);
                add_to_tree(args_pos, ast, Ast::new(AstNode::Identifier(word.clone())));
                ast.last_mut().unwrap().typ = Some(typ);
            } else {
                if unsafe { IS_COMPILED } {
                    panic!("argument `{word}` needs a type")
                }
                // todo check if works
                add_to_tree(args_pos, ast, Ast::new(AstNode::Identifier(word.clone())));
            }
            if let SolidToken::NewLine = tokens[pos] {} else {
                return pos
            }
            pos += 1;
            let mut exited = false;
            for i in 0..=indent {
                if let SolidToken::Tab = tokens[pos + i] {} else {
                    exited = true;
                    break;
                }
            }
            if exited {
                return pos - 1
            } else {
                pos += indent + 1;
            }
        }
    }
    let mut struct_funcs: FuncTypes = HashMap::new();

    let mut vars = HashMap::new();
    for child in unwrap_u(&ast[args_pos].children) {
        let arg = &ast[*child];
        vars.insert(
            unwrap_enum!(&arg.value, AstNode::Identifier(n), n).clone(),
            *child
        );
    }
    let mut vars = vec![vars]; //1 stack
    while pos < tokens.len() && matches!(tokens[pos], SolidToken::Def | SolidToken::Static) {
        vars.push(HashMap::new());
        pos = make_func(
            tokens, pos, ast, body_pos, indent + 1,
            &mut vars, &mut struct_funcs, structs, traits
        );
        vars.pop();

        let func = &ast[*unwrap_u(&ast[body_pos].children).last().unwrap()];
        if let AstNode::Function(_) = func.value {
            let args_def_pos = unwrap_u(&func.children)[1];
            ast.push(Ast {
                children: None,
                parent: Some(args_def_pos),
                value: AstNode::Identifier(String::from("self")),
                typ: Some(typ_with_child! {
                    TypeKind::MutPointer,
                    Type {
                        kind: TypeKind::Struct(TypName::Static("Self")),
                        children: None
                    }
                }),
                is_mut: false
            });
            let self_pos = ast.len() - 1;
            let args_def = &mut ast[args_def_pos];
            if let Some(children) = &mut args_def.children {
                children.insert(0, self_pos);
            } else {
                args_def.children = some_vec![self_pos];
            }
        }
        pos += indent + 2;
        if pos > tokens.len() { break; }
    }
    pos - indent - 2
}

pub fn make_trait(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };

    let index = add_to_tree(parent, ast, Ast::new(AstNode::Trait(name.clone())));

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    let generics_hs = HashSet::from_iter(generics_names.iter().cloned());


    *traits.get_mut(&name).unwrap() = TraitType { generics: Some(generics_names), pos: index};
    let body_pos = add_to_tree(index, ast, Ast::new(AstNode::Module));

    unwrap_enum!(tokens[pos], SolidToken::Colon, "expected colon");
    pos += 3 + indent;
    while let SolidToken::Def = &tokens[pos] {
        pos += 1;
        let func_name = unwrap_enum!(&tokens[pos], SolidToken::Word(n), n);
        let func_pos = add_to_tree(body_pos, ast, Ast::new(AstNode::Function(func_name.clone())));
        pos += 1;
        let func_generics = get_generics(&mut pos, tokens, func_pos, ast);
        pos += 1;
        let params = get_params(tokens, &mut pos, funcs, structs, traits);
        let mut args: Vec<Param> = params.iter().map(|x| Param {
            typ: is_generic(&x.typ, &generics_hs),
            name: x.name.clone(),
            is_mut: x.is_mut
        }).collect();
        args.insert(0, Param {
            name: String::from("self"),
            typ: typ_with_child! {
                TypeKind::MutPointer,
                Type {
                    kind: TypeKind::Struct(TypName::Static("Self")),
                    children: None
                }
            },
            is_mut: true
        });
        pos += 1;
        let return_type = if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
            pos += 1;
            let return_type = get_arg_typ(tokens, &mut pos, funcs, structs, traits);
            Some(find_generics_in_typ(&return_type, &generics_hs))
        } else { None };
        //1 args
        let args_pos = add_to_tree(func_pos, ast, Ast::new(AstNode::ArgsDef));
        for arg in args {
            add_to_tree(args_pos, ast, Ast{
                value: AstNode::Identifier(arg.name.clone()),
                typ: Some(arg.typ),
                children: None,
                parent: None,
                is_mut: arg.is_mut,
            });
        }
        //1 return
        add_to_tree(func_pos, ast, Ast::new_w_typ(AstNode::ReturnType, return_type));
        pos += indent + 2;
    }
    pos - indent - 2
}
