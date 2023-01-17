use std::collections::{HashMap, HashSet};
use std::fmt;
use fmt::Write;
use std::process::id;
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::find_generics::{find_generics_in_typ, get_generic_names, get_generics, is_generic};
use crate::construct_ast::get_functions_and_types::{FuncType, FuncTypes, StructType, StructTypes, TraitType, TraitTypes};
use crate::construct_ast::get_typ::{get_arg_typ, get_params};
use crate::construct_ast::mold_ast::{Info, make_ast_statement, VarTypes};
use crate::construct_ast::tree_utils::{add_to_tree, print_tree};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{GenericType, Type, TypeKind, TypName, UNKNOWN_TYPE, unwrap, unwrap_u};
use crate::{IS_COMPILED, some_vec, typ_with_child, unwrap_enum};
use crate::add_types::ast_add_types::add_types;
use crate::built_in_funcs::BuiltIn;

pub fn make_func(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, info: &mut Info,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>, generics: &mut HashSet<String>
) -> usize {
    let (p, body_pos, mut func_generics) = make_func_signature(
        tokens, pos, ast, parent, vars, info, built_ins, generics
    );
    pos = p;
    make_ast_statement(
        tokens, pos, ast, body_pos, indent + 1,
        vars, info, built_ins, &mut func_generics
    )
}

fn make_func_signature(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    vars: &mut VarTypes, info: &mut Info,
    _built_ins: &HashMap<&str, Box<dyn BuiltIn>>, generics: &mut HashSet<String>
) -> (usize, usize, HashSet<String>) {
    let is_static = matches!(&tokens[pos], SolidToken::Static);
    pos += if is_static { 2 } else { 1 };
    let mut name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };
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
        if is_static {
            AstNode::StaticFunction(name.clone())
        } else {
            AstNode::Function(name.clone())
        }
    ));

    //1 generics
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    let mut generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    let mut generics = generics.union(&generics_hs)
        .map(|x| x.clone())
        .collect();
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

    let SolidToken::Parenthesis(IsOpen::True) = tokens[pos] else {
        panic!("expected `(` or `::`, found {:?}", tokens[pos])
    };
    pos += 1;
    let mut params = get_params(tokens, &mut pos, info, &generics);
    let args_pos = add_to_tree(index, ast, Ast::new(AstNode::ArgsDef));
    let return_pos = add_to_tree(index, ast, Ast::new(AstNode::ReturnType));
    let body_pos = add_to_tree(index, ast, Ast::new(AstNode::Body));

    let mut input = vec![];
    for param in &mut params {
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
            input.push(typ.clone().unwrap());
        }
        let identifier_pos = add_to_tree(args_pos, ast, Ast {
            children: None, parent: Some(args_pos),
            value: AstNode::Identifier(param.name.clone()),
            typ,
            is_mut: param.is_mut
        });
        vars.last_mut().unwrap().insert(param.name.clone(), identifier_pos);
    }

    pos += 1;
    //1 return type
    if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
        pos += 1;
        let typ = get_arg_typ(tokens, &mut pos, info, &mut generics);
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
    info.funcs.insert(name, FuncType {
        input: if input.is_empty() { None } else { Some(input) },
        output: ast[return_pos].typ.clone(),
    });
    (pos, body_pos, generics)
}

pub fn make_struct(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    info: &mut Info,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>, generics: &mut HashSet<String>
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() } else { panic!("Invalid name for function {:?}", tokens[pos]) };

    let index = add_to_tree(parent, ast, Ast::new(AstNode::Struct(name.clone())));

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    let mut generics = &mut generics.union(&HashSet::from_iter(generics_names.clone()))
        .map(|x| x.clone())
        .collect();
    // let generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    *info.structs.get_mut(&name).unwrap() = StructType { generics: Some(generics_names), pos: index };
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
    }
    let mut struct_funcs: FuncTypes = HashMap::new();
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
            _ => ()
        }
        pos += 1;
    }

    if unsafe { IS_COMPILED } {
        let init_pos = func_posses.iter().enumerate().find(|(_, &x)|
            if let SolidToken::Word(name) = &tokens[x + 1] {
                name == "__init__"
            } else { false }
        ).unwrap_or_else(|| panic!("no __init__ function found in {name}")).0; // todo dont need init
        let init_pos = func_posses.swap_remove(init_pos);
        let func_posses: Vec<(usize, usize, HashSet<String>)> = func_posses.iter().map(
            |func_pos| {
                make_func_signature(
                    tokens, *func_pos, ast, body_pos, &mut vec![HashMap::new()],
                    info, built_ins, &mut generics
                )
            }
        ).collect();

        let mut vars = vec![HashMap::from(
            [(String::from("self"), 0)]
        )];
        let (init_body_tok_pos, init_body_pos, mut init_generics) = make_func_signature(
            tokens, init_pos, ast, body_pos, &mut vars,
            info, built_ins, generics
        );

        make_ast_statement(
            tokens, init_body_tok_pos, ast, init_body_pos, indent + 2,
            &mut vars, info, built_ins, &mut init_generics
        );
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

        add_types(ast, init_body_pos, &mut vars, info, &None, built_ins);
        //1 remove self from args
        {
            let fun_pos = ast[init_body_pos].parent.unwrap();
            let args_pos = unwrap_u(&ast[fun_pos].children)[1];
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
        for (body_tok_pos, body_pos, mut func_generics) in func_posses {
            make_ast_statement(
                tokens, body_tok_pos, ast, body_pos, indent + 2,
                &mut vars, info, built_ins, &mut func_generics
            );
        }
    } else {
        for func_pos in func_posses {
            make_func(
                tokens, func_pos, ast, body_pos, indent + 1,
                &mut vec![HashMap::new()], info, built_ins, generics
            );
        }
    }
    pos
}

pub fn make_trait(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    info: &mut Info, generics: &mut HashSet<String>
) -> usize {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };

    let index = add_to_tree(parent, ast, Ast::new(
        AstNode::Trait {
            name: name.clone(),
            strict: matches!(tokens[pos-1], SolidToken::StrictTrait)
        }
    ));

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, ast);
    let generics_hs = HashSet::from_iter(generics_names.iter().cloned());
    let generics = &generics.union(&generics_hs)
        .map(|x| x.clone())
        .collect();

    *info.traits.get_mut(&name).unwrap() = TraitType { generics: Some(generics_names), pos: index};
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
        let params = get_params(tokens, &mut pos, info, generics);
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
            let return_type = get_arg_typ(tokens, &mut pos, info, generics);
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
        if pos >= tokens.len() {
            break
        }
    }
    pos - indent - 2
}
