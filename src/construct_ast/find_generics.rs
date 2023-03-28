use std::collections::HashSet;
use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::construct_ast::get_typ::get_arg_typ;
use crate::construct_ast::mold_ast::Info;
use crate::construct_ast::tree_utils::add_to_tree;
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken, SolidTokenWPos};
use crate::types::{GenericType, Type, TypeKind, TypName};

//TODO I dont think this is needed
pub fn is_generic(typ: &Type, generics_hs: &HashSet<String>) -> Type {
    if let Type { kind: TypeKind::Struct(TypName::Str(name)), children: None } = &typ {
        if generics_hs.contains(name) {
            todo!();
            return Type {
                kind: TypeKind::Generic(GenericType::NoVal(name.clone())),
                children: None,
            }
        }
    }
    typ.clone()
}

pub fn get_generic_names(pos: &mut usize, tokens: &[SolidTokenWPos], info: &mut Info) -> (Vec<String>, Vec<String>) {
    let mut generics_names = vec![];
    let mut associated_types_names = vec![];

    if let SolidToken::Bracket(IsOpen::True) = tokens[*pos].tok { //1 generics
        *pos += 1;
        while let SolidToken::Word(name) | SolidToken::LifeTime(name) = &tokens[*pos].tok {
            if let SolidToken::Operator(OperatorType::Eq) = tokens[*pos + 1].tok {
                *pos += 2;
                let _typ = get_arg_typ(tokens, pos, info);
                associated_types_names.push(name.clone());
                *pos += 1;
            } else {
                generics_names.push(name.clone());
                *pos += 2;
            }
        }
    }
    (generics_names, associated_types_names)
}

pub fn get_generics(pos: &mut usize, tokens: &[SolidTokenWPos], index: usize, ast: &mut Vec<Ast>, info: &mut Info) -> (Vec<String>, Vec<String>) {
    let src_pos = tokens[*pos].pos.clone();
    let (generics_names, associated_type_names) = get_generic_names(pos, tokens, info);
    let generics_vec: Vec<_> = generics_names.iter().map(|name|
        Type {
            kind: TypeKind::Generic(GenericType::Declaration(name.clone())),
            children: None,
        }
    ).chain(associated_type_names.iter().map(|name|
        Type {
            kind: TypeKind::AssociatedType(name.clone()),
            children: None
        }
    )).collect();
    add_to_tree(index, ast, Ast::new_w_typ(
        AstNode::GenericsDeclaration,
        Some(Type {
            kind: TypeKind::Generics,
            children: if generics_vec.is_empty() { None } else { Some(generics_vec) }
        }),
        src_pos
    ));
    (generics_names, associated_type_names)
}

pub fn find_generics_in_typ(typ: &Type, generics_hs: &HashSet<String>) -> Type {
    let mut res = is_generic(typ, generics_hs);
    res.children = res.children.map(|children|
        children.iter().map(|x| find_generics_in_typ(x, generics_hs)).collect());
    res
}