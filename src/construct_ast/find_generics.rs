use std::collections::HashSet;
use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::construct_ast::tree_utils::add_to_tree;
use crate::mold_tokens::{OperatorType, SolidToken};
use crate::types::{GenericType, Type, TypeKind, TypName};

pub fn is_generic(typ: &Type, generics_hs: &HashSet<String>) -> Type {
    if let Type { kind: TypeKind::Struct(TypName::Str(name)), children: None } = &typ {
        if generics_hs.contains(name) {
            return Type {
                kind: TypeKind::Generic(GenericType::NoVal(name.clone())),
                children: None,
            }
        }
    }
    typ.clone()
}

pub fn get_generic_names(pos: &mut usize, tokens: &[SolidToken]) -> Vec<String> {
    let mut generics_names = vec![];
    if let SolidToken::Operator(OperatorType::Smaller) = tokens[*pos] { //1 generics
        *pos += 1;
        while let SolidToken::Word(name) = &tokens[*pos] {
            generics_names.push(name.clone());
            *pos += 2;
        }
    }
    generics_names
}

pub fn get_generics(pos: &mut usize, tokens: &[SolidToken], index: usize, ast: &mut Vec<Ast>) -> Vec<String> {
    let generics_names = get_generic_names(pos, tokens);
    let generics_vec: Vec<_> = generics_names.iter().map(|name|
        Type {
            kind: TypeKind::Generic(GenericType::Declaration(name.clone())),
            children: None,
        }
    ).collect();
    add_to_tree(index, ast, Ast::new_w_typ(
        AstNode::GenericsDeclaration,
        Some(Type {
            kind: TypeKind::Generics,
            children: if generics_vec.is_empty() { None } else { Some(generics_vec) }
        })
    ));
    generics_names
}

pub fn find_generics_in_typ(typ: &Type, generics_hs: &HashSet<String>) -> Type {
    let mut res = is_generic(typ, generics_hs);
    res.children = res.children.map(|children|
        children.iter().map(|x| find_generics_in_typ(x, generics_hs)).collect());
    res
}