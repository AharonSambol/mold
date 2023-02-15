use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::mold_ast::{Info, make_ast_expression, STType};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{clean_type, MUT_STR_TYPE, Type, UNKNOWN_TYPE, TypeKind, GenericType, TypName};
use crate::{typ_with_child, unwrap_enum, some_vec, IS_COMPILED};
use crate::add_types::ast_add_types::add_types;
use crate::add_types::polymorphism::make_enums;

// should be passed pos = one after the opening parenthesis
// returns where pos = the index of the closing brace
// e.g.     x, y: bool | int ) -> int:
//                           ^
pub fn get_params(
    tokens: &[SolidToken], pos: &mut usize, info: &mut Info
) -> Vec<(Param, Option<Vec<Ast>>)> {
    let mut params = Vec::new();
    let mut is_mut = true;
    let mut is_args = false;
    let is_kwargs = false;
    loop {
        match &tokens[*pos] {
            SolidToken::Word(wrd) => {
                let typ = if let SolidToken::Colon = &tokens[*pos + 1] {
                    *pos += 2;
                    get_arg_typ(tokens, pos, info)
                } else { UNKNOWN_TYPE };
                params.push((Param {
                    name: wrd.clone(),
                    typ: if is_args {
                        typ_with_child! {
                            TypeKind::Struct(TypName::Static("Vec")),
                            typ_with_child!{
                                TypeKind::GenericsMap,
                                typ_with_child!{
                                    TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                                    typ
                                }
                            }
                        }
                    } else { typ },
                    is_mut,
                    is_args,
                    is_kwargs,
                    pos: usize::MAX
                }, None));
                is_mut = true;
                is_args = false;
                if let SolidToken::Operator(OperatorType::Eq) = tokens[*pos] {
                    let mut res = vec![Ast::new(AstNode::Module)];
                    *pos = make_ast_expression(
                        tokens, *pos + 1, &mut res,
                        0, &mut vec![], info
                    ); //1 this ignores the result just skips the default_val
                    if unsafe { IS_COMPILED } {
                        add_types(&mut res, 0, &mut vec![], info, &None);
                    }
                    params.last_mut().unwrap().1 = Some(res);
                }
                if let SolidToken::Parenthesis(IsOpen::False) = tokens[*pos] {
                    return params
                }
            },
            SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Operator(OperatorType::Eq) => return params,
            SolidToken::IMut => is_mut = false,
            SolidToken::UnaryOperator(OperatorType::Dereference) => is_args = true,
            SolidToken::Comma => {}
            _ => panic!("unexpected token {:?}", tokens[*pos])
        }
        *pos += 1;
    }
}


// returns where pos is the index of the token after the end of the type
// e.g.     x: int | bool, y: int | None) -> bool:
//                       ^              ^        ^
pub fn get_arg_typ(
    tokens: &[SolidToken], pos: &mut usize, info: &mut Info
) -> Type {
    try_get_arg_typ(tokens, pos, info, true, true).unwrap()
}

enum StructTraitOrEnum {
    Struct, Trait, Enum
}

/// # is_top_call should be passed true as default
pub fn try_get_arg_typ(
    tokens: &[SolidToken], pos: &mut usize, info: &mut Info, panic: bool, is_top_call: bool
) -> Option<Type> {
    if info.structs.is_empty() {
        panic!()
    }
    let mut res: Option<Type> = None;
    loop {
        match &tokens[*pos] {
            SolidToken::Bracket(IsOpen::True) => {
                if let Some(Type{ kind: TypeKind::Struct(_), .. }) = res {
                    if !get_inside_bracket_types(
                        tokens, pos, info, panic, &mut res, StructTraitOrEnum::Struct
                    ) { return None }
                } else if let Some(Type{ kind: TypeKind::Trait(_), .. }) = res {
                    if !get_inside_bracket_types(
                        tokens, pos, info, panic, &mut res, StructTraitOrEnum::Trait
                    ) { return None }
                } else if let Some(Type{ kind: TypeKind::Enum(_), .. }) = res {
                    if !get_inside_bracket_types(
                        tokens, pos, info, panic, &mut res, StructTraitOrEnum::Enum
                    ) { return None }
                }
                *pos -= 1;
            },
            SolidToken::Bracket(IsOpen::False)
            | SolidToken::Comma
            | SolidToken::Colon
            | SolidToken::NewLine
            | SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Operator(OperatorType::Eq)
            => break,
            SolidToken::Operator(OperatorType::BinOr) => {
                let typ = unwrap_enum!(res, Some(x), x, "need a value before |");
                *pos += 1;
                let t = try_get_arg_typ(tokens, pos, info, panic, is_top_call);
                *pos -= 1;
                if let Some(t) = t {
                    res = Some(typ.add_option(t));
                    make_enums(res.as_ref().unwrap(), info.one_of_enums);
                } else if panic { panic!() } else { return None };
            }
            SolidToken::Word(wrd) => {
                if res.is_some() {
                    panic!("unexpected type. res: {:?}, wrd: {wrd}", res)
                }
                let wrd = clean_type(wrd.clone()).to_string();
                if !is_top_call && matches!(tokens[*pos + 1], SolidToken::Operator(OperatorType::Eq)) {
                    res = Some(Type {
                        kind: TypeKind::InnerType(wrd),
                        children: some_vec![{
                            *pos += 2;
                            if let Some(t) = try_get_arg_typ(tokens, pos, info, panic, false)
                            { t } else if panic { panic!() } else { return None }
                        }],
                    });
                    break
                }
                res = if wrd == "str" {
                    Some(typ_with_child! {
                        MUT_STR_TYPE,
                        Type {
                            kind: TypeKind::GenericsMap,
                            children: None
                        }
                    })
                } else if info.structs.contains_key(&wrd) || wrd == "Self" {
                    Some(typ_with_child! {
                        TypeKind::Struct(TypName::Str(wrd)),
                        Type{
                            kind: TypeKind::GenericsMap,
                            children: None
                        }
                    })
                } else if info.traits.contains_key(&wrd) {
                    Some(typ_with_child! {
                        TypeKind::Trait(TypName::Str(wrd.clone())),
                        Type{
                            kind: TypeKind::GenericsMap,
                            children: None
                        }
                    })
                } else if info.enums.contains_key(&wrd) {
                    Some(typ_with_child! {
                        TypeKind::Enum(TypName::Str(wrd.clone())),
                        Type{
                            kind: TypeKind::GenericsMap,
                            children: None
                        }
                    })
                } else if info.types.contains_key(&wrd) {
                    Some(info.types[&wrd].clone())
                } else if info.generics.iter().any(|hs| hs.contains(&wrd)) {
                    Some(Type {
                        kind: TypeKind::Generic(GenericType::NoVal(wrd.clone())),
                        children: None
                    })
                } else if info.struct_inner_types.contains(&wrd) {
                    Some(Type {
                        kind: TypeKind::InnerType(wrd),
                        children: None
                    })
                } else if panic { panic!("unexpected type `{wrd}`") } else { return None };
            },
            SolidToken::UnaryOperator(op_type @ (OperatorType::Pointer | OperatorType::MutPointer)) => {
                *pos += 1;
                res = Some(Type {
                    kind: match op_type {
                        OperatorType::Pointer => TypeKind::Pointer,
                        OperatorType::MutPointer => TypeKind::MutPointer,
                        _ => unreachable!()
                    },
                    children: some_vec![{
                        let t = try_get_arg_typ(tokens, pos, info, panic, is_top_call);
                        if let Some(t) = t { t }
                        else if panic { panic!() }
                        else { return None }
                    }]
                });
                *pos -= 1;
            }
            _ => panic!("unexpected token {:?}, at {}", tokens[*pos], *pos)
        }
        *pos += 1;
    }
    if res.is_none() {
        panic!("expected arg but no arg found")
    }
    res
}

fn get_inside_bracket_types(
    tokens: &[SolidToken], pos: &mut usize, info: &mut Info, panic: bool,
    res: &mut Option<Type>, structs_or_traits: StructTraitOrEnum
) -> bool {
    let (struct_name, res_children) = unwrap_enum!(
        res,
        Some(Type{
            kind: TypeKind::Struct(n) | TypeKind::Trait(n) | TypeKind::Enum(n),
            children: Some(c)
        }),
        (n, c)
    );

    *pos += 1;
    let add_child = |t: Type, i: &mut usize, info: &mut Info| {
        if let TypeKind::InnerType(_) = &t.kind {
            return t
        }
        fn get_generics<'a, T: STType>(hm: &'a HashMap<String, T>, name: &TypName) -> Option<&'a Vec<String>> {
            if let Some(s) = hm.get(name.get_str()) {
                s.get_generics().as_ref()
            } else {
                panic!("cannot find struct `{name}`")
            }
        }
        let generics = match structs_or_traits {
            StructTraitOrEnum::Struct => get_generics(info.structs, struct_name),
            StructTraitOrEnum::Trait => get_generics(info.traits, struct_name),
            StructTraitOrEnum::Enum => get_generics(info.enums, struct_name)
        }.unwrap();
        if *i >= generics.len() {
            panic!("too many generics passed, expected only `{}`", generics.len())
        }
        let res = typ_with_child! {
            TypeKind::Generic(GenericType::WithVal(generics[*i].clone())),
            t
        };
        *i += 1;
        res
    };
    let mut children = vec![];
    let mut generic_num = 0;
    let mut is_first = true;
    *pos -= 1;
    while matches!(&tokens[*pos], SolidToken::Comma) || is_first {
        is_first = false;
        *pos += 1;
        let t = try_get_arg_typ(tokens, pos, info, panic, false);
        if let Some(t) = t {
            children.push(add_child(t, &mut generic_num, info));
        } else if panic { panic!() } else { return false };
    }
    *pos += 1;
    res_children[0].children = Some(children);
    true
}
