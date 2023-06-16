use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::mold_ast::{Info, make_ast_expression, STType};
use crate::mold_tokens::{IsOpen, OperatorType, Pos, SolidToken, SolidTokenWPos};
use crate::types::{clean_type, MUT_STR_TYPE, Type, UNKNOWN_TYPE, TypeKind, GenericType, TypName};
use crate::{typ_with_child, unwrap_enum, some_vec, IS_COMPILED};
use crate::add_types::ast_add_types::add_types;
use crate::add_types::polymorphism::make_enums;
use crate::{throw, CUR_COL, CUR_LINE, CUR_PATH, LINE_DIFF, SRC_CODE};
use crate::add_types::utils::update_pos_from_token;

// should be passed pos = one after the opening parenthesis
// returns where pos = the index of the closing brace
// e.g.     x, y: bool | int ) -> int:
//                           ^
pub fn get_params(
    tokens: &[SolidTokenWPos], pos: &mut usize, info: &mut Info
) -> Vec<(Param, Option<Vec<Ast>>, Pos)> {
    let mut params = Vec::new();
    let mut is_mut = true;
    let mut is_args = false;
    let is_kwargs = false;
    loop {
        match &tokens[*pos].tok {
            SolidToken::Word(wrd) => {
                let src_pos = tokens[*pos + 1].pos.clone();
                let typ  = if let SolidToken::Colon = &tokens[*pos + 1].tok {
                    *pos += 2;
                    get_arg_typ(tokens, pos, info)
                } else { UNKNOWN_TYPE };

                params.push((Param {
                    name: wrd.clone(),
                    typ: if is_args {
                        typ_with_child! {
                            TypeKind::VArgs,
                            typ
                        }
                    } else { typ },
                    is_mut,
                    is_args,
                    is_kwargs,
                    default_val_pos: None
                }, None, src_pos));
                is_mut = true;
                is_args = false;
                if let SolidToken::Operator(OperatorType::Eq) = tokens[*pos].tok {
                    let mut res = vec![Ast::new_no_pos(AstNode::Module)];
                    *pos = make_ast_expression(
                        tokens, *pos + 1, &mut res,
                        0, &mut vec![], info
                    ); //1 this ignores the result just skips the default_val
                    if unsafe { IS_COMPILED } {
                        add_types(&mut res, 0, &mut vec![], info, &None);
                    }
                    params.last_mut().unwrap().1 = Some(res);
                }
                if let SolidToken::Parenthesis(IsOpen::False) = tokens[*pos].tok {
                    return params
                }
            },
            SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Operator(OperatorType::Eq) => return params,
            SolidToken::IMut => is_mut = false,
            SolidToken::UnaryOperator(OperatorType::Dereference) => is_args = true,
            SolidToken::Comma => {}
            _ => {
                update_pos_from_token(&tokens[*pos]);
                throw!("unexpected token `{}`", tokens[*pos].tok)
            }
        }
        *pos += 1;
    }
}


// returns where pos is the index of the token after the end of the type
// e.g.     x: int | bool, y: int | None) -> bool:
//                       ^              ^        ^
pub fn get_arg_typ(
    tokens: &[SolidTokenWPos], pos: &mut usize, info: &mut Info
) -> Type {
    try_get_arg_typ(tokens, pos, info, true, true, &mut 0).unwrap()
}

enum StructTraitEnumOrTuple {
    Struct, Trait, Enum, Tuple
}

/// # is_top_call should be passed true as default
/// # inside_par should be false as default
pub fn try_get_arg_typ(
    tokens: &[SolidTokenWPos], pos: &mut usize, info: &mut Info, panic: bool,
    is_top_call: bool, inside_par: &mut u32
) -> Option<Type> {
    if info.structs.is_empty() {
        throw!()
    }
    let mut res: Option<Type> = None;
    loop {
        match &tokens[*pos].tok {
            SolidToken::Bracket(IsOpen::True) => {
                let typ = match res {
                    Some(Type{ kind: TypeKind::Struct(_), .. }) => StructTraitEnumOrTuple::Struct,
                    Some(Type{ kind: TypeKind::Trait(_), .. }) => StructTraitEnumOrTuple::Trait,
                    Some(Type{ kind: TypeKind::Enum(_), .. }) => StructTraitEnumOrTuple::Enum,
                    Some(Type{ kind: TypeKind::Tuple, .. }) => StructTraitEnumOrTuple::Tuple,
                    _ => {
                        update_pos_from_token(&tokens[*pos]);
                        throw!("unexpected bracket")
                    }
                };
                if !get_inside_bracket_types(tokens, pos, info, panic, &mut res, typ) {
                    return None
                }
                
                *pos -= 1;
            },
            SolidToken::Bracket(IsOpen::False)
            | SolidToken::Comma
            | SolidToken::Colon
            | SolidToken::NewLine
            | SolidToken::Operator(OperatorType::Eq)
            | SolidToken::Where
            => break,
            SolidToken::Operator(OperatorType::BinOr) => {
                update_pos_from_token(&tokens[*pos]);
                let typ = unwrap_enum!(res, Some(x), x, "need a value before |");
                *pos += 1;
                let t = try_get_arg_typ(tokens, pos, info, panic, is_top_call, inside_par);
                *pos -= 1;
                if let Some(t) = t {
                    res = Some(typ.add_option(t));
                    make_enums(res.as_ref().unwrap(), info.one_of_enums);
                } else if panic { throw!() } else { return None };
            }
            SolidToken::Word(wrd) => {
                if res.is_some() {
                    update_pos_from_token(&tokens[*pos]);
                    throw!("unexpected type. res: {:?}, wrd: {}", res, wrd)
                }
                let wrd = clean_type(wrd.clone()).to_string();
                if !is_top_call && matches!(tokens[*pos + 1].tok, SolidToken::Operator(OperatorType::Eq)) {
                    *pos += 2;
                    let typ = try_get_arg_typ(tokens, pos, info, panic, false, inside_par);
                    let typ = if let Some(t) = typ { t } else if panic { throw!() } else { return None };
                    res = Some(typ_with_child! {
                        TypeKind::AssociatedType(wrd),
                        typ
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
                } else if wrd == "tuple" || wrd == "Tuple" {
                    Some(Type {
                        kind: TypeKind::Tuple,
                        children: None
                    })
                } else if info.structs.contains_key(&wrd) || wrd == "Self" { // TODO !!! Formatter !!!
                    Some(typ_with_child! {
                        TypeKind::Struct(TypName::Str(wrd)),
                        Type {
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
                } else if info.struct_associated_types.contains(&wrd) {
                    Some(Type {
                        kind: TypeKind::AssociatedType(wrd),
                        children: None
                    })
                } else if panic {
                    update_pos_from_token(&tokens[*pos]);
                    throw!("unexpected type `{}`", wrd)
                } else { return None };
            },
            SolidToken::UnaryOperator(op_type @ (OperatorType::Pointer | OperatorType::MutPointer)) => {
                *pos += 1;
                let is_parenthesis = matches!(tokens[*pos].tok, SolidToken::Parenthesis(IsOpen::True));
                let t = try_get_arg_typ(tokens, pos, info, panic, is_top_call, inside_par);

                let inner = if let Some(t) = t {
                    if matches!(t.kind, TypeKind::OneOf) && !is_parenthesis {
                        //1 only make the first one a pointer
                        res = Some(Type {
                            kind: TypeKind::OneOf,
                            children: Some(
                                t.children.unwrap().iter().enumerate().map(|(i, child)|
                                    if i == 0 {
                                        pointer_of(op_type, child.clone())
                                    } else {
                                        child.clone()
                                    }
                                ).collect()
                            )
                        });
                        make_enums(res.as_ref().unwrap(), info.one_of_enums);
                        continue
                    } else {
                        t
                    }
                }
                else if panic { throw!() }
                else { return None };

                res = Some(pointer_of(op_type, inner));
                *pos -= 1;
            }
            SolidToken::LifeTime(life_time) => {
                if res.is_some() {
                    update_pos_from_token(&tokens[*pos]);
                    throw!("unexpected type, found life time: `{}`", life_time)
                }
                res = Some(Type {
                    kind: TypeKind::Generic(GenericType::NoVal(life_time.clone())),
                    children: None
                });
            }
            SolidToken::Parenthesis(IsOpen::False) => {
                if *inside_par != 0 {
                    *inside_par -= 1;
                    *pos += 1;
                }
                break
            }
            SolidToken::Parenthesis(IsOpen::True) => {
                if res.is_some() {
                    update_pos_from_token(&tokens[*pos]);
                    throw!("unexpected parenthesis")
                }
                *pos += 1;
                if let SolidToken::Parenthesis(IsOpen::False) = tokens[*pos].tok {
                    res = Some(Type{
                        kind: TypeKind::EmptyType,
                        children: None
                    })
                } else {
                    *inside_par += 1;
                    return try_get_arg_typ(tokens, pos, info, panic, is_top_call, inside_par)
                    // throw!("unexpected parenthesis, at {pos}")
                }
            }
            SolidToken::Null => {
                if res.is_some() {
                    update_pos_from_token(&tokens[*pos]);
                    throw!("unexpected None")
                }
                res = Some(Type{
                    kind: TypeKind::Null,
                    children: None
                })
            }
            _ => {
                update_pos_from_token(&tokens[*pos]);
                throw!("unexpected token `{}`", tokens[*pos].tok)
            }
        }
        *pos += 1;
    }
    if res.is_none() {
        update_pos_from_token(&tokens[*pos]);
        throw!("expected type but no type found")
    }
    res
}

fn get_inside_bracket_types(
    tokens: &[SolidTokenWPos], pos: &mut usize, info: &mut Info, panic: bool,
    res: &mut Option<Type>, structs_or_traits: StructTraitEnumOrTuple
) -> bool {
    let mut no_name = TypName::Static("");
    let (struct_name, generic_map) =
        if let Some(
            Type{
                kind: TypeKind::Struct(n) | TypeKind::Trait(n) | TypeKind::Enum(n),
                children: Some(c)
            }
        ) = res {
            (n, &mut c[0])
        } else if let Some(c @ Type{ kind: TypeKind::Tuple, .. }) = res {
            (&mut no_name, c)
        } else { unreachable!() };
    *pos += 1;
    let add_child = |t: Type, i: &mut usize, info: &mut Info| {
        fn get_generics<'a, T: STType>(hm: &'a HashMap<String, T>, name: &TypName) -> Option<&'a Vec<String>> {
            if let Some(s) = hm.get(name.get_str()) {
                s.get_generics().as_ref()
            } else {
                throw!("cannot find struct `{}`", name)
            }
        }
        fn get_a_types<'a, T: STType>(hm: &'a HashMap<String, T>, name: &TypName) -> Option<&'a Vec<String>> {
            if let Some(s) = hm.get(name.get_str()) {
                s.get_associated_types().as_ref()
            } else {
                // todo update_pos_from_token(&tokens[pos]);
                throw!("cannot find struct `{}`", name)
            }
        }
        let none = vec![];
        let generics = match structs_or_traits {
            StructTraitEnumOrTuple::Struct => get_generics(info.structs, struct_name),
            StructTraitEnumOrTuple::Trait => get_generics(info.traits, struct_name),
            StructTraitEnumOrTuple::Enum => get_generics(info.enums, struct_name),
            StructTraitEnumOrTuple::Tuple => {
                *i += 1;
                return t
            }
        }.unwrap_or(&none);
        let a_types = match structs_or_traits {
            StructTraitEnumOrTuple::Struct => get_a_types(info.structs, struct_name),
            StructTraitEnumOrTuple::Trait => get_a_types(info.traits, struct_name),
            StructTraitEnumOrTuple::Enum => None,
            StructTraitEnumOrTuple::Tuple => {
                *i += 1;
                return t
            }
        }.unwrap_or(&none);
        if *i >= generics.len() + a_types.len() {
            // todo update_pos_from_token(&tokens[pos]);
            throw!("too many generics passed, expected only `{}`", generics.len() + a_types.len())
        }
        let res = if let TypeKind::AssociatedType(_) = &t.kind { t } else {
            typ_with_child! {
                TypeKind::Generic(GenericType::WithVal(generics[*i].clone())),
                t
            }
        };
        *i += 1;
        res
    };
    let mut children = vec![];
    let mut generic_num = 0;
    let mut is_first = true;
    *pos -= 1;
    while matches!(&tokens[*pos].tok, SolidToken::Comma) || is_first {
        is_first = false;
        *pos += 1;
        let t = try_get_arg_typ(tokens, pos, info, panic, false, &mut 0); // todo if it encounters a parenthesis now that is wrong (i think)
        if let Some(t) = t {
            children.push(add_child(t, &mut generic_num, info));
        } else if panic { throw!() } else { return false };
    }
    *pos += 1;
    generic_map.children = Some(children);
    true
}


#[inline] fn pointer_of(kind: &OperatorType, inner: Type) -> Type {
    typ_with_child! {
        match kind {
            OperatorType::Pointer => TypeKind::Pointer,
            OperatorType::MutPointer => TypeKind::MutPointer,
            _ => unreachable!()
        },
        typ_with_child! {
            TypeKind::Generic(GenericType::WithVal(String::from("T"))),
            inner
        }
    }
}