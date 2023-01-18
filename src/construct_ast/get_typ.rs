use crate::construct_ast::ast_structure::Param;
use crate::construct_ast::mold_ast::Info;
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{clean_type, MUT_STR_TYPE, Type, UNKNOWN_TYPE, TypeKind, GenericType, TypName};
use crate::{typ_with_child, unwrap_enum, some_vec};

// should be passed pos = one after the opening parenthesis
// returns where pos = the index of the closing brace
// e.g.     x, y: bool | int ) -> int:
//                           ^
pub fn get_params(
    tokens: &[SolidToken], pos: &mut usize, info: &mut Info
) -> Vec<Param> {
    let mut params = Vec::new();
    let mut is_mut = true;
    loop {
        match &tokens[*pos] {
            SolidToken::Word(wrd) => {
                params.push(Param {
                    name: wrd.clone(),
                    typ:
                        if let SolidToken::Colon = &tokens[*pos + 1] {
                            *pos += 2;
                            get_arg_typ(tokens, pos, info)
                        } else { UNKNOWN_TYPE },
                    is_mut
                });
                is_mut = true;
                if let SolidToken::Parenthesis(IsOpen::False)
                | SolidToken::Operator(OperatorType::Eq) = tokens[*pos] {
                    return params
                }
            },
            SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Operator(OperatorType::Eq) => return params,
            SolidToken::IMut => {
                is_mut = false;
            }
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
    tokens: &[SolidToken], pos: &mut usize,
    info: &Info
) -> Type {
    try_get_arg_typ(tokens, pos, info, true).unwrap()
}

// TODO get generics defined in scope
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
pub fn try_get_arg_typ(
    tokens: &[SolidToken], pos: &mut usize, info: &Info, panic: bool
) -> Option<Type> {
    if info.structs.is_empty() {
        panic!()
    }
    let mut res: Option<Type> = None;
    loop {
        match &tokens[*pos] {
            SolidToken::Bracket(IsOpen::True) => {
                let (struct_name, res_children) = unwrap_enum!(
                    &mut res,
                    Some(Type{
                        kind: TypeKind::Struct(n),
                        children: Some(c)
                    }),
                    (n, c)
                );
                *pos += 1;
                let add_child = |t: Type, i: usize, struct_name: &str| {
                    if !info.structs.contains_key(struct_name) {
                        panic!("cannot find struct `{struct_name}`")
                    }
                    typ_with_child! {
                        TypeKind::Generic(GenericType::Of(
                            unwrap_enum!(&info.structs[struct_name].generics)[i].clone()
                        )),
                        t
                    }
                };
                let t = try_get_arg_typ(tokens, pos, info, panic);
                let mut children = if let Some(t) = t {
                    vec![add_child(
                        t,
                        0,
                        struct_name.get_str()
                    )]
                } else if panic { panic!() } else { return None };
                let mut generic_num = 1;
                while let SolidToken::Comma = &tokens[*pos] {
                    *pos += 1;
                    let t = try_get_arg_typ(tokens, pos, info, panic);
                    if let Some(t) = t {
                        children.push(add_child(
                            t,
                            generic_num,
                            struct_name.get_str()
                        ));
                    } else if panic { panic!() } else { return None };
                    generic_num += 1;
                }
                *pos += 1;
                res_children[0].children = Some(children);
                break
            },
            SolidToken::Comma
            | SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Bracket(IsOpen::False)
            | SolidToken::Colon
            | SolidToken::NewLine
            | SolidToken::Operator(OperatorType::Eq)=> {
                break
            },
            SolidToken::Operator(OperatorType::BinOr) => {
                let typ = unwrap_enum!(res, Some(x), x, "need a value before |");
                *pos += 1;
                let t = try_get_arg_typ(tokens, pos, info, panic);
                if let Some(t) = t {
                    res = Some(typ.add_option(t));
                } else if panic { panic!() } else { return None };
                break
            }
            SolidToken::Word(wrd) => {
                if res.is_some() {
                    panic!("unexpected type. res: {:?}, wrd: {wrd}", res)
                }
                let wrd = clean_type(wrd.clone()).to_string();
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
                } else if info.types.contains_key(&wrd) {
                    Some(info.types[&wrd].clone())
                } else if info.generics.iter().any(|hs| hs.contains(&wrd)) {
                    Some(Type::new(wrd.clone()))
                } else if info.struct_inner_types.contains(&wrd) {
                    Some(Type::new(format!("Self::{}", wrd)))
                } else if panic { panic!("unexpected type `{wrd}`") } else { println!("!!!!!{wrd}!!!"); return None };
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
                        let t = try_get_arg_typ(tokens, pos, info, panic);
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
