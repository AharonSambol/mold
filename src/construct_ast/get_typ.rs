use crate::construct_ast::ast_structure::Param;
use crate::construct_ast::get_functions_and_types::{FuncTypes, StructTypes, TraitTypes};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{clean_type, MUT_STR_TYPE, Type, UNKNOWN_TYPE, TypeKind, GenericType, TypName};
use crate::{typ_with_child, unwrap_enum, some_vec};

// should be passed pos = one after the opening parenthesis
// returns where pos = the index of the closing brace
// e.g.     x, y: bool | int ) -> int:
//                           ^
pub fn get_params(
    tokens: &[SolidToken], pos: &mut usize, funcs: &mut FuncTypes, structs: &StructTypes, traits: &TraitTypes
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
                        get_arg_typ(tokens, pos, funcs, structs, traits)
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
            _ => panic!("unexpected token {:?}", tokens[*pos])
        }
        *pos += 1;
    }
}


// returns where pos is the index of the token after the end of the type
// e.g.     x: int | bool, y: int | None) -> bool:
//                       ^              ^        ^
pub fn get_arg_typ(
    tokens: &[SolidToken], pos: &mut usize, _funcs: &mut FuncTypes, structs: &StructTypes, traits: &TraitTypes
) -> Type {
    if structs.is_empty() {
        panic!()
    }
    let mut res: Option<Type> = None;
    loop {
        match &tokens[*pos] {
            SolidToken::Bracket(IsOpen::True) => {
                unwrap_enum!(&mut res, Some(Type{
                    kind: TypeKind::Struct(struct_name),
                    children: Some(res_children)
                }));
                *pos += 1;
                let add_child = |t: Type, i: usize, struct_name: &str| {
                    if !structs.contains_key(struct_name) {
                        panic!("cannot find struct `{struct_name}`")
                    }
                    typ_with_child! {
                        TypeKind::Generic(GenericType::Of(
                            unwrap_enum!(&structs[struct_name].generics)[i].clone()
                        )),
                        t
                    }
                };
                let mut children = vec![add_child(
                    get_arg_typ(tokens, pos, _funcs, structs, traits),
                    0,
                    struct_name.get_str()
                )];
                let mut generic_num = 1;
                while let SolidToken::Comma = &tokens[*pos] {
                    *pos += 1;
                    children.push(add_child(
                        get_arg_typ(tokens, pos, _funcs, structs, traits),
                        generic_num,
                        struct_name.get_str()
                    ));
                    generic_num += 1;
                }
                *pos += 1;
                res_children[0].children = Some(children);
                // res = Some(typ_with_child! {
                //     TypeKind::Struct(struct_name),
                //     Type {
                //         kind: TypeKind::GenericsMap,
                //         children: Some(children),
                //     }
                // });
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
                res = Some(typ.add_option(get_arg_typ(tokens, pos, _funcs, structs, traits)));
                break
            }
            SolidToken::Word(wrd) => {
                if res.is_some() {
                    panic!("unexpected type. res: {:?}, wrd: {wrd}", res)
                }
                let wrd = clean_type(wrd.clone()).to_string();
                res = if wrd == "str" {
                    Some(MUT_STR_TYPE)
                } else if structs.contains_key(&wrd) {
                    Some(typ_with_child! {
                        TypeKind::Struct(TypName::Str(wrd)),
                        Type{
                            kind: TypeKind::GenericsMap,
                            children: None
                        }
                    })
                } else if traits.contains_key(&wrd) {
                    Some(typ_with_child! {
                        TypeKind::Trait(TypName::Str(wrd.clone())),
                        Type{
                            kind: TypeKind::GenericsMap,
                            children: None
                        }
                    })
                } else {
                    // TODO check that its a generic otherwise panic
                    //  panic!("typ `{}` doesnt exist", wrd)
                    Some(Type::new(wrd.clone()))
                };

            },
            _ => panic!("unexpected token {:?}, at {}", tokens[*pos], *pos)
        }
        *pos += 1;
    }
    res.unwrap_or_else(|| panic!("expected arg but no arg found"))
}
