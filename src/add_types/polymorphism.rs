use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::{typ_with_child, unwrap_enum, some_vec, EMPTY_STR, OneOfEnums, OneOfEnumTypes, IMPL_TRAITS, ImplTraitsKey, get_traits, MUT_POINTER_WITHOUT_LIFETIME, POINTER_WITHOUT_LIFETIME};
use crate::add_types::ast_add_types::{get_enum_property_typ, get_property_idf_typ, get_property_method_typ, SPECIFIED_NUM_TYPE_RE};
use crate::add_types::generics::{get_function_return_type};
use crate::add_types::utils::{get_from_stack, get_pointer_complete_inner, get_pointer_inner, join, join_or};
use crate::construct_ast::mold_ast::{Info};
use crate::construct_ast::tree_utils::{add_as_first_child, add_to_tree, insert_as_parent_of, print_tree, update_pos_from_tree_node};
use crate::mold_tokens::OperatorType;
use crate::types::{GenericType, implements_trait, print_type, Type, TypeKind, TypName, unwrap, unwrap_u};
use crate::{throw, CUR_COL, CUR_LINE, CUR_PATH, LINE_DIFF, SRC_CODE};

// TODO also cast: `as Box<dyn P>`
fn add_box(ast: &mut Vec<Ast>, pos: usize) -> usize {
    let ast_len = ast.len();
    let parent_pos = ast[pos].parent.unwrap();

    let parent_children = ast[parent_pos].children.as_mut().unwrap();
    *parent_children.iter_mut().find(|x| **x == pos).unwrap() = ast_len; //1 ast_len is the position the Property will be pushed into

    ast.push(Ast {
        value: AstNode::Property,
        children: None,
        parent: Some(parent_pos),
        is_mut: false,
        typ: Some(typ_with_child! {
            TypeKind::Struct(TypName::Static("Box")),
            typ_with_child! {
                TypeKind::GenericsMap,
                typ_with_child! {
                    TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                    ast[pos].typ.clone().unwrap()
                }
            }
        }),
        pos: None
    });
    let property = ast.len() - 1;
    add_to_tree(
        property, ast, Ast::new_no_pos(AstNode::Identifier(String::from("Box")))
    );
    let func_call = add_to_tree(
        property, ast, Ast::new_no_pos(AstNode::FunctionCall(true))
    );
    add_to_tree(
        func_call, ast, Ast::new_no_pos(AstNode::Identifier(String::from("new")))
    );
    let args_pos = add_to_tree(func_call, ast, Ast::new_no_pos(AstNode::Args));

    ast[pos].parent = Some(args_pos);
    ast[args_pos].children = Some(vec![pos]);

    property
}

/* TODO
    fn cast_one_of_to_other_one_of(ast: &mut Vec<Ast>, pos: usize, to_typ: Type) -> usize {
        let ast_len = ast.len();
        let parent_pos = ast[pos].parent.unwrap();

        let parent_children = ast[parent_pos].children.as_mut().unwrap();
        *parent_children.iter_mut().find(|x| **x == pos).unwrap() = ast_len; //1 ast_len is the position the Property will be pushed into

        ast.push(Ast {
            value: AstNode::Body,
            children: None,
            parent: Some(parent_pos),
            typ: Some(to_typ),
            is_mut: false,
        });
        let body = ast.len() - 1;

        add_to_tree(
            body, ast, Ast::new(AstNode::Match(String::from("Box")))
        );
        let func_call = add_to_tree(
            body, ast, Ast::new(AstNode::FunctionCall(true))
        );
        add_to_tree(
            func_call, ast, Ast::new(AstNode::Identifier(String::from("new")))
        );
        let args_pos = add_to_tree(func_call, ast, Ast::new(AstNode::Args));

        ast[pos].parent = Some(args_pos);
        ast[args_pos].children = Some(vec![pos]);

        property
    }

 */

pub fn make_enums(typ: &Type, enums: &mut OneOfEnums) {
    // todo sort here
    let types = unwrap(&typ.children);
    let mut generics = vec![];
    types.iter().for_each(|x| {
        if let TypeKind::Generic(GenericType::NoVal(name)) = &x.kind {
            if !generics.contains(name) {
                generics.push(name.clone());
            }
        }
        if let Some(ch) = &x.children {
            if let TypeKind::GenericsMap = &ch[0].kind {
                for generic in unwrap(&ch[0].children) {
                    if let TypeKind::AssociatedType(name) = &generic.kind {
                        generics.push(format!("{name}={}", &generic.ref_children()[0]));
                        // generic = &generic.ref_children()[0];
                        // todo!()
                        continue
                    }
                    if let TypeKind::Generic(GenericType::NoVal(name)) = &generic.kind {
                        if !generics.contains(name) {
                            generics.push(name.clone());
                        }
                    }
                }
            }
        }
    });
    let needs_lifetime = types.iter().any(|x| { //1 if contains a pointer without a lifetime
        let x_str = x.to_string();
        if x_str.contains('&') {
            MUT_POINTER_WITHOUT_LIFETIME.find(&x_str).is_ok()
                || POINTER_WITHOUT_LIFETIME.find(&x_str).is_ok()
        } else { false }
    });

    let generics = if generics.is_empty() {
        EMPTY_STR
    } else {
        format!("<{}>", join(generics.iter(), ","))
    };
    let enm_name = escape_typ_chars(&typ.to_string());

    enums.entry(enm_name).or_insert_with(|| {
        OneOfEnumTypes {
            generics,
            needs_lifetime,
            options: types.clone()
        }
    });
}

#[inline]
pub fn escape_typ_chars(st: &str) -> String {
    st
        .replace("::<", "_of_")
        .replace('>', "_endof_")
        .replace("Box<dyn ", "_boxof_")
        .replace('=', "_eq_")
        .replace("&mut", "_mutpointer_").replace("& mut", "_mutpointer_")
        .replace('&', "_pointer_")
}

fn add_one_of_enum(
    ast: &mut Vec<Ast>, pos: usize, enum_name: &str, enum_option: &str
) -> usize { // todo i think this leaves ast[pos] without anything pointing at it
    let inner_val = ast[pos].clone();

    ast[pos] = Ast {
        value: AstNode::Property,
        children: None,
        parent: Some(inner_val.parent.unwrap()),
        typ: Some(typ_with_child! {
            TypeKind::Enum(TypName::Str(String::from(enum_name))),
            Type { kind: TypeKind::GenericsMap, children: None }
        }),
        is_mut: false,
        pos: None
    };
    let property = pos;
    let enm = add_to_tree(
        property, ast, Ast::new_no_pos(AstNode::Identifier(String::from(enum_name)))
    );
    ast[enm].typ = Some(Type{ kind: TypeKind::Enum(TypName::Str(String::from(enum_name))), children: None }); //1 so that uses `::` and not `.`

    if enum_option == "_None" {
        add_to_tree(property, ast, Ast::new_no_pos(AstNode::Identifier(String::from("_None"))));
        return property
    }
    let func_call = add_to_tree(property, ast, Ast::new_no_pos(AstNode::FunctionCall(true)));
    for x in [AstNode::Identifier(String::from(enum_option)), AstNode::Args] {
        add_to_tree(func_call, ast, Ast::new_no_pos(x));
    }
    let args_pos = ast.len() - 1;
    add_to_tree(args_pos, ast, inner_val);
    property
}


// pub fn box_typ_no_side_effects(expected: &Type, got: &Type, ast: &Vec<Ast>, info: &Info, is_outer: bool) -> Option<Type> {
//     // TODO this doesnt deal with generics at all???
//     if expected == got {
//         return Some(got.clone())
//     }
//     if is_outer {
//         if let TypeKind::OneOf = expected.kind {
//             if let TypeKind::OneOf = &got.kind {
//                 if expected.contains(got) {
//                     return Some(expected.clone())
//                 }
//             }
//             for typ in unwrap(&expected.children) {
//                 let res = box_typ_no_side_effects(typ, got, ast, info, false);
//                 if res.is_some() {
//                     return res
//                 }
//             }
//             throw!(
//                 "expected: `{}` but found `{got}`",
//                 join(expected.ref_children().iter(), "` or `")
//             );
//         }
//     }
//     if let TypeKind::OneOf = &got.kind {
//         if got.children.as_ref().unwrap().iter().all(|typ| can_soft_cast(typ, expected)) {
//             return Some(expected.clone())
//         }
//         if let TypeKind::Trait(_) = &expected.kind {
//             let res = implements_trait(got, expected, ast, info);
//             if res.is_some() {
//                 return res
//             }
//         }
//         throw!("can't cast `{got}` to `{expected}`")
//     }
//
//     if let TypeKind::Trait(_) = &expected.kind {
//         return implements_trait(got, expected, ast, info)
//     }
//     None
// }

#[inline(always)] pub fn box_if_needed(
    expected: Type, ast: &mut Vec<Ast>, pos: usize, info: &mut Info
) -> Type {
    let got_typ = ast[pos].typ.clone().unwrap();
    check_if_need_box(expected, &got_typ, ast, pos, info, true, Mutate::Box)
        .unwrap_or_else(|e| {
            update_pos_from_tree_node(&ast[pos]);
            throw!("{}", e)
        })
}

#[inline(always)] pub fn box_no_side_effects(
    expected: Type, got_typ: &Type, ast: &mut Vec<Ast>, info: &mut Info
) -> Type {
    try_box_no_side_effects(expected, got_typ, ast, info).unwrap_or_else(|e| throw!("{}", e))
}

#[inline(always)] pub fn try_box_no_side_effects(
    expected: Type, got_typ: &Type, ast: &mut Vec<Ast>, info: &mut Info
) -> Result<Type, String> {
    check_if_need_box(expected, got_typ, ast, usize::MAX, info, true, Mutate::Dont)
}

#[inline(always)] pub fn matches_template(expected: Type, got: &Type, ast: &mut Vec<Ast>, info: &mut Info) -> bool {
    check_if_need_box(expected, got, ast, usize::MAX, info, true, Mutate::Dont).is_ok()
}

#[derive(Copy, Clone)]
enum Mutate {
    Dont,
    Box,
    Map
}

fn check_if_need_box(
    expected: Type, got_typ: &Type, ast: &mut Vec<Ast>, pos: usize, info: &mut Info,
    is_outer_call: bool, mutate: Mutate
) -> Result<Type, String> {
    // println!("Exp:");
    // print_type(&Some(expected.clone()));
    // println!("Got:");
    // print_type(&Some(got_typ.clone()));
    let err = || Err(format!("expected `{expected}` but got `{got_typ}`"));

    if expected == *got_typ {
        return Ok(expected)
    }

    match &expected.kind {
        TypeKind::OneOf => {
            // TODO generics and associated types
            if let TypeKind::OneOf = &got_typ.kind {
                if expected.contains(got_typ) {
                    if let Mutate::Dont = mutate {
                        return Ok(expected)
                    }
                    if let Mutate::Map = mutate {
                        todo!();
                        // todo maybe use map instead of list comp?
                        // let stmt_mod = insert_as_parent_of(ast, pos, AstNode::Module);
                        // let comp_pos = insert_as_parent_of(ast, stmt_mod, AstNode::ListComprehension);
                        // add_to_tree(comp_pos, ast, Ast::new(AstNode::Module)); //1 loop_mod
                        // add_to_tree(comp_pos, ast, Ast::new(AstNode::Module)); //1 condition_mod
                    }
                    match_to_convert_types(&expected, got_typ, ast, pos);
                    return Ok(expected)
                } else {
                    todo!()
                }
            }
            for typ in unwrap(&expected.children) {
                if typ == got_typ {
                    if let Mutate::Map = mutate {
                        // todo maybe use map instead of list comp?
                        let for_iter = insert_as_parent_of(ast, pos, AstNode::ForIter, None);
                        let par_pos = insert_as_parent_of(ast, for_iter, AstNode::ColonParentheses, None);
                        let for_pos = insert_as_parent_of(ast, par_pos, AstNode::ForStatement, None);
                        let loop_mod = insert_as_parent_of(ast, for_pos, AstNode::Module, None);
                        let comp_pos = insert_as_parent_of(ast, loop_mod, AstNode::ListComprehension, None);
                        let stmt_mod = add_as_first_child(ast, comp_pos, AstNode::Module, None);
                        let stmt = add_to_tree(
                            stmt_mod, ast, Ast::new_no_pos(AstNode::Identifier(String::from("x")))
                        );
                        ast[stmt].typ = Some(got_typ.clone());
                        add_to_tree(comp_pos, ast, Ast::new_no_pos(AstNode::Module)); //1 condition_mod

                        let for_vars = add_as_first_child(ast, par_pos, AstNode::ForVars, None);
                        add_to_tree(
                            for_vars, ast,
                            Ast::new_no_pos(AstNode::Identifier(String::from("x")))
                        );

                        match_to_convert_types(&expected, &expected, ast, stmt);
                    } else if let Mutate::Box = mutate {
                        add_one_of_enum(
                            ast, pos, &expected.to_string(),
                            &format!("_{}", escape_typ_chars(&typ.to_string()))
                        );
                    }
                    return Ok(expected.clone())
                }
                if matches!(&typ.kind, TypeKind::Trait(_)) {
                    if let Some(trt) = implements_trait(got_typ, typ, ast, info) {
                        if let Mutate::Box = mutate {
                            let new_pos = add_box(ast, pos);
                            add_one_of_enum(
                                ast, new_pos, &expected.to_string(),
                                &format!("_{}", escape_typ_chars(&typ.to_string()))
                            );
                        }
                        return Ok(trt)
                    }
                }
            }
            Err(format!(
                "expected: `{}` but found `{}`",
                join_or(expected.ref_children().iter()),
                got_typ
            ))
        }
        TypeKind::Trait(_) => {
            if let Some(trt) = implements_trait(got_typ, &expected, ast, info) {
                if !matches!(&got_typ.kind, TypeKind::Trait(_)) {
                    if !is_outer_call { return err() }
                    if let Mutate::Box = mutate { add_box(ast, pos); }
                }
                Ok(trt)
            } else { err() }
        }
        TypeKind::Pointer | TypeKind::MutPointer => {
            let res = if got_typ.kind == expected.kind
                || matches!((&expected.kind, &got_typ.kind), (TypeKind::Pointer, TypeKind::MutPointer))
            {
                let expected = get_pointer_inner(&expected).clone();
                let got_typ = get_pointer_inner(got_typ);
                if is_outer_call {
                    check_if_need_box(expected, got_typ, ast, pos, info, true, mutate)
                } else {
                    check_if_need_box(expected, got_typ, ast, pos, info, false, Mutate::Dont)
                }
            } else { err() };
            if res.is_err() && is_outer_call {
                if let TypeKind::Pointer = &expected.kind {
                    let expected = get_pointer_inner(&expected).clone();
                    return Ok(typ_with_child! {
                        TypeKind::Pointer,
                        typ_with_child! {
                            TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                            check_if_need_box(expected, got_typ, ast, pos, info, true, mutate)?
                        }
                    })
                } else {
                    // todo need to check if typ is mutable
                }
            }
            Ok(typ_with_child! {
                got_typ.kind.clone(),
                typ_with_child! {
                    TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                    res?
                }
            })
        }
        TypeKind::Struct(expected_struct_name) => {
            if let TypeKind::Struct(got_struct_name) = &got_typ.kind {
                if expected_struct_name != got_struct_name {
                    return err()
                }
                if expected_struct_name == "Vec" {
                    let res = compare_generic_and_a_typ_children(&expected, got_typ, err);
                    if res.is_ok() || !is_outer_call { res } else {
                        let expected = &expected
                            .children.as_ref().unwrap()[0] //1 generic map
                            .children.as_ref().unwrap()[0] //1 the generic
                            .children.as_ref().unwrap()[0];//1 the value of the generic
                        let got = &got_typ
                            .children.as_ref().unwrap()[0] //1 generic map
                            .children.as_ref().unwrap()[0] //1 the generic
                            .children.as_ref().unwrap()[0];//1 the value of the generic
                        check_if_need_box(
                            expected.clone(), got, ast, pos, info, true, Mutate::Map
                        )
                    }
                } else {
                    compare_generic_and_a_typ_children(&expected, got_typ, err)
                }
            } else { err() }
        }
        TypeKind::Enum(expected_enum_name) => {
            if let TypeKind::Enum(got_enum_name) = &got_typ.kind {
                if expected_enum_name != got_enum_name {
                    return err()
                }
                compare_generic_and_a_typ_children(&expected, got_typ, err)
            } else { err() }
        }
        TypeKind::Tuple => { todo!() }
        TypeKind::Args => {
            let TypeKind::Args = got_typ.kind else {
                unreachable!()
            };
            let res_typs: Vec<_> =
                expected.ref_children().iter().zip(got_typ.ref_children()).map(|(exp, got)| {
                    // TODO adding the box will prob be problematic
                    check_if_need_box(exp.clone(), got, ast, pos, info, true, mutate)
                }).collect();
            if let Some(err_one) = res_typs.iter().find(|x| x.is_err()) {
                return Err(unsafe { err_one.as_ref().err().unwrap_unchecked().clone() })
            }
            Ok(Type {
                kind: TypeKind::Args,
                children: Some(res_typs.iter().map(|x|
                    unsafe { x.as_ref().ok().unwrap_unchecked().clone() }
                ).collect())
            })
        }
        TypeKind::Generic(GenericType::NoVal(_)) => Ok(got_typ.clone()),
        _ => unreachable!("{:?}", expected.kind)
    }
}

fn match_to_convert_types(expected: &Type, got_typ: &Type, ast: &mut Vec<Ast>, pos: usize) {
    let match_pos = insert_as_parent_of(ast, pos, AstNode::Match, None);
    let from_enum_name = got_typ.to_string();
    let to_enum_name = expected.to_string();
    for typ in got_typ.ref_children() {
        let case = add_to_tree(match_pos, ast, Ast::new_no_pos(AstNode::Case));
        /**/let condition = add_to_tree(case, ast, Ast::new_no_pos(AstNode::Body));
        /*-*/let prop = add_to_tree(condition, ast, Ast::new_no_pos(AstNode::Property));
        /*--*/let frm_enm = add_to_tree(prop, ast, Ast::new_no_pos(AstNode::Identifier(from_enum_name.clone())));
        ast[frm_enm].typ = Some(Type { kind: TypeKind::Enum(TypName::Static("")), children: None }); //1 so that is uses `::` and not `.`
        /*--*/add_to_tree(prop, ast, Ast::new_no_pos(AstNode::Identifier(format!("_{typ}"))));
        /**/add_to_tree(case, ast, Ast::new_no_pos(AstNode::Identifier(String::from("x"))));

        /**/let body = add_to_tree(case, ast, Ast::new_no_pos(AstNode::Module));
        /*-*/let prop = add_to_tree(body, ast, Ast::new_no_pos(AstNode::Property));
        /*--*/add_to_tree(prop, ast, Ast::new_no_pos(AstNode::Identifier(to_enum_name.clone())));
        /*--*/let func_call = add_to_tree(prop, ast, Ast::new_no_pos(AstNode::FunctionCall(true)));
        /*---*/add_to_tree(func_call, ast, Ast::new_no_pos(AstNode::Identifier(format!("_{typ}"))));
        /*---*/let args = add_to_tree(func_call, ast, Ast::new_no_pos(AstNode::Args));
        /*----*/add_to_tree(args, ast, Ast::new_no_pos(AstNode::Identifier(String::from("x"))));
    }
    // TODO add case _: unreachable!()
}

fn compare_generic_and_a_typ_children<F: Fn() -> Result<Type, String>>(
    expected: &Type, got_typ: &Type, err: F
) -> Result<Type, String> {
    let expected_children = unwrap(&expected.ref_children()[0].children);
    let got_children = unwrap(&got_typ.ref_children()[0].children);

    let mut res = got_typ.clone();
    let mut empty = vec![];
    let res_children = res.children.as_mut().unwrap()[0].children.as_mut().unwrap_or(&mut empty);

    #[derive(PartialEq, Eq, Hash)]
    enum GOrT {
        Generic(String),
        AType(String)
    }
    let mut expected_hm = HashMap::new();
    for exp_arg in expected_children {
        match &exp_arg.kind {
            TypeKind::Generic(GenericType::NoVal(name)) =>
                expected_hm.insert(GOrT::Generic(name.clone()), None),
            TypeKind::Generic(GenericType::WithVal(name)) =>
                expected_hm.insert(GOrT::Generic(name.clone()), Some(exp_arg.ref_children()[0].clone())),
            TypeKind::AssociatedType(name) =>
                expected_hm.insert(
                    GOrT::AType(name.clone()),
                    exp_arg.children.as_ref().map(|children| children[0].clone())
                ),
            _ => unreachable!("{:?}", exp_arg),
        };
    }
    for (got_arg, res_arg) in got_children.iter().zip(res_children) {
        match &got_arg.kind {
            TypeKind::Generic(GenericType::NoVal(name)) => {
                if let Some(exp) = expected_hm.get(&GOrT::Generic(name.clone())) {
                    if exp.is_some() { return err() }
                } else { return err() }
            }
            TypeKind::Generic(GenericType::WithVal(name)) => {
                if let Some(exp) = expected_hm.get(&GOrT::Generic(name.clone())) {
                    if let Some(exp) = exp {
                        if let TypeKind::Unknown = &got_arg.ref_children()[0].kind {
                            res_arg.children.as_mut().unwrap()[0] = exp.clone();
                        } else if *exp != got_arg.ref_children()[0] { return err() }
                    }
                } else { return err() }
            }
            TypeKind::AssociatedType(name) => {
                let inner_typ = got_arg.children.as_ref()
                    .map(|children| children[0].clone());

                if let Some(exp) = expected_hm.get(&GOrT::AType(name.clone())) {
                    if matches!(exp, Some(_) if *exp != inner_typ) { return err() }
                } else { return err() }
            }
            _ => unreachable!("{:?}", got_arg),
        };
    }
    Ok(res)
}
/*
// 5 *** polymorphism doesn't work for `OneOf` types ***
pub fn out_of_service___check_for_boxes(
    expected: Type, ast: &mut Vec<Ast>, pos: usize, info: &mut Info, vars: &VarTypes, is_outer_call: bool
) -> Type {
    #[inline] fn is_box_typ(typ: &Type) -> bool {
        match &typ.kind {
            TypeKind::Struct(bx) => bx == "Box",
            TypeKind::Trait(_) => true,
            _ => false
        }
    }
    #[inline] fn supplied_box(
        got: &Ast, vars: &VarTypes, ast: &[Ast], info: &mut Info, pos: usize
    ) -> bool {
        match &got.value {
            AstNode::Struct(bx) => bx == "Box",
            AstNode::Trait { .. } => true,
            AstNode::Identifier(idf) => {
                let typ = get_from_stack(vars, idf).unwrap();
                let typ = ast[typ].typ.clone().unwrap();
                match &typ.kind {
                    TypeKind::Struct(name)  =>
                        if name == "Box" { return true },
                    TypeKind::Trait(_) => return true,
                    _ => ()
                }
                false
            },
            AstNode::Index => {
                is_box_typ(
                    &find_index_typ(
                        ast, info, got.ref_children()[0], pos
                    ).unwrap()
                )
            }
            AstNode::FunctionCall(_) => {
                let func_name = unwrap_enum!(
                    &ast[unwrap_u(&got.children)[0]].value,
                    AstNode::Identifier(n), n
                );
                let return_type = info.funcs[func_name].output.as_ref().unwrap();
                is_box_typ(return_type)
            }
            AstNode::Property => {
                let typ = get_property_typ(got, ast, info, pos);
                if let Some(t) = typ { is_box_typ(&t) } else { false }
            }
            _ => false
        }
    }

    let got = &ast[pos].clone();
    if let AstNode::NamedArg(_) = got.value {
        return box_if_needed(
            expected, ast, unwrap_u(&got.children)[0], info, vars, false
        ).unwrap_or_else(|e| throw!("{e}"))
    }
    if let TypeKind::OneOf = expected.kind {
        if let Some(got_typ @ Type{ kind: TypeKind::OneOf, .. }) = &got.typ {
            if expected == *got_typ {
                return expected
            } else if expected.contains(got_typ) {
                let match_pos = insert_as_parent_of(ast, pos, AstNode::Match);
                let from_enum_name = got_typ.to_string();
                let to_enum_name = expected.to_string();
                for typ in got_typ.ref_children() {
                    let case = add_to_tree(match_pos, ast, Ast::new(AstNode::Case));
                    /**/let condition = add_to_tree(case, ast, Ast::new(AstNode::Body));
                    /*-*/let prop = add_to_tree(condition, ast, Ast::new(AstNode::Property));
                    /*--*/let frm_enm =add_to_tree(prop, ast, Ast::new(AstNode::Identifier(from_enum_name.clone())));
                    ast[frm_enm].typ = Some(Type { kind: TypeKind::Enum(TypName::Static("")), children: None}); //1 so that is uses `::` and not `.`
                    /*--*/add_to_tree(prop, ast, Ast::new(AstNode::Identifier(format!("_{typ}"))));
                    /**/add_to_tree(case, ast, Ast::new(AstNode::Identifier(String::from("x"))));

                    /**/let body = add_to_tree(case, ast, Ast::new(AstNode::Body));
                    /*-*/let prop = add_to_tree(body, ast, Ast::new(AstNode::Property));
                    /*--*/add_to_tree(prop, ast, Ast::new(AstNode::Identifier(to_enum_name.clone())));
                    /*--*/let func_call = add_to_tree(prop, ast, Ast::new(AstNode::FunctionCall(true)));
                    /*---*/add_to_tree(func_call, ast, Ast::new(AstNode::Identifier(format!("_{typ}"))));
                    /*---*/let args = add_to_tree(func_call, ast, Ast::new(AstNode::Args));
                    /*----*/add_to_tree(args, ast, Ast::new(AstNode::Identifier(String::from("x"))));
                }
                print_tree(ast, match_pos);
                // TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                return expected
            }
        }
        for typ in unwrap(&expected.children) {
            if let Some(t) = &got.typ {
                if t == typ {
                    add_one_of_enum(
                        ast, pos, &expected.to_string(),
                        &format!("_{}", escape_typ_chars(&typ.to_string()))
                    );
                    return got.typ.clone().unwrap()
                }
                if matches!(
                    &typ.kind, TypeKind::Trait(_) if implements_trait(t, typ, ast, info).is_some()
                ) {
                    // print_tree(ast, pos);
                    let new_pos = add_box(ast, pos);
                    add_one_of_enum(
                        ast, new_pos, &expected.to_string(),
                        &format!("_{}", escape_typ_chars(&typ.to_string()))
                    );
                    // todo does this have the correct generics?
                    return expected
                }
            }
        }
        print_tree(ast, 0);
        println!("{}", join(expected.ref_children().iter(), "` or `"));
        println!("{:?}", got);
        throw!(
            "expected: `{}` but found `{}`",
            join(expected.ref_children().iter(), "` or `"),
            got.typ.clone().unwrap()
        );
    }
    if let Some(Type { kind: TypeKind::OneOf, children: Some(types) }) = &got.typ {
        #[inline(always)] fn can_cast(types: &Vec<Type>, expected: &Type, ast: &mut Vec<Ast>, pos: usize) -> bool {
            for typ in types {
                if !can_soft_cast(typ, expected) {
                    return false
                }
            }
            insert_as_parent_of(ast, pos, AstNode::As(expected.to_string()));
            true
        }

        if can_cast(types, &expected, ast, pos) {
            return expected
        }
        if let TypeKind::Trait(_) = &expected.kind {
            if let Some(res) = implements_trait(got.typ.as_ref().unwrap(), &expected, ast, info) {
                add_box(ast, pos);
                return res
            }
        }
        throw!("can't cast `{}` to `{expected}`", got.typ.as_ref().unwrap())
    }
    if is_box_typ(&expected) {
        if !supplied_box(got, vars, ast, info, pos) {
            if let TypeKind::Trait(_) = &expected.kind {
                let mut got_typ = got.typ.as_ref().unwrap();
                if let TypeKind::Generic(GenericType::WithVal(_)) = &got_typ.kind {
                    got_typ = &got_typ.ref_children()[0];
                }
                if let Some(res) = implements_trait(got_typ, &expected, ast, info) {
                    let pos = add_box(ast, pos);
                    ast[pos].typ = Some(res.clone());
                    return res
                } else {
                    print_type(&Some(expected.clone()));
                    print_type(&Some(got_typ.clone()));
                    // todo!()
                    throw!(
                        "`{got_typ}` doesn't implement `{}`",
                        expected.to_string()
                            .strip_prefix("Box<dyn ").unwrap()
                            .strip_suffix('>').unwrap()
                    )
                }
            } else {
                // check_for_boxes(
                //     unwrap(&expected.children)[0].clone(), ast, pos, info, vars
                // );

                let pos = add_box(ast, pos);
                ast[pos].typ = Some(expected.clone());
                // TODO shouldn't it keep checking?
            }
        } else {
            let TypeKind::Trait(_) = &expected.kind else {
                return box_if_needed(
                    unwrap(&expected.children)[0].clone(), ast, pos, info, vars, false
                ).unwrap_or_else(|e| throw!("{e}"))
            };
        }
        return expected;
    }
    match expected.kind.clone() {
        TypeKind::Struct(ex_name)
        | TypeKind::Trait(ex_name)
        | TypeKind::Enum(ex_name) => {
            let expected_children = unwrap(&expected.children).clone();
            let got_children = unwrap_u(&got.children).clone();
            let typ = match &got.value {
                AstNode::ListLiteral | AstNode::SetLiteral => {
                    match &got.value {
                        AstNode::ListLiteral => if ex_name != "Vec" {
                            throw!("expected: `{}` but found: `Vec`", ex_name)
                        }
                        AstNode::SetLiteral => if ex_name != "HashSet" {
                            throw!("expected: `{}` but found: `HashSet`", ex_name)
                        }
                        _ => unreachable!()
                    }
                    if expected_children.len() != 1 { throw!() }
                    let exp_c = if let Type{
                        kind: TypeKind::GenericsMap,
                        children: Some(c)
                    } = &expected_children[0] {
                        if c.len() != 1 { throw!() }
                        if let Type{
                            kind: TypeKind::Generic(GenericType::WithVal(_)),
                            children: Some(c)
                        } = &c[0] {
                            if c.len() != 1 { throw!() }
                            &c[0]
                        } else { throw!() }
                    } else { throw!() };

                    for i in got_children {
                        box_if_needed(
                            exp_c.clone(), ast, i, info, vars, false
                        );
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                AstNode::DictLiteral => {
                    if ex_name != "HashMap" {
                        throw!("expected: `{}` but found: `HashMap`", ex_name.get_str())
                    }
                    if expected_children.len() != 1 { throw!() }
                    let exp_c: [&Type; 2] = if let Type{
                        kind: TypeKind::GenericsMap,
                        children: Some(c)
                    } = &expected_children[0] {
                        if c.len() != 2 { throw!() }
                        let mut iter = c.iter().map(|t| {
                            if let Type {
                                kind: TypeKind::Generic(GenericType::WithVal(_)),
                                children: Some(c)
                            } = &t {
                                if c.len() != 1 { throw!() }
                                &c[0]
                            } else { throw!() }
                        });
                        [iter.next().unwrap(), iter.next().unwrap()]
                    } else { throw!() };

                    for (i, c) in got_children.iter().enumerate() {
                        box_if_needed(
                            exp_c[i % 2].clone(), ast, *c, info, vars, false
                        );
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                AstNode::StructInit => {
                    if let TypeKind::Trait(_) = expected.kind {
                        let struct_name = unwrap_enum!(
                            &ast[unwrap_u(&got.children)[0]].value, AstNode::Identifier(x), x
                        );
                        let traits = get_traits!(TypName::Str(struct_name.clone()), info);
                        if traits.is_none() || traits.unwrap().iter().all(|x| ex_name != x.trt_name) {
                            throw!("the struct `{struct_name}` doesn't implement the trait `{}`", ex_name.get_str())
                        }
                        // let struct_pos = info.structs[&struct_name].pos;
                        // let traits = &ast[unwrap_u(&ast[struct_pos].children)[3]];
                        // if !unwrap_u(&traits.children).iter().any(|x|
                        //     unwrap_enum!(&ast[*x].value, AstNode::Identifier(trt), trt) == ex_name.get_str()
                        // ) {
                        //     throw!("the struct `{struct_name}` doesn't implement the trait `{}`", ex_name.get_str())
                        // }
                    } else {
                        let struct_name = unwrap_enum!(&ast[got_children[0]].value, AstNode::Identifier(s), s);
                        if struct_name != ex_name.get_str() {
                            throw!("expected: `{ex_name}` got: `{struct_name}`");
                        }

                        let mut generics_map = HashMap::new();
                        let generics = unwrap(&expected.children);
                        if !generics.is_empty() {
                            if let Type { kind: TypeKind::GenericsMap, children: Some(children) } = &generics[0] {
                                for child in children {
                                    if let Type {
                                        kind: TypeKind::Generic(GenericType::WithVal(name)),
                                        children: Some(c)
                                    } = child {
                                        generics_map.insert(name.clone(), c[0].clone());
                                    }
                                }
                            }
                        }

                        let struct_pos = info.structs[ex_name.get_str()].pos;
                        let args_def = &ast[unwrap_u(&ast[struct_pos].children)[1]];

                        let expected_args: Vec<_> = unwrap_u(&args_def.children).iter().map(|x|
                            if let Some(Type { kind: TypeKind::Generic(GenericType::NoVal(name)), .. }) = &ast[*x].typ {
                                generics_map[name].clone()
                            } else {
                                ast[*x].typ.clone().unwrap()
                            }
                        ).collect();
                        let got_args = unwrap_u(&ast[got_children[1]].children).clone();
                        if expected_args.len() != got_args.len() {
                            throw!("{} != {}", expected_children.len(), got_args.len())
                        }
                        for (got_c, exp_c) in got_args.iter().zip(expected_args) {
                            box_if_needed(
                                exp_c, ast, *got_c, info, vars, false
                            );
                        }
                    }
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::Identifier(idf) => {
                    let typ = get_from_stack(vars, idf).unwrap();
                    ast[typ].typ.clone().unwrap()
                }
                AstNode::Number(num) => {
                    // if !SPECIFIED_NUM_TYPE_RE.is_match(ex_name.get_str()) {
                    //      throw!("expected: `{}` but found an ambiguous number", ex_name.get_str())
                    // }
                    if let Some(t) = SPECIFIED_NUM_TYPE_RE.find(num) {
                        if t.as_str() != ex_name.get_str() {
                            throw!("expected: `{}` but found: `{}`", ex_name.get_str(), t.as_str())
                        }
                    } else {
                        let new_num = format!("{}{}", num, ex_name.get_str());
                        ast[pos].value = AstNode::Number(new_num);
                    }
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::Bool(_) => {
                    if ex_name.get_str() != "bool" {
                        throw!("expected: `{}` but found: `bool`", ex_name.get_str())
                    }
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::Char(_) => {
                    if ex_name.get_str() != "char" {
                        throw!("expected: `{}` but found: `char`", ex_name.get_str())
                    }
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::String { mutable, .. } => {
                    if *mutable && ex_name.get_str() == "String" {
                        //1 then its good
                    } else if !*mutable{
                        todo!()
                    } else {
                        throw!(
                            "expected: `{}` but found: `{}`",
                            ex_name.get_str(),
                            if *mutable { "String" } else { "&str" }
                        )
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                AstNode::Property => {
                    get_property_typ(got, ast, info, pos).unwrap()
                }
                AstNode::Index => {
                    find_index_typ(
                        ast, info, got.ref_children()[0], pos
                    ).unwrap()
                }
                AstNode::FunctionCall(_) => {
                    let func_name = unwrap_enum!(
                        &ast[unwrap_u(&got.children)[0]].value,
                        AstNode::Identifier(n), n
                    );
                    let args = &ast[unwrap_u(&got.children)[1]].ref_children();
                    let args: Vec<_> = args.iter().map(|x| ast[*x].typ.clone().unwrap()).collect();
                    get_function_return_type(
                        &info.funcs[func_name].output,
                        &info.funcs[func_name].input,
                        &if args.is_empty() { None } else { Some(args) },
                        ast, info
                    ).unwrap()
                    // println!("&&& {}", info.funcs[func_name].output.clone().unwrap());
                    // info.funcs[func_name].output.clone().unwrap()
                }
                AstNode::Parentheses => {
                    box_if_needed(
                        expected.clone(), ast, got_children[0], info, vars, false
                    ).unwrap_or_else(|e| throw!("{e}"))
                }
                AstNode::Operator(op) | AstNode::UnaryOp(op) => {
                    if let AstNode::UnaryOp(OperatorType::Dereference) = &got.value {
                        let new_expected = typ_with_child! {
                            ast[got_children[0]].typ.as_ref().unwrap().kind.clone(), //1 either pointer or mut pointer
                            typ_with_child! {
                                TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                                expected.clone()
                            }
                        };
                        box_if_needed(new_expected, ast, got_children[0], info, vars, false);
                    } else {
                        let new_expected = match op {
                            OperatorType::Div => todo!(),
                            OperatorType::OpEq(op) if OperatorType::Div == **op => todo!(),
                            _ => expected.clone()
                        };
                        for child in got_children {
                            box_if_needed(new_expected.clone(), ast, child, info, vars, false);
                        }
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                AstNode::Cast => got.typ.clone().unwrap(),
                _ => throw!("expected: {:?}, got.kind: {:?}", expected.kind, got.value)
            };
            if !can_soft_cast(&typ, &expected) {
                print_type(&Some(expected.clone()));
                print_type(&Some(typ.clone()));
                print_tree(ast, 0);
                throw!("expected: `{expected}` got: `{typ}`");
            }
        },
        TypeKind::Generic(_) => {}
        TypeKind::Pointer | TypeKind::MutPointer => {
            // let got_children = unwrap_u(&got.children).clone();
            let typ = match &got.value {
                AstNode::UnaryOp(OperatorType::Pointer | OperatorType::MutPointer) => {
                    let expected_inner = get_pointer_inner(&expected).clone();
                    box_if_needed(
                        expected_inner, ast, unwrap_u(&got.children)[0],
                        info, vars, false
                    );
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::Identifier(idf) => {
                    let typ = get_from_stack(vars, idf).unwrap();
                    ast[typ].typ.clone().unwrap()
                }
                AstNode::Property => {
                    get_property_typ(got, ast, info, pos).unwrap()
                }
                AstNode::Index => {
                    find_index_typ(
                        ast, info, got.ref_children()[0], pos
                    ).unwrap()
                }
                AstNode::FunctionCall(_) => {
                    let func_name = unwrap_enum!(
                        &ast[unwrap_u(&got.children)[0]].value,
                        AstNode::Identifier(n), n
                    );
                    let args = &ast[unwrap_u(&got.children)[1]].ref_children();
                    let args: Vec<_> = args.iter().map(|x| ast[*x].typ.clone().unwrap()).collect();
                    get_function_return_type(
                        &info.funcs[func_name].output,
                        &info.funcs[func_name].input,
                        &if args.is_empty() { None } else { Some(args) },
                        ast, info
                    ).unwrap()
                }
                AstNode::Parentheses => {
                    box_if_needed(
                        expected.clone(), ast, unwrap_u(&got.children)[0],
                        info, vars, false
                    ).unwrap_or_else(|e| throw!("{e}"))
                }
                AstNode::Operator(_) | AstNode::UnaryOp(_) => {
                    for child in unwrap_u(&got.children) {
                        box_if_needed(expected.clone(), ast, *child, info, vars, false);
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                _ => throw!("expected: {:?}, got.kind: {:?}", expected.kind, got.value)
            };
            if typ != expected {
                print_type(&Some(expected.clone()));
                print_type(&Some(typ.clone()));
                throw!("expected: `{expected}` got: `{typ}`");
            }
        }
        TypeKind::Tuple => {
            let typ = got.typ.as_ref().unwrap();
            if *typ != expected {
                throw!("expected: `{expected}` got: `{typ}`");
            }
        }
        _ => {
            print_tree(ast, 0);
            throw!("{:?}", expected)
        },
    }
    expected
}
*/
fn get_property_typ(
    got: &Ast, ast: &mut Vec<Ast>, info: &mut Info, pos: usize
) -> Option<Type> {
    let children = unwrap_u(&got.children);
    let left_kind = ast[children[0]].typ.clone().unwrap_or_else(|| {
        update_pos_from_tree_node(&ast[children[0]]);
        throw!("{:?}", ast[children[0]].value)
    });
    fn get_from_typ(
        left_kind: Type, ast: &mut Vec<Ast>, info: &mut Info, pos: usize, children: &Vec<usize>
    ) -> Option<Type> {
        match &left_kind.kind {
            TypeKind::Struct(struct_name) => {
                let struct_description = &ast[info.structs[&struct_name.to_string()].pos];
                match &ast[children[1]].value {
                    AstNode::Identifier(right) =>
                        get_property_idf_typ(ast, &left_kind, struct_description, right, true),
                    AstNode::FunctionCall(_) =>
                        get_property_method_typ(
                            ast, info, children, &left_kind, struct_name, true, pos
                        ).0,
                    _ => unreachable!(),
                }
            }
            TypeKind::Enum(enum_name) => {
                get_enum_property_typ(ast, info, children, enum_name)
                // todo!();
            }
            TypeKind::Trait(trait_name) => {
                let trait_description = &ast[info.traits[trait_name.get_str()].pos];
                match &ast[children[1]].value {
                    AstNode::Identifier(right) =>
                        get_property_idf_typ(
                            ast, &left_kind, trait_description, right, false
                        ),
                    AstNode::FunctionCall(_) =>
                        get_property_method_typ(
                            ast, info, children, &left_kind, trait_name, false, pos
                        ).0,
                    _ => unreachable!()
                }
            }
            TypeKind::Pointer | TypeKind::MutPointer => {
                let l = unwrap(&left_kind.children)[0].clone();
                get_from_typ(l, ast, info, pos, children)
            }
            _ => throw!("{:?}", left_kind)
        }
    }
    get_from_typ(left_kind, ast, info, pos, children)
}

pub fn can_soft_cast(typ: &Type, expected: &Type) -> bool {
    if typ == expected { return true }
    if let TypeKind::Enum(enum_name1) = &typ.kind {
        if matches!(&expected.kind, TypeKind::Enum(enum_name2) if enum_name1 == enum_name2) {
            print_type(&Some(typ.clone()));
            print_type(&Some(expected.clone()));
            // if unwrap(&typ.children).is_empty() && unwrap(&expected.children)[0].children.is_none() {
            //     return true
            // }
            let generic_map1 = &unwrap(&typ.children)[0];
            let generic_map2 = &unwrap(&expected.children)[0];
            for generic1 in unwrap(&generic_map1.children) {
                if let Some(generic2) = unwrap(&generic_map2.children).iter().find(
                    |x| x.kind == generic1.kind
                ) {
                    let generic_val1 = &unwrap(&generic1.children)[0];
                    let generic_val2 = &unwrap(&generic2.children)[0];
                    if !can_soft_cast(generic_val1, generic_val2) {
                        return false
                    }
                }
            }
            return true
        }
        return false
    }
    // TODO can probably cast more things, e.g. structs and stuff
    false
}
