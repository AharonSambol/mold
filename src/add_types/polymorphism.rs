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
fn add_box(ast: &mut Vec<Ast>, pos: usize) {
    // let ast_len = ast.len();
    let parent_pos = ast[pos].parent.unwrap();

    // let parent_children = ast[parent_pos].children.as_mut().unwrap();
    // *parent_children.iter_mut().find(|x| **x == pos).unwrap() = ast_len; //1 ast_len is the position the Property will be pushed into

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
    ast.swap(property, pos);

    let property = pos;
    let pos = ast.len() - 1;

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
}

pub fn make_enums(typ: &Type, enums: &mut OneOfEnums) {
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

#[inline(always)] pub fn box_if_needed(
    expected: Type, ast: &mut Vec<Ast>, pos: usize, info: &mut Info
) -> Type {
    let got_typ = ast[pos].typ.clone().unwrap();
    update_pos_from_tree_node(&ast[pos]);
    check_if_need_box(expected, &got_typ, ast, pos, info, true, Mutate::Box)
        .unwrap_or_else(|e| throw!("{}", e))
}

#[inline(always)] pub fn box_no_side_effects(
    expected: Type, got_typ: &Type, ast: &mut Vec<Ast>, info: &mut Info, pos: usize
) -> Type {
    try_box_no_side_effects(expected, got_typ, ast, info, pos).unwrap_or_else(|e| throw!("{}", e))
}

#[inline(always)] pub fn try_box_no_side_effects(
    expected: Type, got_typ: &Type, ast: &mut Vec<Ast>, info: &mut Info, pos: usize
) -> Result<Type, String> {
    check_if_need_box(expected, got_typ, ast, pos, info, true, Mutate::Dont)
}

#[inline(always)] pub fn matches_template(expected: Type, got: &Type, ast: &mut Vec<Ast>, info: &mut Info, pos: usize) -> bool {
    check_if_need_box(expected, got, ast, pos, info, true, Mutate::Dont).is_ok()
}

#[derive(Copy, Clone)]
enum Mutate {
    Dont,
    Box
}

fn check_if_need_box(
    expected: Type, got_typ: &Type, ast: &mut Vec<Ast>, pos: usize, info: &mut Info,
    is_outer_call: bool, mutate: Mutate
) -> Result<Type, String> {
    // println!("Exp:");
    // print_type(&Some(expected.clone()));
    // println!("Got:");
    // print_type(&Some(got_typ.clone()));
    #[track_caller]
    fn err(expected: &Type, got_typ: &Type) -> Result<Type, String> {
        let caller_location = std::panic::Location::caller();
        let caller_file = caller_location.file();
        let caller_line_number = caller_location.line();
        Err(format!("expected `{expected}` but got `{got_typ}`\n\
        \x1b[0m{caller_file}:{caller_line_number} [DBG INFO]")) // todo remove dbg info
    }

    if expected == *got_typ {
        return Ok(expected)
    }

    match &expected.kind {
        TypeKind::OneOf => {
            // TODO generics and associated types
            if let TypeKind::OneOf = &got_typ.kind {
                if expected.contains(got_typ) {
                    if let Mutate::Box = mutate {
                        match_to_convert_types(&expected, got_typ, ast, pos);
                    }
                    return Ok(expected)
                } else {
                    todo!()
                }
            }
            for typ in unwrap(&expected.children) {
                if check_if_need_box(typ.clone(), got_typ, ast, pos, info, is_outer_call, Mutate::Dont).is_ok() {
                    if let Mutate::Box = mutate {

                        check_if_need_box(typ.clone(), got_typ, ast, pos, info, is_outer_call, Mutate::Box).unwrap();

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
                            add_box(ast, pos);
                            add_one_of_enum(
                                ast, pos, &expected.to_string(),
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
            println!("expected: {expected}");
            if let Some(trt) = implements_trait(got_typ, &expected, ast, info) {
                if !matches!(&got_typ.kind, TypeKind::Trait(_)) {
                    if !is_outer_call {
                        update_pos_from_tree_node(&ast[pos]);
                        return err(&expected, got_typ)
                    }
                    if let Mutate::Box = mutate {
                        add_box(ast, pos);
                    }
                }
                Ok(trt)
            } else {
                update_pos_from_tree_node(&ast[pos]);
                err(&expected, got_typ)
            }
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
            } else {
                update_pos_from_tree_node(&ast[pos]);
                err(&expected, got_typ)
            };
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
                    update_pos_from_tree_node(&ast[pos]);
                    return err(&expected, got_typ)
                }
                if expected_struct_name == "Vec" || expected_struct_name == "HashSet" {
                    let res = compare_generic_and_a_typ_children(&expected, got_typ, err);
                    if res.is_ok() {
                        res
                    } else {
                        let is_literal = matches!(&ast[pos].value, AstNode::ListLiteral | AstNode::SetLiteral);
                        if !is_outer_call || !is_literal {
                            return res //1 which is err
                        }
                        let expected = expected
                            .children.as_ref().unwrap()[0] //1 generic map
                            .children.as_ref().unwrap()[0] //1 the generic
                            .children.as_ref().unwrap()[0]
                            .clone();//1 the value of the generic
                        let children = ast[pos].children.clone().unwrap_or_default();
                        if let TypeKind::OneOf = &expected.kind {
                            for child in &children {
                                let mut found_one = false;
                                for opt in expected.ref_children() {
                                    if check_if_need_box(
                                        opt.clone(),
                                        &ast[*child].typ.clone().unwrap(),
                                        ast, *child, info, true, Mutate::Dont
                                    ).is_ok() {
                                        found_one = true;
                                        break
                                    }
                                }
                                if !found_one {
                                    update_pos_from_tree_node(&ast[*child]);
                                    return err(&expected, ast[*child].typ.as_ref().unwrap())
                                }
                            }
                            if let Mutate::Box = mutate {
                                for child in children {
                                    check_if_need_box(
                                        expected.clone(),
                                        &ast[child].typ.clone().unwrap(),
                                        ast, child, info, true, Mutate::Box
                                    ).unwrap(); // can prob be unwrap_unsafe
                                }
                            }
                        } else {
                            for inner_pos in children {
                                check_if_need_box(
                                    expected.clone(),
                                    &ast[inner_pos].typ.clone().unwrap(),
                                    ast, inner_pos, info, true, mutate
                                )?;
                            }
                        }

                        Ok(typ_with_child! {
                            TypeKind::Struct(expected_struct_name.clone()),
                            typ_with_child! {
                                TypeKind::GenericsMap,
                                typ_with_child! {
                                    TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                                    expected
                                }
                            }
                        })
                    }
                } else {
                    compare_generic_and_a_typ_children(&expected, got_typ, err)
                }
            } else {
                update_pos_from_tree_node(&ast[pos]);
                err(&expected, got_typ)
            }

        }
        TypeKind::Enum(expected_enum_name) => {
            if let TypeKind::Enum(got_enum_name) = &got_typ.kind {
                if expected_enum_name != got_enum_name {
                    update_pos_from_tree_node(&ast[pos]);
                    return err(&expected, got_typ)
                }
                compare_generic_and_a_typ_children(&expected, got_typ, err)
            } else {
                update_pos_from_tree_node(&ast[pos]);
                err(&expected, got_typ)
            }
        }
        TypeKind::Tuple => { todo!() }
        TypeKind::VArgs => {
            let TypeKind::VArgs = got_typ.kind else {
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
                kind: TypeKind::VArgs,
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

fn compare_generic_and_a_typ_children<F: Fn(&Type, &Type) -> Result<Type, String>>(
    expected: &Type, got_typ: &Type, err: F
) -> Result<Type, String> {
    let expected_children = unwrap(&expected.ref_children()[0].children);
    let got_children = unwrap(&got_typ.ref_children()[0].children);

    let mut res = got_typ.clone();
    let mut empty = vec![];
    let res_children = res.children.as_mut().unwrap()[0]
        .children.as_mut().unwrap_or(&mut empty);

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
                    if exp.is_some() { return err(expected, got_typ) }
                } else { return err(expected, got_typ) }
            }
            TypeKind::Generic(GenericType::WithVal(name)) => {
                if let Some(exp) = expected_hm.get(&GOrT::Generic(name.clone())) {
                    if let Some(exp) = exp {
                        if *exp != got_arg.ref_children()[0] { return err(expected, got_typ) }
                        // if let TypeKind::Unknown = &got_arg.ref_children()[0].kind {
                        //     res_arg.children.as_mut().unwrap()[0] = exp.clone();
                        // } else if *exp != got_arg.ref_children()[0] { return err(expected, got_typ) }
                    }
                } else { return err(expected, got_typ) }
            }
            TypeKind::AssociatedType(name) => {
                let inner_typ = got_arg.children.as_ref()
                    .map(|children| children[0].clone());

                if let Some(exp) = expected_hm.get(&GOrT::AType(name.clone())) {
                    if matches!(exp, Some(_) if *exp != inner_typ) { return err(expected, got_typ) }
                } else { return err(expected, got_typ) }
            }
            _ => unreachable!("{:?}", got_arg),
        };
    }
    Ok(res)
}

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
