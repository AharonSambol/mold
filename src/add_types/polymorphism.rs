use std::collections::HashMap;
use pretty_print_tree::Color;
use crate::construct_ast::ast_structure::{Ast, AstNode, join};
use crate::{typ_with_child, unwrap_enum, some_vec, EMPTY_STR, OneOfEnums, OneOfEnumTypes, IMPL_TRAITS, ImplTraitsKey, get_traits};
use crate::add_types::ast_add_types::{find_index_typ, get_enum_property_typ, get_inner_type, get_property_idf_typ, get_property_method_typ, SPECIFIED_NUM_TYPE_RE};
use crate::add_types::generics::{apply_generics_from_base, get_function_return_type};
use crate::add_types::utils::{get_from_stack, get_pointer_inner};
use crate::construct_ast::mold_ast::{Info, VarTypes};
use crate::construct_ast::tree_utils::{add_to_tree, insert_as_parent_of, insert_as_parent_of_prev, print_tree};
use crate::mold_tokens::OperatorType;
use crate::types::{GenericType, implements_trait, print_type, print_type_b, Type, TypeKind, TypName, unwrap, unwrap_u};

// TODO also cast: `as Box<dyn P>`
fn add_box(ast: &mut Vec<Ast>, pos: usize) -> usize { // todo i think this leaves ast[pos] without anything pointing at it
    let ast_len = ast.len();
    let inner_val = ast[pos].clone();
    let parent_pos = inner_val.parent.unwrap();
    let parent_children = ast[parent_pos].children.as_mut().unwrap();
    *parent_children.iter_mut().find(|x| **x == pos).unwrap() = ast_len; //1 ast_len is the position the Property will be pushed into

    ast.push(Ast {
        value: AstNode::Property,
        children: None,
        parent: Some(parent_pos),
        typ: None,
        is_mut: false,
    });
    let property = ast.len() - 1;
    ast[property].typ = Some(typ_with_child! {
        TypeKind::Struct(TypName::Static("Box")),
        typ_with_child! {
            TypeKind::GenericsMap,
            typ_with_child! {
                TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                inner_val.typ.clone().unwrap()
            }
        }
    });
    add_to_tree(
        property, ast, Ast::new(AstNode::Identifier(String::from("Box")))
    );
    let func_call = add_to_tree(
        property, ast, Ast::new(AstNode::FunctionCall(true))
    );
    add_to_tree(
        func_call, ast, Ast::new(AstNode::Identifier(String::from("new")))
    );
    let args_pos = add_to_tree(func_call, ast, Ast::new(AstNode::Args));
    add_to_tree(args_pos, ast, inner_val);
    property
}

pub fn make_enums(typ: &Type, enums: &mut OneOfEnums) {
    let types = unwrap(&typ.children);
    let mut generics = vec![];
    types.iter().for_each(|x| if let Some(ch) = &x.children {
        if let TypeKind::GenericsMap = &ch[0].kind {
            for mut generic in unwrap(&ch[0].children) {
                if let TypeKind::InnerType(_) = &generic.kind {
                    generic = &generic.children.as_ref().unwrap()[0];
                }
                if let TypeKind::Generic(GenericType::NoVal(name)) = &generic.kind {
                    if !generics.contains(name) {
                        generics.push(name.clone());
                    }
                }
            }
        }
    });

    let generics = if generics.is_empty() {
        EMPTY_STR
    } else {
        format!("<{}>", join(generics.iter(), ","))
    };
    let enm_name = escape_typ_chars(&typ.to_string());
    enums.entry(enm_name.clone()).or_insert_with(|| {
        OneOfEnumTypes {
            generics,
            options: types.clone(),
            traits: HashMap::new(),
        }
        // let elems = types
        //     .iter()
        //     .map(|x| format!("_{}({x})", escape_typ_chars(&x.to_string())));
        // // let mut impls = vec![];
        // for typ in types {
        //
        // }
        // let res = format!(
        //     "pub enum {enm_name} {generics} {{ {} }} {}",
        //     join(elems, ","),
        //     todo!() //1 impls
        // );
        // res
    });
}

#[inline]
pub fn escape_typ_chars(st: &str) -> String {
    st
        .replace("::<", "_of_")
        .replace('>', "_endof_")
        .replace("Box<dyn ", "_boxof_")
        .replace('=', "_eq_")
}

fn add_one_of_enum(
    ast: &mut Vec<Ast>, pos: usize, enum_name: &str, enum_option: &str, info: &Info
) -> usize { // todo i think this leaves ast[pos] without anything pointing at it
    let ast_len = ast.len();
    let inner_val = ast[pos].clone();
    let parent_pos = inner_val.parent.unwrap();
    let parent_children = ast[parent_pos].children.as_mut().unwrap();
    *parent_children.iter_mut().find(|x| **x == pos).unwrap() = ast_len; //1 ast_len is the position the Property will be pushed into

    ast.push(Ast {
        value: AstNode::Property,
        children: None,
        parent: Some(parent_pos),
        typ: Some(typ_with_child! {
            TypeKind::Enum(TypName::Str(String::from(enum_name))),
            Type { kind: TypeKind::GenericsMap, children: None }
        }),
        is_mut: false,
    });
    let property = ast.len() - 1;
    for x in [AstNode::Identifier(String::from(enum_name)), AstNode::FunctionCall(true)] {
        add_to_tree(property, ast, Ast::new(x));
    }
    let func_call = ast.len() - 1;
    for x in [AstNode::Identifier(String::from(enum_option)), AstNode::Args] {
        add_to_tree(func_call, ast, Ast::new(x));
    }
    let args_pos = ast.len() - 1;
    add_to_tree(args_pos, ast, inner_val);
    property
}

// 5 *** polymorphism doesn't work for `OneOf` types ***
pub fn check_for_boxes(
    expected: Type, ast: &mut Vec<Ast>, pos: usize, info: &mut Info, vars: &VarTypes
) -> Type {
    #[inline] fn is_box_typ(typ: &Type) -> bool {
        match &typ.kind {
            TypeKind::Struct(bx) => bx == "Box",
            TypeKind::Trait(_) => true,
            _ => false
        }
    }
    #[inline] fn supplied_box(
        got: &Ast, vars: &VarTypes, ast: &[Ast], info: &Info, pos: usize
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
                        ast, info, got.children.as_ref().unwrap()[0], pos
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
        return check_for_boxes(expected, ast, unwrap_u(&got.children)[0], info, vars);
    }
    if let TypeKind::OneOf = expected.kind {
        for typ in unwrap(&expected.children) {
            if let Some(t) = &got.typ {
                if t == typ {
                    add_one_of_enum(
                        ast, pos, &expected.to_string(),
                        &format!("_{}", escape_typ_chars(&typ.to_string())), info
                    );
                    return got.typ.clone().unwrap()
                }
                if matches!(
                    &typ.kind, TypeKind::Trait(trt) if implements_trait(t, trt.get_str(), ast, info)
                ) {
                    // print_tree(ast, pos);
                    let new_pos = add_box(ast, pos);
                    add_one_of_enum(
                        ast, new_pos, &expected.to_string(),
                        &format!("_{}", escape_typ_chars(&typ.to_string())), info
                    );
                    return expected
                }
            }
        }
        panic!(
            "expected: `{}` but found `{}`",
            join(unwrap(&expected.children).iter().map(|x| x.to_string()), "` or `"),
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
        if let TypeKind::Trait(expected_trait) = &expected.kind {
            if implements_trait(got.typ.as_ref().unwrap(), expected_trait.get_str(), ast, info) {
                add_box(ast, pos);
                return expected
            }
        }
        panic!("can't cast `{}` to `{expected}`", got.typ.as_ref().unwrap())
    }
    if is_box_typ(&expected) {
        let expected_trait =
            if let TypeKind::Trait(name) = &expected.kind { Some(name) } else { None };
        if !supplied_box(got, vars, ast, info, pos) {
            if let Some(expected_trait) = expected_trait {

                let mut got_typ = got.typ.as_ref().unwrap();
                if let TypeKind::Generic(GenericType::WithVal(_)) = &got_typ.kind {
                    got_typ = &got_typ.children.as_ref().unwrap()[0];
                }
                if implements_trait(got_typ, expected_trait.get_str(), ast, info) {
                    let res = typ_with_child! {
                        TypeKind::Trait(expected_trait.clone()),
                        Type {
                            kind: TypeKind::GenericsMap,
                            children: None,
                            // TODO
                            //  if let Some(types) = &info.traits[expected_trait].types {
                            //      Some(types.iter().map(|(name, typ)|
                            //          typ_with_child! {
                            //              TypeKind::InnerType(name.clone()),
                            //              typ.clone()
                            //          }
                            //      ).collect())
                            //  } else { None }
                        }
                    };
                    let pos = add_box(ast, pos);
                    ast[pos].typ = Some(res.clone());
                    return res
                // }

                // if let TypeKind::Struct(struct_name) = &got_typ.kind {
                //     let traits = get_traits!(struct_name, info);
                //     if let Some(traits) = traits {
                //         if let Some(trt) = traits.iter().find(
                //             |x| *expected_trait == x.trt_name
                //         ) {
                //             let res = typ_with_child! {
                //                 TypeKind::Trait(expected_trait.clone()),
                //                 Type {
                //                     kind: TypeKind::GenericsMap,
                //                     children: if let Some(types) = &trt.types {
                //                         Some(types.iter().map(|(name, typ)|
                //                             typ_with_child! {
                //                                 TypeKind::InnerType(name.clone()),
                //                                 typ.clone()
                //                             }
                //                         ).collect())
                //                     } else { None }
                //                 }
                //             };
                //             let pos = add_box(ast, pos);
                //             ast[pos].typ = Some(res.clone());
                //             return res
                //         }
                //     }
                    // let struct_def = &ast[info.structs[struct_name.get_str()].pos];
                    // // let traits = &ast[unwrap_u(&struct_def.children)[3]];
                    // for trt in unwrap_u(&traits.children) {
                    //     if matches!(&ast[*trt].value, AstNode::Identifier(name) if expected_trait == name) {
                    //         let types = unwrap_u(&ast[*trt].children)
                    //             .iter()
                    //             .map(|i| (
                    //                 unwrap_enum!(&ast[*i].value, AstNode::Type(name), name.clone()),
                    //                 apply_generics_from_base(&ast[*i].typ, got_typ)
                    //             ));
                    //         let res = typ_with_child! {
                    //             TypeKind::Trait(expected_trait.clone()),
                    //             Type {
                    //                 kind: TypeKind::GenericsMap,
                    //                 children: if types.len() == 0 { None } else {
                    //                     Some(types.map(|(name, typ)|
                    //                         typ_with_child! {
                    //                             TypeKind::InnerType(name),
                    //                             typ.unwrap()
                    //                         }
                    //                     ).collect())
                    //                 }
                    //             }
                    //         };
                    //         let pos = add_box(ast, pos);
                    //         ast[pos].typ = Some(res.clone());
                    //         return res
                    //     }
                    // }
                    // panic!("`{struct_name}` doesn't implement trait `{expected_trait}`")
                } else if expected_trait == "__str__" && matches!(&got_typ.kind, TypeKind::Pointer | TypeKind::MutPointer) {
                    if implements_trait(get_pointer_inner(got_typ), expected_trait.get_str(), ast, info) {
                        return expected
                    }
                    panic!("expected: `{expected_trait}` got: `{got_typ}`")
                } else {
                    print_type(&Some(expected.clone()));
                    print_type(&Some(got_typ.clone()));
                    todo!()
                }
            }
            // check_for_boxes(
            //     unwrap(&expected.children)[0].clone(), ast, pos, info, vars
            // );

            let pos = add_box(ast, pos);
            ast[pos].typ = Some(expected.clone());
            // TODO shouldn't it keep checking?
        } else {
            let TypeKind::Trait(_) = &expected.kind else {
                return check_for_boxes(
                    unwrap(&expected.children)[0].clone(), ast, pos, info, vars
                )
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
                            panic!("expected: `{}` but found: `Vec`", ex_name)
                        }
                        AstNode::SetLiteral => if ex_name != "HashSet" {
                            panic!("expected: `{}` but found: `HashSet`", ex_name)
                        }
                        _ => unreachable!()
                    }
                    if expected_children.len() != 1 { panic!() }
                    let exp_c = if let Type{
                        kind: TypeKind::GenericsMap,
                        children: Some(c)
                    } = &expected_children[0] {
                        if c.len() != 1 { panic!() }
                        if let Type{
                            kind: TypeKind::Generic(GenericType::WithVal(_)),
                            children: Some(c)
                        } = &c[0] {
                            if c.len() != 1 { panic!() }
                            &c[0]
                        } else { panic!() }
                    } else { panic!() };

                    for i in got_children {
                        check_for_boxes(
                            exp_c.clone(), ast, i, info, vars
                        );
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                AstNode::DictLiteral => {
                    if ex_name != "HashMap" {
                        panic!("expected: `{}` but found: `HashMap`", ex_name.get_str())
                    }
                    if expected_children.len() != 1 { panic!() }
                    let exp_c: [&Type; 2] = if let Type{
                        kind: TypeKind::GenericsMap,
                        children: Some(c)
                    } = &expected_children[0] {
                        if c.len() != 2 { panic!() }
                        let mut iter = c.iter().map(|t| {
                            if let Type {
                                kind: TypeKind::Generic(GenericType::WithVal(_)),
                                children: Some(c)
                            } = &t {
                                if c.len() != 1 { panic!() }
                                &c[0]
                            } else { panic!() }
                        });
                        [iter.next().unwrap(), iter.next().unwrap()]
                    } else { panic!() };

                    for (i, c) in got_children.iter().enumerate() {
                        check_for_boxes(
                            exp_c[i % 2].clone(), ast, *c, info, vars
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
                            panic!("the struct `{struct_name}` doesn't implement the trait `{}`", ex_name.get_str())
                        }
                        // let struct_pos = info.structs[&struct_name].pos;
                        // let traits = &ast[unwrap_u(&ast[struct_pos].children)[3]];
                        // if !unwrap_u(&traits.children).iter().any(|x|
                        //     unwrap_enum!(&ast[*x].value, AstNode::Identifier(trt), trt) == ex_name.get_str()
                        // ) {
                        //     panic!("the struct `{struct_name}` doesn't implement the trait `{}`", ex_name.get_str())
                        // }
                    } else {
                        let struct_name = unwrap_enum!(&ast[got_children[0]].value, AstNode::Identifier(s), s);
                        if struct_name != ex_name.get_str() {
                            panic!("expected: `{ex_name}` got: `{struct_name}`");
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
                            panic!("{} != {}", expected_children.len(), got_args.len())
                        }
                        for (got_c, exp_c) in got_args.iter().zip(expected_args) {
                            check_for_boxes(
                                exp_c, ast, *got_c, info, vars
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
                    //      panic!("expected: `{}` but found an ambiguous number", ex_name.get_str())
                    // }
                    if let Some(t) = SPECIFIED_NUM_TYPE_RE.find(num) {
                        if t.as_str() != ex_name.get_str() {
                            panic!("expected: `{}` but found: `{}`", ex_name.get_str(), t.as_str())
                        }
                    } else {
                        let new_num = format!("{}{}", num, ex_name.get_str());
                        ast[pos].value = AstNode::Number(new_num);
                    }
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::Bool(_) => {
                    if ex_name.get_str() != "bool" {
                        panic!("expected: `{}` but found: `bool`", ex_name.get_str())
                    }
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::Char(_) => {
                    if ex_name.get_str() != "char" {
                        panic!("expected: `{}` but found: `char`", ex_name.get_str())
                    }
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::String { mutable, .. } => {
                    if *mutable && ex_name.get_str() == "String" {
                        //1 then its good
                    } else if !*mutable{
                        todo!()
                    } else {
                        panic!(
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
                        ast, info, got.children.as_ref().unwrap()[0], pos
                    ).unwrap()
                }
                AstNode::FunctionCall(_) => {
                    let func_name = unwrap_enum!(
                        &ast[unwrap_u(&got.children)[0]].value,
                        AstNode::Identifier(n), n
                    );
                    let args = &ast[unwrap_u(&got.children)[1]].children.as_ref().unwrap();
                    let args: Vec<_> = args.iter().map(|x| ast[*x].typ.clone().unwrap()).collect();
                    get_function_return_type(
                        &info.funcs[func_name].output,
                        &info.funcs[func_name].input,
                        &if args.is_empty() { None } else { Some(args) }
                    ).unwrap()
                    // println!("&&& {}", info.funcs[func_name].output.clone().unwrap());
                    // info.funcs[func_name].output.clone().unwrap()
                }
                AstNode::Parentheses => {
                    check_for_boxes(expected.clone(), ast, got_children[0], info, vars)
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
                        check_for_boxes(new_expected, ast, got_children[0], info, vars);
                    } else {
                        let new_expected = match op {
                            OperatorType::Div => todo!(),
                            OperatorType::OpEq(op) if OperatorType::Div == **op => todo!(),
                            _ => expected.clone()
                        };
                        for child in got_children {
                            check_for_boxes(new_expected.clone(), ast, child, info, vars);
                        }
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                AstNode::Cast => got.typ.clone().unwrap(),
                _ => panic!("expected: {:?}, got.kind: {:?}", expected.kind, got.value)
            };
            if !can_soft_cast(&typ, &expected) {
                print_type(&Some(expected.clone()));
                print_type(&Some(typ.clone()));
                print_tree(ast, 0);
                panic!("expected: `{expected}` got: `{typ}`");
            }
        },
        TypeKind::Generic(_) => {}
        TypeKind::Pointer | TypeKind::MutPointer => {
            // let got_children = unwrap_u(&got.children).clone();
            let typ = match &got.value {
                AstNode::UnaryOp(OperatorType::Pointer | OperatorType::MutPointer) => {
                    let expected_children = unwrap(&expected.children).clone();
                    check_for_boxes(
                        expected_children[0].clone(), ast, unwrap_u(&got.children)[0],
                        info, vars
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
                        ast, info, got.children.as_ref().unwrap()[0], pos
                    ).unwrap()
                }
                AstNode::FunctionCall(_) => {
                    let func_name = unwrap_enum!(
                        &ast[unwrap_u(&got.children)[0]].value,
                        AstNode::Identifier(n), n
                    );
                    let args = &ast[unwrap_u(&got.children)[1]].children.as_ref().unwrap();
                    let args: Vec<_> = args.iter().map(|x| ast[*x].typ.clone().unwrap()).collect();
                    get_function_return_type(
                        &info.funcs[func_name].output,
                        &info.funcs[func_name].input,
                        &if args.is_empty() { None } else { Some(args) }
                    ).unwrap()
                }
                AstNode::Parentheses => {
                    check_for_boxes(
                        expected.clone(), ast, unwrap_u(&got.children)[0],
                        info, vars
                    )
                }
                AstNode::Operator(_) | AstNode::UnaryOp(_) => {
                    for child in unwrap_u(&got.children) {
                        check_for_boxes(expected.clone(), ast, *child, info, vars);
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                _ => panic!("expected: {:?}, got.kind: {:?}", expected.kind, got.value)
            };
            if typ != expected {
                print_type(&Some(expected.clone()));
                print_type(&Some(typ.clone()));
                panic!("expected: `{expected}` got: `{typ}`");
            }
        }
        _ => {
            print_tree(ast, 0);

            panic!("{:?}", expected)
        },
    }
    expected
}

fn get_property_typ(
    got: &Ast, ast: &[Ast], info: &Info, pos: usize
) -> Option<Type> {
    let children = unwrap_u(&got.children);
    let left_kind = ast[children[0]].typ.clone().unwrap_or_else(||
        panic!("{:?}", ast[children[0]].value)
    );
    fn get_from_typ(
        left_kind: Type, ast: &[Ast], info: &Info, pos: usize, children: &Vec<usize>
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
            _ => panic!("{:?}", left_kind)
        }
    }
    get_from_typ(left_kind, ast, info, pos, children)
}

fn can_soft_cast(typ: &Type, expected: &Type) -> bool {
    // let typ = if let TypeKind::Generic(GenericType::Of(_)) = &typ.kind {
    //     &unwrap(&typ.children)[0]
    // } else { typ };
    if typ == expected { return true }
    if let TypeKind::Enum(enum_name1) = &typ.kind {
        if matches!(&expected.kind, TypeKind::Enum(enum_name2) if enum_name1 == enum_name2) {
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
