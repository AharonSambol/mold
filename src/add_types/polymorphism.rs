use std::collections::HashMap;
use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::{typ_with_child, unwrap_enum, some_vec};
use crate::add_types::ast_add_types::{find_index_typ, get_property_idf_typ, get_property_method_typ, SPECIFIED_NUM_TYPE_RE};
use crate::add_types::utils::get_from_stack;
use crate::construct_ast::get_functions_and_types::{FuncTypes, StructTypes, TraitTypes};
use crate::construct_ast::mold_ast::{Info, VarTypes};
use crate::construct_ast::tree_utils::{add_to_tree, print_tree};
use crate::mold_tokens::OperatorType;
use crate::types::{GenericType, print_type, print_type_b, Type, TypeKind, TypName, unwrap, unwrap_u};

// TODO also cast: `as Box<dyn P>`
fn add_box(ast: &mut Vec<Ast>, pos: usize) {
    let ast_len = ast.len();
    let inner_val = ast[pos].clone();
    let parent_pos = inner_val.parent.unwrap();
    let parent = unwrap_enum!(&mut ast[parent_pos].children);
    let pos_in_parent = parent.iter().enumerate().find(|(_, &x)| x == pos).unwrap().0;
    parent[pos_in_parent] = ast_len; //1 ast_len is the position the Property will be pushed into

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
            Type {
                kind: TypeKind::Generic(GenericType::Of(String::from("T"))),
                children: some_vec![inner_val.typ.clone().unwrap()]
            }
        }
    });
    for x in [AstNode::Identifier(String::from("Box")), AstNode::FunctionCall(true)] {
        add_to_tree(property, ast, Ast::new(x));
    }
    let func_call = ast.len() - 1;
    for x in [AstNode::Identifier(String::from("new")), AstNode::Args] {
        add_to_tree(func_call, ast, Ast::new(x));
    }
    let args_pos = ast.len() - 1;
    add_to_tree(args_pos, ast, inner_val);
}

pub fn check_for_boxes(
    expected: Type, ast: &mut Vec<Ast>, pos: usize,
    info: &Info, vars: &VarTypes
) -> Type {
    fn is_box_typ(typ: &Type) -> bool {
        match &typ.kind {
            TypeKind::Struct(bx) => bx == "Box",
            TypeKind::Trait(_) => true,
            _ => false
        }
    }
    fn supplied_box(
        got: &Ast, vars: &VarTypes, ast: &[Ast], info: &Info
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
                        ast, info, unwrap_u(&got.children)
                    ).unwrap()
                )
            }
            AstNode::FunctionCall(_) => {
                let func_name = unwrap_enum!(
                    &ast[unwrap_u(&got.children)[0]].value,
                    AstNode::Identifier(n), n
                );
                let return_type = unwrap_enum!(&info.funcs[func_name].output);
                is_box_typ(return_type)
            }
            AstNode::Property => {
                let typ = get_property_typ(got, ast, info);
                if let Some(t) = typ { is_box_typ(&t) } else { false }
            }
            _ => false
        }
    }

    let got = &ast[pos].clone();
    if is_box_typ(&expected) {
        if !supplied_box(got, vars, ast, info) {
            add_box(ast, pos);
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
        TypeKind::Struct(ex_name) | TypeKind::Trait(ex_name) => {
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
                            kind: TypeKind::Generic(GenericType::Of(_)),
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
                                kind: TypeKind::Generic(GenericType::Of(_)),
                                children: Some(c)
                            } = &t {
                                if c.len() != 1 { panic!() }
                                &c[0]
                            } else { panic!() }
                        });
                        [iter.next().unwrap(), iter.next().unwrap()]
                    } else { panic!() };

                    for i in got_children {
                        check_for_boxes(
                            exp_c[i % 2].clone(), ast, i, info, vars
                        );
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                AstNode::StructInit => {
                    if let TypeKind::Trait(_) = expected.kind {
                        let struct_name = unwrap_enum!(
                            &ast[unwrap_u(&got.children)[0]].value, AstNode::Identifier(x), x.clone()
                        );
                        let struct_pos = info.structs[&struct_name].pos;
                        let traits = &ast[unwrap_u(&ast[struct_pos].children)[3]];
                        if !unwrap_u(&traits.children).iter().any(|x|
                            unwrap_enum!(&ast[*x].value, AstNode::Identifier(trt), trt) == ex_name.get_str()
                        ) {
                            panic!("the struct `{struct_name}` doesn't implement the trait `{}`", ex_name.get_str())
                        }
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
                                        kind: TypeKind::Generic(GenericType::Of(name)),
                                        children: Some(c)
                                    } = child {
                                        generics_map.insert(name.clone(), c[0].clone());
                                    }
                                }
                            }
                        }
                        let struct_pos = info.structs[ex_name.get_str()].pos;
                        let args_def = &ast[unwrap_u(&ast[struct_pos].children)[1]];
                        let expected_args: Vec<Type> = unwrap_u(&args_def.children).iter().map(|x|
                            if let Some(Type { kind: TypeKind::Generic(GenericType::Of(name)), .. }) = &ast[*x].typ {
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
                    if !SPECIFIED_NUM_TYPE_RE.is_match(ex_name.get_str()) {
                        panic!("expected: `{}` but found a number", ex_name.get_str())
                    }
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
                    get_property_typ(got, ast, info).unwrap()
                }
                AstNode::Index => {
                    find_index_typ(
                        ast, info, unwrap_u(&got.children)
                    ).unwrap()
                }
                AstNode::FunctionCall(_) => {
                    let func_name = unwrap_enum!(
                        &ast[unwrap_u(&got.children)[0]].value,
                        AstNode::Identifier(n), n
                    );
                    unwrap_enum!(info.funcs[func_name].output.clone())

                }
                AstNode::Parentheses => {
                    check_for_boxes(expected.clone(), ast, got_children[0], info, vars)
                }
                AstNode::Operator(_) | AstNode::UnaryOp(_) => {
                    for child in got_children {
                        check_for_boxes(expected.clone(), ast, child, info, vars);
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
        },
        TypeKind::Generic(_) => {}
        TypeKind::Pointer | TypeKind::MutPointer => {
            let got_children = unwrap_u(&got.children).clone();
            let typ = match &got.value {
                AstNode::UnaryOp(OperatorType::Pointer | OperatorType::MutPointer) => {
                    let expected_children = unwrap(&expected.children).clone();
                    check_for_boxes(
                        expected_children[0].clone(), ast, got_children[0],
                        info, vars
                    );
                    expected.clone()  //1 got what expected, no need to panic
                }
                AstNode::Identifier(idf) => {
                    let typ = get_from_stack(vars, idf).unwrap();
                    ast[typ].typ.clone().unwrap()
                }
                AstNode::Property => {
                    get_property_typ(got, ast, info).unwrap()
                }
                AstNode::Index => {
                    find_index_typ(
                        ast, info, unwrap_u(&got.children)
                    ).unwrap()
                }
                AstNode::FunctionCall(_) => {
                    let func_name = unwrap_enum!(
                        &ast[unwrap_u(&got.children)[0]].value,
                        AstNode::Identifier(n), n
                    );
                    unwrap_enum!(info.funcs[func_name].output.clone())
                }
                AstNode::Parentheses => {
                    check_for_boxes(
                        expected.clone(), ast, got_children[0],
                        info, vars
                    )
                }
                AstNode::Operator(_) | AstNode::UnaryOp(_) => {
                    for child in got_children {
                        check_for_boxes(expected.clone(), ast, child, info, vars);
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
        _ => panic!("{:?}", expected),
    }
    expected
}


fn get_property_typ(got: &Ast, ast: &[Ast], info: &Info) -> Option<Type> {
    let children = unwrap_u(&got.children);
    let left_kind = ast[children[0]].typ.clone().unwrap_or_else(||
        panic!("{:?}", ast[children[0]].value)
    );
    let typ = match &left_kind.kind {
        TypeKind::Struct(struct_name) => {
            println!("s name: {}", struct_name);
            let struct_description = &ast[info.structs[&struct_name.to_string()].pos];
            match &ast[children[1]].value {
                AstNode::Identifier(right) =>
                    get_property_idf_typ(ast, &left_kind, struct_description, right, true),
                AstNode::FunctionCall(_) =>
                    get_property_method_typ(ast, info, children, &left_kind, struct_name, true).0,
                _ => unreachable!(),
            }
        }
        TypeKind::Trait(trait_name) => {
            let trait_description = &ast[info.traits[trait_name.get_str()].pos];
            match &ast[children[1]].value {
                AstNode::Identifier(right) =>
                    get_property_idf_typ(ast, &left_kind, trait_description, right, false),
                AstNode::FunctionCall(_) =>
                    get_property_method_typ(ast, info, children, &left_kind, trait_name, false).0,
                _ => unreachable!()
            }
        }
        _ => panic!("{:?}", left_kind)
    };
    typ
}
