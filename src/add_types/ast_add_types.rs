use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::mold_ast::{Info, VarTypes};
use crate::types::{BOOL_TYPE, CHAR_TYPE, FLOAT_TYPE, GenericType, UNKNOWN_TYPE, INT_TYPE, MUT_STR_TYPE, STR_TYPE, Type, TypeKind, TypName, unwrap, unwrap_u, print_type_b, print_type};
use lazy_static::lazy_static;
use pretty_print_tree::Color;
use regex::Regex;
use crate::{some_vec, unwrap_enum, typ_with_child};
use crate::add_types::generics::{apply_generics_to_method_call, get_function_return_type, map_generic_types};
use crate::add_types::polymorphism::check_for_boxes;
use crate::add_types::utils::{add_to_stack, find_function_in_struct, find_function_in_trait, get_from_stack};
use crate::construct_ast::tree_utils::{add_to_tree, insert_as_parent_of_prev, print_tree};
use crate::mold_tokens::OperatorType;
use crate::types::TypeKind::GenericsMap;

lazy_static! {
    pub static ref SPECIFIED_NUM_TYPE_RE: Regex = Regex::new(r"(?:[iu](?:8|16|32|64|128|size)|f32|f64)$").unwrap();
    pub static ref NUM_TYPES: HashSet<&'static str> = HashSet::from([
        "i8", "i16", "i32", "i64", "i128", "isize",
        "u8", "u16", "u32", "u64", "u128", "usize",
        "f32", "f64"
    ]);
}

// todo first save all uses of each var (in mold_ast) then check if any of them are used in a
//  function and if so assign their type
pub fn add_types(
    ast: &mut Vec<Ast>, pos: usize,
    vars: &mut VarTypes, info: &mut Info,
    parent_struct: &Option<HashMap<String, usize>>,
) {
    let children = ast[pos].children.clone().unwrap_or_default();
    match &ast[pos].value {
        AstNode::ListComprehension | AstNode::SetComprehension | AstNode::DictComprehension => {
            vars.push(HashMap::new());
            for r#loop in ast[children[1]].children.clone().unwrap() {
                let colon_par = ast[r#loop].children.as_ref().unwrap()[0];
                add_types( //1 iter type
                    ast,
                    unwrap_u(&ast[colon_par].children)[1],
                    vars, info, parent_struct
                );
                add_types_to_for_vars_and_iters(ast, vars, info, &ast[r#loop].children.clone().unwrap());
            }
            if let Some(condition) = &ast[children[2]].children {
                add_types(ast, condition[0], vars, info, parent_struct);
            }
            let stmt = ast[children[0]].children.as_ref().unwrap()[0];
            // panic!();
            add_types(ast, stmt, vars, info, parent_struct);
            if let AstNode::DictComprehension = &ast[pos].value {
                ast[pos].typ = Some(typ_with_child!{
                    TypeKind::Struct(TypName::Static("HashMap")),
                    Type{
                        kind: TypeKind::GenericsMap,
                        children: some_vec![
                            typ_with_child!{
                                TypeKind::Generic(GenericType::Of(String::from("K"))),
                                ast[ast[stmt].children.as_ref().unwrap()[0]].typ.clone().unwrap()

                            },
                            typ_with_child!{
                                TypeKind::Generic(GenericType::Of(String::from("V"))),
                                ast[ast[stmt].children.as_ref().unwrap()[1]].typ.clone().unwrap()
                            },
                        ]
                    }
                });
            } else {
                ast[pos].typ = Some(typ_with_child! {
                    TypeKind::Struct(TypName::Static(match &ast[pos].value {
                        AstNode::ListComprehension => "Vec",
                        AstNode::SetComprehension => "HashSet",
                        _ => unreachable!()
                    })),
                    typ_with_child!{
                        TypeKind::GenericsMap,
                        typ_with_child!{
                            TypeKind::Generic(GenericType::Of(String::from("T"))),
                            ast[stmt].typ.clone().unwrap()
                        }
                    }
                });
            }
            vars.pop();
        },
        AstNode::ForStatement => {
            //1 add_types to for-iter (not for-vars)
            add_types(
                ast,
                unwrap_u(&ast[children[0]].children)[1],
                vars, info, parent_struct
            );
            vars.push(HashMap::new());
            add_types_to_for_vars_and_iters(ast, vars, info, &children);
            add_types(ast, children[1], vars, info, parent_struct);
            vars.pop();
        }
        AstNode::Function(_) | AstNode::StaticFunction(_) | AstNode::WhileStatement | AstNode::IfStatement
        => {
            vars.push(HashMap::new());
            for child in children {
                add_types(ast, child, vars, info, parent_struct);
            }
            vars.pop();
        }
        AstNode::Module | AstNode::Body | AstNode::ReturnType
        | AstNode::GenericsDeclaration | AstNode::ColonParentheses
        | AstNode::Args | AstNode::NamedArg(_) => {
            for child in children {
                add_types(ast, child, vars, info, parent_struct);
            }
        }
        AstNode::Return => {
            add_types(ast, children[0], vars, info, parent_struct);
            let mut func_pos = ast[pos].parent.unwrap();
            while !matches!(ast[func_pos].value, AstNode::Function(_) | AstNode::StaticFunction(_)) {
                func_pos = ast[func_pos].parent.unwrap();
            }
            #[allow(unused_parens)]
            let func_name = unwrap_enum!(
                &ast[func_pos].value,
                (AstNode::Function(n) | AstNode::StaticFunction(n)),
                n
            );
            let return_type = if let Some(p) = ast[ast[func_pos].parent.unwrap()].parent {
                if let AstNode::Trait { .. } | AstNode::Struct(_) = &ast[p].value {
                    ast[unwrap_u(&ast[func_pos].children)[2]].typ.clone()
                } else { unreachable!() }
            } else {
                info.funcs[func_name].output.clone()
            };

            if let Some(return_typ) = return_type {
                check_for_boxes(return_typ, ast, children[0], info, vars);
            } else {
                //1 doesnt return anything
                // todo!()
            }
        }
        AstNode::ArgsDef => {
            for child in children {
                let name = unwrap_enum!(&ast[child].value, AstNode::Arg { name, .. }, name);
                add_to_stack(vars, name.clone(), child);
            }
        }
        AstNode::Identifier(name) => {
            if let Some(x) = get_from_stack(vars, name) {
                //1 optimize: things like this could probably be references instead of clone
                ast[pos].typ = ast[x].typ.clone();
                return;
            }
            if info.structs.contains_key(name) {
                ast[pos].typ = Some(Type {
                    kind: TypeKind::Struct(TypName::Str(name.clone())),
                    children: None,
                });
                return;
            }
            if info.funcs.contains_key(name) {
                ast[pos].typ = Some(Type {
                    kind: TypeKind::Function(name.clone()),
                    children: None,
                });
                return;
            }
            if info.enums.contains_key(name) {
                ast[pos].typ = Some(Type {
                    kind: TypeKind::Enum(TypName::Str(name.clone())),
                    children: None,
                });
                return;
            }
            panic!("used `{name}` before assignment") //todo // 5 unreachable!
        }
        AstNode::Assignment => {
            add_types(ast, children[1], vars, info, parent_struct);
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                let name = name.clone();
                if get_from_stack(vars, &name).is_none() {
                    add_to_stack(vars, name, children[0]);
                    ast[pos].value = AstNode::FirstAssignment;
                    let typ = check_for_boxes(
                        ast[pos].typ.clone().unwrap(), ast, children[1],
                        info, vars
                    );
                    ast[children[0]].typ = Some(typ)
                }
            }
            add_types(ast, children[0], vars, info, parent_struct);
        }
        AstNode::FirstAssignment => {
            add_types(ast, children[1], vars, info, parent_struct);
            if ast[pos].typ.is_some() {
                let typ = check_for_boxes(
                    ast[pos].typ.clone().unwrap(), ast, children[1],
                    info, vars
                );
                ast[children[0]].typ = Some(typ);
            } else {
                let typ = ast[children[1]].typ.clone();
                if typ.is_none() {
                    print_tree(ast, 0);
                    panic!("`{}` needs a type annotation", ast[children[0]].value)
                }
                ast[children[0]].typ = typ;
            }

            if let AstNode::Identifier(name) = &ast[children[0]].value {
                let name = name.clone();
                add_to_stack(vars, name, children[0]);
            } else {
                todo!()
            }
        }
        AstNode::FunctionCall(_) => {
            // let func_name = unwrap_enum!(&ast[children[0]].value, AstNode::Identifier(n), n.clone());
            //1 children[1] is the args
            add_types(ast, children[1], vars, info, parent_struct);

            let name = unwrap_enum!(
                &ast[children[0]].value, AstNode::Identifier(x), x.clone(), "function without identifier?"
            );
            let input = if let Some(fnc) = info.funcs.get(&name) {
                &fnc.input
            } else { panic!("unrecognized function `{name}`") };

            let mut args = unwrap_u(&ast[children[1]].children).clone();
            if let Some(expected_args) = input {
                put_args_in_vec(ast, &children, &mut args, expected_args);
                add_optional_args(ast, &children, &mut args, expected_args);
                if args.len() != expected_args.len() {
                    panic!(
                        "`{name}` expected `{}` arguments, but got `{}`",
                        expected_args.len(), args.len()
                    )
                }
            } else if !args.is_empty() {
                panic!("wasn't expecting any args")
            }
            let args: Option<Vec<Type>> =
                if args.is_empty() { None }
                else if input.is_some() && name != "print" { //3 this might be problematic (print)
                    let expected_inputs = input.as_ref().unwrap();
                    Some(expected_inputs.clone().iter().zip(args.iter()).map(
                        |(exp, got)|
                            check_for_boxes(exp.typ.clone(), ast, *got, info, vars)
                    ).collect())
                } else {
                    Some(args.iter().map(
                        |idx| ast[*idx].typ.clone().unwrap()
                    ).collect())
                };
            ast[pos].typ = get_function_return_type(
                &info.funcs[&name].output,
                &info.funcs[&name].input,
                &args
            );
        }
        AstNode::StructInit => {
            add_types(ast, children[1], vars, info, parent_struct);

            if let AstNode::Identifier(name) = &ast[children[0]].value.clone() {
                let mut generics_map = HashMap::new();

                let names_pos = add_to_tree(
                    pos, ast, Ast::new(AstNode::Args)
                );
                let args_def = &ast[unwrap_u(&ast[info.structs[name].pos].children)[1]];
                let args = args_def.children.clone();
                if let Some(args) = args {
                    let supplied_vec = ast[children[1]].children.clone().unwrap();
                    for (supplied, arg) in supplied_vec.iter().zip(args) {
                        map_generic_types(
                            ast[arg].typ.as_ref().unwrap(),
                            ast[*supplied].typ.as_ref().unwrap(),
                            &mut generics_map
                        );
                        add_to_tree(
                            names_pos, ast, Ast::new(ast[arg].value.clone())
                        );
                    }
                }
                ast[pos].typ = Some(typ_with_child!{
                    TypeKind::Struct(TypName::Str(name.clone())),
                    Type {
                        kind: TypeKind::GenericsMap,
                        children:  if generics_map.is_empty() { None } else {
                            Some(
                                info.structs[name].generics.as_ref().unwrap().iter().map(
                                    |name| typ_with_child! {
                                        TypeKind::Generic(GenericType::Of(name.clone())),
                                        generics_map.remove(name).unwrap()
                                    }
                                ).collect()
                            )
                        },
                    }
                });
            } else {
                todo!()
            }

        }
        AstNode::Property => {
            add_types(ast, children[0], vars, info, parent_struct);
            let left_kind = ast[children[0]].typ.clone().unwrap_or_else(||
                panic!("{:?}", ast[children[0]].value)
            );
            let mut lk = &left_kind;
            while let TypeKind::MutPointer | TypeKind::Pointer = &lk.kind {
                lk = &unwrap(&lk.children)[0];
            }
            match &lk.kind {
                TypeKind::Struct(struct_name) => {
                    let struct_description = if *struct_name == "Self" {
                        let mut struct_pos = ast[pos].parent.unwrap();

                        while !matches!(ast[struct_pos].value, AstNode::Struct(_)) {
                            struct_pos = ast[struct_pos].parent.unwrap();
                        }
                        &ast[struct_pos]
                    } else {
                        &ast[info.structs[&struct_name.to_string()].pos]
                    };
                    ast[pos].typ = match &ast[children[1]].value {
                        AstNode::Identifier(right) =>
                            get_property_idf_typ(ast, &left_kind, struct_description, right, true),
                        AstNode::FunctionCall(_) => {
                            //1 add types to the args
                            let arg_pos = unwrap_u(&ast[children[1]].children)[1];
                            add_types(ast, arg_pos, vars, info, parent_struct);

                            let (res, is_static) = get_property_method_typ(
                                ast, info, &children, &left_kind,
                                struct_name, true, pos
                            );
                            if is_static {
                                ast[children[1]].value = AstNode::FunctionCall(true);
                            }
                            res
                        }
                        _ => unreachable!()
                    };
                }
                TypeKind::Trait(trait_name) => {
                    let trait_description = &ast[info.traits[trait_name.get_str()].pos];
                    ast[pos].typ = match &ast[children[1]].value {
                        AstNode::Identifier(right) =>
                            get_property_idf_typ(
                                ast, &left_kind, trait_description,
                                right, false
                            ),
                        AstNode::FunctionCall(_) => {
                            //1 add types to the args //TODO IDK if this is needed here
                            let arg_pos = unwrap_u(&ast[children[1]].children)[1];
                            add_types(ast, arg_pos, vars, info, parent_struct);

                            get_property_method_typ(
                                ast, info, &children, &left_kind, trait_name, false, pos
                            ).0
                        }
                        _ => unreachable!()
                    };
                }
                TypeKind::Enum(enm_name) => {
                    let args = &ast[children[1]];
                    if let AstNode::FunctionCall(_) = &args.value {
                        let args = &ast[unwrap_u(&args.children)[1]];
                        for arg in unwrap_u(&args.children).clone() {
                            add_types(ast, arg, &mut vec![], info, &None);
                        }
                    }
                    ast[pos].typ = get_enum_property_typ(ast, info, &children, enm_name);
                }
                _ => panic!("{:?}", left_kind)
            }
        }
        AstNode::Number(num) => {
            ast[pos].typ = Some(
                typ_with_child! {
                    if let Some(cap) = SPECIFIED_NUM_TYPE_RE.captures(num) {
                        TypeKind::Struct(TypName::Str(String::from(cap.get(0).unwrap().as_str())))
                    } else if num.contains('.') {
                        FLOAT_TYPE
                    } else {
                        INT_TYPE
                    },
                    Type {
                        kind: TypeKind::GenericsMap,
                        children: None
                    }
                }
            );
        }
        AstNode::Char(_) => {
            ast[pos].typ = Some(typ_with_child! {
                CHAR_TYPE,
                Type {
                    kind: TypeKind::GenericsMap,
                    children: None
                }
            });
        }
        AstNode::String{ mutable, .. } => {
            ast[pos].typ = Some(typ_with_child! {
                if *mutable {
                    MUT_STR_TYPE
                } else {
                    STR_TYPE
                },
                Type {
                    kind: TypeKind::GenericsMap,
                    children: None
                }
            });
        }
        AstNode::Bool(_) => {
            ast[pos].typ = Some(typ_with_child! {
                BOOL_TYPE,
                Type {
                    kind: TypeKind::GenericsMap,
                    children: None
                }
            });
        }
        AstNode::Operator(_) => {
            add_types(ast, children[0], vars, info, parent_struct);
            add_types(ast, children[1], vars, info, parent_struct);
            let mut t1 = ast[children[0]].typ.as_ref().unwrap();
            let mut t2 = ast[children[1]].typ.as_ref().unwrap();
            let t1 = {
                while let TypeKind::Pointer | TypeKind::MutPointer = t1.kind {
                    t1 = &unwrap(&t1.children)[0];
                }
                t1
            };
            let t2 = {
                while let TypeKind::Pointer | TypeKind::MutPointer = t2.kind {
                    t2 = &unwrap(&t2.children)[0];
                }
                t2
            };
            let t1_name = unwrap_enum!(
                &t1.kind, TypeKind::Struct(t), t,
                "operator not valid for this type"
            );
            let t2_name = unwrap_enum!(
                &t2.kind, TypeKind::Struct(t), t,
                "operator not valid for this type"
            );
            if *t1_name != *t2_name && !(
                *t1_name == TypName::Static("String")
                && *t2_name  == TypName::Static("str")
            ) {
                panic!("`{}` not implemented for `{t1_name}` and `{t2_name}`",
                    unwrap_enum!(&ast[pos].value, AstNode::Operator(op), op)
                )
            }
            let op_typ = unwrap_enum!(&ast[pos].value, AstNode::Operator(o), o);
            ast[pos].typ = match op_typ {
                OperatorType::And | OperatorType::Or | OperatorType::Bigger
                | OperatorType::Smaller | OperatorType::IsEq | OperatorType::SEq
                | OperatorType::BEq | OperatorType::Not => Some(typ_with_child!{
                    BOOL_TYPE,
                    Type {
                        kind: GenericsMap,
                        children: None
                    }
                }),
                OperatorType::Div | OperatorType::DivEq => Some(typ_with_child!{
                    TypeKind::Struct(TypName::Static(
                        if matches!(&t1.kind, TypeKind::Struct(name) if name == "f64") { "f64" }
                        else { "f32" }
                    )),
                    Type {
                        kind: GenericsMap,
                        children: None
                    }
                }),
                OperatorType::FloorDiv | OperatorType::FloorDivEq => Some(typ_with_child!{
                    TypeKind::Struct(
                        if matches!(
                            &t1.kind,
                            TypeKind::Struct(name) if NUM_TYPES.contains(&name.get_str())
                        ) { unwrap_enum!(&t1.kind, TypeKind::Struct(name), name.clone()) }
                        else { panic!() }
                    ),
                    Type {
                        kind: GenericsMap,
                        children: None
                    }
                }),

                _ => Some(t1.clone())
            };

        }
        AstNode::UnaryOp(op) => {
            let op = op.clone();
            add_types(ast, children[0], vars, info, parent_struct);
            ast[pos].typ = match op {
                OperatorType::Pointer => Some(typ_with_child! {
                    TypeKind::Pointer,
                    ast[children[0]].typ.clone().unwrap()
                }),
                OperatorType::MutPointer => Some(typ_with_child! {
                    TypeKind::MutPointer,
                    ast[children[0]].typ.clone().unwrap()
                }),
                OperatorType::Dereference => {
                    fn dref(typ: &Type) -> Option<Type> {
                        match &typ.kind {
                            TypeKind::Pointer | TypeKind::MutPointer => {
                                if let Type {
                                    kind: TypeKind::Generic(GenericType::Of(_)), children
                                } = &unwrap(&typ.children)[0] {
                                    Some(children.as_ref().unwrap()[0].clone())
                                } else {
                                    Some(typ.children.as_ref().unwrap()[0].clone())
                                }
                            },
                            TypeKind::Generic(GenericType::Of(of)) => {
                                let children = unwrap_enum!(
                                    &typ.children, Some(c), c,
                                    "generic type `{of}` cannot be dereferenced"
                                );
                                dref(&children[0])
                            }
                            _ => panic!("type `{}` cannot be dereferenced", typ),
                        }
                    }
                    let res = dref(ast[children[0]].typ.as_ref().unwrap());
                    print_type_b(&res, Color::Black);
                    res
                }
                _ => ast[children[0]].typ.clone()
            }
        }
        AstNode::ForIter | AstNode::Parentheses => {
            add_types(ast, children[0], vars, info, parent_struct);
            ast[pos].typ = ast[children[0]].typ.clone();
        }
        AstNode::ListLiteral | AstNode::SetLiteral => {
            let inner_types: Vec<_> = children.iter().map(|x| {
                add_types(ast, *x, vars, info, parent_struct);
                *x
            }).collect(); //1 collecting so that all of them get a type
            ast[pos].typ = Some(typ_with_child!{
                TypeKind::Struct(TypName::Static(
                    if let AstNode::ListLiteral = &ast[pos].value { "Vec" } else { "HashSet" }
                )),
                typ_with_child!{
                    TypeKind::GenericsMap,
                    typ_with_child!{
                        TypeKind::Generic(GenericType::Of(String::from("T"))),
                        if let Some(x) = inner_types.first() {
                            ast[*x].typ.clone().unwrap()
                        } else { UNKNOWN_TYPE }
                    }
                }
            });
        }
        AstNode::DictLiteral => {
            let inner_types: Vec<_> = children.iter().map(|x| {
                add_types(ast, *x, vars, info, parent_struct);
                *x
            }).collect(); //1 collecting so that all of them get a type
            ast[pos].typ = Some(typ_with_child!{
                TypeKind::Struct(TypName::Static("HashMap")),
                Type{
                    kind: TypeKind::GenericsMap,
                    children: some_vec![
                        typ_with_child!{
                            TypeKind::Generic(GenericType::Of(String::from("K"))),
                            if let Some(x) = inner_types.first() {
                                ast[*x].typ.clone().unwrap()
                            } else { UNKNOWN_TYPE }
                        },
                        typ_with_child!{
                            TypeKind::Generic(GenericType::Of(String::from("V"))),
                            if let Some(x) = inner_types.get(1) {
                                ast[*x].typ.clone().unwrap()
                            } else { UNKNOWN_TYPE }
                        },
                    ]
                }
            });
        }
        AstNode::Index => {
            add_types(ast, children[0], vars, info, parent_struct);
            add_types(ast, children[1], vars, info, parent_struct);
            ast[pos].typ = find_index_typ(ast, info, &children, pos);
            if ast[pos].typ.is_none() {
                panic!();
            }
        }
        AstNode::Struct(name) => {
            let name = name.clone();
            vars.push(HashMap::new());
            add_types(ast, children[1], vars, info, parent_struct); //1 args
            vars.pop();
            let mut hm = HashMap::new();
            for child in unwrap_u(&ast[children[1]].children) {
                hm.insert(
                    unwrap_enum!(&ast[*child].value, AstNode::Identifier(name), name.clone()),
                    *child
                );
            }
            let parent_struct = Some(hm);
            info.structs.insert(String::from("Self"), info.structs[&name].clone());
            vars.push(HashMap::new());
            add_types(ast, children[2], vars, info, &parent_struct); //1 module
            vars.pop();
        }
        AstNode::ForVars | AstNode::Pass | AstNode::Continue | AstNode::Break | AstNode::Enum(_)
        | AstNode::Trait { .. } | AstNode::Traits | AstNode::Type(_) | AstNode::Types
        | AstNode::Arg { .. } | AstNode::Cast => {}
    }
}

fn add_types_to_for_vars_and_iters(
    ast: &mut Vec<Ast>, vars: &mut VarTypes, info: &mut Info, children: &Vec<usize>
) {
    let par = &ast[children[0]];
    let for_vars = &ast[unwrap_u(&par.children)[0]];
    let iter = &ast[unwrap_u(&par.children)[1]];
    if unwrap_u(&for_vars.children).len() > 1 {
        todo!()
    }
    let for_var_pos = unwrap_u(&for_vars.children)[0];
    let mut typ = get_into_iter_return_typ(ast, info, iter);
    if let Some(Type { kind: TypeKind::Generic(GenericType::Of(_)), children }) = &typ {
        println!("THIS ISNT USELESS");
        typ = Some(children.as_ref().unwrap()[0].clone());
    }
    ast[for_var_pos].typ = typ;
    add_to_stack(
        vars,
        unwrap_enum!(&ast[for_var_pos].value, AstNode::Identifier(n), n.clone()),
        for_var_pos
    );
}

fn add_optional_args( //3 not optimized // todo can't use name for positional args
    ast: &mut Vec<Ast>, children: &[usize], args: &mut Vec<usize>, expected_args: &[Param]
) {
    let amount_of_pos_args = args.iter().take_while(|i|
        !matches!(&ast[**i].value, AstNode::NamedArg(_))
    ).count();
    let mut supplied_kws = HashMap::new();
    for i in args.iter().skip(amount_of_pos_args) {
        let name = unwrap_enum!(&ast[*i].value, AstNode::NamedArg(name), name);
        supplied_kws.insert(name.clone(), *i);
    }
    ast[children[1]].children.as_mut().unwrap().truncate(amount_of_pos_args);

    let ast_len = ast.len();
    let mut to_add= vec![];
    let mut amount_of_pushed = 0;
    for ex_arg in expected_args.iter().skip(amount_of_pos_args) {
        if let Some(pos) = supplied_kws.get(&ex_arg.name) {
            to_add.push(*pos);
        } else if ex_arg.pos != usize::MAX {
            add_to_tree(
                children[1], ast,
                ast[ex_arg.pos].clone()
            );
            ast[children[1]].children.as_mut().unwrap().pop();
            amount_of_pushed += 1;
            to_add.push(ast_len - 1 + amount_of_pushed);
        } else {
            panic!("not all positional args supplied")
        }
    }
    let args_children = ast[children[1]].children.as_mut().unwrap();
    for pos in to_add {
        args_children.push(pos);
    }
    *args = ast[children[1]].children.clone().unwrap();
}

fn put_args_in_vec(
    ast: &mut Vec<Ast>, children: &[usize], args: &mut Vec<usize>, expected_args: &[Param]
) {
    let args_kwargs_pos = expected_args.iter().enumerate()
        .find(|(_, par)| par.is_args || par.is_kwargs);
    if let Some((pos_in_args, _)) = args_kwargs_pos {
        let vec_pos = add_to_tree(
            children[1],
            ast,
            Ast {
                value: AstNode::ListLiteral,
                children: Some(
                    unwrap_u(&ast[children[1]].children)
                        .iter()
                        .skip(pos_in_args)
                        .take_while(|x| !matches!(ast[**x].value, AstNode::NamedArg(_))) //1 while isn't named
                        .cloned().collect()
                ),
                parent: None,
                typ: Some(expected_args[pos_in_args].typ.clone()),
                is_mut: false,
            }
        );
        let amount_of_arg_in_args = unwrap_u(&ast[vec_pos].children).len();
        let args_children = ast[children[1]].children.as_mut().unwrap();
        let removed: Vec<_> = args_children.drain(
            pos_in_args..pos_in_args + amount_of_arg_in_args
        ).collect();
        let vec_child = args_children.pop().unwrap();
        args_children.insert(pos_in_args, vec_child);
        *args = args_children.clone();
        for i in removed {
            ast[i].parent = Some(vec_pos);
        }
    }
}

pub fn get_enum_property_typ(
    ast: &[Ast], info: &Info, children: &[usize], enm_name: &TypName
) -> Option<Type> {
    let generics = &info.enums[enm_name.get_str()].generics;
    let len = if let Some(g) = generics {
        g.len()
    } else { 0 };
    if len == 0 {
        Some(Type {
            kind: TypeKind::Enum(enm_name.clone()),
            children: None
        })
    } else {
        let mut generics_map = HashMap::new();
        let args = &ast[children[1]];
        if let AstNode::FunctionCall(_) = &args.value {
            let args = &ast[unwrap_u(&args.children)[1]];
            let generics = generics.clone().unwrap();
            let zip = generics
                .iter()
                .zip(unwrap_u(&args.children).clone());
            for (generic, arg) in zip {
                let typ = ast[arg].typ.as_ref().unwrap();
                map_generic_types(&Type {
                    kind: TypeKind::Generic(GenericType::Of(generic.clone())),
                    children: None
                }, typ, &mut generics_map);
            }
        }

        Some(typ_with_child! {
            TypeKind::Enum(enm_name.clone()),
            Type {
                kind: TypeKind::GenericsMap,
                children: Some(
                    generics_map.iter().map(|(name, t)|
                        typ_with_child! {
                            TypeKind::Generic(GenericType::Of(name.clone())),
                            t.clone()
                        }
                    ).collect()
                )
            }
        })
    }
}

//3 A very hacky solution...
fn get_into_iter_return_typ(ast: &[Ast], info: &Info, iter: &Ast) -> Option<Type> {
    // todo this is how the for loop actually works
    /*
    match IntoIterator::into_iter(iter) {
        mut iter => loop {
            match Iterator::next(&mut iter) {
                None => break,
                Some(i) => {
                    // Body
                }
            }
        },
    }
     */

    fn match_kind(ast: &[Ast], info: &Info, typ: &Type) -> Option<Type> {
        match &typ.kind {
            TypeKind::Struct(struct_name) => {
                let strct_def = &ast[info.structs[struct_name.get_str()].pos];
                let strct_traits = &ast[unwrap_u(&strct_def.children)[3]];
                for trt_pos in unwrap_u(&strct_traits.children) {
                    let trt = &ast[*trt_pos];
                    if matches!(&trt.value, AstNode::Identifier(name) if name == "IntoIterator" || name == "Iterator") {
                        let typs = unwrap_u(&trt.children);
                        for i in typs {
                            if matches!(&ast[*i].value, AstNode::Type(t) if t == "Item") {
                                return apply_generics_to_method_call(
                                    &ast[*i].typ, typ
                                );
                            } else { panic!() }
                        }
                    }
                }
                panic!("`{struct_name}` doesn't implement `IntoIterator` or `Iterator`")
            },
            TypeKind::Trait(trait_name) => {
                if trait_name != "IntoIterator" && trait_name != "Iterator" {
                    panic!("expected `IntoIterator` or `Iterator` found `{trait_name}`")
                }
                let generic_map = &unwrap(&typ.children)[0];
                for generic in unwrap(&generic_map.children) {
                    if let TypeKind::InnerType(name) = &generic.kind {
                        if name == "Item" {
                            return apply_generics_to_method_call(
                                &Some(unwrap(&generic.children)[0].clone()),
                                typ
                            )
                        }
                    }
                }
                panic!("couldn't find `Item` type in `{trait_name}`")
            },
            TypeKind::Pointer | TypeKind::MutPointer => {
                let generic = &unwrap(&typ.children)[0];
                let inner_typ = &unwrap(&generic.children)[0];
                Some(typ_with_child! {
                    typ.kind.clone(),
                    match_kind(ast, info, inner_typ).unwrap()
                })
            }
            _ => panic!("kind: {:?}", typ.kind)
        }
    }
    match_kind(ast, info, iter.typ.as_ref().unwrap())
}

pub fn get_property_method_typ(
    ast: &[Ast], info: &Info, children: &[usize],
    left_kind: &Type, trait_name: &TypName, is_struct: bool, pos: usize
) -> (Option<Type>, bool) {
    let func_call = &ast[children[1]];
    let func_name = &ast[unwrap_u(&func_call.children)[0]].value;
    let func_name = if let AstNode::Identifier(name) = func_name { name } else { panic!() };
    let func_pos = if is_struct {
        find_function_in_struct(
            ast, info.structs, &trait_name.to_string(), func_name, pos
        )
    }  else {
        find_function_in_trait(
            ast, info.traits, &trait_name.to_string(), func_name
        )
    };
    let func_pos = func_pos.unwrap_or_else(||
        panic!("function `{func_name}` not found in struct `{trait_name}`")
    );
    let is_static = matches!(ast[func_pos].value, AstNode::StaticFunction(_));
    let func_children = unwrap_u(&ast[func_pos].children);

    let return_type = &ast[func_children[2]].typ;
    if is_static { //1 only for structs
        let arg_pos = unwrap_u(&ast[children[1]].children)[1];

        let input_arg_types = ast[func_children[1]].children.as_ref()
            .map(|c| c.iter().map(|x| {
                let (name, is_args, is_kwargs) = unwrap_enum!(
                    &ast[*x].value,
                    AstNode::Arg { name, is_arg, is_kwarg },
                    (name.clone(), *is_arg, *is_kwarg)
                );
                Param {
                    typ: ast[*x].typ.clone().unwrap(),
                    is_mut: ast[*x].is_mut,
                    name, is_args, is_kwargs,
                    pos: *x
                }
            }).collect());


        let expected_arg_types =
            ast[arg_pos].children.as_ref().map(|c| c.iter().map(|x|
                    ast[*x].typ.clone().unwrap()
                ).collect());

        (get_function_return_type(
            return_type, &input_arg_types, &expected_arg_types
        ), true)
    } else {
        (apply_generics_to_method_call(return_type, left_kind).or_else(|| return_type.clone()), false)
    }
}

pub fn get_property_idf_typ(
    ast: &[Ast], left_kind: &Type, struct_description: &Ast, right: &String, is_struct: bool
) -> Option<Type>{
    let args_def = &ast[unwrap_u(&struct_description.children)[usize::from(is_struct)]];
    for child_pos in unwrap_u(&args_def.children) {
        if let AstNode::Identifier(name) = &ast[*child_pos].value {
            if name == right {
                return apply_generics_to_method_call(
                    &ast[*child_pos].typ.clone(),
                    left_kind
                ).or_else(|| ast[*child_pos].typ.clone())
            }
        }
    }
    None
}

pub fn find_index_typ(ast: &[Ast], info: &Info, children: &[usize], pos: usize) -> Option<Type> {
    fn find_index_func(typ: &Option<Type>, ast: &[Ast], info: &Info, pos: usize) -> usize {
        match typ {
            Some(Type { kind: TypeKind::Struct(struct_name), .. }) =>
                find_function_in_struct(
                    ast, info.structs, struct_name.get_str(), "index", pos
                ).unwrap_or_else(|| panic!("Didn't find `index` function in `{}`", struct_name)),
            Some(Type { kind: TypeKind::Trait(trait_name), .. }) =>
                find_function_in_trait(
                    ast, info.traits, trait_name.get_str(), "index"
                ).unwrap_or_else(|| panic!("Didn't find `index` function in `{}`", trait_name)),
            Some(Type { kind: TypeKind::Pointer | TypeKind::MutPointer, children: chs, .. }) =>
                return find_index_func(&Some(unwrap(chs)[0].clone()), ast, info, pos), // todo pos here might not be correct in the case of &Self
            _ => panic!()
        }
    }
    let index_func = find_index_func(&ast[children[0]].typ, ast, info, pos);
    let typ = &ast[unwrap_u(&ast[index_func].children)[2]].typ;

    apply_generics_to_method_call(typ, ast[children[0]].typ.as_ref().unwrap())
}
