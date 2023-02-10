use std::collections::{HashMap, HashSet};
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::mold_ast::{Info, VarTypes};
use crate::types::{BOOL_TYPE, CHAR_TYPE, FLOAT_TYPE, GenericType, UNKNOWN_TYPE, INT_TYPE, MUT_STR_TYPE, STR_TYPE, Type, TypeKind, TypName, unwrap, unwrap_u, print_type_b, print_type};
use lazy_static::lazy_static;
use pretty_print_tree::Color;
use regex::Regex;
use crate::{some_vec, unwrap_enum, typ_with_child, IGNORE_FUNCS};
use crate::add_types::generics::{apply_generics_from_base, apply_map_to_generic_typ, get_function_return_type, map_generic_types};
use crate::add_types::polymorphism::check_for_boxes;
use crate::add_types::utils::{add_to_stack, find_function_in_struct, find_function_in_trait, get_from_stack, get_pointer_inner};
use crate::construct_ast::tree_utils::{add_to_tree, insert_as_parent_of_prev, print_tree};
use crate::mold_tokens::OperatorType;
use crate::to_rust::implements_trait;

lazy_static! {
    pub static ref SPECIFIED_NUM_TYPE_RE: Regex = Regex::new(r"(?:[iu](?:8|16|32|64|128|size)|f32|f64)$").unwrap();
    pub static ref NUM_TYPES: HashSet<&'static str> = HashSet::from([
        "i8", "i16", "i32", "i64", "i128", "isize",
        "u8", "u16", "u32", "u64", "u128", "usize",
        "f32", "f64"
    ]);
}

// TODO a pointer to a trait is technically useless and the prob is that I cant cast &v to &Box<v>
// todo first save all uses of each var (in mold_ast) then check if any of them are used in a
//  function and if so assign their type
pub fn add_types(
    ast: &mut Vec<Ast>, pos: usize, vars: &mut VarTypes, info: &mut Info,
    parent_struct: &Option<HashMap<String, usize>>,
) {
    let children = ast[pos].children.clone().unwrap_or_default();
    match &ast[pos].value {
        AstNode::ListComprehension | AstNode::SetComprehension | AstNode::DictComprehension => {
            ast[pos].typ = get_type_comprehension(
                ast, pos, vars, info, parent_struct, &children
            );
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
        AstNode::Function(_) | AstNode::StaticFunction(_) | AstNode::WhileStatement
        | AstNode::IfStatement => {
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
                //TODO doesnt return anything
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
                ast[pos].is_mut = ast[x].is_mut;
                return;
            }
            ast[pos].typ = Some(Type {
                kind: if info.structs.contains_key(name) {
                    TypeKind::Struct(TypName::Str(name.clone()))
                } else if info.funcs.contains_key(name) {
                    TypeKind::Function(name.clone())
                } else if info.enums.contains_key(name) {
                    TypeKind::Enum(TypName::Str(name.clone()))
                } else {
                    panic!("used `{name}` before assignment") //todo // 5 unreachable!
                },
                children: None,
            });
        }
        AstNode::Assignment => {
            add_types(ast, children[1], vars, info, parent_struct);
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if let Some(i) = get_from_stack(vars, name) {
                    let should_be = &ast[i].typ;
                    if ast[children[1]].typ != *should_be {
                        panic!(
                            "expected `{}` but found `{}`. variables can't change type, if you want to override use `:=`",
                            should_be.as_ref().unwrap(),
                            ast[children[1]].typ.as_ref().unwrap()
                        );
                    }
                } else { //1 first assignment
                    add_to_stack(vars, name.clone(), children[0]);
                    ast[pos].value = AstNode::FirstAssignment;
                    ast[children[0]].typ = Some(check_for_boxes(
                        ast[children[1]].typ.clone().unwrap(), ast, children[1],
                        info, vars
                    ));
                }
            }
            add_types(ast, children[0], vars, info, parent_struct);
        }
        AstNode::FirstAssignment => {
            add_types(ast, children[1], vars, info, parent_struct);
            if ast[pos].typ.is_some() {
                ast[children[0]].typ = Some(check_for_boxes(
                    ast[pos].typ.clone().unwrap(), ast, children[1],
                    info, vars
                ));
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
            } else { todo!() }
        }
        AstNode::OpAssignment(op) => {
            let op = op.clone();
            add_types(ast, children[0], vars, info, parent_struct);
            add_types(ast, children[1], vars, info, parent_struct);
            let t1 = &ast[children[0]].typ.as_ref().unwrap();
            let t2 = &ast[children[1]].typ.as_ref().unwrap();
            if t1 != t2 {
                panic!(
                    "Can't {}",
                    match op {
                        OperatorType::Plus => format!("add `{t2}` to `{t1}`"),
                        OperatorType::Minus => format!("subtract `{t2}` from `{t1}`"),
                        OperatorType::Mul => format!("multiply `{t1}` by `{t2}`"),
                        OperatorType::Div => format!("divide `{t1}` by `{t2}`"),
                        OperatorType::FloorDiv => format!("floor divide `{t1}` by `{t2}`"),
                        OperatorType::Pow => format!("raise `{t1}` to the `{t2}`"),
                        OperatorType::Mod => format!("modulo `{t1}` by `{t2}`"),
                        _ => unreachable!()
                    }
                )
            }
            ast[pos].typ = Some((*t1).clone());
        }
        AstNode::FunctionCall(_) => {
            add_type_func_call(ast, pos, vars, info, parent_struct, &children);
        }
        AstNode::StructInit => {
            add_type_struct_innit(ast, pos, vars, info, parent_struct, &children)
        }
        AstNode::Property => {
            add_type_property(ast, pos, vars, info, parent_struct, &children)
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
        AstNode::Operator(op) => {
            add_type_operator(ast, pos, vars, info, parent_struct, &children, op.clone());
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
                    #[inline] fn dref(typ: &Type) -> Option<Type> {
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
                    dref(ast[children[0]].typ.as_ref().unwrap())
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
            ast[pos].typ = find_index_typ(ast, info, children[0], pos);
            ast[pos].is_mut = ast[children[0]].is_mut; // todo if it doesnt need to be mut then maybe not?
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
                    unwrap_enum!(&ast[*child].value, AstNode::Arg{ name, .. }, name.clone()),
                    *child
                );
            }
            let parent_struct = Some(hm);
            info.structs.insert(String::from("Self"), info.structs[&name].clone());
            vars.push(HashMap::new());
            add_types(ast, children[2], vars, info, &parent_struct); //1 module
            vars.pop();
        }
        AstNode::Ternary => {
            add_types(ast, children[0], vars, info, parent_struct);
            add_types(ast, children[1], vars, info, parent_struct);
            add_types(ast, children[2], vars, info, parent_struct);

            if !matches!(
                &ast[children[1]].typ.as_ref().unwrap().kind,
                TypeKind::Struct(name) if name == "bool"
            ) {
                panic!("condition isn't bool") //todo truthy falsy
            }
            if ast[children[0]].typ != ast[children[2]].typ {
                panic!(
                    "`if` and `else` have incompatible types. expected `{}`, found `{}`",
                    ast[children[0]].typ.as_ref().unwrap(),
                    ast[children[2]].typ.as_ref().unwrap()
                )
            }
            ast[pos].typ = ast[children[0]].typ.clone();
        }
        AstNode::ForVars | AstNode::Pass | AstNode::Continue | AstNode::Break | AstNode::Enum(_)
        | AstNode::Trait { .. } | AstNode::Traits | AstNode::Type(_) | AstNode::Types
        | AstNode::Arg { .. } | AstNode::Cast => {}
    }
}

fn add_type_operator(
    ast: &mut Vec<Ast>, pos: usize, vars: &mut VarTypes, info: &mut Info,
    parent_struct: &Option<HashMap<String, usize>>, children: &[usize], op: OperatorType
) {
    add_types(ast, children[0], vars, info, parent_struct);
    add_types(ast, children[1], vars, info, parent_struct);
    let t1 = ast[children[0]].typ.as_ref().unwrap();
    let t2 = ast[children[1]].typ.as_ref().unwrap();
    let t1 = get_pointer_inner(t1);
    let t2 = get_pointer_inner(t2);
    let t1_name = unwrap_enum!(
        &t1.kind, TypeKind::Struct(t), t, "operator not valid for this type"
    );
    let t2_name = unwrap_enum!(
        &t2.kind, TypeKind::Struct(t), t, "operator not valid for this type"
    );
    if *t1_name != *t2_name
        && !(*t1_name == TypName::Static("String") && *t2_name == TypName::Static("str"))
        && !matches!(op, OperatorType::In | OperatorType::NotIn)
    {
        panic!("`{op}` not implemented for `{t1_name}` and `{t2_name}`")
    }
    ast[pos].typ = match op {
        OperatorType::And | OperatorType::Or | OperatorType::Bigger
        | OperatorType::Smaller | OperatorType::IsEq | OperatorType::SEq
        | OperatorType::BEq | OperatorType::NEq | OperatorType::Not | OperatorType::Is
        | OperatorType::In | OperatorType::NotIn | OperatorType::IsNot
        => Some(typ_with_child! {
            BOOL_TYPE,
            Type {
                kind: TypeKind::GenericsMap,
                children: None
            }
        }),
        _ if matches!(&op, OperatorType::Div)
            || matches!(&op, OperatorType::OpEq(op) if OperatorType::Div == **op)
        => Some(typ_with_child! {
            TypeKind::Struct(TypName::Static(
                if matches!(&t1.kind, TypeKind::Struct(name) if name == "f64") { "f64" }
                else { "f32" }
            )),
            Type {
                kind: TypeKind::GenericsMap,
                children: None
            }
        }),
        _ if matches!(&op, OperatorType::FloorDiv)
            || matches!(&op, OperatorType::OpEq(op) if OperatorType::FloorDiv == **op)
        => Some(typ_with_child! {
            TypeKind::Struct(
                if matches!(
                    &t1.kind,
                    TypeKind::Struct(name) if NUM_TYPES.contains(&name.get_str())
                ) { unwrap_enum!(&t1.kind, TypeKind::Struct(name), name.clone()) }
                else { panic!() }
            ),
            Type {
                kind: TypeKind::GenericsMap,
                children: None
            }
        }),
        _ => Some(t1.clone())
    };
}

fn add_type_property(
    ast: &mut Vec<Ast>, pos: usize, vars: &mut VarTypes, info: &mut Info,
    parent_struct: &Option<HashMap<String, usize>>, children: &[usize]
) {
    add_types(ast, children[0], vars, info, parent_struct);
    let left_kind = ast[children[0]].typ.clone().unwrap_or_else(||
        panic!("{:?}", ast[children[0]].value)
    );
    let left_kind = get_pointer_inner(&left_kind);
    match &left_kind.kind {
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
                    get_property_idf_typ(ast, left_kind, struct_description, right, true),
                AstNode::FunctionCall(_) => {
                    //1 add types to the args
                    let arg_pos = unwrap_u(&ast[children[1]].children)[1];
                    add_types(ast, arg_pos, vars, info, parent_struct);

                    let (res, is_static) = get_property_method_typ(
                        ast, info, children, left_kind,
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
                        ast, left_kind, trait_description,
                        right, false
                    ),
                AstNode::FunctionCall(_) => {
                    //1 add types to the args //TODO IDK if this is needed here
                    let arg_pos = unwrap_u(&ast[children[1]].children)[1];
                    add_types(ast, arg_pos, vars, info, parent_struct);

                    get_property_method_typ(
                        ast, info, children, left_kind, trait_name, false, pos
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
            ast[pos].typ = get_enum_property_typ(ast, info, children, enm_name);
        }
        _ => panic!("{:?}", left_kind)
    }
}

fn add_type_struct_innit(ast: &mut Vec<Ast>, pos: usize, vars: &mut VarTypes, info: &mut Info, parent_struct: &Option<HashMap<String, usize>>, children: &[usize]) {
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
        ast[pos].typ = Some(typ_with_child! {
            TypeKind::Struct(TypName::Str(name.clone())),
            Type {
                kind: TypeKind::GenericsMap,
                children: if generics_map.is_empty() { None } else {
                    Some(
                        info.structs[name].generics.as_ref().unwrap().iter().filter_map(
                            |name| {
                                if generics_map.contains_key(name) {
                                    Some(typ_with_child! {
                                        TypeKind::Generic(GenericType::Of(name.clone())),
                                        generics_map.remove(name).unwrap()
                                    })
                                } else { None }
                            }
                        ).collect()
                    )
                },
            }
        });
    } else { todo!() }
}

fn get_type_comprehension(
    ast: &mut Vec<Ast>, pos: usize, vars: &mut VarTypes, info: &mut Info,
    parent_struct: &Option<HashMap<String, usize>>, children: &[usize]
) -> Option<Type> {
    #[inline] fn add_loop_types(
        ast: &mut Vec<Ast>, vars: &mut VarTypes, info: &mut Info,
        parent_struct: &Option<HashMap<String, usize>>, children: &[usize]
    ) {
        for r#loop in ast[children[1]].children.clone().unwrap() {
            let colon_par = ast[r#loop].children.as_ref().unwrap()[0];
            add_types( //1 iter type
                       ast,
                       unwrap_u(&ast[colon_par].children)[1],
                       vars, info, parent_struct
            );
            let children = &ast[r#loop].children.clone().unwrap();
            add_types_to_for_vars_and_iters(ast, vars, info, children);
        }
    }

    vars.push(HashMap::new());

    add_loop_types(ast, vars, info, parent_struct, children);

    if let Some(condition) = &ast[children[2]].children {
        add_types(ast, condition[0], vars, info, parent_struct);
    }

    let stmt = ast[children[0]].children.as_ref().unwrap()[0];
    add_types(ast, stmt, vars, info, parent_struct);

    vars.pop();

    #[inline] fn get_generics(ast: &mut [Ast], pos: usize, stmt: usize) -> Vec<Type> {
        if let AstNode::DictComprehension = &ast[pos].value {
            let stmt_children = ast[stmt].children.as_ref().unwrap();
            ["K", "V"].iter().enumerate().map(|(i, name)|
                typ_with_child! {
                    TypeKind::Generic(GenericType::Of(String::from(*name))),
                    ast[stmt_children[i]].typ.clone().unwrap()
                }
            ).collect()
        } else {
            vec![typ_with_child! {
                TypeKind::Generic(GenericType::Of(String::from("T"))),
                ast[stmt].typ.clone().unwrap()
            }]
        }
    }
    Some(typ_with_child! {
        TypeKind::Struct(TypName::Static(match &ast[pos].value {
            AstNode::ListComprehension => "Vec",
            AstNode::SetComprehension => "HashSet",
            AstNode::DictComprehension => "HashMap",
            _ => unreachable!()
        })),
        Type {
            kind: TypeKind::GenericsMap,
            children: Some(get_generics(ast, pos, stmt))
        }
    })
}

fn add_type_func_call(
    ast: &mut Vec<Ast>, pos: usize, vars: &mut VarTypes, info: &mut Info,
    parent_struct: &Option<HashMap<String, usize>>, children: &[usize]
) {
    //1 children[1] is the args
    add_types(ast, children[1], vars, info, parent_struct);

    let name = unwrap_enum!(
        &ast[children[0]].value, AstNode::Identifier(x), x.clone(),
        "function without identifier?"
    );
    let input = if let Some(fnc) = info.funcs.get(&name) {
        &fnc.input
    } else { panic!("unrecognized function `{name}`") };

    #[inline] fn format_args(
        ast: &mut Vec<Ast>, children: &[usize], input: &Option<Vec<Param>>, name: &str
    ) -> Vec<usize> {
        let mut args = unwrap_u(&ast[children[1]].children).clone();
        if let Some(expected_args) = input {
            put_args_in_vec(ast, children, &mut args, expected_args);
            add_optional_args(ast, children, &mut args, expected_args);
            if args.len() != expected_args.len() {
                panic!(
                    "`{name}` expected `{}` arguments, but got `{}`",
                    expected_args.len(), args.len()
                )
            }
        } else if !args.is_empty() {
            panic!("wasn't expecting any args")
        }
        args
    }
    let args = format_args(ast, children, input, &name);

    #[inline] fn get_generics(input: &Option<Vec<Param>>, args: &[usize], ast: &mut [Ast]) -> HashMap<String, Type> {
        let mut generic_map = HashMap::new();
        if let Some(input) = input {
            for (exp, got) in input.iter().zip(args.iter()) {
                map_generic_types(&exp.typ, ast[*got].typ.as_ref().unwrap(), &mut generic_map);
            }
        }
        generic_map
    }
    let generic_map = get_generics(input, &args, ast);
    #[inline] fn check_that_is_castable(
        expected_inputs: &Vec<Param>, args: &[usize], info: &Info, ast: &[Ast], is_built_in: bool
    ) -> Option<Vec<Type>> {
        Some(expected_inputs.clone().iter().zip(args.iter()).map(
            |(exp, got)| {
                if !is_castable(
                    &exp.typ, ast[*got].typ.as_ref().unwrap(), ast, info, is_built_in
                ) {
                    panic!("expected `{}` got `{}`", exp.typ, *ast[*got].typ.as_ref().unwrap())
                }
                ast[*got].typ.clone().unwrap()
            }
        ).collect())
    }
    let expected_inputs = input.as_ref().unwrap();
    let args: Option<Vec<Type>> = {
        if args.is_empty() { None }
        else if unsafe { IGNORE_FUNCS.contains(name.as_str()) } {
            check_that_is_castable(
                expected_inputs, &args, info, ast, true
            )
        } else {
            Some(expected_inputs.clone().iter().zip(args.iter()).map(
                |(exp, got)|
                    check_for_boxes(exp.typ.clone(), ast, *got, info, vars)
            ).collect())
        }
    };
    let typ = get_function_return_type(
        &info.funcs[&name].output,
        &info.funcs[&name].input,
        &args
    );
    ast[pos].typ = typ.map(|typ| apply_map_to_generic_typ(&typ, &generic_map, true));
}

fn is_castable(exp: &Type, got: &Type, ast: &[Ast], info: &Info, is_built_in: bool) -> bool {
    match &exp.kind {
        TypeKind::Generic(GenericType::Of(_)) => true,
        TypeKind::OneOf => {
            exp.children.as_ref().unwrap().iter().any(|opt|
                is_castable(opt, got, ast, info, is_built_in)
            )
        }
        TypeKind::Trait(exp_trait) => {
            implements_trait(got, exp_trait.get_str(), ast, info)
        }
        _ if is_built_in && matches!(exp.kind, TypeKind::Pointer | TypeKind::MutPointer) => {
            if got.kind == exp.kind || matches!((&exp.kind, &got.kind), (TypeKind::Pointer, TypeKind::MutPointer)) {
                let got = &got.children.as_ref().unwrap()[0];
                let exp = &exp.children.as_ref().unwrap()[0];
                is_castable(exp, got, ast, info, is_built_in)
            } else {
                let exp = &exp.children.as_ref().unwrap()[0];
                is_castable(exp, got, ast, info, is_built_in)
            }
        }
        _ => exp == got
    }
}

fn add_types_to_for_vars_and_iters(
    ast: &mut [Ast], vars: &mut VarTypes, info: &mut Info, children: &[usize]
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
                                return apply_generics_from_base(
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
                            return apply_generics_from_base(
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
    let func_name = if func_name == "len" { "__len__" } else { func_name };
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
    if is_static || func_name == "__init__" { //1 only for structs
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
        let res = get_function_return_type(
            return_type, &input_arg_types, &expected_arg_types
        );
        (res, true)
    } else {
        let res = apply_generics_from_base(return_type, left_kind)
            .or_else(|| return_type.clone());
        (res, false)
    }
}

pub fn get_property_idf_typ(
    ast: &[Ast], left_kind: &Type, struct_description: &Ast, right: &String, is_struct: bool
) -> Option<Type>{
    let args_def = &ast[unwrap_u(&struct_description.children)[usize::from(is_struct)]];
    for child_pos in unwrap_u(&args_def.children) {
        if let AstNode::Arg{ name, .. } = &ast[*child_pos].value {
            if name == right {
                return apply_generics_from_base(
                    &ast[*child_pos].typ.clone(),
                    left_kind
                ).or_else(|| ast[*child_pos].typ.clone())
            }
        }
    }
    None
}

pub fn find_index_typ(ast: &[Ast], info: &Info, base: usize, pos: usize) -> Option<Type> {
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
    let index_func = find_index_func(&ast[base].typ, ast, info, pos);
    let typ = &ast[unwrap_u(&ast[index_func].children)[2]].typ;

    apply_generics_from_base(typ, ast[base].typ.as_ref().unwrap())
}

pub fn get_inner_type(ast: &[Ast], typ: &Type, trait_name: &str, inner_type_name: &str, info: &Info) -> Option<Type> {
    match &typ.kind {
        TypeKind::Struct(struct_name) => {
            let struct_pos = info.structs[struct_name.get_str()].pos;
            let traits_pos = ast[struct_pos].children.as_ref().unwrap()[3];
            for trt in unwrap_u(&ast[traits_pos].children) {
                if matches!(&ast[*trt].value, AstNode::Identifier(idf) if idf == trait_name) {
                    for typ in unwrap_u(&ast[*trt].children) {
                        if matches!(&ast[*typ].value, AstNode::Type(name) if name == inner_type_name) {
                            return ast[*typ].typ.clone();
                        }
                    }
                }
            }
            None
        }
        TypeKind::Trait(trt_name) => {
            if trt_name != trait_name { return None }
            let generic_map = &typ.children.as_ref().unwrap()[0];
            for child in unwrap(&generic_map.children) {
                if matches!(&child.kind, TypeKind::InnerType(name) if name == inner_type_name) {
                    return Some(child.children.as_ref().unwrap()[0].clone());
                }
            }
            // println!("{}\\{}", trt_name, trait_name);
            // print_type(&Some(typ.clone()));
            // let trt_pos = info.traits[trait_name].pos;
            // let mod_pos = ast[trt_pos].children.as_ref().unwrap()[1];
            // for child in unwrap_u(&ast[mod_pos].children) {
            //     println!("TYP {:?}", ast[*child].value);
            //     if matches!(&ast[*child].value, AstNode::Type(name) if name == inner_type_name) {
            //         println!("!{:?}", ast[*child].typ);
            //         return ast[*child].typ.clone();
            //     }
            // }
            None
        }
        _ => todo!()
    }
}
