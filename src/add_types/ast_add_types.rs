use std::collections::{HashMap, HashSet};
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::construct_ast::mold_ast::{Info, VarTypes};
use crate::types::{BOOL_TYPE, CHAR_TYPE, FLOAT_TYPE, GenericType, UNKNOWN_TYPE, INT_TYPE, MUT_STR_TYPE, STR_TYPE, Type, TypeKind, TypName, unwrap, unwrap_u, implements_trait, print_type, clean_type, print_type_b};
use lazy_static::lazy_static;
use pretty_print_tree::Color;
use regex::Regex;
use crate::{some_vec, unwrap_enum, typ_with_child, IGNORE_FUNCS, IMPL_TRAITS, ImplTraitsKey, get_traits};
use crate::add_types::generics::{apply_generics_from_base, apply_map_to_generic_typ, get_function_return_type, map_generic_types};
use crate::add_types::polymorphism::{box_if_needed, box_no_side_effects, make_enums, matches_template};
use crate::add_types::utils::{add_to_stack, find_function_in_struct, find_function_in_trait, get_from_stack, get_pointer_complete_inner, get_pointer_inner, is_float, join, join_or, update_pos_from_token};
use crate::construct_ast::tree_utils::{add_to_tree, insert_as_parent_of, print_tree, update_pos_from_tree_node};
use crate::mold_tokens::OperatorType;
use crate::to_rust::to_rust;
use crate::{throw, CUR_COL, CUR_LINE, CUR_PATH, LINE_DIFF, SRC_CODE};

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
    update_pos_from_tree_node(&ast[pos]);

    let children = ast[pos].children.clone().unwrap_or_default();
    match &ast[pos].value {
        AstNode::Tuple => {
            for child in children {
                add_types(ast, child, vars, info, parent_struct);
            }
            ast[pos].typ = Some(Type {
                kind: TypeKind::Tuple,
                children: Some(ast[pos].children.as_ref().unwrap().iter().map(
                    |child| ast[*child].typ.clone().unwrap()
                ).collect())
            });
        }
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
        AstNode::Match => {
            vars.push(HashMap::new());
            add_types(ast, children[0], vars, info, parent_struct);
            let match_on = ast[children[0]].typ.as_ref().unwrap();
            let match_on = get_pointer_complete_inner(match_on);
            let amount_of_options = if let TypeKind::Enum(enum_name) = &match_on.kind {
                let enm = &ast[info.enums[&enum_name.to_string()].pos];
                let enm_module = &ast[enm.ref_children()[1]];
                unwrap_u(&enm_module.children).len()
            } else if let TypeKind::OneOf = &match_on.kind {
                info.one_of_enums[&match_on.to_string()].options.len()
            } else {
                throw!("only matches on enums are currently supported (got `{}`), \
                please use if/elif/else instead", match_on)
            };

            let mut covered = HashSet::new();
            for child in children.iter().skip(1) {
                add_types(ast, *child, vars, info, parent_struct);
                let name_pos = ast[ast[*child].ref_children()[0]].ref_children()[0];
                let case_str = to_rust(ast, name_pos,0, info);
                if covered.contains(&case_str) {
                    update_pos_from_tree_node(&ast[*child]);
                    throw!("same case twice `{}`", case_str)
                }
                covered.insert(case_str);
            }
            let has_default = covered.remove("_");
            if !has_default && covered.len() != amount_of_options {
                if covered.len() > amount_of_options {
                    unreachable!()
                } else {
                    update_pos_from_tree_node(&ast[pos]);
                    throw!("not all cases covered") // todo should say which arent
                }
            }
            vars.pop();
        }
        AstNode::Case => {
            let parent_match = &ast[*ast[pos].parent.as_ref().unwrap()];
            let match_on = parent_match.ref_children()[0];
            let match_type = ast[match_on].typ.as_ref().unwrap();
            let match_type = get_pointer_complete_inner(match_type).clone();
            let mut expression_children = ast[children[0]].children.clone().unwrap();
            if let TypeKind::Enum(enum_name_temp) = &match_type.kind {
                let enum_name = enum_name_temp.to_string();
                is_enum_option_valid(
                    ast, info, ast[expression_children[0]].ref_children()[1],
                    if expression_children.len() == 2 { Some(expression_children[1]) } else { None },
                    enum_name_temp, true
                );

                let option = match &ast[expression_children[0]].value {
                    AstNode::Identifier(_) => {
                        throw!("expected property, please prefix this with the enum e.g. `Color.red`");
                        /*
                        let prop = insert_as_parent_of(ast, expression_children[0], AstNode::Property);
                        add_to_tree(prop, ast, Ast::new(AstNode::Identifier(enum_name.clone())));
                        ast[prop].children.as_mut().unwrap().reverse();
                        add_types(ast, ast[prop].ref_children()[0], vars, info, parent_struct); //1 add type to enum so that it knows to use `::` and not `.` to separate hem
                        unwrap_enum!(&ast[expression_children[0]].value, AstNode::Identifier(idf), idf)
                        */
                    },
                    AstNode::Property => {
                        add_types(ast, ast[expression_children[0]].ref_children()[0], vars, info, parent_struct); //1 add type to enum so that it knows to use `::` and not `.` to separate hem
                        let sides = ast[expression_children[0]].ref_children();
                        let left = unwrap_enum!(&ast[sides[0]].value, AstNode::Identifier(name), name, "expected identifier"); // todo this is a bad exception
                        let right = unwrap_enum!(&ast[sides[1]].value, AstNode::Identifier(name), name, "expected identifier"); // todo this is a bad exception
                        if *left != enum_name {
                            throw!("expected `{}` but found `{}`", enum_name, left);
                        }
                        right
                    }
                    _ => throw!("expected identifier but found `{}`", ast[expression_children[0]].value)
                };
                if option == "_" {
                    vars.push(HashMap::new());
                    add_types(ast, *children.last().unwrap(), vars, info, parent_struct); //1 body
                    vars.pop();
                    return
                }

                let enm = &ast[info.enums[&enum_name].pos];
                let enm_module = &ast[enm.ref_children()[1]];
                let opt_pos = unwrap_u(&enm_module.children).iter().find(
                    |opt| matches!(&ast[**opt].value, AstNode::Identifier(idf) if idf == option)
                ).unwrap_or_else(|| {
                    update_pos_from_tree_node(&ast[ast[expression_children[0]].ref_children()[1]]);
                    throw!("couldn't find `{}` in enum `{}`", option, enum_name)
                });
                let enum_types = unwrap_u(&ast[*opt_pos].children).clone();

                vars.push(HashMap::new());

                if expression_children.len() == 2 {
                    let par = &ast[expression_children[1]];
                    let par_children = unwrap_u(&par.children).clone();
                    if par_children.len() != enum_types.len() {
                        update_pos_from_tree_node(par);
                        throw!("incorrect amount of elements, expected `{}` but found `{}`", enum_types.len(), par_children.len())
                    }
                    for (elem, typ) in par_children.iter().zip(enum_types) {
                        add_to_stack(
                            vars,
                            unwrap_enum!(&ast[*elem].value, AstNode::Identifier(name), name.clone()),
                            *elem
                        );
                        ast[*elem].typ = ast[typ].typ.clone();
                    }
                }
                add_types(ast, *children.last().unwrap(), vars, info, parent_struct); //1 body
                vars.pop();
            } else if let TypeKind::OneOf = &match_type.kind {
                if expression_children.len() == 2 {
                    update_pos_from_tree_node(&ast[expression_children[1]]);
                    throw!("unexpected parenthesis")
                }
                let one_of_enums_name = match_type.to_string();
                let option_name = if let AstNode::Identifier(idf) = &ast[expression_children[0]].value {
                    if idf == "_" {
                        vars.push(HashMap::new());
                        add_types(ast, *children.last().unwrap(), vars, info, parent_struct); //1 body
                        vars.pop();
                        return
                    }

                    let idf = clean_type(idf.clone());
                    ast[expression_children[0]].value = AstNode::Identifier(format!("_{idf}"));
                    idf
                } else if let AstNode::Null = &ast[expression_children[0]].value {
                    ast[expression_children[0]].value = AstNode::Identifier(String::from("_None"));
                    TypName::Static("None")
                } else {
                    throw!("expected identifier but found `{}`", ast[expression_children[0]].value)
                };
                let property = insert_as_parent_of(
                    ast, expression_children[0], AstNode::Property, ast[expression_children[0]].pos.clone()
                );
                //1 needs to update it cuz the tree just changed
                expression_children = ast[children[0]].children.clone().unwrap();

                add_to_tree(property, ast, Ast::new_no_pos(
                    AstNode::Identifier(one_of_enums_name.clone())
                ));
                ast[property].children.as_mut().unwrap().reverse();

                vars.push(HashMap::new());
                if children.len() == 3 {
                    let as_name = unwrap_enum!(&ast[children[1]].value, AstNode::Identifier(n), n);
                    add_to_stack(vars, as_name.clone(), expression_children[0]);
                }
                add_types(ast, expression_children[0], vars, info, parent_struct); //1 this will add the enum type
                let enum_typ = ast[expression_children[0]].typ.as_ref().unwrap();
                let enm = &info.one_of_enums[
                    unwrap_enum!(&enum_typ.kind, TypeKind::Enum(nme), nme.get_str())
                ];
                let typ = enm.options.iter().find(
                    |opt| opt.to_string() == option_name.get_str()
                ).unwrap_or_else(
                    || {
                        print_tree(ast, expression_children[0]);
                        update_pos_from_tree_node(&ast[expression_children[0]]);
                        throw!("couldn't find `{}` in `{}`", option_name, one_of_enums_name)
                    }
                );

                ast[expression_children[0]].typ = Some(typ.clone());

                add_types(ast, *children.last().unwrap(), vars, info, parent_struct); //1 body
                vars.pop();
            } else { unreachable!() };
        }
        AstNode::Module | AstNode::Body | AstNode::ReturnType
        | AstNode::GenericsDeclaration | AstNode::ColonParentheses
        | AstNode::Args | AstNode::NamedArg(_) => {
            for child in children {
                add_types(ast, child, vars, info, parent_struct);
            }
        }
        AstNode::Return => {
            if let AstNode::RustStructInit = &ast[children[0]].value {
                return;
            }
            if !children.is_empty() {
                add_types(ast, children[0], vars, info, parent_struct);
            }
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
                box_if_needed(return_typ, ast, children[0], info);
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
            // todo not sure how this works... doesnt it need to have generic map as child?
            ast[pos].typ = Some(Type {
                kind: if info.structs.contains_key(name) {
                    TypeKind::Struct(TypName::Str(name.clone()))
                } else if info.funcs.contains_key(name) {
                    TypeKind::Function(name.clone())
                } else if info.enums.contains_key(name) | info.one_of_enums.contains_key(name) {
                    TypeKind::Enum(TypName::Str(name.clone()))
                } else {
                    throw!("used `{}` before assignment", name) //todo // 5 unreachable!
                },
                children: None,
            });
        }
        AstNode::Assignment => {
            add_types(ast, children[1], vars, info, parent_struct);
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                if let Some(i) = get_from_stack(vars, name) {
                    let should_be = ast[i].typ.clone().unwrap();
                    box_if_needed(should_be, ast, children[1], info);
                    // if ast[children[1]].typ != *check_for_boxes(should_be, children[1], info, vars) {
                    //     throw!(
                    //         "expected `{}` but found `{}`. variables can't change type, if you want to override use `:=`",
                    //         should_be.as_ref().unwrap(),
                    //         ast[children[1]].typ.as_ref().unwrap()
                    //     );
                    // }
                } else { //1 first assignment
                    add_to_stack(vars, name.clone(), children[0]);
                    ast[pos].value = AstNode::FirstAssignment;
                    ast[children[0]].typ = Some(box_if_needed(
                        ast[children[1]].typ.clone().unwrap(), ast, children[1], info
                    ));
                }
            }
            add_types(ast, children[0], vars, info, parent_struct);
        }
        AstNode::FirstAssignment => {
            add_types(ast, children[1], vars, info, parent_struct);
            if ast[pos].typ.is_some() {
                ast[children[0]].typ = ast[pos].typ.clone();
                // throw!("\ne: {:?}\ng: {:?}", ast[pos].typ.clone().unwrap(), ast[children[1]].typ.clone().unwrap());
                box_if_needed(
                    ast[pos].typ.clone().unwrap(), ast, children[1], info
                );
                // throw!("!{:?}", ast[children[0]].typ);
            } else {
                let typ = ast[children[1]].typ.clone();
                if typ.is_none() {
                    throw!("`{}` needs a type annotation", ast[children[0]].value)
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
            let t1 = ast[children[0]].typ.as_ref().unwrap();
            let t2 = ast[children[1]].typ.as_ref().unwrap();
            update_pos_from_tree_node(&ast[pos]);
            if t1 != t2 {
                throw!(
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
            if let OperatorType::Div = op {
                if !is_float(&t1.kind) {
                    throw!("can't div `{}`s only `f32` or `f64`, did you mean floor div `//`?", t1)
                }
            }
            ast[pos].typ = Some(t1.clone());
        }
        AstNode::FunctionCall(_) => {
            add_type_func_call(ast, pos, vars, info, parent_struct, &children);
        }
        // AstNode::StructInit => {
        //     add_type_struct_innit(ast, pos, vars, info, parent_struct, &children)
        // }
        AstNode::Property => {
            add_type_property(ast, pos, vars, info, parent_struct, &children)
        }
        AstNode::Null => {
            ast[pos].typ = Some(Type {
                kind: TypeKind::Null,
                children: None
            });
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
                    typ_with_child! {
                        TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                        ast[children[0]].typ.clone().unwrap()
                    }
                }),
                OperatorType::MutPointer => Some(typ_with_child! {
                    TypeKind::MutPointer,
                    typ_with_child! {
                        TypeKind::Generic(GenericType::WithVal(String::from("T"))),
                        ast[children[0]].typ.clone().unwrap()
                    }
                }),
                OperatorType::Dereference => {
                    #[inline] fn dref(typ: &Type) -> Option<Type> {
                        match &typ.kind {
                            TypeKind::Pointer | TypeKind::MutPointer => {
                                if let Type {
                                    kind: TypeKind::Generic(GenericType::WithVal(_)), children
                                } = &unwrap(&typ.children)[0] {
                                    Some(children.as_ref().unwrap()[0].clone())
                                } else {
                                    Some(typ.ref_children()[0].clone())
                                }
                            },
                            TypeKind::Generic(GenericType::WithVal(of)) => {
                                let children = unwrap_enum!(
                                    &typ.children, Some(c), c,
                                    "generic type `{}` cannot be dereferenced", of
                                );
                                dref(&children[0])
                            }
                            _ => throw!("type `{}` cannot be dereferenced", typ),
                        }
                    }
                    update_pos_from_tree_node(&ast[pos]);
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
                        TypeKind::Generic(GenericType::WithVal(String::from("T"))),
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
                            TypeKind::Generic(GenericType::WithVal(String::from("K"))),
                            if let Some(x) = inner_types.first() {
                                ast[*x].typ.clone().unwrap()
                            } else { UNKNOWN_TYPE }
                        },
                        typ_with_child!{
                            TypeKind::Generic(GenericType::WithVal(String::from("V"))),
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
                update_pos_from_tree_node(&ast[pos]);
                throw!("can't infer type");
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
                update_pos_from_tree_node(&ast[pos]);
                throw!("condition isn't bool") //todo truthy falsy
            }
            if ast[children[0]].typ != ast[children[2]].typ {
                update_pos_from_tree_node(&ast[pos]);
                throw!(
                    "`if` and `else` have incompatible types. expected `{}`, found `{}`",
                    ast[children[0]].typ.as_ref().unwrap(),
                    ast[children[2]].typ.as_ref().unwrap()
                )
            }
            ast[pos].typ = ast[children[0]].typ.clone();
        }
        AstNode::ForVars | AstNode::Pass | AstNode::Continue | AstNode::Break | AstNode::Enum(_)
        | AstNode::Trait { .. } /*| AstNode::Traits*/ | AstNode::Type(_) | AstNode::Types
        | AstNode::Arg { .. } | AstNode::Cast | AstNode::As(_) | AstNode::From | AstNode::Import
        | AstNode::Ignore | AstNode::RustStructInit => {}
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
    let t1 = get_pointer_complete_inner(t1);
    let t2 = get_pointer_complete_inner(t2);
    update_pos_from_tree_node(&ast[pos]);
    let t1_name = unwrap_enum!(
        &t1.kind, TypeKind::Struct(t), t, "`{}` not valid for `{}`", op, t1
    );
    let t2_name = unwrap_enum!(
        &t2.kind, TypeKind::Struct(t), t, "`{}` not valid for `{}`", op, t2
    );
    if *t1_name != *t2_name
        && !(*t1_name == TypName::Static("String") && *t2_name == TypName::Static("str"))
        && !matches!(op, OperatorType::In | OperatorType::NotIn)
    {
        throw!("`{}` not implemented for `{}` and `{}`", op, t1_name, t2_name)
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
        OperatorType::Div => {
            Some(typ_with_child! {
            TypeKind::Struct(TypName::Static(
                if matches!(&t1.kind, TypeKind::Struct(name) if name == "f64") { "f64" }
                else { "f32" }
            )),
            Type {
                kind: TypeKind::GenericsMap,
                children: None
            }
        })
        },
        OperatorType::FloorDiv => Some(typ_with_child! {
            TypeKind::Struct(
                if matches!(
                    &t1.kind,
                    TypeKind::Struct(name) if NUM_TYPES.contains(&name.get_str())
                ) { unwrap_enum!(&t1.kind, TypeKind::Struct(name), name.clone()) }
                else { throw!("`//` not implemented for `{}`", t1) }
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
    let left_kind = ast[children[0]].typ.clone().unwrap_or_else(|| {
        update_pos_from_tree_node(&ast[pos]);
        throw!("{:?}", ast[children[0]].value) // todo better message
    });
    let left_kind = get_pointer_complete_inner(&left_kind);
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

                    let (res, is_static) = get_property_method_typ_and_set_args(
                        ast, children, struct_name, true, pos, left_kind, info, vars
                    );
                    // let (res, is_static) = get_property_method_typ(
                    //     ast, info, children, left_kind,
                    //     struct_name, true, pos
                    // );
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

                    get_property_method_typ_and_set_args(
                        ast, children, trait_name, false, pos,  left_kind, info, vars
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
        _ => {
            update_pos_from_tree_node(&ast[pos]);
            throw!("expected `struct`, `trait` or `enum` but got `{}`", left_kind)
        }
    }
}

// fn add_type_struct_innit(ast: &mut Vec<Ast>, pos: usize, vars: &mut VarTypes, info: &mut Info, parent_struct: &Option<HashMap<String, usize>>, children: &[usize]) {
//     add_types(ast, children[1], vars, info, parent_struct);
//     if let AstNode::Identifier(name) = &ast[children[0]].value.clone() {
//         let mut generics_map = HashMap::new();
//
//         let names_pos = add_to_tree(
//             pos, ast, Ast::new(AstNode::Args)
//         );
//         // let args_def = &ast[ast[info.structs[name].pos].ref_children()[1]];
//         // let args = args_def.children.clone();
//         let module = &ast[ast[info.structs[name].pos].ref_children()[2]];
//         for func in module.ref_children() {
//             if matches!(&ast[*func].value, AstNode::Function(func_name) if func_name == "__init__") {
//                 let init_args = ast[ast[*func].ref_children()[1]].children.clone().unwrap_or_default();
//                 let supplied_vec = ast[children[1]].children.clone().unwrap_or_default();
//                 if init_args.len() != supplied_vec.len() {
//                     throw!("expected `{}` arguments but found `{}`", init_args.len(), supplied_vec.len())
//                 }
//                 for (arg, supplied) in init_args.iter().zip(supplied_vec) {
//                     map_generic_types(
//                         &ast[*arg].typ.clone().unwrap(),
//                         &ast[supplied].typ.clone().unwrap(),
//                         &mut generics_map, ast, info
//                     );
//                     add_to_tree(
//                         names_pos, ast, Ast::new(ast[*arg].value.clone())
//                     );
//                 }
//                 break
//             }
//         }
//         ast[pos].typ = Some(typ_with_child! {
//             TypeKind::Struct(TypName::Str(name.clone())),
//             Type {
//                 kind: TypeKind::GenericsMap,
//                 children: if generics_map.is_empty() {
//                     if info.structs[name].generics.is_some() && !info.structs[name].generics.as_ref().unwrap().is_empty() {
//                         throw!(
//                             "unresolved generic\\s in innit of `{name}`: `{}`",
//                             info.structs[name].generics.as_ref().unwrap().join(", ")
//                         )
//                     }
//                     None
//                 } else {
//                     Some(
//                         info.structs[name].generics.as_ref().unwrap().iter().filter_map(
//                             |name| {
//                                 if generics_map.contains_key(name) {
//                                     Some(typ_with_child! {
//                                         TypeKind::Generic(GenericType::WithVal(name.clone())),
//                                         generics_map.remove(name).unwrap()
//                                     })
//                                 } else { throw!("unresolved generic in innit `{name}`") }
//                             }
//                         ).collect()
//                     )
//                 },
//             }
//         });
//     } else { todo!() }
// }

fn get_type_comprehension(
    ast: &mut Vec<Ast>, pos: usize, vars: &mut VarTypes, info: &mut Info,
    parent_struct: &Option<HashMap<String, usize>>, children: &[usize]
) -> Option<Type> {
    #[inline] fn add_loop_types(
        ast: &mut Vec<Ast>, vars: &mut VarTypes, info: &mut Info,
        parent_struct: &Option<HashMap<String, usize>>, children: &[usize]
    ) {
        for r#loop in ast[children[1]].children.clone().unwrap() {
            let colon_par = ast[r#loop].ref_children()[0];
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

    let stmt = ast[children[0]].ref_children()[0];
    add_types(ast, stmt, vars, info, parent_struct);

    vars.pop();

    #[inline] fn get_generics(ast: &mut [Ast], pos: usize, stmt: usize) -> Vec<Type> {
        if let AstNode::DictComprehension = &ast[pos].value {
            let stmt_children = ast[stmt].ref_children();
            ["K", "V"].iter().enumerate().map(|(i, name)|
                typ_with_child! {
                    TypeKind::Generic(GenericType::WithVal(String::from(*name))),
                    ast[stmt_children[i]].typ.clone().unwrap()
                }
            ).collect()
        } else {
            vec![typ_with_child! {
                TypeKind::Generic(GenericType::WithVal(String::from("T"))),
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

    update_pos_from_tree_node(&ast[children[0]]);
    let name = unwrap_enum!(
        &ast[children[0]].value, AstNode::Identifier(x), x.clone(), "function without identifier?"
    );
    let expected_input = if let Some(fnc) = info.funcs.get(&name) {
        fnc.input.clone()
    } else { throw!("unrecognized function `{}`", name) };
    ast[pos].typ = format_args_and_get_return_typ(
        expected_input, &ast[children[1]].children.clone(),
        &name, ast, children, info, vars, None, pos
    );
}

fn format_args_and_get_return_typ(
    mut expected_input: Option<Vec<Param>>, args: &Option<Vec<usize>>, func_name: &str,
    ast: &mut Vec<Ast>, children: &[usize], info: &mut Info, vars: &VarTypes, base: Option<&Type>,
    pos: usize
) -> Option<Type> {
    #[inline] fn format_args(
        ast: &mut Vec<Ast>, children: &[usize], expected_input: &Option<Vec<Param>>, func_name: &str,
        info: &mut Info, vars: &VarTypes, args: &Option<Vec<usize>>
    ) -> Option<Vec<usize>> {
        // let mut args = unwrap_u(&ast[children[1]].children).clone();
        if expected_input.is_some() && !expected_input.as_ref().unwrap().is_empty() {
            let expected_args = expected_input.as_ref().unwrap();
            // if args.is_none() {
            //     throw!("expected `{}` args but found `0`", expected_args.len())
            // }
            let mut args = args.clone().unwrap_or_default();
            put_args_in_vec(ast, children, &mut args, expected_args, info, vars);
            add_optional_args(ast, children, &mut args, expected_args);
            if args.len() != expected_args.len() {
                update_pos_from_tree_node(&ast[ast[children[0]].parent.unwrap()]);
                throw!(
                    "`{}` expected `{}` arguments, but got `{}`",
                    func_name, expected_args.len(), args.len()
                )
            }
            return Some(args)
        } else if args.is_some() && !args.as_ref().unwrap().is_empty() {
            update_pos_from_tree_node(&ast[ast[children[0]].parent.unwrap()]);
            throw!("wasn't expecting any args")
        }
        None
    }
    #[inline] fn get_generics(expected_input: &Option<Vec<Param>>, args: &Option<Vec<usize>>, ast: &mut Vec<Ast>, info: &mut Info, vars: &VarTypes) -> HashMap<String, Type> {
        let mut generic_map = HashMap::new();
        if let Some(expected_input) = &expected_input {
            for (exp, got) in expected_input.iter().zip(args.as_ref().unwrap()) {
                let got_typ = box_no_side_effects(
                    exp.typ.clone(), &ast[*got].typ.clone().unwrap(), ast, info
                );
                map_generic_types(
                    &exp.typ, &got_typ, &mut generic_map,
                    ast, info
                );
            }
        }
        generic_map
    }
    // #[inline] fn check_that_is_castable(expected_inputs: &[Param], args: &[usize], info: &Info, ast: &[Ast], is_built_in: bool) -> Option<Vec<Type>> {
    //     Some(expected_inputs.iter().zip(args.iter()).map(
    //         |(exp, got)| {
    //             if !is_castable(
    //                 &exp.typ, ast[*got].typ.as_ref().unwrap(), ast, info, is_built_in
    //             ) {
    //                 throw!("expected `{}` got `{}`", exp.typ, *ast[*got].typ.as_ref().unwrap())
    //             }
    //             ast[*got].typ.clone().unwrap()
    //         }
    //     ).collect())
    // }

    if let Some(base) = base {
        if let Some(vc) = &mut expected_input {
            let typ = get_pointer_complete_inner(&vc[0].typ);
            if !matches!(&typ.kind, TypeKind::Struct(name) if name == "Self") {
                update_pos_from_tree_node(&ast[pos]);
                throw!("used static function as a method")
            }
            // vc.remove(0);

            *vc = vc.iter().skip(1).map(|x|
                Param {
                    typ: apply_generics_from_base(
                        &Some(x.typ.clone()), base
                    ).unwrap_or_else(|| x.typ.clone()),
                    ..x.clone()
                }
            ).collect();
        }
    }
    if expected_input.is_some() && expected_input.as_ref().unwrap().is_empty() {
        expected_input = None;
    }
    let args = format_args(
        ast, children, &expected_input, func_name, info, vars, args
    );
    let generic_map = get_generics(&expected_input, &args, ast, info, vars);

    let args: Option<Vec<Type>> = if let Some(args) = args {
        if unsafe { IGNORE_FUNCS.contains(func_name) } {
            // check_that_is_castable(
            //     expected_input.as_ref().unwrap(), &args, info, ast, true
            // )
            Some(expected_input.as_ref().unwrap().iter().zip(args.iter()).map(
                |(exp, got)| {
                    let got_typ = ast[*got].typ.as_ref().unwrap().clone();
                    if !matches_template(exp.typ.clone(), &got_typ, ast, info) {
                        update_pos_from_tree_node(&ast[pos]); // todo not sure if correct node
                        throw!("expected `{}` but got `{}`", &exp.typ, got_typ)
                    }
                    got_typ
                }
            ).collect())
        } else {
            Some(expected_input.as_ref().unwrap().iter().zip(args.iter()).map(
                |(exp, got)|
                    box_if_needed(exp.typ.clone(), ast, *got, info)
            ).collect())
        }
    } else { None };

    let rtrn_typ = if base.is_some() || func_name == "__init__" {
        let init_type = if func_name == "__init__" {
            let mut class_typ = ast[ast[pos].ref_children()[0]].typ.clone();
            let generics = match &class_typ.as_ref().unwrap().kind {
                TypeKind::Struct(name) => &info.structs[name.get_str()].generics,
                TypeKind::Trait(name) => &info.traits[name.get_str()].generics,
                TypeKind::Enum(name) => &info.enums[name.get_str()].generics,
                _ => {
                    update_pos_from_tree_node(&ast[ast[pos].ref_children()[0]]);
                    throw!("expected `struct`, `trait` or `enum` but found {}", class_typ.as_ref().unwrap())
                }
            };
            class_typ.as_mut().unwrap().children = some_vec![Type {
                kind: TypeKind::GenericsMap,
                children: if generic_map.is_empty() { None } else {
                    Some(
                        generics.as_ref().unwrap().iter().filter_map(
                            |name| {
                                if generic_map.contains_key(name) {
                                    Some(typ_with_child! {
                                            TypeKind::Generic(GenericType::WithVal(name.clone())),
                                            generic_map.get(name).unwrap().clone()
                                        })
                                } else { None }
                            }
                        ).collect()
                    )
                }
            }];
            class_typ
            // TODO generics !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        } else { None };
        let base =  if let Some(t) = &init_type { t } else { base.unwrap() };
        let func_pos = match &base.kind {
            TypeKind::Struct(name) => {
                find_function_in_struct(
                    ast, info.structs, name.get_str(), func_name, pos
                ).unwrap()
            },
            TypeKind::Enum(_name) => {
                todo!()
            },
            TypeKind::Trait(name) => {
                find_function_in_trait(
                    ast, info.traits, name.get_str(), func_name
                ).unwrap()
            },
            _ => {
                update_pos_from_tree_node(&ast[ast[pos].ref_children()[0]]);
                throw!("expected `struct`, `trait` or `enum` but found {}", base)
            }
        };

        let return_pos = ast[func_pos].ref_children()[1];
        if let Some(t) = init_type {
            return Some(t)
            // get_function_return_type(
            //     &Some(t),
            //     &expected_input,
            //     &args
            // )
        } else {
            let return_typ = &ast[return_pos].typ;
            apply_generics_from_base(return_typ, base).or_else(|| return_typ.clone())
        }
    } else {
        get_function_return_type(
            &info.funcs[func_name].output.clone(),
            &info.funcs[func_name].input.clone(),
            &args, ast, info
        )
    };
    rtrn_typ.map(|rtrn_typ| apply_map_to_generic_typ(&rtrn_typ, &generic_map))
}

pub fn is_castable(exp: &Type, got: &Type, ast: &[Ast], info: &Info, is_built_in: bool) -> bool {
    match &exp.kind {
        TypeKind::Generic(GenericType::NoVal(_)) => true,
        TypeKind::Generic(_) => throw!("huh? (not sure why I wrote this...)"),
        TypeKind::OneOf => {
            exp.ref_children().iter().any(|opt|
                is_castable(opt, got, ast, info, is_built_in)
            )
        }
        TypeKind::Trait(_) => {
            implements_trait(got, exp, ast, info).is_some()
        }
        _ if is_built_in && matches!(exp.kind, TypeKind::Pointer | TypeKind::MutPointer) => {
            if got.kind == exp.kind || matches!((&exp.kind, &got.kind), (TypeKind::Pointer, TypeKind::MutPointer)) {
                let got = get_pointer_inner(got);
                let exp = get_pointer_inner(exp);
                is_castable(exp, got, ast, info, is_built_in)
            } else {
                let exp = get_pointer_inner(exp);
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
    if let Some(Type { kind: TypeKind::Generic(GenericType::WithVal(_)), children }) = &typ {
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
    if ast[children[1]].children.is_none() {
        if expected_args.len() > amount_of_pos_args { //1 has positional args
            ast[children[1]].children = Some(vec![]);
        } else { return }
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
            update_pos_from_tree_node(&ast[children[1]]);
            throw!("not all positional args supplied")
        }
    }
    let args_children = ast[children[1]].children.as_mut().unwrap();
    for pos in to_add {
        args_children.push(pos);
    }
    *args = ast[children[1]].children.clone().unwrap();
}

fn put_args_in_vec(
    ast: &mut Vec<Ast>, children: &[usize], args: &mut Vec<usize>, expected_args: &[Param],
    info: &mut Info, vars: &VarTypes
) {
    let args_kwargs_pos = expected_args.iter().enumerate()
        .find(|(_, par)| par.is_args || par.is_kwargs);
    if let Some((pos_in_args, _)) = args_kwargs_pos {
        let vec_children: Vec<_> = unwrap_u(&ast[children[1]].children)
            .iter()
            .skip(pos_in_args)
            .take_while(|x| !matches!(ast[**x].value, AstNode::NamedArg(_))) //1 while isn't named
            .cloned().collect();
        let expected_typ = &expected_args[pos_in_args].typ.ref_children()[0];

        let inner_typ = box_if_needed(
            expected_typ.clone(), ast, vec_children[0], info
        );
        for pos in vec_children.iter().skip(1) {
            box_if_needed(expected_typ.clone(), ast, *pos, info);
        }

        let vec_pos = add_to_tree(
            children[1],
            ast,
            Ast {
                value: AstNode::ListLiteral,
                children: Some(vec_children),
                parent: None,
                typ: Some(typ_with_child! {
                    TypeKind::Args,
                    inner_typ
                }),
                is_mut: false,
                pos: None
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
    ast: &mut Vec<Ast>, info: &mut Info, children: &[usize], enm_name: &TypName
) -> Option<Type> {
    if let AstNode::FunctionCall(_) = &ast[children[1]].value {
        let children = ast[children[1]].ref_children();
        is_enum_option_valid(
            ast, info, children[0],
            if children.len() == 2 { Some(children[1]) } else { None },
            enm_name, false
        );
    } else {
        is_enum_option_valid(
            ast, info, children[1],
            if ast[children[1]].children.is_some() { Some(children[1]) } else { None },
            enm_name, false
        );
    }

    let generics = if let Some(enm) = info.enums.get(enm_name.get_str()) {
        if matches!(&enm.generics, Some(vc) if vc.is_empty()) {
            None
        } else { enm.generics.clone() }
    } else {
        let generics = &info.one_of_enums[enm_name.get_str()].generics;
        if generics.is_empty() { None } else {
            Some(generics.split(',').map(|x| x.to_string()).collect::<Vec<_>>())
        }
    };
    // let generics = &info.enums[enm_name.get_str()].generics;

    if let Some(generics) = generics {
        let mut generics_map = HashMap::new();
        let args = &ast[children[1]];
        if let AstNode::FunctionCall(_) = &args.value {
            let args = &ast[unwrap_u(&args.children)[1]];
            let zip = generics
                .iter()
                .zip(unwrap_u(&args.children).clone());
            for (generic, arg) in zip {
                let typ = ast[arg].typ.clone().unwrap();
                map_generic_types(&Type {
                    kind: TypeKind::Generic(GenericType::NoVal(generic.clone())),
                    children: None
                }, &typ, &mut generics_map, ast, info);
            }
        }

        Some(typ_with_child! {
            TypeKind::Enum(enm_name.clone()),
            Type {
                kind: TypeKind::GenericsMap,
                children: Some(
                    generics_map.iter().map(|(name, t)|
                        typ_with_child! {
                            TypeKind::Generic(GenericType::WithVal(name.clone())),
                            t.clone()
                        }
                    ).collect()
                )
            }
        })
    } else {
        Some(typ_with_child! {
            TypeKind::Enum(enm_name.clone()),
            Type {
                kind: TypeKind::GenericsMap,
                children: None
            }
        })
    }
}

fn is_enum_option_valid(ast: &mut Vec<Ast>, info: &mut Info, variant: usize, parentheses: Option<usize>, enm_name: &TypName, ignore_types: bool) {
    let variant_name = unwrap_enum!(&ast[variant].value, AstNode::Identifier(n), n);

    if let Some(enm) = info.enums.get(enm_name.get_str()) {
        let enm_ast = &ast[enm.pos];
        let enm_module = &ast[enm_ast.ref_children()[1]];

        if let Some(parentheses) = parentheses {
            let got_types = ast[parentheses].ref_children();
            if let Some(opt) = unwrap_u(&enm_module.children).iter().find(|&child| {
                matches!(&ast[*child].value, AstNode::Identifier(n) if n == variant_name)
            }) {
                //1 check that all the types are the same
                if matches!(&ast[*opt].children, Some(chld) if chld.len() == got_types.len()) {
                    if ignore_types { return; }
                    let exp_children = ast[*opt].ref_children()
                        .iter().map(|i| ast[*i].typ.clone().unwrap())
                        .collect::<Vec<_>>();
                    let got_children = got_types
                        .iter().map(|i| ast[*i].typ.clone().unwrap())
                        .collect::<Vec<_>>();
                    let variant_name = variant_name.clone();
                    if !exp_children.iter().zip(got_children.iter()).all(
                        |(exp, got)| matches_template(exp.clone(), got, ast, info)
                    ) {
                        update_pos_from_tree_node(&ast[parentheses]);
                        throw!(
                            "incorrect types given to `{}.{}`, expected `{}` but found `{}`",
                            enm_name, variant_name,
                            join(exp_children.iter(), "`, `"), join(got_children.iter(), "`, `")
                        )
                    }
                } else {
                    update_pos_from_tree_node(&ast[parentheses]);
                    if matches!(&ast[*opt].children, Some(chld) if chld.len() > got_types.len()) {
                        throw!(
                            "not all values supplied to `{}.{}`, expected `{}` but found `{}`",
                            enm_name, variant_name, unwrap_u(&ast[*opt].children).len(), got_types.len()
                        )
                    } else {
                        throw!(
                            "too many values supplied to `{}.{}`, expected `{}` but found `{}`",
                            enm_name, variant_name, unwrap_u(&ast[*opt].children).len(), got_types.len()
                        )
                    }
                }
            } else {
                update_pos_from_tree_node(&ast[variant]);
                throw!("couldn't find `{}` in enum `{}`", variant_name, enm_name)
            }
        } else if let Some(opt) = unwrap_u(&enm_module.children).iter().find(|&child| {
            matches!(&ast[*child].value, AstNode::Identifier(n2) if variant_name == n2)
        }) {
            if ast[*opt].children.is_some() {
                update_pos_from_tree_node(&ast[variant]);
                let inner_types = ast[*opt].ref_children().iter()
                    .map(|x| ast[*x].typ.as_ref().unwrap());
                throw!(
                    "`{}.{}` has inner value{} supplied: `{}`",
                    enm_name, variant_name,
                    if inner_types.len() == 1 { " which wasn't" } else { "s which weren't" },
                    join(inner_types, "`, `")
                )
            }
        } else {
            update_pos_from_tree_node(&ast[variant]);
            throw!("couldn't find `{}` in enum `{}`", variant_name, enm_name)
        }
    } else {
        let got_opt = variant_name.strip_prefix('_').unwrap();
        let enm_options = &info.one_of_enums[enm_name.get_str()].options;
        if !enm_options.iter().any(
            |exp| exp.to_string() == got_opt
        ){
            throw!("expected `{}` but got `{}`", join_or(enm_options.iter()), got_opt);
        }
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
                let inner_typ = get_associated_type(
                    typ, vec!["IntoIterator", "Iterator"], "Item", info
                ).unwrap_or_else(|| throw!("`{}` doesn't implement `IntoIterator` or `Iterator`", struct_name));
                apply_generics_from_base(&Some(inner_typ), typ)
            },
            TypeKind::Trait(trait_name) => {
                if trait_name != "IntoIterator" && trait_name != "Iterator" {
                    throw!("expected `IntoIterator` or `Iterator` found `{}`", trait_name)
                }
                let generic_map = &unwrap(&typ.children)[0];
                for generic in unwrap(&generic_map.children) {
                    if let TypeKind::AssociatedType(name) = &generic.kind {
                        if name == "Item" {
                            return apply_generics_from_base(
                                &Some(unwrap(&generic.children)[0].clone()),
                                typ
                            )
                        }
                    }
                }
                throw!("couldn't find `Item` type in `{}`", trait_name)
            },
            TypeKind::Pointer | TypeKind::MutPointer => {
                Some(typ_with_child! {
                    typ.kind.clone(),
                    match_kind(ast, info, get_pointer_inner(typ)).unwrap()
                })
            }
            _ => throw!("kind: {:?}", typ.kind)
        }
    }
    match_kind(ast, info, iter.typ.as_ref().unwrap())
}

// TODO this function and `get_property_method_typ` are too similar...
pub fn get_property_method_typ_and_set_args(
    ast: &mut Vec<Ast>, children: &[usize],
    trait_name: &TypName, is_struct: bool, pos: usize, base: &Type,
    info: &mut Info, vars: &VarTypes
) -> (Option<Type>, bool) {
    let func_call = &ast[children[1]];
    let func_name = &ast[unwrap_u(&func_call.children)[0]].value;
    let func_name = unwrap_enum!(func_name, AstNode::Identifier(name), name);
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
    let func_pos = func_pos.unwrap_or_else(|| {
        update_pos_from_tree_node(&ast[unwrap_u(&func_call.children)[0]]);
        throw!("function `{}` not found in struct `{}`", func_name, trait_name)
    });
    let is_static = matches!(ast[func_pos].value, AstNode::StaticFunction(_));
    let func_children = unwrap_u(&ast[func_pos].children);

    let arg_pos = unwrap_u(&ast[children[1]].children)[1];
    let expected_input: Option<Vec<_>> = ast[func_children[1]].children.as_ref()
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

    let input_arg_types = ast[arg_pos].children.clone();

    #[allow(clippy::unnecessary_to_owned)]
    (format_args_and_get_return_typ(
        expected_input, &input_arg_types, &func_name.to_string(), ast,
        &ast[children[1]].children.clone().unwrap(), info, vars,
        if is_static || func_name == "__init__" { None } else { Some(base) },
        pos
    ), is_static)
}

// TODO this function and `get_property_method_typ_and_set_args` are too similar...
pub fn get_property_method_typ(
    ast: &mut Vec<Ast>, info: &mut Info, children: &[usize],
    left_kind: &Type, trait_name: &TypName, is_struct: bool, pos: usize
) -> (Option<Type>, bool) {
    let func_call = &ast[children[1]];
    let func_name = &ast[unwrap_u(&func_call.children)[0]].value;
    let func_name = unwrap_enum!(func_name, AstNode::Identifier(name), name);
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
    let func_pos = func_pos.unwrap_or_else(|| {
        update_pos_from_tree_node(&ast[unwrap_u(&func_call.children)[0]]);
        throw!("function `{}` not found in struct `{}`", func_name, trait_name)
    });
    let is_static = matches!(ast[func_pos].value, AstNode::StaticFunction(_));
    let func_children = unwrap_u(&ast[func_pos].children);

    let return_type = &ast[func_children[2]].typ;
    if is_static || func_name == "__init__" { //1 only for structs
        let arg_pos = unwrap_u(&ast[children[1]].children)[1];

        let expected_arg_types = ast[func_children[1]].children.as_ref()
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

        let input_arg_types =
            ast[arg_pos].children.as_ref().map(|c| c.iter().map(|x|
                    ast[*x].typ.clone().unwrap()
                ).collect());
        let res = get_function_return_type(
            &return_type.clone(), &expected_arg_types, &input_arg_types, ast, info
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

pub fn find_index_typ(ast: &[Ast], info: &mut Info, base: usize, pos: usize) -> Option<Type> {
    fn find_index_typ_inner(typ: &Option<Type>, ast: &[Ast], info: &mut Info, pos: usize) -> Option<Type> {
        match typ {
            Some(Type { kind: TypeKind::Struct(struct_name), .. }) => {
                let index_func = find_function_in_struct(
                    ast, info.structs, struct_name.get_str(), "index", pos
                ).unwrap_or_else(|| {
                    update_pos_from_tree_node(&ast[pos]);
                    throw!("Didn't find `index` function in `{}`", struct_name)
                });
                let idx_typ = &ast[unwrap_u(&ast[index_func].children)[2]].typ;
                apply_generics_from_base(idx_typ, typ.as_ref().unwrap())
            }
            Some(Type { kind: TypeKind::Trait(trait_name), .. }) => {
                let index_func = find_function_in_trait(
                    ast, info.traits, trait_name.get_str(), "index"
                ).unwrap_or_else(|| {
                    update_pos_from_tree_node(&ast[pos]);
                    throw!("Didn't find `index` function in `{}`", trait_name)
                });
                let idx_typ = &ast[unwrap_u(&ast[index_func].children)[2]].typ;
                apply_generics_from_base(idx_typ, typ.as_ref().unwrap())
            }
            Some(Type { kind: TypeKind::Pointer | TypeKind::MutPointer, .. }) =>
                find_index_typ_inner(&Some(
                    get_pointer_complete_inner(typ.as_ref().unwrap()).clone()
                ), ast, info, pos), // todo pos here might not be correct in the case of &Self
            Some(Type { kind: TypeKind::Tuple, children }) => {
                let index = ast[pos].children.as_ref().unwrap()[1];
                if let AstNode::Number(num) = &ast[index].value {
                    if let Ok(num) = num.parse::<usize>() {
                        return Some(children.as_ref().unwrap()[num].clone())
                    }
                }
                let options = HashSet::<&Type>::from_iter(
                    children.as_ref().unwrap().iter()
                ).iter().map(|t| (*t).clone()).collect::<Vec<_>>();
                if options.len() > 1 {
                    let typ = Type {
                        kind: TypeKind::OneOf,
                        children: Some(options)
                    };
                    make_enums(&typ, info.one_of_enums);
                    Some(typ)
                } else {
                    Some(options[0].clone())
                }
            },
            _ => {
                update_pos_from_tree_node(&ast[pos]);
                throw!("expected `struct`, `trait`, `enum` or `tuple` {:?}", typ)
            }
        }
    }
    find_index_typ_inner(&ast[base].typ, ast, info, pos)
}

pub fn get_associated_type(typ: &Type, trait_names: Vec<&str>, associated_type_name: &str, info: &Info) -> Option<Type> {
    match &typ.kind {
        TypeKind::Struct(struct_name) => {
            let traits = get_traits!(struct_name, info);
            let traits = if let Some(vc) = traits { vc.clone() }
            else { vec![] };

            let trt = traits.iter().find(
                |x| {
                    trait_names.contains(&&*x.trt_name)
                }
            );
            if let Some(trt) = trt {
                apply_generics_from_base(
                    &Some(trt.types.as_ref().unwrap().get("Item").unwrap().clone()),
                    typ
                )
            } else { None }
        }
        TypeKind::Trait(trt_name) => {
            if !trait_names.contains(&trt_name.get_str()) { return None }
            let generic_map = &typ.ref_children()[0];
            for child in unwrap(&generic_map.children) {
                if matches!(&child.kind, TypeKind::AssociatedType(name) if name == associated_type_name) {
                    return Some(child.ref_children()[0].clone());
                }
            }
            None
        }
        _ => todo!()
    }
}
