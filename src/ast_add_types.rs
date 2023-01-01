use std::collections::HashMap;
use crate::ast_structure::{Ast, AstNode};
use crate::mold_ast::{add_to_tree, FuncTypes, PPT, StructTypes, VarTypes};
use crate::types::{BOOL_TYPE, CHAR_TYPE, FLOAT_TYPE, GenericType, generify, INT_TYPE, MUT_STR_TYPE, STR_TYPE, Type, TypeKind, TypName, unwrap, unwrap_u};
use lazy_static::lazy_static;
use pretty_print_tree::{Color, PrettyPrintTree};
use regex::Regex;
use crate::built_in_funcs::BuiltIn;
use crate::{some_vec, unwrap_enum, typ_with_child};

lazy_static! {
    static ref SPECIFIED_NUM_TYPE_RE: Regex = Regex::new(r"[iuf](8|16|32|64|128|size)$").unwrap();
}


// todo first save all uses of each var (in mold_ast) then check if any of them are used in a function and if so assign their type
pub fn add_types(
    ast: &mut Vec<Ast>, pos: usize,
    vars: &mut VarTypes, funcs: &FuncTypes, structs: &StructTypes,
    parent_struct: &Option<HashMap<String, usize>>,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    ppt: &PPT
) {
    let children = ast[pos].children.clone().unwrap_or(vec![]);
    match &ast[pos].value {
        AstNode::ForStatement => {
            //1 add_types to for-iter (not for-vars)
            add_types(ast, unwrap_u(&ast[children[0]].children)[1], vars, funcs, structs, parent_struct, built_ins, ppt);
            vars.push(HashMap::new());
            let par = &ast[children[0]];
            let for_vars = &ast[unwrap_u(&par.children)[0]];
            let iter = &ast[unwrap_u(&par.children)[1]];
            if unwrap_u(&for_vars.children).len() > 1 {
                todo!()
            }
            let for_var_pos = unwrap_u(&for_vars.children)[0];
            if let Some(Type { kind: TypeKind::Struct(struct_name), .. }) = &iter.typ {
                //1 find the into_iter func in the struct and get its return ype
                let func_pos = find_function_in_struct(
                    ast, structs, &struct_name.to_string(), "into_iter"
                ).unwrap();
                let into_iters_return = &ast[unwrap_u(&ast[func_pos].children)[2]].typ;
                let into_iters_return = apply_generics_to_method_call(
                    into_iters_return,
                    unwrap_enum!(&iter.typ, Some(x), x),
                );
                //1 now get the IntoIter object's `next` function's return type
                let func_pos = find_function_in_struct(
                    ast, structs, "IntoIter", "next"
                ).unwrap();
                let next_return = &ast[unwrap_u(&ast[func_pos].children)[2]].typ;
                ast[for_var_pos].typ = apply_generics_to_method_call(
                    next_return,
                    &unwrap_enum!(into_iters_return, Some(x), x)
                )
            } else {
                panic!("{:?}", iter.typ)
            }
            vars.last_mut().unwrap().insert(
                unwrap_enum!(&ast[for_var_pos].value, AstNode::Identifier(n), n.clone()),
                for_var_pos
            );
            add_types(ast, children[1], vars, funcs, structs, parent_struct, built_ins, ppt);
            vars.pop();
        }
        AstNode::Function(_) | AstNode::StaticFunction(_) | AstNode::WhileStatement | AstNode::IfStatement
        => {
            vars.push(HashMap::new());
            for child in children {
                add_types(ast, child, vars, funcs, structs, parent_struct, built_ins, ppt);
            }
            vars.pop();
        }
        AstNode::Assignment | AstNode::Module | AstNode::Body | AstNode::ReturnType
        | AstNode::GenericsDeclaration | AstNode::ColonParentheses | AstNode::Return
        | AstNode::Args => {
            for child in children {
                add_types(ast, child, vars, funcs, structs, parent_struct, built_ins, ppt);
            }
        }
        AstNode::ArgsDef => {
            for child in children {
                let name = unwrap_enum!(&ast[child].value, AstNode::Identifier(n), n);
                vars.last_mut().unwrap().insert(name.clone(), child);
            }
        }
        AstNode::Identifier(name) => {
            for frame in vars.iter().rev(){
                if frame.contains_key(name) {
                    //1 optimize: things like this could probably be references instead of clone
                    ast[pos].typ = ast[frame[name]].typ.clone();
                    return;
                }
            }
            if structs.contains_key(name) {
                ast[pos].typ = Some(Type {
                    kind: TypeKind::Struct(TypName::Str(name.clone())),
                    children: None,
                });
                return;
            }
            if funcs.contains_key(name) {
                ast[pos].typ = Some(Type {
                    kind: TypeKind::Function(name.clone()),
                    children: None,
                });
                return;
            }
            if let Some(ps) = parent_struct {
                if ps.contains_key(name) {
                    ast[pos] = Ast{
                        children: None,
                        parent:  ast[ps[name]].parent,
                        value: AstNode::Identifier(format!("self.{name}")),
                        typ: ast[ps[name]].typ.clone(),
                        is_mut: true
                    };
                    return;
                }
            }
            panic!("used `{name}` before assignment")
        }
        AstNode::FirstAssignment => {
            let name = unwrap_enum!(&ast[children[0]].value, AstNode::Identifier(name), name.clone());
            add_types(ast, children[1], vars, funcs, structs, parent_struct, built_ins, ppt);
            if let Some(_) = ast[pos].typ {
                ast[children[0]].typ = ast[pos].typ.clone();
            } else {
                let typ = ast[children[1]].typ.clone();
                if let None = typ {
                    panic!("`{name}` needs to a type annotation")
                }
                ast[children[0]].typ = typ;
            }
            vars.last_mut().unwrap().insert(name, children[0]);
        }
        AstNode::FunctionCall(_) => {
            add_types(ast, children[1], vars, funcs, structs, parent_struct, built_ins, ppt);
            let args = unwrap_u(&ast[children[1]].children);
            let args: Option<Vec<Type>> = if args.len() == 0 { None } else {
                Some(args.iter().map(|x| ast[*x].typ.clone().unwrap()).collect())
            };
            let name = unwrap_enum!(&ast[children[0]].value, AstNode::Identifier(x), x, "function without identifier?");
            if funcs.contains_key(name){
                ast[pos].typ = get_return_type(
                    &funcs[name].output,
                    &funcs[name].input,
                    &args);
            } else if built_ins.contains_key(name.as_str()) {
                ast[pos].typ = get_return_type(
                    built_ins[name.as_str()].output(),
                    built_ins[name.as_str()].input(),
                    &args)
            } else {
                panic!("unrecognized function `{name}`")
            }
        }
        AstNode::StructInit => {
            add_types(ast, children[1], vars, funcs, structs, parent_struct, built_ins, ppt);

            if let AstNode::Identifier(name) = &ast[children[0]].value.clone() {
                let mut generics_map = HashMap::new();

                add_to_tree(pos, ast, Ast::new(AstNode::Args));
                let names_pos = ast.len() - 1;
                let args_def = &ast[unwrap_u(&ast[structs[name].pos].children)[1]];
                let args = args_def.children.clone().unwrap();
                let supplied_vec = ast[children[1]].children.clone().unwrap();
                for (supplied, arg) in supplied_vec.iter().zip(args) {
                    map_generic_types(
                        unwrap_enum!(&ast[arg].typ, Some(x), x),
                        unwrap_enum!(&ast[*supplied].typ, Some(x), x),
                        &mut generics_map
                    );
                    add_to_tree(names_pos, ast, Ast::new(ast[arg].value.clone()));
                }
                ast[pos].typ = Some(typ_with_child!{
                    TypeKind::Struct(TypName::Str(name.clone())),
                    Type {
                        kind: TypeKind::GenericsMap,
                        children:  if generics_map.len() == 0 { None } else {
                            Some(
                                structs[name].generics.clone().unwrap().iter().map(
                                    |name| typ_with_child! {
                                        TypeKind::Generic(GenericType::Of(name.clone())),
                                        generics_map[name].clone()
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
            add_types(ast, children[0], vars, funcs, structs, parent_struct, built_ins, ppt);
            let left_kind = ast[children[0]].typ.clone().unwrap_or_else(|| panic!("{:?}", ast[children[0]].value));
            if let TypeKind::Struct(struct_name) = &left_kind.kind {
                let struct_description = &ast[structs[&struct_name.to_string()].pos];
                let mut is_static = false;
                match &ast[children[1]].value {
                    AstNode::Identifier(right) => {
                        //1 element
                        let args_def = &ast[unwrap_u(&struct_description.children)[0]];
                        for child_pos in unwrap_u(&args_def.children) {
                            if let AstNode::Identifier(name) = &ast[*child_pos].value {
                                if name == right {
                                    ast[pos].typ = ast[*child_pos].typ.clone();
                                    break;
                                }
                            }
                        }
                    },
                    AstNode::FunctionCall(_) => {
                        //1 func
                        let func_call = &ast[children[1]];
                        let right = if let AstNode::Identifier(name) = &ast[unwrap_u(&func_call.children)[0]].value { name } else { panic!() };
                        let func_pos = find_function_in_struct(ast, structs, &struct_name.to_string(), right);
                        let func_pos = func_pos.unwrap_or_else(|| panic!("function `{right}` not found in struct `{struct_name}`"));
                        is_static = matches!(ast[func_pos].value, AstNode::StaticFunction(_));

                        let return_type = &ast[unwrap_u(&ast[func_pos].children)[2]].typ;
                        ast[pos].typ = if let Some(rt) =
                            apply_generics_to_method_call(return_type, &left_kind)
                            { Some(rt) } else { return_type.clone() };
                    }
                    _ => unreachable!()
                };
                if is_static {
                    ast[children[1]].value = AstNode::FunctionCall(true);
                }

            } else { panic!("{:?}", left_kind) }
        }
        AstNode::Number(num) => {
            ast[pos].typ = Some(
                if let Some(cap) = SPECIFIED_NUM_TYPE_RE.captures(num) {
                    Type::new(String::from(
                        cap.get(0).unwrap().as_str()
                    ))
                } else if num.contains('.') {
                    FLOAT_TYPE
                } else {
                    INT_TYPE
                }
            );
        }
        AstNode::Char(_) => {
            ast[pos].typ = Some(CHAR_TYPE);
        }
        AstNode::String{ mutable, .. } => {
            if *mutable {
                ast[pos].typ = Some(MUT_STR_TYPE);
            } else {
                ast[pos].typ = Some(STR_TYPE);
            }
        }
        AstNode::Bool(_) => {
            ast[pos].typ = Some(BOOL_TYPE);
        }
        AstNode::Operator(_) => {
            add_types(ast, children[0], vars, funcs, structs, parent_struct, built_ins, ppt);
            add_types(ast, children[1], vars, funcs, structs, parent_struct, built_ins, ppt);

            let t1 = ast[children[0]].typ.clone().unwrap();

            let t2 = unwrap_enum!(&ast[children[1]].typ, Some(x), x);
            let t1_name = unwrap_enum!(&t1.kind, TypeKind::Struct(t), t, "operator not valid for this type");
            let t2_name = unwrap_enum!(&t2.kind, TypeKind::Struct(t), t, "operator not valid for this type");
            if *t1_name != *t2_name && !(*t1_name == TypName::Static("String") && *t2_name  == TypName::Static("str")) {
                panic!("`{}` not implemented for `{t1_name}` and `{t2_name}`",
                    unwrap_enum!(&ast[pos].value, AstNode::Operator(op), op)
                )
            }
            ast[pos].typ = Some(t1.clone());
        }
        AstNode::ForIter | AstNode::Parentheses | AstNode::UnaryOp(_) => {
            add_types(ast, children[0], vars, funcs, structs, parent_struct, built_ins, ppt);
            ast[pos].typ = ast[children[0]].typ.clone();
        }
        AstNode::ForVars | AstNode::Pass | AstNode::Continue | AstNode::Break => {}
        AstNode::ListLiteral => {
            ast[pos].typ = Some(typ_with_child!{
                TypeKind::Struct(TypName::Static("Vec")),
                typ_with_child!{
                    TypeKind::GenericsMap,
                    typ_with_child!{
                        TypeKind::Generic(GenericType::Of(String::from("T"))),
                        generify(&children.iter().map(|x| {
                            add_types(ast, *x, vars, funcs, structs, parent_struct, built_ins, ppt);
                            ast[*x].typ.clone().unwrap()
                        }).collect())
                    }
                }
            });
        }
        AstNode::Index => {
            add_types(ast, children[0], vars, funcs, structs, parent_struct, built_ins, ppt);
            add_types(ast, children[1], vars, funcs, structs, parent_struct, built_ins, ppt);
            print_type(&ast[children[0]].typ);
            if let Some(Type { kind: TypeKind::Struct(struct_name), .. }) = &ast[children[0]].typ {
                let index_func = find_function_in_struct(
                    ast, structs, struct_name.get_str(), "index"
                ).unwrap_or_else(|| panic!("Didn't find `index` function in `{}`", struct_name));
                let typ = &ast[unwrap_u(&ast[index_func].children)[2]].typ;
                ast[pos].typ = apply_generics_to_method_call(
                    typ,
                    unwrap_enum!(&ast[children[0]].typ, Some(x), x)
                );
                print_type_b(&ast[pos].typ);
            }
        }
        AstNode::Struct(_) => {
            vars.push(HashMap::new());
            add_types(ast, children[1], vars, funcs, structs, parent_struct, built_ins, ppt);
            vars.pop();

            let mut hm = HashMap::from([(String::from("self"), pos)]);
            for child in unwrap_u(&ast[children[1]].children) {
                hm.insert(
                    unwrap_enum!(&ast[*child].value, AstNode::Identifier(name), name.clone()),
                    *child
                );
            }

            let parent_struct = Some(hm);
            vars.push(HashMap::new());
            add_types(ast, children[2], vars, funcs, structs, &parent_struct, built_ins, ppt);
            vars.pop();
        }
    }
}

fn find_function_in_struct(ast: &Vec<Ast>, structs: &StructTypes, struct_name: &str, func_name: &str) -> Option<usize> {
    let struct_description = &ast[structs[struct_name].pos];
    let struct_module = &ast[unwrap_u(&struct_description.children)[2]];
    for child_pos in unwrap_u(&struct_module.children) {
        if let AstNode::Function(name) | AstNode::StaticFunction(name) = &ast[*child_pos].value {
            if name == func_name {
                return Some(*child_pos);
            }
        }
    }
    None
}

// todo only generics whose children are also T
fn get_return_type(return_type: &Option<Type>, expected_inputs: &Option<Vec<Type>>, inputs: &Option<Vec<Type>>) -> Option<Type> {
    let return_type = if let Some(x) = return_type { x } else { return None };
    if let Some(expected_inputs) = expected_inputs {
        let inputs = unwrap_enum!(inputs, Some(x), x);
        let mut hm = HashMap::new();
        for (ex_ipt, ipt) in expected_inputs.iter().zip(inputs) {
            map_generic_types(ex_ipt, &ipt, &mut hm);
        }
        let res = apply_map_to_generic_typ(return_type, &hm, true);
        return Some(res);
    }
    Some(return_type.clone())
}

/// # is_outer should be passed as true
fn apply_map_to_generic_typ(typ: &Type, map: &HashMap<String, Type>, is_outer: bool) -> Type {
    if let Type { kind: TypeKind::Generic(GenericType::Of(name)), children } = typ {
        if generic_isnt_defined(children, name) {
            return if is_outer {
                map[name].clone()
            } else {
                typ_with_child! {
                    typ.kind.clone(),
                    if let Type { kind: TypeKind::Generic(GenericType::Of(_)), children: Some(children) } = &map[name] {
                        children[0].clone()
                    } else {
                        map[name].clone()
                    }
                }
            }
        }
    }
    Type {
        kind: typ.kind.clone(),
        children: if let Some(v) = &typ.children {
            Some(v.iter().map(|x| apply_map_to_generic_typ(x, map, false)).collect())
        } else { None },
    }
}

// feels like a hack
fn generic_isnt_defined(children: &Option<Vec<Type>>, name: &String) -> bool{
    if let Some(children) = children {
        if let TypeKind::Generic(GenericType::Of(n2)) = &children[0].kind {
            n2 == name
        } else if let TypeKind::Struct(n) = &children[0].kind {
            n.get_str() == name && children.len() == 1
        } else { false }
    } else { true }
}

fn map_generic_types(generic: &Type, t: &Type, res: &mut HashMap<String, Type>) {
    if let Type { kind: TypeKind::Generic(GenericType::Of(name)), children } = generic {
        if generic_isnt_defined(children, name) {
            res.insert(name.clone(), t.clone());
        }
    }

    // todo these should be of the same Type::kind
    for (child1, child2) in unwrap(&generic.children).iter().zip(unwrap(&t.children)) {
        map_generic_types(child1, child2, res);
    }
}

fn apply_generics_to_method_call(return_typ: &Option<Type>, base: &Type) -> Option<Type> {
    if let Some(rt) = return_typ {
        if let Some(struct_def) = &base.children {
            if let Type { kind: TypeKind::GenericsMap, children: Some(generic_map) } = &struct_def[0] {
                let hm = HashMap::from_iter(
                    generic_map.iter().map(|x| (
                        unwrap_enum!(&x.kind, TypeKind::Generic(GenericType::Of(name)), name.clone()),
                        unwrap_enum!(&x.children, Some(c), c[0].clone())
                    ))
                );
                return Some(apply_map_to_generic_typ(rt, &hm, true));
            }
        }
    }
    return None;
}


#[allow(dead_code)]
pub fn print_type(typ: &Option<Type>) {
    let ppt: PrettyPrintTree<Type> = {
        PrettyPrintTree::<Type>::new(
            Box::new(|typ| {
                format!("{:?}", typ.kind)
            }),
            Box::new(|typ| {
                unwrap(&typ.children).clone()
            }),
        )
    };
    if let Some(t) = typ {
        println!("{}", ppt.to_str(t));
    } else {
        println!("None");
    }
    println!("\n");
}

#[allow(dead_code)]
pub fn print_type_b(typ: &Option<Type>){
    let mut ppt: PrettyPrintTree<Type> = {
        PrettyPrintTree::<Type>::new(
            Box::new(|typ| {
                format!("{:?}", typ.kind)
            }),
            Box::new(|typ| {
                unwrap(&typ.children).clone()
            }),
        )
    };
    ppt.color = Color::Black;
    if let Some(t) = typ {
        println!("{}", ppt.to_str(t));
    } else {
        println!("None");
    }
    println!("\n");
}
