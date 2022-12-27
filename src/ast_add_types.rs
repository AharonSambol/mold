use std::collections::HashMap;
use crate::ast_structure::{Ast, AstNode};
use crate::mold_ast::{add_to_tree, FuncTypes, PPT, StructTypes, VarTypes};
use crate::types::{BOOL_TYPE, CHAR_TYPE, FLOAT_TYPE, GenericType, generify, INT_TYPE, MUT_STR_TYPE, STR_TYPE, Type, TypeKind, TypName, unwrap, unwrap_u};
use lazy_static::lazy_static;
use regex::Regex;
use crate::built_in_funcs::BuiltIn;
use crate::unwrap_enum;
// use crate::IS_COMPILED;
// use crate::mold_tokens::OperatorType;

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
            add_types(ast, unwrap_u(&ast[children[0]].children)[1], vars, funcs, structs, parent_struct, built_ins, ppt);
            vars.push(HashMap::new());
            let par = &ast[children[0]];
            let for_vars = &ast[unwrap_u(&par.children)[0]];
            let iter = &ast[unwrap_u(&par.children)[1]];
            if unwrap_u(&for_vars.children).len() > 1 {
                todo!()
            }
            let for_var_pos = unwrap_u(&for_vars.children)[0];
            ast[for_var_pos].typ = if let Some(
                Type {
                    kind: TypeKind::Struct(struct_name),
                    children
                }
            ) = &iter.typ {
                let func_pos = find_function_in_struct(
                    ast, structs, &struct_name.to_string(), "into_iter"
                ).unwrap();
                //1 into_iters return type
                let into_iters_return = &ast[unwrap_u(&ast[func_pos].children)[2]].typ;
                let into_iters_return = unwrap_enum!(into_iters_return, Some(x), x);
                //1 if the iter is a struct with generics then make a map of the generics and
                //  apply those generics (if they appear) to the returned value
                //  otherwise just return the value as is
                if let Some(children) = children {
                    let generics = &children[0];
                    if let Type{ kind: TypeKind::GenericsMap, children: Some(generics) } = generics {
                        let hm: HashMap<String, Type> = HashMap::from_iter(
                            generics.iter().map(|x| (
                                unwrap_enum!(&x.kind, TypeKind::Generic(GenericType::Of(name)), name.clone()),
                                unwrap(&x.children)[0].clone()
                            ))
                        );
                        Some(apply_map_to_generic_typ(into_iters_return, &hm))
                    } else { Some(into_iters_return.clone()) }
                } else {  Some(into_iters_return.clone()) }
            } else { panic!("{:?}", iter.typ) };
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
                let args_def = &ast[unwrap_u(&ast[structs[name]].children)[1]];
                let args = args_def.children.clone().unwrap();
                let supplied_vec = ast[children[1]].children.clone().unwrap();
                for (supplied, arg) in supplied_vec.iter().zip(args) {
                    map_generic_types(
                        unwrap_enum!(&ast[arg].typ, Some(x), x),
                        unwrap_enum!(&ast[*supplied].typ, Some(x), x),
                        &mut generics_map
                    );
                    // let name = if let AstNode::Identifier(n) = &ast[*arg].value { n } else { panic!() };
                    add_to_tree(names_pos, ast, Ast::new(ast[arg].value.clone()));
                }
                ast[pos].typ = Some(Type {
                    kind: TypeKind::Struct(TypName::Str(name.clone())),
                    children: Some(vec![
                        Type {
                            kind: TypeKind::GenericsMap,
                            children: if generics_map.len() == 0 { None } else { Some(
                                generics_map.iter().map(|(name, typ)| Type {
                                    kind: TypeKind::Generic(GenericType::Of(name.clone())),
                                    children: Some(vec![typ.clone()]),
                                }).collect()
                            ) },
                        }
                    ])
                });

            } else {
                todo!()
            }

        }
        AstNode::Property => {
            add_types(ast, children[0], vars, funcs, structs, parent_struct, built_ins, ppt);
            let left_kind = ast[children[0]].typ.clone().unwrap_or_else(|| panic!("{:?}", ast[children[0]].value));
            if let TypeKind::Struct(struct_name) = left_kind.kind {
                let struct_description = &ast[structs[&struct_name.to_string()]];
                // let mut typ = None;
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
                        // TODO check if has generics and if does deal with them
                        ast[pos].typ = ast[unwrap_u(&ast[func_pos].children)[1]].typ.clone();

                        // let module = &ast[unwrap_u(&struct_description.children)[1]];
                        // let mut found = false;
                        // for child_pos in unwrap_u(&module.children) {
                        //     if let AstNode::Function(name) = &ast[*child_pos].value {
                        //         if name == right {
                        //             found = true;
                        //             typ = ast[unwrap_u(&ast[*child_pos].children)[1]].typ.clone();
                        //             break;
                        //         }
                        //     } else if let AstNode::StaticFunction(name) = &ast[*child_pos].value {
                        //         if name == right {
                        //             found = true;
                        //             is_static = true;
                        //             typ = ast[unwrap_u(&ast[*child_pos].children)[1]].typ.clone();
                        //             break;
                        //         }
                        //     }
                        // }
                    }
                    _ => unreachable!()
                };
                if is_static {
                    ast[children[1]].value = AstNode::FunctionCall(true);
                }
                // ast[pos].typ = typ;

            } else { panic!("{:?}", left_kind.kind) }
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

            // if let AstNode::Operator(OperatorType::Plus) = ast[pos].value {
            //     if *t1_name == TypName::Static("String") {
            //         panic!("{t1_name}");
            //
            //     }
            // }
        }
        AstNode::ForIter | AstNode::Parentheses | AstNode::UnaryOp(_) => {
            add_types(ast, children[0], vars, funcs, structs, parent_struct, built_ins, ppt);
            ast[pos].typ = ast[children[0]].typ.clone();
        }
        AstNode::ForVars | AstNode::Pass | AstNode::Continue | AstNode::Break => {}
        AstNode::ListLiteral => {
            ast[pos].typ = Some(Type {
                kind: TypeKind::Struct(TypName::Static("Vec")),
                children: Some(vec![
                    Type {
                        kind: TypeKind::GenericsMap,
                        children: Some(vec![
                            Type {
                                kind: TypeKind::Generic(GenericType::Of(String::from("T"))),
                                children: Some(vec![
                                    generify(&children.iter().map(|x| {
                                        add_types(ast, *x, vars, funcs, structs, parent_struct, built_ins, ppt);
                                        ast[*x].typ.clone().unwrap()
                                    }).collect())
                                ]),
                            },

                        ])
                    },
                ]),
            });
        }
        AstNode::Index => {
            add_types(ast, children[0], vars, funcs, structs, parent_struct, built_ins, ppt);
            add_types(ast, children[1], vars, funcs, structs, parent_struct, built_ins, ppt);
            // TODO generics
            if let Some(Type { kind: TypeKind::Struct(struct_name), .. }) = &ast[children[0]].typ {
                let index_func = find_function_in_struct(
                    ast, structs, struct_name.get_str(), "index"
                ).unwrap_or_else(|| panic!("Didn't find `index` function in `{}`", struct_name));
                ast[pos].typ = ast[unwrap_u(&ast[index_func].children)[1]].typ.clone();
            }
        }
        AstNode::Struct(_) => {
            println!("!!!!!!");
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
    let struct_description = &ast[structs[struct_name]];
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

fn get_return_type(return_type: &Option<Type>, expected_inputs: &Option<Vec<Type>>, inputs: &Option<Vec<Type>>) -> Option<Type> {
    let return_type = if let Some(x) = return_type { x } else { return None };
    if let Some(expected_inputs) = expected_inputs {
        let inputs = unwrap_enum!(inputs, Some(x), x);
        let mut hm = HashMap::new();
        for (ex_ipt, ipt) in expected_inputs.iter().zip(inputs) {
            map_generic_types(ex_ipt, &ipt, &mut hm);
        }
        let res = apply_map_to_generic_typ(return_type, &hm);
        return Some(res);
    }
    Some(return_type.clone())
}

fn apply_map_to_generic_typ(typ: &Type, map: &HashMap<String, Type>) -> Type {
    if let TypeKind::Generic(GenericType::Of(name)) = &typ.kind {
        map[name].clone()
    } else {
        Type {
            kind: typ.kind.clone(),
            children: if let Some(v) = &typ.children {
                Some(v.iter().map(|x| apply_map_to_generic_typ(x, map)).collect())
            } else { None },
        }
    }
}

fn map_generic_types(generic: &Type, t: &Type, res: &mut HashMap<String, Type>) {
    if let TypeKind::Generic(GenericType::Of(name)) = &generic.kind {
        res.insert(name.clone(), t.clone());
    }
    for (child1, child2) in unwrap(&generic.children).iter().zip(unwrap(&t.children)) {
        map_generic_types(child1, child2, res);
    }
}
