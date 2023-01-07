use std::collections::HashMap;
use crate::ast_structure::{Ast, AstNode};
use crate::mold_ast::{add_to_tree, FuncTypes, StructTypes, TraitTypes, VarTypes};
use crate::types::{BOOL_TYPE, CHAR_TYPE, FLOAT_TYPE, GenericType, generify, INT_TYPE, MUT_STR_TYPE, STR_TYPE, Type, TypeKind, TypName, unwrap, unwrap_u};
use lazy_static::lazy_static;
use pretty_print_tree::{Color, PrettyPrintTree};
use regex::Regex;
use crate::built_in_funcs::BuiltIn;
use crate::{some_vec, unwrap_enum, typ_with_child};

lazy_static! {
    static ref SPECIFIED_NUM_TYPE_RE: Regex = Regex::new(r"(?:[iu](?:8|16|32|64|128|size)|f32|f64)$").unwrap();
}


// todo first save all uses of each var (in mold_ast) then check if any of them are used in a function and if so assign their type
pub fn add_types(
    ast: &mut Vec<Ast>, pos: usize,
    vars: &mut VarTypes, funcs: &FuncTypes, structs: &StructTypes, traits: &TraitTypes,
    parent_struct: &Option<HashMap<String, usize>>,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>
) {
    let children = ast[pos].children.clone().unwrap_or_default();
    match &ast[pos].value {
        AstNode::ForStatement => {
            //1 add_types to for-iter (not for-vars)
            add_types(ast, unwrap_u(&ast[children[0]].children)[1], vars, funcs, structs, traits, parent_struct, built_ins);
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
                    unwrap_enum!(&iter.typ),
                );
                //1 now get the IntoIter object's `next` function's return type
                let func_pos = find_function_in_struct(
                    ast, structs, "IntoIter", "next"
                ).unwrap();
                let next_return = &ast[unwrap_u(&ast[func_pos].children)[2]].typ;
                ast[for_var_pos].typ = apply_generics_to_method_call(
                    next_return,
                    &unwrap_enum!(into_iters_return)
                )
            } else {
                panic!("{:?}", iter.typ)
            }
            vars.last_mut().unwrap().insert(
                unwrap_enum!(&ast[for_var_pos].value, AstNode::Identifier(n), n.clone()),
                for_var_pos
            );
            add_types(ast, children[1], vars, funcs, structs, traits, parent_struct, built_ins);
            vars.pop();
        }
        AstNode::Function(_) | AstNode::StaticFunction(_) | AstNode::WhileStatement | AstNode::IfStatement
        => {
            vars.push(HashMap::new());
            for child in children {
                add_types(ast, child, vars, funcs, structs, traits, parent_struct, built_ins);
            }
            vars.pop();
        }
        AstNode::Assignment | AstNode::Module | AstNode::Body | AstNode::ReturnType
        | AstNode::GenericsDeclaration | AstNode::ColonParentheses
        | AstNode::Args => {
            for child in children {
                add_types(ast, child, vars, funcs, structs, traits, parent_struct, built_ins);
            }
        }
        AstNode::Return => {
            add_types(ast, children[0], vars, funcs, structs, traits, parent_struct, built_ins);
            let mut func_pos = ast[pos].parent.unwrap();
            while !matches!(ast[func_pos].value, AstNode::Function(_) | AstNode::StaticFunction(_)) {
                func_pos = ast[func_pos].parent.unwrap();
            }
            let func_name =
                if let AstNode::Function(n) | AstNode::StaticFunction(n) = &ast[func_pos].value { //1 always true
                    n
                } else { unreachable!() };
            let return_type = if let Some(p) = ast[ast[func_pos].parent.unwrap()].parent {
                if let AstNode::Trait(_) | AstNode::Struct(_) = &ast[p].value {
                    ast[unwrap_u(&ast[func_pos].children)[2]].typ.clone()
                } else { unreachable!() }
            } else {
                funcs[func_name].output.clone()
            };

            if let Some(return_typ) = return_type {
                check_for_boxes(return_typ, ast, children[0], structs, traits, vars, funcs);
            } else {
                // doesnt return anything
                todo!()
            }


        }
        AstNode::ArgsDef => {
            for child in children {
                let name = unwrap_enum!(&ast[child].value, AstNode::Identifier(n), n);
                vars.last_mut().unwrap().insert(name.clone(), child);
            }
        }
        AstNode::Identifier(name) => {
            let from_stack = get_from_stack(vars, name);
            if let Some(x) = from_stack {
                //1 optimize: things like this could probably be references instead of clone
                ast[pos].typ = ast[x].typ.clone();
                return;
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
            add_types(ast, children[1], vars, funcs, structs, traits, parent_struct, built_ins);
            if ast[pos].typ.is_some() {
                let typ = check_for_boxes(
                    ast[pos].typ.clone().unwrap(), ast, children[1],
                    structs, traits, vars, funcs
                );
                // let new_children = unwrap_u(&ast[pos].children);
                ast[children[0]].typ = Some(typ); //ast[new_children[1]].typ.clone();
            } else {
                let typ = ast[children[1]].typ.clone();
                if typ.is_none() {
                    panic!("`{name}` needs a type annotation")
                }
                ast[children[0]].typ = typ;
            }
            vars.last_mut().unwrap().insert(name, children[0]);
        }
        AstNode::FunctionCall(_) => {
            let name = unwrap_enum!(
                &ast[children[0]].value, AstNode::Identifier(x), x, "function without identifier?"
            );
            let (input, output) =  if funcs.contains_key(name){
                (&funcs[name].input, &funcs[name].output)
            } else if built_ins.contains_key(name.as_str()) {
                (built_ins[name.as_str()].input(), built_ins[name.as_str()].output())
            } else {
                panic!("unrecognized function `{name}`")
            };

            //1 children[1] is the args
            add_types(ast, children[1], vars, funcs, structs, traits, parent_struct, built_ins);
            let args = unwrap_u(&ast[children[1]].children).clone();
            let args: Option<Vec<Type>> = if args.is_empty() { None } else {
                if let Some(expected_inputs) = input {
                    for (exp, got) in expected_inputs.iter().zip(args.iter()) {
                        check_for_boxes(
                            exp.clone(), ast, *got, structs, traits, vars, funcs
                        );
                    }
                }

                Some(args.iter().map(|idx| {
                    ast[*idx].typ.clone().unwrap()
                }).collect())
            };
            ast[pos].typ = get_function_return_type(output, input, &args);
        }
        AstNode::StructInit => {
            add_types(ast, children[1], vars, funcs, structs, traits, parent_struct, built_ins);

            if let AstNode::Identifier(name) = &ast[children[0]].value.clone() {
                let mut generics_map = HashMap::new();

                add_to_tree(pos, ast, Ast::new(AstNode::Args));
                let names_pos = ast.len() - 1;
                let args_def = &ast[unwrap_u(&ast[structs[name].pos].children)[1]];
                let args = args_def.children.clone();
                if let Some(args) = args {
                    let supplied_vec = ast[children[1]].children.clone().unwrap();
                    for (supplied, arg) in supplied_vec.iter().zip(args) {
                        map_generic_types(
                            unwrap_enum!(&ast[arg].typ),
                            unwrap_enum!(&ast[*supplied].typ),
                            &mut generics_map
                        );
                        add_to_tree(names_pos, ast, Ast::new(ast[arg].value.clone()));
                    }
                }
                ast[pos].typ = Some(typ_with_child!{
                    TypeKind::Struct(TypName::Str(name.clone())),
                    Type {
                        kind: TypeKind::GenericsMap,
                        children:  if generics_map.is_empty() { None } else {
                            Some(
                                unwrap_enum!(&structs[name].generics).iter().map(
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
            add_types(ast, children[0], vars, funcs, structs, traits, parent_struct, built_ins);
            let left_kind = ast[children[0]].typ.clone().unwrap_or_else(||
                panic!("{:?}", ast[children[0]].value)
            );
            match &left_kind.kind {
                TypeKind::Struct(struct_name) => {
                    let struct_description = &ast[structs[&struct_name.to_string()].pos];
                    ast[pos].typ = match &ast[children[1]].value {
                        AstNode::Identifier(right) =>
                            get_property_idf_typ(ast, &left_kind, struct_description, right, true),
                        AstNode::FunctionCall(_) => {
                            //1 add types to the args
                            let arg_pos = unwrap_u(&ast[children[1]].children)[1];
                            add_types(ast, arg_pos, vars, funcs, structs, traits, parent_struct, built_ins);

                            let (res, is_static) = get_property_method_typ(
                                ast, structs, traits, &children, &left_kind, struct_name, true
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
                    let trait_description = &ast[traits[trait_name.get_str()].pos];
                    ast[pos].typ = match &ast[children[1]].value {
                        AstNode::Identifier(right) =>
                            get_property_idf_typ(ast, &left_kind, trait_description, right, false),
                        AstNode::FunctionCall(_) => {
                            //1 add types to the args //TODO IDK if this is needed here
                            let arg_pos = unwrap_u(&ast[children[1]].children)[1];
                            add_types(ast, arg_pos, vars, funcs, structs, traits, parent_struct, built_ins);

                            get_property_method_typ(ast, structs, traits, &children, &left_kind, trait_name, false).0
                        }
                        _ => unreachable!()
                    };
                }
                _ => panic!("{:?}", left_kind)
            }
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
            add_types(ast, children[0], vars, funcs, structs, traits, parent_struct, built_ins);
            add_types(ast, children[1], vars, funcs, structs, traits, parent_struct, built_ins);

            let t1 = ast[children[0]].typ.clone().unwrap();

            let t2 = unwrap_enum!(&ast[children[1]].typ);
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
            add_types(ast, children[0], vars, funcs, structs, traits, parent_struct, built_ins);
            ast[pos].typ = ast[children[0]].typ.clone();
        }
        AstNode::ForVars | AstNode::Pass | AstNode::Continue
        | AstNode::Break | AstNode::Trait(_) | AstNode::Traits => {}
        AstNode::ListLiteral => {
            ast[pos].typ = Some(typ_with_child!{
                TypeKind::Struct(TypName::Static("Vec")),
                typ_with_child!{
                    TypeKind::GenericsMap,
                    typ_with_child!{
                        TypeKind::Generic(GenericType::Of(String::from("T"))),
                        generify(&children.iter().map(|x| {
                            add_types(ast, *x, vars, funcs, structs, traits, parent_struct, built_ins);
                            ast[*x].typ.clone().unwrap()
                        }).collect())
                    }
                }
            });
        }
        AstNode::Index => {
            add_types(ast, children[0], vars, funcs, structs, traits, parent_struct, built_ins);
            add_types(ast, children[1], vars, funcs, structs, traits, parent_struct, built_ins);
            ast[pos].typ = find_index_typ(ast, structs, traits, &children);
        }
        AstNode::Struct(_) => {
            vars.push(HashMap::new());
            add_types(ast, children[1], vars, funcs, structs, traits, parent_struct, built_ins);
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
            add_types(ast, children[2], vars, funcs, structs, traits, &parent_struct, built_ins);
            vars.pop();
        }
    }
}

fn get_property_typ(got: &Ast, ast: &[Ast], structs: &StructTypes, traits: &TraitTypes) -> Option<Type> {
    let children = unwrap_u(&got.children);
    let left_kind = ast[children[0]].typ.clone().unwrap_or_else(||
        panic!("{:?}", ast[children[0]].value)
    );
    let typ = match &left_kind.kind {
        TypeKind::Struct(struct_name) => {
            let struct_description = &ast[structs[&struct_name.to_string()].pos];
            match &ast[children[1]].value {
                AstNode::Identifier(right) =>
                    get_property_idf_typ(ast, &left_kind, struct_description, right, true),
                AstNode::FunctionCall(_) =>
                    get_property_method_typ(ast, structs, traits, children, &left_kind, struct_name, true).0,
                _ => unreachable!(),
            }
        }
        TypeKind::Trait(trait_name) => {
            let trait_description = &ast[traits[trait_name.get_str()].pos];
            match &ast[children[1]].value {
                AstNode::Identifier(right) =>
                    get_property_idf_typ(ast, &left_kind, trait_description, right, false),
                AstNode::FunctionCall(_) =>
                    get_property_method_typ(ast, structs, traits, children, &left_kind, trait_name, false).0,
                _ => unreachable!()
            }
        }
        _ => panic!("{:?}", left_kind)
    };
    typ
}

fn get_property_method_typ(
    ast: &[Ast], structs: &StructTypes, traits: &TraitTypes, children: &[usize],
    left_kind: &Type, trait_name: &TypName, is_struct: bool
) -> (Option<Type>, bool) {
    let func_call = &ast[children[1]];
    let func_name = &ast[unwrap_u(&func_call.children)[0]].value;
    let func_name = if let AstNode::Identifier(name) = func_name { name } else { panic!() };
    let func_pos = if is_struct {
        find_function_in_struct(
            ast, structs, &trait_name.to_string(), func_name
        )
    }  else {
        find_function_in_trait(
            ast, traits, &trait_name.to_string(), func_name
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
            .map(|c| c.iter().map(|x| ast[*x].typ.clone().unwrap()).collect());


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

fn get_property_idf_typ(
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

fn find_index_typ(ast: &[Ast], structs: &StructTypes, traits: &TraitTypes, children: &[usize]) -> Option<Type> {
    let index_func = if let Some(Type { kind: TypeKind::Struct(struct_name), .. }) = &ast[children[0]].typ {
        find_function_in_struct(
            ast, structs, struct_name.get_str(), "index"
        ).unwrap_or_else(|| panic!("Didn't find `index` function in `{}`", struct_name))
    } else if let Some(Type { kind: TypeKind::Trait(trait_name), .. }) = &ast[children[0]].typ {
        find_function_in_trait(
            ast, traits, trait_name.get_str(), "index"
        ).unwrap_or_else(|| panic!("Didn't find `index` function in `{}`", trait_name))
    } else {
        panic!()
    };
    let typ = &ast[unwrap_u(&ast[index_func].children)[2]].typ;
    apply_generics_to_method_call(
        typ,
        unwrap_enum!(&ast[children[0]].typ)
    )
}

fn find_function_in_struct(ast: &[Ast], structs: &StructTypes, struct_name: &str, func_name: &str) -> Option<usize> {
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
fn find_function_in_trait(ast: &[Ast], structs: &TraitTypes, struct_name: &str, func_name: &str) -> Option<usize> {
    let struct_description = &ast[structs[struct_name].pos];
    let struct_module = &ast[unwrap_u(&struct_description.children)[1]];
    for child_pos in unwrap_u(&struct_module.children) {
        if let AstNode::Function(name) | AstNode::StaticFunction(name) = &ast[*child_pos].value {
            if name == func_name {
                return Some(*child_pos);
            }
        }
    }
    None
}


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
fn check_for_boxes(
    expected: Type, ast: &mut Vec<Ast>, pos: usize,
    structs: &StructTypes, traits: &TraitTypes, vars: &VarTypes, funcs: &FuncTypes
) -> Type {
    fn is_box_typ(typ: &Type) -> bool {
        match &typ.kind {
            TypeKind::Struct(bx) => bx.get_str() == "Box",
            TypeKind::Trait(_) => true,
            _ => false
        }
    }
    fn supplied_box(
        got: &Ast, vars: &VarTypes, ast: &[Ast],
        structs: &StructTypes, traits: &TraitTypes, funcs: &FuncTypes
    ) -> bool {
        match &got.value {
            AstNode::Struct(bx) => bx == "Box",
            AstNode::Trait(_) => true,
            AstNode::Identifier(idf) => {
                let typ = get_from_stack(vars, idf).unwrap();
                let typ = ast[typ].typ.clone().unwrap();
                match &typ.kind {
                    TypeKind::Struct(name)  =>
                        if name.get_str() == "Box" { return true },
                    TypeKind::Trait(_) => return true,
                    _ => ()
                }
                false
            },
            AstNode::Index => {
                is_box_typ(
                    &find_index_typ(
                        ast, structs, traits, unwrap_u(&got.children)
                    ).unwrap()
                )
            }
            AstNode::FunctionCall(_) => {
                let func_name = unwrap_enum!(
                    &ast[unwrap_u(&got.children)[0]].value,
                    AstNode::Identifier(n), n
                );
                let return_type = unwrap_enum!(&funcs[func_name].output);
                is_box_typ(return_type)
            }
            AstNode::Property => {
                let typ = get_property_typ(got, ast, structs, traits);
                if let Some(t) = typ { is_box_typ(&t) } else { false }
            }
            _ => false
        }
    }

    let got = &ast[pos].clone();
    if is_box_typ(&expected) {
        if !supplied_box(got, vars, ast, structs, traits, funcs) {
            add_box(ast, pos);
        }
        let TypeKind::Trait(_) = &expected.kind else {
            return check_for_boxes(
                unwrap(&expected.children)[0].clone(), ast, pos, structs, traits, vars, funcs
            )
        };
        return expected;
    }
    match expected.kind.clone() {
        TypeKind::Struct(ex_name) | TypeKind::Trait(ex_name) => {
            let expected_children = unwrap(&expected.children).clone();
            let got_children = unwrap_u(&got.children).clone();
            let typ = match &got.value {
                AstNode::ListLiteral => {
                    if ex_name.get_str() != "Vec" {
                        panic!("expected: `{}` but found: `Vec`", ex_name.get_str())
                    }
                    if expected_children.len() != 1 { panic!() }
                    let got_c = if let Type{
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
                            got_c.clone(), ast, i, structs, traits, vars, funcs
                        );
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                AstNode::StructInit => {
                    if let TypeKind::Trait(_) = expected.kind {
                        let struct_name = unwrap_enum!(
                            &ast[unwrap_u(&got.children)[0]].value, AstNode::Identifier(x), x.clone()
                        );
                        let struct_pos = structs[&struct_name].pos;
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
                        let struct_pos = structs[ex_name.get_str()].pos;
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
                                exp_c, ast, *got_c, structs, traits, vars, funcs
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
                    get_property_typ(got, ast, structs, traits).unwrap()
                }
                AstNode::Index => {
                    find_index_typ(
                        ast, structs, traits, unwrap_u(&got.children)
                    ).unwrap()
                }
                AstNode::FunctionCall(_) => {
                    let func_name = unwrap_enum!(
                        &ast[unwrap_u(&got.children)[0]].value,
                        AstNode::Identifier(n), n
                    );
                    unwrap_enum!(funcs[func_name].output.clone())

                }
                AstNode::Parentheses => {
                    check_for_boxes(expected.clone(), ast, got_children[0], structs, traits, vars, funcs)
                }
                AstNode::Operator(_) | AstNode::UnaryOp(_) => {
                    for child in got_children {
                        check_for_boxes(expected.clone(), ast, child, structs, traits, vars, funcs);
                    }
                    expected.clone()  //1 got what expected, no need to panic
                },
                _ => panic!("expected: {:?}, got.kind: {:?}", expected.kind, got.value)
            };
            if typ != expected {
                panic!("expected: `{expected}` got: `{typ}`");
            }
        },
        TypeKind::Generic(_) => {}
        _ => panic!("{:?}", expected),
    }
    expected
}



//2 only generics whose children are also T
//  as in [Generic(Of("T"))]
//               |
//        [Generic(Of("T"))]
fn get_function_return_type(return_type: &Option<Type>, expected_inputs: &Option<Vec<Type>>, inputs: &Option<Vec<Type>>) -> Option<Type> {
    let return_type = if let Some(x) = return_type { x } else { return None };
    if let Some(expected_inputs) = expected_inputs {
        let inputs = unwrap_enum!(inputs);
        let mut hm = HashMap::new();
        for (ex_ipt, ipt) in expected_inputs.iter().zip(inputs) {
            map_generic_types(ex_ipt, ipt, &mut hm);
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
        children: typ.children.as_ref().map(
            |v| v.iter().map(
                |x| apply_map_to_generic_typ(x, map, false)
            ).collect()
        ),
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
                        unwrap_enum!(&x.children)[0].clone()
                    ))
                );
                return Some(apply_map_to_generic_typ(rt, &hm, true));
            }
        }
    }
    None
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

fn get_from_stack(vars: &VarTypes, var: &String) -> Option<usize> {
    for frame in vars.iter().rev() {
        if frame.contains_key(var) {
            return Some(frame[var])
        }
    }
    None
}
