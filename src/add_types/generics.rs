use std::collections::HashMap;
use crate::add_types::polymorphism::{box_no_side_effects, try_box_no_side_effects};
use crate::add_types::utils::{get_pointer_complete_inner, join, join_or};
use crate::construct_ast::ast_structure::{Ast, Param};
use crate::construct_ast::mold_ast::{Info};
use crate::{IMPL_TRAITS, StrToType};
use crate::types::{GenericType, print_type, Type, TypeKind, unwrap};
use crate::{throw, CUR_COL, CUR_LINE, CUR_PATH, LINE_DIFF, SRC_CODE};
use crate::construct_ast::tree_utils::update_pos_from_tree_node;

//2 only generics whose children are also T
//  as in [Generic(Of("T"))]
//               |
//        [Generic(Of("T"))]
pub fn get_function_return_type(
    return_type: &Option<Type>, expected_inputs: &Option<Vec<Param>>, inputs: &Option<Vec<(Type, usize)>>,
    ast: &[Ast], info: &Info
) -> Option<Type> {
    let return_type = if let Some(x) = return_type { x } else { return None };
    if let Some(expected_inputs) = expected_inputs {
        let inputs = inputs.as_ref().unwrap();
        let mut hm = HashMap::new();
        for (ex_ipt, (ipt, pos)) in expected_inputs.iter().zip(inputs) {
            // println!("EXP:");
            // print_type(&Some(ex_ipt.typ.clone()));
            // println!("IPT:");
            // print_type(&Some(ipt.clone()));

            map_generic_types(&ex_ipt.typ, ipt, &mut hm, ast, info, *pos);
        }
        let res = apply_map_to_generic_typ(return_type, &hm);
        return Some(res);
    }
    Some(return_type.clone())
}

pub fn map_generic_types(template: &Type, got: &Type, res: &mut StrToType, ast: &[Ast], info: &Info, pos: usize) {
    // let cast_to_trait = |trt: &Type| if matches!(&trt.kind, TypeKind::Trait(_)) && !matches!(&got.kind, TypeKind::Trait(_)) {
    //     let res = implements_trait(got, trt, ast, info);
    //     if res.is_some() { Ok(res) } else { Err(format!("expected `{template}` but found `{got}`")) }
    // } else { Ok(None) };

    if let TypeKind::OneOf = &template.kind {
        if let TypeKind::OneOf = &got.kind {
            if template == got {
                // TODO
                return;
            } else {
                todo!()
            }
        }
        for option in template.children.as_ref().unwrap() {
            // let temp_got = cast_to_trait(option);
            let temp_got = try_box_no_side_effects(
                option.clone(), got, ast, info, pos
            );
            if temp_got.is_err() { continue }
            let got = temp_got.unwrap();

            let mut temp_map = HashMap::new();
            let try_map = map_generic_types_inner(option, &got, &mut temp_map);
            if try_map.is_ok() {
                res.extend(temp_map);
                return;
            }
        }
        throw!(
            "expected: `{}` but found `{}`",
            join_or(template.ref_children().iter()),
            got
        );
    }
    // let temp_got = cast_to_trait(template).unwrap_or_else(|err| throw!("{err}"));
    // let got = if let Some(g) = &temp_got { g } else { got };
    let got = box_no_side_effects(template.clone(), got, ast, info, pos);
    // if !matches_template(template.clone(), &got, ast, info) {
    //     throw!("(5) expected `{template}` but got `{got}`")
    // }
    fn map_generic_types_inner(template: &Type, got: &Type, res: &mut StrToType) -> Result<(), String> {
        if let TypeKind::Generic(GenericType::NoVal(name)) = &template.kind {
            if let Some(r) = res.get(name) {
                if r != got {
                    return Err(format!("expected `{}` and `{}` to be of the same type", r, got));
                }
            } else {
                res.insert(name.clone(), got.clone());
            }
        } else if let TypeKind::OneOf = &template.kind {
            // for option in template.children.as_ref().unwrap() {
            //     if is_castable(option, got, ast, info, false) {
            //         return Ok(())
            //     }
            // }
            // todo
            return Ok(())
        }
        let template_children = unwrap(&template.children);
        let got_children = unwrap(&got.children);
        'generics: for (tmpl_child, got_child) in template_children.iter().zip(got_children) {
            let map_child = |tmpl: &Type, res: &mut StrToType| {
                if !matches!(tmpl.kind, TypeKind::Generic(GenericType::NoVal(_))) && got_child.kind != tmpl.kind {
                    let TypeKind::OneOf = tmpl.kind else {
                        return Err(format!("(1) expected `{tmpl}` but found `{got_child}`"))
                    };
                    let mut found_some = false;
                    for option in tmpl.ref_children() {
                        if option.kind == got_child.kind {
                            found_some = true;
                            map_generic_types_inner(tmpl, got_child, res)?;
                        }
                    }

                    return if found_some { Ok(()) } else { Err(format!("(2) expected `{tmpl_child}` but found `{got_child}`")) }
                }
                map_generic_types_inner(tmpl, got_child, res)?;
                Ok(())
            };
            if let TypeKind::OneOf = tmpl_child.kind {
                if let TypeKind::OneOf = got_child.kind {
                    if tmpl_child == got_child {
                        continue
                    } else {
                        return Err(format!("(4) expected `{tmpl_child}` but found `{got_child}`"))
                    }
                }
                for option in tmpl_child.ref_children() {
                    let mut temp_hm = HashMap::new();
                    if map_child(option, &mut temp_hm).is_ok() {
                        res.extend(temp_hm);
                        continue 'generics
                    }
                }
                return Err(format!("(3) expected `{tmpl_child}` but found `{got_child}`"))
            }
            map_child(tmpl_child, res)?;
        }
        Ok(())
    }
    if let Err(err) = map_generic_types_inner(template, &got, res) {
        throw!("{}", err)
    }
}

pub fn apply_generics_from_base(return_typ: &Option<Type>, base: &Type) -> Option<Type> {
    if let Some(rt) = return_typ {
        let base = get_pointer_complete_inner(base);

        if let Some(struct_def) = &base.children {
            if let Type { kind: TypeKind::GenericsMap, children: Some(generic_map) } = &struct_def[0] {
                let hm = HashMap::from_iter(
                    generic_map.iter().filter_map(|x|
                        if let TypeKind::Generic(GenericType::WithVal(name)) = &x.kind {
                            Some((name.clone(), x.ref_children()[0].clone()))
                        } else { None }
                    )
                );
                return Some(apply_map_to_generic_typ(rt, &hm));
            }
        }
    }
    None
}

pub fn apply_map_to_generic_typ(typ: &Type, map: &StrToType) -> Type {
    if let TypeKind::Generic(GenericType::NoVal(name)) = &typ.kind {
        // println!("{name}, {map:?}");
        map[name].clone()
    } else {
        Type {
            kind: typ.kind.clone(),
            children: typ.children.as_ref().map(
                |v| v.iter().map(
                    |x| apply_map_to_generic_typ(x, map)
                ).collect()
            ),
        }
    }
}
