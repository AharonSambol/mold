use std::collections::HashMap;
use crate::add_types::utils::get_pointer_inner;
use crate::construct_ast::ast_structure::Param;
use crate::types::{GenericType, Type, TypeKind, unwrap};

//2 only generics whose children are also T
//  as in [Generic(Of("T"))]
//               |
//        [Generic(Of("T"))]
pub fn get_function_return_type(return_type: &Option<Type>, expected_inputs: &Option<Vec<Param>>, inputs: &Option<Vec<Type>>) -> Option<Type> {
    let return_type = if let Some(x) = return_type { x } else { return None };
    if let Some(expected_inputs) = expected_inputs {
        let inputs = inputs.as_ref().unwrap();
        let mut hm = HashMap::new();
        for (ex_ipt, ipt) in expected_inputs.iter().zip(inputs) {
            map_generic_types(&ex_ipt.typ, ipt, &mut hm);
        }
        let res = apply_map_to_generic_typ(return_type, &hm);
        return Some(res);
    }
    Some(return_type.clone())
}

pub fn map_generic_types(generic: &Type, t: &Type, res: &mut HashMap<String, Type>) {
    if let TypeKind::Generic(GenericType::NoVal(name)) = &generic.kind {
        if let Some(r) = res.get(name) {
            if r != t {
                panic!("expected `{}` and `{}` to be of the same type", r, t);
            }
        } else {
            res.insert(name.clone(), t.clone());
        }
    }

    // todo these should be of the same Type::kind
    for (child1, child2) in unwrap(&generic.children).iter().zip(unwrap(&t.children)) {
        map_generic_types(child1, child2, res);
    }
}

pub fn apply_generics_from_base(return_typ: &Option<Type>, base: &Type) -> Option<Type> {
    if let Some(rt) = return_typ {
        let base = get_pointer_inner(base);

        if let Some(struct_def) = &base.children {
            if let Type { kind: TypeKind::GenericsMap, children: Some(generic_map) } = &struct_def[0] {
                let hm = HashMap::from_iter(
                    generic_map.iter().filter_map(|x|
                        if let TypeKind::Generic(GenericType::WithVal(name)) = &x.kind {
                            Some((name.clone(), x.children.as_ref().unwrap()[0].clone()))
                        } else { None }
                    )
                );
                return Some(apply_map_to_generic_typ(rt, &hm));
            }
        }
    }
    None
}

pub fn apply_map_to_generic_typ(typ: &Type, map: &HashMap<String, Type>) -> Type {
    if let TypeKind::Generic(GenericType::NoVal(name)) = &typ.kind {
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
