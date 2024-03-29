use std::fmt::Display;
use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::construct_ast::mold_ast::{VarTypes, StructTypes, TraitTypes};
use crate::{CUR_COL, CUR_LINE};
use crate::mold_tokens::{Pos, SolidToken, SolidTokenWPos};
use crate::types::{GenericType, Type, TypeKind, unwrap_u};

pub fn find_function_in_struct(
    ast: &[Ast], structs: &StructTypes, struct_name: &str, func_name: &str, pos: usize
) -> Option<usize> {
    let struct_description = if struct_name == "Self" {
        let mut struct_pos = ast[pos].parent.unwrap();
        while !matches!(ast[struct_pos].value, AstNode::Struct(_)) {
            struct_pos = ast[struct_pos].parent.unwrap();
        }
        &ast[struct_pos]
    } else {
        &ast[structs[struct_name].pos]
    };
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

pub fn find_function_in_trait(
    ast: &[Ast], structs: &TraitTypes, struct_name: &str, func_name: &str
) -> Option<usize> {
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

// pub fn find_type_in_trait(
//     ast: &[Ast], structs: &TraitTypes, struct_name: &str, type_name: &str
// ) -> Option<usize> {
//     let struct_description = &ast[structs[struct_name].pos];
//     let struct_module = &ast[unwrap_u(&struct_description.children)[1]];
//     for child_pos in unwrap_u(&struct_module.children) {
//         if let AstNode::Type(name) = &ast[*child_pos].value {
//             if name == type_name {
//                 return Some(*child_pos);
//             }
//         }
//     }
//     None
// }

pub fn get_from_stack(vars: &VarTypes, var: &String) -> Option<(usize, usize)> {
    for frame in vars.iter().rev() {
        if frame.contains_key(var) {
            return Some(frame[var])
        }
    }
    None
}

#[inline]
pub fn add_to_stack(vars: &mut VarTypes, var: String, pos: usize) {
    vars.last_mut().unwrap().insert(var, (pos, 0));
}

pub fn get_pointer_complete_inner(mut typ: &Type) -> &Type {
    while let TypeKind::MutPointer | TypeKind::Pointer = &typ.kind {
        typ = &typ.ref_children()[0];
        if let TypeKind::Generic(GenericType::WithVal(_)) = &typ.kind {
            typ = &typ.ref_children()[0];
        } else { unreachable!() }
    }
    typ
}

pub fn try_get_pointer_inner(mut typ: &Type) -> &Type {
    if let TypeKind::MutPointer | TypeKind::Pointer = &typ.kind {
        typ = &typ.ref_children()[0];
        if let TypeKind::Generic(GenericType::WithVal(_)) = &typ.kind {
            typ = &typ.ref_children()[0];
        } else { unreachable!() }
    }
    typ
}

#[inline]
pub fn get_pointer_inner(typ: &Type) -> &Type {
    &typ.ref_children()[0].ref_children()[0]
}

#[inline]
pub fn is_float(typ: &TypeKind) -> bool {
    matches!(typ, TypeKind::Struct(name) if name == "f32" || name == "f64")
}

#[inline]
pub fn add_new_line(toks: &mut Vec<SolidTokenWPos>) {
    toks.push(SolidTokenWPos {
        tok: SolidToken::NewLine,
        pos: Pos::default()
    });
}

#[inline]
pub fn update_pos_from_token(tok: &SolidTokenWPos) {
    unsafe {
        CUR_LINE = tok.pos.start_line;
        CUR_COL = tok.pos.start_col;
    }
}


// 2 probably a bit faster if for iters of &str of String didnt do this unnecessary map
#[inline]
pub fn join<T: Display, I: Iterator<Item=T>>(lst: I, sep: &str) -> String {
    lst.map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

#[inline]
pub fn join_or<T: Display, I: Iterator<Item=T>>(lst: I) -> String {
    let mut vc = lst.map(|x| x.to_string())
        .collect::<Vec<String>>();
    let last = vc.pop().unwrap();
    format!("{}` or `{last}", vc.join("`, `"))
}