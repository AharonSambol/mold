use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::construct_ast::get_functions_and_types::{StructTypes, TraitTypes};
use crate::construct_ast::mold_ast::VarTypes;
use crate::types::unwrap_u;

pub fn find_function_in_struct(ast: &[Ast], structs: &StructTypes, struct_name: &str, func_name: &str) -> Option<usize> {
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

pub fn find_function_in_trait(ast: &[Ast], structs: &TraitTypes, struct_name: &str, func_name: &str) -> Option<usize> {
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

pub fn get_from_stack(vars: &VarTypes, var: &String) -> Option<usize> {
    for frame in vars.iter().rev() {
        if frame.contains_key(var) {
            return Some(frame[var])
        }
    }
    None
}
