use std::collections::HashMap;
use crate::ast_structure::{Ast, AstNode};
use crate::mold_ast::{add_to_tree, FuncTypes, PPT, StructTypes, VarTypes};
use crate::types::{BOOL_TYPE, CHAR_TYPE, FLOAT_TYPE, generify, INT_TYPE, STR_TYPE, Type, TypeKind, TypName, unwrap, unwrap_u};
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref SPECIFIED_NUM_TYPE_RE: Regex = Regex::new(r"[iuf](8|16|32|64|128|size)$").unwrap();
}

// TODO first save all uses of each var (in mold_ast) then check if any of them are used in a function and if so assign their type
pub fn add_types(
    ast: &mut Vec<Ast>, pos: usize,
    vars: &mut VarTypes, funcs: &FuncTypes, structs: &StructTypes,
    ppt: &PPT
) {
    let children = ast[pos].children.clone().unwrap_or(vec![]);
    match &ast[pos].value {
        AstNode::ForStatement => {
            //3 ****************************************************************
            //3 **** WARNING: ugly code ahead, viewer discretion is advised ****
            //3 ****************************************************************
            add_types(ast, unwrap_u(&ast[children[0]].children)[1], vars, funcs, structs, ppt);
            vars.push(HashMap::new());
            let par = &ast[children[0]];
            let for_vars = &ast[unwrap_u(&par.children)[0]];
            let iter = &ast[unwrap_u(&par.children)[1]];
            if unwrap_u(&for_vars.children).len() > 1 {
                todo!()
            }
            let for_var_pos = unwrap_u(&for_vars.children)[0];
            ast[for_var_pos].typ = if let Some(
                Type { kind: TypeKind::TypWithSubTypes, children: Some(children) }
            ) = &iter.typ {
                Some(children[1].clone())
            } else { panic!() };
            vars.last_mut().unwrap().insert(
            if let AstNode::Identifier(n) = &ast[for_var_pos].value { n.clone() } else { panic!() },
            for_var_pos
            );
            add_types(ast, children[1], vars, funcs, structs, ppt);
            vars.pop();
        }
        AstNode::Function(_) | AstNode::WhileStatement | AstNode::IfStatement
        => {
            vars.push(HashMap::new());
            for child in children {
                add_types(ast, child, vars, funcs, structs, ppt);
            }
            vars.pop();
        }
        AstNode::Assignment | AstNode::Module | AstNode::Body | AstNode::ReturnType
        | AstNode::ColonParentheses | AstNode::Return | AstNode::Args
        => {
            for child in children {
                add_types(ast, child, vars, funcs, structs, ppt);
            }
        }
        AstNode::ArgsDef => {
            for child in children {
                let name = if let AstNode::Identifier(n) = &ast[child].value { n } else { panic!() };
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
            panic!("used `{}` before assignment", name)
        }
        AstNode::FirstAssignment => {
            let name = if let AstNode::Identifier(name) = &ast[children[0]].value { name.clone() } else { panic!() };
            add_types(ast, children[1], vars, funcs, structs, ppt);
            let typ = ast[children[1]].typ.clone();
            if let None = typ {
                panic!()
            }
            ast[children[0]].typ = typ;
            vars.last_mut().unwrap().insert(name, children[0]);
        }
        AstNode::FunctionCall => {
            if let AstNode::Identifier(name) = &ast[children[0]].value {
                ast[pos].typ = funcs[name].output.clone();
            } else {
                todo!()
            }
            add_types(ast, children[1], vars, funcs, structs, ppt);
        }
        AstNode::StructInit => {
            if let AstNode::Identifier(name) = &ast[children[0]].value.clone() {
                ast[pos].typ = Some(Type::new(name.clone()));
                add_to_tree(pos, ast, Ast::new(AstNode::Args));
                let names_pos = ast.len() - 1;
                let args_def = &ast[unwrap_u(&ast[structs[name]].children)[0]];
                let args = unwrap_u(&args_def.children);
                for arg in args.clone() {
                    // let name = if let AstNode::Identifier(n) = &ast[*arg].value { n } else { panic!() };
                    add_to_tree(names_pos, ast, Ast::new(ast[arg].value.clone()));
                }
            } else {
                todo!()
            }

            add_types(ast, children[1], vars, funcs, structs, ppt);
        }
        AstNode::Property => {
            add_types(ast, children[0], vars, funcs, structs, ppt);
            let left_kind = ast[children[0]].typ.clone().unwrap();
            let right = if let AstNode::Identifier(i) = &ast[children[1]].value { i } else { panic!() };
            if let TypeKind::Class(hm)
            | TypeKind::Struct(hm) = left_kind.kind {
                ast[pos].typ = Some(unwrap(&left_kind.children)[hm[right]].clone());
            } else { panic!() }
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
        AstNode::String(_) => {
            ast[pos].typ = Some(STR_TYPE);
        }
        AstNode::Bool(_) => {
            ast[pos].typ = Some(BOOL_TYPE);
        }
        AstNode::Operator(_) => {
            add_types(ast, children[0], vars, funcs, structs, ppt);
            add_types(ast, children[1], vars, funcs, structs, ppt);

            let t1 = ast[children[0]].typ.clone().unwrap_or_else(||
                                                                 panic!("op: {}, children: {}, {}",
                   if let AstNode::Operator(op) = &ast[pos].value { op } else { panic!() },
                    ast[children[0]].value,
                    ast[children[1]].value,
                )
            );
            let t2 = if let Some(x) = &ast[children[1]].typ { x } else { panic!() };
            let t1_name = if let TypeKind::Typ(t) = &t1.kind { t } else { panic!() };
            let t2_name = if let TypeKind::Typ(t) = &t2.kind { t } else { panic!() };
            if *t1_name != *t2_name {
                panic!("`{}` not implemented for `{}` and `{}`",
                       if let AstNode::Operator(op) = &ast[pos].value { op } else { panic!() },
                       t1_name, t2_name
                )
            }
            ast[pos].typ = Some(t1);
        }
        AstNode::ForIter | AstNode::Parentheses | AstNode::UnaryOp(_) => {
            add_types(ast, children[0], vars, funcs, structs, ppt);
            ast[pos].typ = ast[children[0]].typ.clone();
        }
        AstNode::ForVars => {}
        AstNode::Pass => {}
        AstNode::ListLiteral => {
            ast[pos].typ = Some(Type {
                kind: TypeKind::TypWithSubTypes,
                children: Some(vec![
                    Type::new(String::from("Vec")),
                    generify(&children.iter().map(|x| {
                        add_types(ast, *x, vars, funcs, structs, ppt);
                        ast[*x].typ.clone().unwrap()
                    }).collect()),
                ]),
            });
        }
        AstNode::Index => {
            add_types(ast, children[0], vars, funcs, structs, ppt);
            add_types(ast, children[1], vars, funcs, structs, ppt);
            if let Some(Type { kind: TypeKind::TypWithSubTypes, children: Some(children) }) = &ast[children[0]].typ {
                if children.len() != 1 {
                    panic!()
                }
                ast[pos].typ = Some(children[0].clone());
            } else {
                panic!()
            }
        }
        AstNode::Struct(_) => {
            add_types(ast, children[2], vars, funcs, structs, ppt);
        }
        AstNode::Functions(_) => {}
    }
}