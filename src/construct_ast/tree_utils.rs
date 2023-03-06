use pretty_print_tree::PrettyPrintTree;
use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::{IGNORE_ENUMS, IGNORE_FUNCS, IGNORE_STRUCTS, IGNORE_TRAITS, some_vec};
use crate::types::unwrap_u;

#[inline]
pub fn get_last(arr: &Option<Vec<usize>>) -> usize {
    *arr.as_ref().unwrap().last().unwrap()
}

pub fn insert_as_parent_of_prev(ast: &mut Vec<Ast>, parent: usize, value: AstNode) -> usize {
    let index = get_last(&ast[parent].children);
    ast.insert(index, Ast {
        value,
        children: some_vec![index + 1],
        parent: Some(parent),
        typ: None,
        is_mut: true
    });
    for (i, node) in ast.iter_mut().enumerate() {
        if i == index {
            continue
        }
        if let Some(children) = &node.children {
            node.children = Some(children.iter().map(|x|
                if *x > index {
                    x + 1
                } else { *x }
            ).collect())
        }
        if let Some(parent) = &node.parent {
            if *parent >= index {
                node.parent = Some(parent + 1);
            }
        }
    }
    ast[index + 1].parent = Some(index);
    index
}

pub fn insert_as_parent_of(ast: &mut Vec<Ast>, node: usize, value: AstNode) -> usize {
    let parent = ast[node].parent.unwrap();
    let index_in_parent = ast[parent].children.as_ref().unwrap().iter()
        .position(|x| *x == node).unwrap();
    ast[parent].children.as_mut().unwrap()[index_in_parent] = ast.len();
    
    ast.push(Ast {
        value,
        children: some_vec![node],
        parent: Some(parent),
        typ: None,
        is_mut: true
    });
    ast.len() - 1
}

pub fn add_to_tree(parent: usize, ast: &mut Vec<Ast>, mut new_node: Ast) -> usize {
    new_node.parent = Some(parent);
    ast.push(new_node);
    let pos = ast.len() - 1;
    if let Some(children) = &mut ast[parent].children {
        children.push(pos)
    } else {
        ast[parent].children = some_vec![pos]
    }
    pos
}

pub fn extend_tree(ast: &mut Vec<Ast>, parent: usize, mut other: Vec<Ast>) {
    let ast_len = ast.len();
    for node in other.iter_mut() {
        if let Some(p) = &mut node.parent {
            *p += ast_len;
        } else {
            node.parent = Some(ast_len - 1);
        };
        if let Some(ch) = &mut node.children {
            ch.iter_mut().for_each(|i| *i += ast_len);
        }
    }
    ast.extend(other);
    if let Some(ch) = &mut ast[parent].children {
        ch.push(ast_len);
    } else {
        ast[parent].children = Some(vec![ast_len])
    }
}

pub fn clone_sub_tree(ast: &[Ast], head: usize, exclude_children: Option<usize>) -> Vec<Ast> {
    fn add_node(ast: &[Ast], res: &mut Vec<Ast>, pos: usize, parent: Option<usize>, exclude: Option<usize>) {
        let index = res.len();
        let node = Ast {
            parent,
            children: None,
            ..ast[pos].clone()
        };
        res.push(node);
        let mut children = vec![];
        for i in unwrap_u(&ast[pos].children) {
            children.push(res.len());

            if matches!(exclude, Some(pos) if pos == *i) {
                let node = Ast {
                    parent,
                    children: None,
                    ..ast[*i].clone()
                };
                res.push(node);
                continue
            }
            add_node(ast, res, *i, Some(index), exclude);
        }
        if !children.is_empty() {
            res[index].children = Some(children);
        }
    }
    let mut res = vec![];
    add_node(ast, &mut res, head, None, exclude_children);
    res
}

pub fn print_tree(ast: &[Ast], pos: usize){
    let ppt = {
        PrettyPrintTree::<(&[Ast], usize)>::new(
            Box::new(|(vc, pos)| {
                if let Some(t) = &vc[*pos].typ {
                    format!("{pos}. {}\n:{t}\n({:?})\n[{}]", vc[*pos].value, vc[*pos].parent, vc[*pos].is_mut)
                } else {
                    format!("{pos}. {}\n({:?})\n[{}]", vc[*pos].value, vc[*pos].parent, vc[*pos].is_mut)
                }
            }),
            Box::new(|(vc, pos)| {
                let children = vc.get(*pos).unwrap().clone().children;
                if children.is_none() {
                    return Vec::new();
                }
                children.unwrap().iter().map(|x| (*vc, *x)).filter(
                    |(_, x)|
                        match &vc[*x].value {
                            AstNode::StaticFunction(name)
                            | AstNode::Function(name) =>    !unsafe { IGNORE_FUNCS  .contains(name.as_str()) },
                            AstNode::Struct(name) =>        !unsafe { IGNORE_STRUCTS.contains(name.as_str()) },
                            AstNode::Trait{name, .. } =>    !unsafe { IGNORE_TRAITS .contains(name.as_str()) },
                            AstNode::Enum(name) =>          !unsafe { IGNORE_ENUMS  .contains(name.as_str()) },
                            _ => true
                        }
                ).collect()
            }),
        )
    };
    println!("{}\n", ppt.to_str(&(ast, pos)));
}

