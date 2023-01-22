use pretty_print_tree::PrettyPrintTree;
use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::{IGNORE_ENUMS, IGNORE_FUNCS, IGNORE_STRUCTS, IGNORE_TRAITS, some_vec, unwrap_enum};

pub fn get_last(arr: &mut Option<Vec<usize>>) -> usize {
    *unwrap_enum!(arr).last().unwrap()
}

pub fn insert_as_parent_of_prev(ast: &mut Vec<Ast>, parent: usize, value: AstNode) -> usize {
    let index = get_last(&mut ast[parent].children);
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


pub fn print_tree(tree: (Vec<Ast>, usize)){
    let ppt = {
        PrettyPrintTree::<(Vec<Ast>, usize)>::new(
            Box::new(|(vc, pos)| {
                if let Some(t) = &vc[*pos].typ {
                    format!("{pos}. {}\n:{t}\n({:?})", vc[*pos].value, vc[*pos].parent)
                } else {
                    format!("{pos}. {}\n({:?})", vc[*pos].value, vc[*pos].parent)
                }
            }),
            Box::new(|(vc, pos)| {
                let children = vc.get(*pos).unwrap().clone().children;
                if children.is_none() {
                    return Vec::new();
                }
                children.unwrap().iter().map(|x| (vc.clone(), *x)).filter(
                    |(_, x)|
                        match &vc[*x].value {
                            AstNode::Struct(name) =>        !unsafe { IGNORE_STRUCTS.contains(name.as_str()) },
                            AstNode::Trait{name, .. } =>    !unsafe { IGNORE_TRAITS .contains(name.as_str()) },
                            AstNode::Function(name) =>      !unsafe { IGNORE_FUNCS  .contains(name.as_str()) },
                            AstNode::Enum(name) =>          !unsafe { IGNORE_ENUMS  .contains(name.as_str()) },
                            _ => true
                        }
                ).collect()
            }),
        )
    };
    println!("{}\n", ppt.to_str(&tree));
}

