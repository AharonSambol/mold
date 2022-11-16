use pretty_print_tree::PrettyPrintTree;
use crate::ast_structure::{Ast, AstNode, Param};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{Type, UNKNOWN_TYPE};

// todo I think it allows to use any type of closing )}]

pub fn construct_ast(
    tokens: &Vec<SolidToken>, pos: usize, ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    make_ast_statement(tokens, pos, vec![Ast::new(AstNode::Module)], 0, 0, ppt)
}

fn make_ast_statement(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize, indent: usize,
    ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Def => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::new_func()));
                let len = tree.len();
                let temp = make_func(tokens, pos + 1, tree, len - 1, indent, ppt);
                pos = temp.0; tree = temp.1;
                println!("\n\n{}", ppt.to_str(&(tree.clone(), 0)));
            },
            SolidToken::If => {
                let (p, t) = if_expression(tokens, pos, tree, parent, indent, ppt);
                pos = p;
                tree = t;
            },
            SolidToken::Else => {
                if let SolidToken::Colon = tokens[pos + 1] {
                    pos += 1;
                } else {
                    panic!("expected colon")
                }
                let last = get_last(&mut tree[parent].children);
                if let AstNode::IfExpression = tree[last].value {
                    add_to_tree(last, &mut tree, Ast {
                        children: None,
                        parent: Some(last),
                        value: AstNode::Module
                    });
                    let len = tree.len();
                    let (p, t) = make_ast_statement(
                        tokens, pos + 1, tree, len - 1, indent + 1, ppt
                    );
                    pos = p - 1;
                    tree = t;
                } else {
                    panic!("else to unknown if")
                }
            },
            SolidToken::Elif => {
                let last = get_last(&mut tree[parent].children);
                if let AstNode::IfExpression = tree[last].value {
                    let (p, t) = if_expression(tokens, pos, tree, last, indent, ppt);
                    pos = p;
                    tree = t;
                } else {
                    panic!("elif to unknown if")
                }
            },
            SolidToken::Word(st) => {
                // todo only if in correct context
                add_to_tree(parent, &mut tree, Ast {
                    children: None,
                    parent: Some(parent),
                    value: AstNode::Identifier(st.clone())
                });

                loop {
                    pos += 1;
                    let next = &tokens[pos];
                    if let SolidToken::NewLine = next {
                        pos -= 1;
                        break;
                    }
                    let index = insert_as_parent_of_prev(&mut tree, parent, match next {
                        SolidToken::Period => AstNode::Property,
                        SolidToken::Parenthesis(IsOpen::True) => AstNode::FunctionCall,
                        SolidToken::Operator(OperatorType::Eq) => AstNode::Assignment,
                        _ => panic!("Unexpected token {:?}", next)
                    });

                    if let SolidToken::Parenthesis(IsOpen::True) | SolidToken::Operator(OperatorType::Eq) = next {
                        let (p, t) = make_ast_expression(
                            tokens, pos + 1, tree, index, ppt
                        );
                        pos = p - 1;
                        tree = t;
                        break
                    }
                }
            }
            SolidToken::NewLine => {
                let tabs = tokens.iter().skip(pos + 1)
                    .take_while(|&x| matches!(x, SolidToken::Tab)).count();
                if tabs < indent {
                    return (pos, tree)
                } else if tabs > indent {
                    panic!("unexpected indentation")
                }
                pos += tabs;
            },

            _ => panic!("unexpected token `{:?}`", token)
        }
        pos += 1;
    }
    (pos, tree)
}

fn get_last(arr: &mut Option<Vec<usize>>) -> usize{
    if let Some(arr) = arr {
        let last = arr.pop().unwrap();
        arr.push(last);
        last
    } else { panic!() }
}
fn get_len(arr: &Option<Vec<usize>>) -> usize {
    if let Some(arr) = arr {
        arr.len()
    } else { 0 }
}

fn if_expression(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    indent: usize, ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut tree, Ast {
        children: None,
        parent: Some(parent),
        value: AstNode::IfExpression
    });
    let len = tree.len();
    //4 condition:
    add_to_tree(len - 1, &mut tree, Ast {
        children: None,
        parent: Some(len - 1),
        value: AstNode::ColonParentheses
    });
    let (p, t) = make_ast_expression(
        tokens, pos + 1, tree, len, ppt
    );
    pos = p; tree = t;
    //4 body:
    add_to_tree(len - 1, &mut tree, Ast {
        children: None,
        parent: Some(len - 1),
        value: AstNode::Module
    });
    let len = tree.len();
    let (p, t) = make_ast_statement(
        tokens, pos + 1, tree, len - 1, indent + 1, ppt
    );
    (p - 1, t)
}

fn insert_as_parent_of_prev(tree: &mut Vec<Ast>, parent: usize, value: AstNode) -> usize {
    let index = get_last(&mut tree[parent].children);
    tree[index].parent = Some(index);
    tree.insert(index, Ast {
        value,
        children: Some(vec![index + 1]),
        parent: Some(parent)
    });
    for i in index + 1..tree.len() {
        if let Some(children) = &tree[i].children {
            tree[i].children = Some(children.iter().map(|x| x+1).collect())
        }
        if let Some(parent) = &tree[i].parent {
            if *parent >= index {
                tree[i].parent = Some(parent + 1);
            }
        }
    }
    index
}

fn make_ast_expression(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    let mut amount_of_open = 0;
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Parenthesis(IsOpen::True) | SolidToken::Bracket(IsOpen::True) | SolidToken::Brace(IsOpen::True) => {
                amount_of_open += 1;
                if let SolidToken::Parenthesis(_) = token {
                    add_to_tree(parent, &mut tree, Ast {
                        children: None,
                        parent: Some(parent),
                        value: AstNode::Parentheses
                    });
                    let len = tree.len();
                    let (p, t) = make_ast_expression(tokens, pos + 1, tree, len - 1, ppt);
                    pos = p - 1;
                    tree = t;
                }
            },
            SolidToken::Parenthesis(IsOpen::False) | SolidToken::Bracket(IsOpen::False) | SolidToken::Brace(IsOpen::False) => {
                amount_of_open -= 1;
                if amount_of_open == -1 { break }
            },
            SolidToken::NewLine if amount_of_open == 0 => break,
            SolidToken::Colon => break,
            SolidToken::Int(num) => {
                add_to_tree(parent, &mut tree, Ast {
                    children: None,
                    parent: Some(parent),
                    value: AstNode::Number(num.clone())
                });
            },
            SolidToken::Operator(op) => {
                let mut parent = parent;
                let unary = is_unary(&tree, parent);
                if !unary {
                    while let AstNode::Operator(prev_op) | AstNode::UnaryOp(prev_op) = &tree[parent].value {
                        if !matches!(&tree[parent].value, AstNode::UnaryOp(_))
                            && prev_op.get_priority() < op.get_priority() { break }
                        parent = tree[parent].parent.unwrap();
                    }
                }
                let index = if unary {
                    add_to_tree(parent, &mut tree, Ast{
                        children: None,
                        parent: Some(parent),
                        value: AstNode::UnaryOp(op.clone())
                    });
                    tree.len() - 1
                } else {
                    insert_as_parent_of_prev(&mut tree, parent, {
                        AstNode::Operator(op.clone())
                    })
                };
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, ppt);
                pos = p - 1;
                tree = t;
            }
            _ => panic!("unexpected token {:?}", token)
        }
        pos += 1;
    }
    (pos, tree)
}

fn is_unary(tree: &Vec<Ast>, parent: usize) -> bool {
    match tree[parent].value {
        AstNode::Assignment => get_len(&tree[parent].children) < 2,
        AstNode::Operator(_) => get_len(&tree[parent].children) < 2,
        AstNode::UnaryOp(_) => get_len(&tree[parent].children) == 0,
        AstNode::Parentheses => get_len(&tree[parent].children) == 0,
        AstNode::ColonParentheses => get_len(&tree[parent].children) == 0,
        _ => panic!("unexpected parent ({:?}) for operator", tree[parent].value)
    }
}

fn add_to_tree(parent: usize, tree: &mut Vec<Ast>, new_node: Ast) {
    tree.push(new_node);
    let pos = tree.len() - 1;
    if let Some(children) = &mut tree[parent].children {
        children.push(pos)
    } else {
        tree[parent].children = Some(vec![pos])
    }
}

fn make_func(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, index: usize, indent: usize,
    ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    let mut func =
        if let AstNode::Function(func) = &mut tree[index].value { func }
        else { panic!() };

    func.name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };
    pos += 1;
    if let SolidToken::Parenthesis(IsOpen::True) = tokens[pos] {} else {
        panic!("expected `(`, found {:?}", tokens[pos])
    }
    pos += 1;
    func.params = get_params(tokens, &mut pos);
    pos += 1;
    if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
        pos += 1;
        func.return_type = Some(get_arg_typ(tokens, &mut pos));
    }
    if let SolidToken::Colon = tokens[pos] {} else {
        panic!("expected colon")
    }
    pos += 1;

    make_ast_statement(tokens, pos, tree, index, indent + 1, ppt)
}

// returns where pos = the index of the closing brace
// e.g.     x, y: bool | int ) -> int:
//                           ^
fn get_params(tokens: &Vec<SolidToken>, mut pos: &mut usize) -> Vec<Param> {
    let mut params = Vec::new();
    loop {
        match &tokens[*pos] {
            SolidToken::Word(wrd) => {
                params.push(Param {
                    name: wrd.clone(),
                    typ:
                    if let SolidToken::Colon = &tokens[*pos + 1] {
                        *pos += 2;
                        get_arg_typ(tokens, &mut pos)
                    } else { UNKNOWN_TYPE }
                });
                if let SolidToken::Parenthesis(IsOpen::False) = tokens[*pos] {
                    return params
                }
            },
            _ => panic!("unexpected token {:?}", tokens[*pos])
        }
        *pos += 1;
    }
}

// returns where pos is the index of the token after the end of the type
// e.g.     x: int | bool, y: int | None) -> bool:
//                       ^              ^
fn get_arg_typ(tokens: &Vec<SolidToken>, pos: &mut usize) -> Type {
    let mut amount_of_open = 1;
    let mut res: Option<Type> = None;
    loop {
        match &tokens[*pos] {
            SolidToken::Brace(IsOpen::True) | SolidToken::Bracket(IsOpen::True) | SolidToken::Parenthesis(IsOpen::True) => amount_of_open += 1,
            SolidToken::Brace(IsOpen::False) | SolidToken::Bracket(IsOpen::False) | SolidToken::Parenthesis(IsOpen::False) => {
                amount_of_open -= 1;
                if amount_of_open == 0 { break }
            },
            SolidToken::Comma | SolidToken::Colon => {
                if amount_of_open == 1 { break }
                else {
                    panic!("unexpected token {:?}, at {}", tokens[*pos], *pos)
                    // TODO !!!!
                }
            },
            SolidToken::Operator(OperatorType::Or) => {
                if let Some(typ) = res {
                    *pos += 1;
                    res = Some(typ.add_option(get_arg_typ(tokens, pos)));
                    break
                }
            }
            SolidToken::Word(wrd) => {
                if let Some(_) = res {
                    panic!("unexpected type")
                }
                res = Some(Type::new(wrd.clone()));
            },
            _ => panic!("unexpected token {:?}, at {}", tokens[*pos], *pos)
        }
        *pos += 1;
    }
    res.unwrap_or_else(|| panic!("expected arg but no arg found"))
}


/*
fn eval(tree: &Vec<Ast>, pos: usize) -> f64 {
    let children = tree[pos].children.clone().unwrap_or(vec![]);
    match &tree[pos].value {
        AstNode::Operator(OperatorType::Plus) => eval(tree, children[0]) + eval(tree, children[1]),
        AstNode::Operator(OperatorType::Minus) => eval(tree, children[0]) - eval(tree, children[1]),
        AstNode::Operator(OperatorType::Div) => eval(tree, children[0]) / eval(tree, children[1]),
        AstNode::Operator(OperatorType::Mul) => eval(tree, children[0]) * eval(tree, children[1]),
        AstNode::UnaryOp(OperatorType::Minus) => -eval(tree, children[0]),
        AstNode::Parentheses => eval(tree, children[0]),
        AstNode::Module => eval(tree, children[0]),
        AstNode::Number(num) => num.parse::<f64>().unwrap()
    }
}
*/