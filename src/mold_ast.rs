use pretty_print_tree::PrettyPrintTree;
use crate::ast_structure::{Ast, AstNode, Param};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{Type, TypeKind, UNKNOWN_TYPE};

// todo I think it allows to use any type of closing )}]

pub fn construct_ast(
    tokens: &Vec<SolidToken>, pos: usize, ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    let res = make_ast_statement(tokens, pos, vec![Ast::new(AstNode::Module)], 0, 0, ppt);
    println!("\n\n{}", ppt.to_str(&(res.1.clone(), 0)));
    res
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
            },
            SolidToken::While => {
                let (p, t) = if_while_expression(false, tokens, pos, tree, parent, indent, ppt);
                pos = p;
                tree = t;
            },
            SolidToken::If => {
                let (p, t) = if_while_expression(true, tokens, pos, tree, parent, indent, ppt);
                pos = p;
                tree = t;
            },
            SolidToken::Elif => {
                let last = get_last(&mut tree[parent].children);
                if let AstNode::IfStatement = tree[last].value {
                    let (p, t) = if_while_expression(true, tokens, pos, tree, last, indent, ppt);
                    pos = p;
                    tree = t;
                } else {
                    panic!("elif to unknown if")
                }
            },
            SolidToken::Else => {
                if let SolidToken::Colon = tokens[pos + 1] {
                    pos += 1;
                } else {
                    panic!("expected colon")
                }
                let last = get_last(&mut tree[parent].children);
                if let AstNode::IfStatement = tree[last].value {
                    add_to_tree(last, &mut tree, Ast::new(AstNode::Body));
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
            SolidToken::Pass => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Pass));
            },
            SolidToken::Word(st) => {
                add_to_tree(
                    parent, &mut tree,
                    Ast::new(AstNode::Identifier(st.clone()))
                );

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
                        SolidToken::Operator(OperatorType::Eq)
                        | SolidToken::Colon => AstNode::Assignment,
                        _ => panic!("Unexpected token {:?}", next)
                    });
                    if let SolidToken::Colon = next {
                        pos -= 1;
                        let params = get_params(&tokens, &mut pos).remove(0); // 5 for now only taking the first
                        add_to_tree(
                            tree.len() - 1, &mut tree,
                            Ast::new(AstNode::Type(params.typ))
                        );
                    }
                    if let SolidToken::Operator(OperatorType::Eq)
                        | SolidToken::Colon = next {
                        let (p, t) = make_ast_expression(
                            tokens, pos + 1, tree, index, ppt
                        );
                        pos = p - 1;
                        tree = t;
                        break
                    } else if let SolidToken::Parenthesis(IsOpen::True) = next {
                        add_to_tree(index, &mut tree, Ast{
                            children: None,
                            parent: Some(indent),
                            value: AstNode::Args
                        });
                        let last = tree.len() - 1;
                        let (p, t) = make_ast_expression(
                            tokens, pos + 1, tree, last, ppt
                        );
                        pos = p;
                        tree = t;
                        while let SolidToken::Comma = tokens[pos] {
                            let (p, t) = make_ast_expression(
                                tokens, pos + 1, tree, last, ppt
                            );
                            pos = p;
                            tree = t;
                        }
                        break
                        // 5 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
            // SolidToken::Int(num) => {
            //     add_to_tree(parent, &mut tree, Ast::new(AstNode::Number(num.clone())));
            // },
            _ => panic!("unexpected token `{:?}`", token)
        }
        pos += 1;
    }
    (pos, tree)
}

fn make_ast_expression(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    // println!("{}\n", ppt.to_str(&(tree.clone(), 0)));

    let mut amount_of_open = 0;
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Parenthesis(IsOpen::True) => {
                amount_of_open += 1;
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Parentheses));
                let len = tree.len();
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, len - 1, ppt);
                pos = p - 1;
                tree = t;
            },
            SolidToken::Bracket(IsOpen::True) =>{
                amount_of_open += 1;
                if let SolidToken::Parenthesis(IsOpen::False)
                    | SolidToken::Bracket(IsOpen::False)
                    | SolidToken::Brace(IsOpen::False)
                    | SolidToken::Str(_)
                    | SolidToken::Word(_) = &tokens[pos - 1] {
                    //1 index
                    let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Index);
                    let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, ppt);
                    pos = p - 1;
                    tree = t;
                } else {
                    //1 list-literal or comprehension
                    add_to_tree(parent, &mut tree, Ast::new(AstNode::ListLiteral));
                    let list_parent = tree.len() - 1;
                    while let SolidToken::Comma | SolidToken::Bracket(IsOpen::True) = &tokens[pos] {
                        let (p, t) = make_ast_expression(tokens, pos + 1, tree, list_parent, ppt);
                        pos = p;
                        tree = t;
                    }
                    pos -= 1;
                }
            },
            SolidToken::Brace(IsOpen::True) => {
                amount_of_open += 1;
            },
            SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Bracket(IsOpen::False)
            | SolidToken::Brace(IsOpen::False) => {
                amount_of_open -= 1;
                if amount_of_open == -1 { break }
            },
            SolidToken::NewLine if amount_of_open == 0 => break,
            SolidToken::Colon | SolidToken::Comma => break,
            SolidToken::Int(num) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Number(num.clone())));
            },
            SolidToken::Word(word) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Identifier(word.clone())));
            },
            SolidToken::UnaryOperator(op) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::UnaryOp(op.clone())));
                let index = tree.len() - 1;
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, ppt);
                pos = p - 1;
                tree = t;
            },
            SolidToken::Operator(op) => {
                let mut parent = parent;
                while let AstNode::Operator(prev_op) | AstNode::UnaryOp(prev_op) = &tree[parent].value {
                    if !matches!(&tree[parent].value, AstNode::UnaryOp(_))
                    && prev_op.get_priority() < op.get_priority() { break }
                    parent = tree[parent].parent.unwrap();
                }
                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Operator(op.clone()));
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, ppt);
                pos = p - 1;
                tree = t;
            },
            _ => panic!("unexpected token {:?}", token)
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

fn if_while_expression(
    is_if: bool, tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    indent: usize, ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut tree, Ast::new(
        if is_if { AstNode::IfStatement } else { AstNode::WhileStatement }
    ));
    let len = tree.len();
    //4 condition:
    add_to_tree(len - 1, &mut tree, Ast::new(AstNode::ColonParentheses));
    let (p, t) = make_ast_expression(
        tokens, pos + 1, tree, len, ppt
    );
    pos = p; tree = t;
    //4 body:
    add_to_tree(len - 1, &mut tree, Ast::new(AstNode::Body));
    let len = tree.len() + 0;
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

fn add_to_tree(parent: usize, tree: &mut Vec<Ast>, mut new_node: Ast) {
    new_node.parent = Some(parent);
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
                if let SolidToken::Parenthesis(IsOpen::False)
                    | SolidToken::Operator(OperatorType::Eq) = tokens[*pos] {
                    return params
                }
            },
            SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Operator(OperatorType::Eq) => return params,
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
            SolidToken::Bracket(IsOpen::True) => {
                amount_of_open += 1;
                if let Some(typ) = res {
                    *pos += 1;
                    let mut children = vec![typ, get_arg_typ(tokens, pos)];
                    let mut typ = Type{
                        kind: TypeKind::TypWithGenerics,
                        children: None
                    };
                    while let SolidToken::Comma = &tokens[*pos] {
                        *pos += 1;
                        children.push(get_arg_typ(tokens, pos));
                    }
                    typ.children = Some(children);
                    res = Some(typ);
                    break
                }
            },
            SolidToken::Brace(IsOpen::True)
            | SolidToken::Parenthesis(IsOpen::True) => amount_of_open += 1,
            SolidToken::Brace(IsOpen::False)
            | SolidToken::Bracket(IsOpen::False)
            | SolidToken::Parenthesis(IsOpen::False) => {
                amount_of_open -= 1;
                if amount_of_open == 0 { break }
            },
            SolidToken::Comma
            | SolidToken::Colon
            | SolidToken::Operator(OperatorType::Eq)=> {
                if amount_of_open == 1 { break }
                else {
                    panic!("unexpected token {:?}, at {}", tokens[*pos], *pos)
                }
            },
            SolidToken::Operator(OperatorType::Or) => {
                if let Some(typ) = res {
                    *pos += 1;
                    res = Some(typ.add_option(get_arg_typ(tokens, pos)));
                    break
                } else {
                    panic!("need a value before |")
                }
            }
            SolidToken::Word(wrd) => {
                if let Some(_) = res {
                    panic!("unexpected type. res: {:?}, wrd: {}", res, wrd)
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