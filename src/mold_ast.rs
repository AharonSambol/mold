use std::collections::HashSet;
use pretty_print_tree::PrettyPrintTree;
use crate::ast_structure::{Ast, AstNode, Param};
use crate::IS_COMPILED;
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{Type, TypeKind, UNKNOWN_TYPE};

// todo I think it allows to use any type of closing )}]

pub fn construct_ast(
    tokens: &Vec<SolidToken>, pos: usize, ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    let mut vars = HashSet::new();
    for (i, tok) in tokens.iter().enumerate() {
        if let SolidToken::Def = tok {
            if let SolidToken::Word(name) = &tokens[i + 1] {
                vars.insert(name.clone());
            }
        }
    }

    let res = make_ast_statement(
        tokens, pos, vec![Ast::new(AstNode::Module)], 0, 0, &mut vec![vars], ppt
    );
    println!("\n\n{}", ppt.to_str(&(res.1.clone(), 0)));
    res
}

fn make_ast_statement(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize, indent: usize,
    vars: &mut Vec<HashSet<String>>,
    ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Def => {
                vars.push(HashSet::new());
                let temp = make_func(tokens, pos + 1, tree, parent, indent, vars, ppt);
                pos = temp.0; tree = temp.1;
                vars.pop();
            },
            SolidToken::For => {
                let (p, t) = for_statement(tokens, pos + 1, tree, parent, indent, vars, ppt);
                pos = p; tree = t;
            },
            SolidToken::While => {
                let (p, t) = if_while_statement(false, tokens, pos + 1, tree, parent, indent, vars, ppt);
                pos = p; tree = t;
            },
            SolidToken::If => {
                let (p, t) = if_while_statement(true, tokens, pos + 1, tree, parent, indent, vars, ppt);
                pos = p; tree = t;
            },
            SolidToken::Elif => {
                let last = get_last(&mut tree[parent].children);
                if let AstNode::IfStatement = tree[last].value {
                    let (p, t) = if_while_statement(true, tokens, pos + 1, tree, last, indent, vars, ppt);
                    pos = p; tree = t;
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
                    vars.push(HashSet::new());
                    let len = tree.len();
                    let (p, t) = make_ast_statement(
                        tokens, pos + 1, tree, len - 1, indent + 1, vars, ppt
                    );
                    pos = p - 1;
                    tree = t;
                    vars.pop();
                } else {
                    panic!("else to unknown if")
                }
            },
            SolidToken::Pass => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Pass));
            },
            SolidToken::Word(st) => {
                let (p, t) = word_tok(
                    tokens, pos, tree, parent, indent, vars, ppt, st, false
                );
                pos = p; tree = t;
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
            SolidToken::Return => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Return));
                if let SolidToken::NewLine = tokens[pos + 1] {

                } else {
                    let len = tree.len();
                    let (p, t) = make_ast_expression(
                        tokens, pos + 1, tree, len - 1, vars, ppt
                    );
                    pos = p - 1;
                    tree = t;
                }
            },
            _ => panic!("unexpected token `{:?}`", token)
        }
        pos += 1;
    }
    (pos, tree)
}

fn make_ast_expression(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    vars: &mut Vec<HashSet<String>>,
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
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, len - 1, vars, ppt);
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
                    let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, vars, ppt);
                    pos = p - 1;
                    tree = t;
                } else {
                    //1 list-literal or comprehension
                    add_to_tree(parent, &mut tree, Ast::new(AstNode::ListLiteral));
                    let list_parent = tree.len() - 1;
                    while let SolidToken::Comma | SolidToken::Bracket(IsOpen::True) = &tokens[pos] {
                        let (p, t) = make_ast_expression(tokens, pos + 1, tree, list_parent, vars, ppt);
                        pos = p; tree = t;
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
            SolidToken::Colon | SolidToken::Comma => {
                break
            },
            SolidToken::Num(num) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Number(num.clone())));
            },
            SolidToken::Str(str) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::String(str.clone())));
            },
            SolidToken::Char(ch) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Char(ch.clone())));
            },
            // todo bool
            SolidToken::Word(wrd) => {
                if !is_in_stack(vars, wrd) {
                    panic!("use of var before assignment `{}`", wrd)
                }
                let (p, t) = word_tok(
                    tokens, pos, tree, parent, 0, vars, ppt, wrd, true
                );
                pos = p; tree = t;
            },
            SolidToken::UnaryOperator(op) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::UnaryOp(op.clone())));
                let index = tree.len() - 1;
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, vars, ppt);
                pos = p - 1; tree = t;
            },
            SolidToken::Operator(op) => {
                let mut parent = parent;
                while let AstNode::Operator(prev_op) | AstNode::UnaryOp(prev_op) = &tree[parent].value {
                    if !matches!(&tree[parent].value, AstNode::UnaryOp(_))
                        && prev_op.get_priority() < op.get_priority() { break }
                    parent = tree[parent].parent.unwrap();
                }
                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Operator(op.clone()));
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, vars, ppt);
                pos = p - 1; tree = t;
            },
            _ => panic!("unexpected token {:?}", token)
        }
        pos += 1;
    }
    (pos, tree)
}

fn word_tok(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize, indent: usize,
    vars: &mut Vec<HashSet<String>>,
    ppt: &PrettyPrintTree<(Vec<Ast>, usize)>, st: &String,
    is_expression: bool
) -> (usize, Vec<Ast>) {
    add_to_tree(
        parent, &mut tree,
        Ast::new(AstNode::Identifier(st.clone()))
    );
    loop {
        pos += 1;
        match &tokens[pos] {
            SolidToken::NewLine => { return (pos - 1, tree) },
            SolidToken::Operator(OperatorType::Eq) => {
                if is_expression { return (pos - 1, tree) }
                if !is_in_stack(vars, st) {
                    panic!("var hasn't been initialized `{}`", st)
                }

                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Assignment);
                let (p, t) = make_ast_expression(
                    tokens, pos + 1, tree, index, vars, ppt
                );
                pos = p; tree = t;
            },
            SolidToken::Colon => {
                if is_expression { return (pos - 1, tree) }
                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::FirstAssignment);
                vars.last_mut().unwrap().insert(st.clone());
                if let SolidToken::Operator(OperatorType::Eq) = tokens[pos + 1] {
                    pos += 1;
                } else {
                    pos -= 1;
                    let params = get_params(&tokens, &mut pos).remove(0); // 5 for now only taking the first
                    add_to_tree(
                        tree.len() - 1, &mut tree,
                        Ast::new(AstNode::Type(params.typ))
                    );
                }

                let (p, t) = make_ast_expression(
                    tokens, pos + 1, tree, index, vars, ppt
                );
                return (p - 1, t);
            },
            SolidToken::Parenthesis(IsOpen::True) => {
                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::FunctionCall);
                add_to_tree(index, &mut tree, Ast {
                    children: None,
                    parent: Some(indent),
                    value: AstNode::Args
                });
                let last = tree.len() - 1;
                let (p, t) = make_ast_expression(
                    tokens, pos + 1, tree, last, vars, ppt
                );
                pos = p; tree = t;
                while let SolidToken::Comma = tokens[pos] {
                    let (p, t) = make_ast_expression(
                        tokens, pos + 1, tree, last, vars, ppt
                    );
                    pos = p;
                    tree = t;
                }
                return (pos, tree);
            },
            SolidToken::Period => {
                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Property);
                if let SolidToken::Word(st) = &tokens[pos + 1] {
                    let(p, t) = word_tok(tokens, pos + 1, tree, index, indent, vars, ppt, st, is_expression);
                    pos = p; tree = t;
                } else {
                    panic!("expected word after period")
                }
            },
            _ if is_expression => return (pos - 1, tree),
            _ => panic!("Unexpected token {:?}", tokens[pos]),
        }
    }
}

fn is_in_stack(vars: &mut Vec<HashSet<String>>, st: &String) -> bool {
    if unsafe { !IS_COMPILED } {
        return true;
    }
    for frame in vars.iter().rev() {
        if frame.contains(st) {
            return true;
        }
    }
    return false;
}

fn get_last(arr: &mut Option<Vec<usize>>) -> usize{
    if let Some(arr) = arr {
        let last = arr.pop().unwrap();
        arr.push(last);
        last
    } else { panic!() }
}

fn if_while_statement(
    is_if: bool, tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    indent: usize,
    vars: &mut Vec<HashSet<String>>,
    ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut tree, Ast::new(
        if is_if { AstNode::IfStatement } else { AstNode::WhileStatement }
    ));
    let len = tree.len();
    //4 condition:
    add_to_tree(len - 1, &mut tree, Ast::new(AstNode::ColonParentheses));
    let (p, t) = make_ast_expression(
        tokens, pos, tree, len, vars, ppt
    );
    pos = p; tree = t;
    //4 body:
    add_to_tree(len - 1, &mut tree, Ast::new(AstNode::Body));
    vars.push(HashSet::new());
    let len = tree.len();
    let (p, t) = make_ast_statement(
    tokens, pos + 1, tree, len - 1, indent + 1, vars, ppt
    );
    vars.pop();
    (p - 1, t)
}

fn for_statement(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    indent: usize,
    vars: &mut Vec<HashSet<String>>,
ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut tree, Ast::new(AstNode::ForStatement));
    let loop_pos = tree.len() - 1;
    add_to_tree(loop_pos, &mut tree, Ast::new(AstNode::ForVars));
    add_to_tree(loop_pos, &mut tree, Ast::new(AstNode::ForIter));
    add_to_tree(loop_pos, &mut tree, Ast::new(AstNode::Body));
    let vars_pos = loop_pos + 1;
    let iter_pos = loop_pos + 2;
    let body_pos = loop_pos + 3;
    vars.push(HashSet::new());
    //4 vars:
    loop {
        if let SolidToken::Word(name) = &tokens[pos] {
            add_to_tree(
            vars_pos, &mut tree,
            Ast::new(AstNode::Identifier(name.clone()))
            );
            vars.last_mut().unwrap().insert(name.clone());
        } else {
            panic!("expected identifier")
        }
        pos += 2;
        match &tokens[pos - 1] {
            SolidToken::Comma => continue,
            SolidToken::In => break,
            _ => panic!("expected `in` or `,`")
        }
    }
    //4 iters:
    let (p, t) = make_ast_expression(
    tokens, pos, tree, iter_pos, vars, ppt
    );
    pos = p; tree = t;
    //4 body:
    let (p, t) = make_ast_statement(
        tokens, pos + 1, tree, body_pos, indent + 1, vars, ppt
    );
    vars.pop();
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
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize, indent: usize,
    vars: &mut Vec<HashSet<String>>,
    ppt: &PrettyPrintTree<(Vec<Ast>, usize)>
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut tree, Ast::new(AstNode::new_func()));
    let index = tree.len() - 1;

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
    for param in &func.params {
        vars.last_mut().unwrap().insert(param.name.clone());
    }
    pos += 1;
    if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
        pos += 1;
        func.return_type = Some(get_arg_typ(tokens, &mut pos));
    }
    if let SolidToken::Colon = tokens[pos] {} else {
        panic!("expected colon")
    }
    pos += 1;

    make_ast_statement(tokens, pos, tree, index, indent + 1, vars, ppt)
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
            SolidToken::Operator(OperatorType::BinOr) => {
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
