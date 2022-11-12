use pretty_print_tree::PrettyPrintTree;
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::mold_tokens::SolidToken::{Parenthesis, Bracket, Brace, Word, Operator, Comma};
use crate::ast_structure::{Ast, AstNode, Param};
use crate::types::{Type, UNKNOWN_TYPE};


pub fn construct_ast(tokens: &Vec<SolidToken>, pos: usize, ppt: &PrettyPrintTree<(Vec<Ast>, usize)>) -> (usize, Vec<Ast>) {
    make_ast(tokens, pos, vec![Ast::new(AstNode::Module)], 0, ppt)
}

fn make_ast(tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize, ppt: &PrettyPrintTree<(Vec<Ast>, usize)>) -> (usize, Vec<Ast>) {
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            Word(st) if st == "def" => {
                let func = Ast::new(AstNode::new_func());
                add_to_tree(parent, &mut tree, func);
                let len = tree.len();
                (pos, tree) = make_func(tokens, pos + 1, tree, len - 1, ppt);

                println!("{}", ppt.to_str(&(tree.clone(), 0)));
            },
            SolidToken::NewLine => (),
            _ => panic!("unexpected token {:?}", token)
        }

    pos += 1; }
    (pos, tree)
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

fn make_func(tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, index: usize, ppt: &PrettyPrintTree<(Vec<Ast>, usize)>) -> (usize, Vec<Ast>) {
    let mut func =
        if let AstNode::Function(func) = &mut tree[index].value { func }
        else { panic!() };

    func.name =
        if let Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };
    pos += 1;
    if let Parenthesis(IsOpen::True) = tokens[pos] {} else {
        panic!("expected `(`, found {:?}", tokens[pos])
    }
    pos += 1;
    func.params = get_params(tokens, &mut pos);
    pos += 1;
    if let Operator(OperatorType::Returns) = tokens[pos] {
        pos += 1;
        func.return_type = Some(get_arg_typ(tokens, &mut pos));
    }
    if let SolidToken::Colon = tokens[pos] {} else {
        panic!("expected colon")
    }
    pos += 1;

    make_ast(tokens, pos, tree, index, ppt)
}

// returns where pos = the index of the closing brace
// e.g.     x, y: bool | int ) -> int:
//                           ^
fn get_params(tokens: &Vec<SolidToken>, mut pos: &mut usize) -> Vec<Param> {
    let mut params = Vec::new();
    loop {
        match &tokens[*pos] {
            Word(wrd) => {
                params.push(Param {
                    name: wrd.clone(),
                    typ:
                    if let SolidToken::Colon = &tokens[*pos + 1] {
                        *pos += 2;
                        get_arg_typ(tokens, &mut pos)
                    } else { UNKNOWN_TYPE }
                });
                if let Parenthesis(IsOpen::False) = tokens[*pos] {
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
            Brace(IsOpen::True) | Bracket(IsOpen::True) | Parenthesis(IsOpen::True) => amount_of_open += 1,
            Brace(IsOpen::False) | Bracket(IsOpen::False) | Parenthesis(IsOpen::False) => {
                amount_of_open -= 1;
                if amount_of_open == 0 { break }
            },
            Comma | SolidToken::Colon => {
                if amount_of_open == 1 { break }
                else {
                    panic!("unexpected token {:?}, at {}", tokens[*pos], *pos)
                    // TODO !!!!
                }
            },
            Operator(OperatorType::Or) => {
                if let Some(typ) = res {
                    *pos += 1;
                    res = Some(typ.add_option(get_arg_typ(tokens, pos)));
                    break
                }
            }
            Word(wrd) => {
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