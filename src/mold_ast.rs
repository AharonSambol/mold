use std::collections::HashMap;
use pretty_print_tree::PrettyPrintTree;
use crate::ast_structure::{Ast, AstNode, Param};
use crate::IS_COMPILED;
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{GenericType, INT_TYPE, ITER_TYPE, STR_TYPE, Type, TypeKind, TypName, UNKNOWN_TYPE, unwrap_u};
use crate::ast_add_types::add_types;
use crate::built_in_funcs::{BuiltIn, make_built_ins};


pub type StructTypes = HashMap<String, usize>;
pub type FuncTypes = HashMap<String, FuncType>;
pub type VarTypes = Vec<HashMap<String, usize>>;
pub type PPT = PrettyPrintTree<(Vec<Ast>, usize)>;

// todo I think it allows to use any type of closing )}]

#[derive(Clone, Debug)]
pub struct FuncType {
    pub input: Option<Vec<Type>>,
    pub output: Option<Type>
}
const UNKNOWN_FUNC_TYPE: FuncType = FuncType{ input: None, output: None };

pub fn construct_ast(
tokens: &Vec<SolidToken>, pos: usize, ppt: &PPT,
built_ins: &HashMap<&str, Box<dyn BuiltIn>>
) -> (usize, Vec<Ast>) {
    let (mut structs, mut funcs) = get_struct_and_func_names(tokens);

    let mut res = make_ast_statement(
        tokens, pos, vec![Ast::new(AstNode::Module)], 0, 0,
        &mut vec![HashMap::new()], &mut funcs, &mut structs, ppt
    );
    println!("\n\n{}", ppt.to_str(&(res.1.clone(), 0)));
    if unsafe { IS_COMPILED } {
        add_types(&mut res.1, 0, &mut vec![HashMap::new()], &funcs, &structs, &None, &built_ins, ppt);
        println!("\n\n{}", ppt.to_str(&(res.1.clone(), 0)));
    }
    res
}

fn get_struct_and_func_names(tokens: &Vec<SolidToken>) -> (StructTypes, FuncTypes){
    let mut funcs = HashMap::new();
    let mut structs = HashMap::new();

    for (i, tok) in tokens.iter().enumerate() {
        match tok {
            SolidToken::Def =>
                if let SolidToken::Word(name) = &tokens[i + 1] {
                    funcs.insert(name.clone(), UNKNOWN_FUNC_TYPE);
                },
            SolidToken::Struct =>
                if let SolidToken::Word(name) = &tokens[i + 1] {
                    structs.insert(name.clone(), 0);
                },
            _ => ()
        }
    }

    structs.insert(String::from("String"), 0);
    (structs, funcs)
}

fn make_ast_statement(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes,
    ppt: &PPT
) -> (usize, Vec<Ast>) {
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Struct => {
                vars.push(HashMap::new());
                let temp = make_struct(tokens, pos + 1, tree, parent, indent, funcs, structs, ppt);
                pos = temp.0; tree = temp.1;
                vars.pop();
            },
            SolidToken::Def | SolidToken::Static  => {
                vars.push(HashMap::new());
                let temp = make_func(tokens, pos, tree, parent, indent, vars, funcs, structs, ppt);
                pos = temp.0 - 1; tree = temp.1;
                vars.pop();
            },
            SolidToken::For => {
                let (p, t) = for_statement(tokens, pos + 1, tree, parent, indent, vars, funcs, structs, ppt);
                pos = p; tree = t;
            },
            SolidToken::While => {
                let (p, t) = if_while_statement(false, tokens, pos + 1, tree, parent, indent, vars, funcs, structs, ppt);
                pos = p; tree = t;
            },
            SolidToken::If => {
                let (p, t) = if_while_statement(true, tokens, pos + 1, tree, parent, indent, vars, funcs, structs, ppt);
                pos = p; tree = t;
            },
            SolidToken::Elif => {
                let last = get_last(&mut tree[parent].children);
                if let AstNode::IfStatement = tree[last].value {
                    let (p, t) = if_while_statement(true, tokens, pos + 1, tree, last, indent, vars, funcs, structs, ppt);
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
                    vars.push(HashMap::new());
                    let len = tree.len();
                    let (p, t) = make_ast_statement(
                        tokens, pos + 1, tree, len - 1, indent + 1, vars, funcs, structs, ppt
                    );
                    pos = p - 1; tree = t;
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
                tokens, pos, tree, parent, indent, vars, funcs, structs, ppt, st, false
                );
                pos = p; tree = t;
            }
            SolidToken::NewLine => {
                let tabs = tokens.iter().skip(pos + 1)
                    .take_while(|&x| matches!(x, SolidToken::Tab)).count();
                if tabs < indent {
                    return (pos, tree)
                } else if tabs > indent {
                    panic!("unexpected indentation, expected `{indent}` found `{tabs}`")
                }
                pos += tabs;
            },
            SolidToken::Return => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Return));
                if let SolidToken::NewLine = tokens[pos + 1] {

                } else {
                    let len = tree.len();
                    let (p, t) = make_ast_expression(
                        tokens, pos + 1, tree, len - 1, vars, funcs, structs, ppt
                    );
                    pos = p - 1;
                    tree = t;
                }
            },
            SolidToken::Continue => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Continue));
            },
            SolidToken::Break => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Break));
            }
            _ => panic!("unexpected token `{:?}`", token)
        }
        pos += 1;
    }
    (pos, tree)
}

fn make_ast_expression(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes,
    ppt: &PPT
) -> (usize, Vec<Ast>) {
    // println!("{}\n", ppt.to_str(&(tree.clone(), 0)));
    let mut amount_of_open = 0;
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Parenthesis(IsOpen::True) => {
                amount_of_open += 1;
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Parentheses));
                let par_pos = tree.len() - 1;
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, par_pos, vars, funcs, structs, ppt);
                pos = p - 1; tree = t;
            },
            SolidToken::Bracket(IsOpen::True) =>{
                amount_of_open += 1;
                if let SolidToken::Parenthesis(IsOpen::False)
                | SolidToken::Bracket(IsOpen::False)
                | SolidToken::Brace(IsOpen::False)
                | SolidToken::Str { .. }
                | SolidToken::Word(_) = &tokens[pos - 1] {
                    //1 index
                    let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Index);
                    let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, vars, funcs, structs, ppt);
                    pos = p - 1; tree = t;
                } else {
                    //1 list-literal or comprehension
                    add_to_tree(parent, &mut tree, Ast::new(AstNode::ListLiteral));
                    let list_parent = tree.len() - 1;
                    while let SolidToken::Comma | SolidToken::Bracket(IsOpen::True) = &tokens[pos] {
                        let (p, t) = make_ast_expression(tokens, pos + 1, tree, list_parent, vars, funcs, structs, ppt);
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
            SolidToken::Str { val: str, mutable } => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::String{ val: str.clone(), mutable: *mutable }));
            },
            SolidToken::Char(ch) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Char(ch.clone())));
            },
            SolidToken::Bool(bl) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::Bool(bl.clone())));
            },
            // todo bool
            SolidToken::Word(wrd) => {
                let (p, t) = word_tok(
                    tokens, pos, tree, parent, 0, vars, funcs, structs, ppt, wrd, true
                );
                pos = p; tree = t;
            },
            SolidToken::UnaryOperator(op) => {
                add_to_tree(parent, &mut tree, Ast::new(AstNode::UnaryOp(op.clone())));
                let index = tree.len() - 1;
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, vars, funcs, structs, ppt);
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
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, vars, funcs, structs, ppt);
                pos = p - 1; tree = t;
            },
            SolidToken::Period => {
                let (p, t) = add_property(&tokens, pos, tree, 0, vars, funcs, structs, ppt, true, parent);
                pos = p; tree = t;
            }
            _ => panic!("unexpected token {:?}", token)
        }
        pos += 1;
    }
    (pos, tree)
}

fn word_tok(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes,
    ppt: &PPT, st: &String,
    is_expression: bool
) -> (usize, Vec<Ast>) {

    add_to_tree(
        parent, &mut tree,
        Ast::new(AstNode::Identifier(st.clone()))
    );
    let mut identifier_pos = tree.len() - 1;
    loop {
        pos += 1;
        match &tokens[pos] {
            SolidToken::NewLine => { return (pos - 1, tree) },
            SolidToken::Operator(OperatorType::Eq) => {
                if is_expression { return (pos - 1, tree) }

                let mut parent = parent;
                while tree[parent].value.is_expression() {
                    parent = tree[parent].parent.unwrap();
                }

                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Assignment);
                let (p, t) = make_ast_expression(
                    tokens, pos + 1, tree, index, vars, funcs, structs, ppt
                );
                return (p - 1, t);
            },
            SolidToken::Colon => /*4 first assignment*/{
                if is_expression { return (pos - 1, tree) }
                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::FirstAssignment);
                identifier_pos += 1;
                vars.last_mut().unwrap().insert(st.clone(), identifier_pos);
                if let SolidToken::Operator(OperatorType::Eq) = tokens[pos + 1] {
                    pos += 1;
                } else {
                    pos -= 1;
                    let param = get_params(&tokens, &mut pos, funcs, structs).remove(0); // 5 for now only taking the first
                    tree[index].typ = Some(param.typ);
                }
                let (p, t) = make_ast_expression(
                    tokens, pos + 1, tree, index, vars, funcs, structs, ppt
                );
                return (p - 1, t);
            },
            SolidToken::Brace(IsOpen::True)
            | SolidToken::Parenthesis(IsOpen::True) => {
                let type_call = if let SolidToken::Brace(_) = tokens[pos] { AstNode::StructInit } else { AstNode::FunctionCall(false) };
                let index = insert_as_parent_of_prev(&mut tree, parent, type_call);
                add_to_tree(index, &mut tree, Ast::new(AstNode::Args));
                let last = tree.len() - 1;
                let (p, t) = make_ast_expression(
                    tokens, pos + 1, tree, last, vars, funcs, structs, ppt
                );
                pos = p; tree = t;
                while let SolidToken::Comma = tokens[pos] {
                    let (p, t) = make_ast_expression(
                        tokens, pos + 1, tree, last, vars, funcs, structs, ppt
                    );
                    pos = p; tree = t;
                }
            },
            SolidToken::Bracket(IsOpen::True) => {
                let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Index);
                let (p, t) = make_ast_expression(tokens, pos + 1, tree, index, vars, funcs, structs, ppt);
                pos = p; tree = t;
            },
            SolidToken::Period => {
                let (p, t) = add_property(&tokens, pos, tree, indent, vars, funcs, structs, ppt, is_expression, parent);
                pos = p; tree = t;
            },
            _ if is_expression => return (pos - 1, tree),
            _ => panic!("Unexpected token {:?}", tokens[pos]),
        }
    }
}

fn add_property(
    tokens: &Vec<SolidToken>, pos: usize, mut tree: Vec<Ast>, indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes,
    ppt: &PPT, is_expression: bool, mut parent: usize
) -> (usize, Vec<Ast>){
    while let AstNode::Property = tree[parent].value {
        parent = tree[parent].parent.unwrap();
    }
    let index = insert_as_parent_of_prev(&mut tree, parent, AstNode::Property);
    if let SolidToken::Word(st) = &tokens[pos + 1] {
        return word_tok(tokens, pos + 1, tree, index, indent, vars, funcs, structs, ppt, st, is_expression);
    } else {
        panic!("expected word after period")
    }
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
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes,
    ppt: &PPT
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut tree, Ast::new(
        if is_if { AstNode::IfStatement } else { AstNode::WhileStatement }
    ));
    let len = tree.len();
    //4 condition:
    add_to_tree(len - 1, &mut tree, Ast::new(AstNode::ColonParentheses));
    let (p, t) = make_ast_expression(
        tokens, pos, tree, len, vars, funcs, structs, ppt
    );
    pos = p; tree = t;
    //4 body:
    add_to_tree(len - 1, &mut tree, Ast::new(AstNode::Body));
    vars.push(HashMap::new());
    let len = tree.len();
    let (p, t) = make_ast_statement(
        tokens, pos + 1, tree, len - 1, indent + 1, vars, funcs, structs, ppt
    );
    vars.pop();
    (p - 1, t)
}

fn for_statement(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize,
    indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes,
    ppt: &PPT
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut tree, Ast::new(AstNode::ForStatement));
    let loop_pos = tree.len() - 1;
    let pars_pos = loop_pos + 1;
    let vars_pos = loop_pos + 2;
    let iter_pos = loop_pos + 3;
    let body_pos = loop_pos + 4;
    add_to_tree(loop_pos, &mut tree, Ast::new(AstNode::ColonParentheses));
    add_to_tree(pars_pos, &mut tree, Ast::new(AstNode::ForVars));
    add_to_tree(pars_pos, &mut tree, Ast::new(AstNode::ForIter));
    add_to_tree(loop_pos, &mut tree, Ast::new(AstNode::Body));
    vars.push(HashMap::new());
    //4 vars:
    loop {
        if let SolidToken::Word(name) = &tokens[pos] {
            add_to_tree(
            vars_pos, &mut tree,
            Ast::new(AstNode::Identifier(name.clone()))
            );
            vars.last_mut().unwrap().insert(name.clone(), tree.len() - 1);
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
        tokens, pos, tree, iter_pos, vars, funcs, structs, ppt
    );
    pos = p; tree = t;
    //4 body:
    let (p, t) = make_ast_statement(
        tokens, pos + 1, tree, body_pos, indent + 1, vars, funcs, structs, ppt
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
        parent: Some(parent),
        typ: None
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

pub fn add_to_tree(parent: usize, tree: &mut Vec<Ast>, mut new_node: Ast) {
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
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes,
    ppt: &PPT
) -> (usize, Vec<Ast>) {
    let is_static = matches!(&tokens[pos], SolidToken::Static);
    pos += if is_static { 2 } else { 1 };
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };

    add_to_tree(parent, &mut tree, Ast::new(
        if is_static {
            AstNode::StaticFunction(name.clone())
        } else {
            AstNode::Function(name.clone())
        }
    ));
    let index = tree.len() - 1;

    pos += 1;
    if let SolidToken::Parenthesis(IsOpen::True) = tokens[pos] {} else {
        panic!("expected `(`, found {:?}", tokens[pos])
    }
    pos += 1;
    let params = get_params(tokens, &mut pos, funcs, structs);
    add_to_tree(index, &mut tree, Ast::new(AstNode::ArgsDef));
    add_to_tree(index, &mut tree, Ast::new(AstNode::ReturnType));
    add_to_tree(index, &mut tree, Ast::new(AstNode::Body));
    let args_pos = tree.len() - 3;
    let return_pos = args_pos + 1;
    let body_pos = args_pos + 2;

    for param in &params {
        add_to_tree(args_pos, &mut tree, Ast {
            children: None, parent: Some(args_pos),
            value: AstNode::Identifier(param.name.clone()),
            typ: Some(param.typ.clone())
        });
        let identifier_pos = tree.len() - 1;
        vars.last_mut().unwrap().insert(param.name.clone(), identifier_pos);
    }

    pos += 1;
    if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
        pos += 1;
        let typ = get_arg_typ(tokens, &mut pos, funcs, structs);
        tree[return_pos].typ = Some(typ);
    }
    if let SolidToken::Colon = tokens[pos] {} else {
        panic!("expected colon, found `{:?}`", tokens[pos])
    }
    pos += 1;
    funcs.insert(name, FuncType{
        input: Some(params.iter().map(|x| x.typ.clone()).collect()),
        output: tree[return_pos].typ.clone(),
    });
    make_ast_statement(tokens, pos, tree, body_pos, indent + 1, vars, funcs, structs, ppt)
}

fn make_struct(
    tokens: &Vec<SolidToken>, mut pos: usize, mut tree: Vec<Ast>, parent: usize, indent: usize,
    funcs: &mut FuncTypes, structs: &mut StructTypes,
    ppt: &PPT
) -> (usize, Vec<Ast>) {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };

    add_to_tree(parent, &mut tree, Ast::new(AstNode::Struct(name.clone())));
    let index = tree.len() - 1;
    *structs.get_mut(&name).unwrap() = index;

    pos += 1;
    add_to_tree(index, &mut tree, Ast::new(AstNode::ArgsDef));
    add_to_tree(index, &mut tree, Ast::new(AstNode::Module));
    let args_pos = tree.len() - 2;
    let body_pos = args_pos + 1;

    if let SolidToken::Colon = tokens[pos] {} else {
        panic!("expected colon")
    }
    pos += 3 + indent;
    while let SolidToken::Word(word) = &tokens[pos] {
        pos += 1;
        if let SolidToken::Colon = &tokens[pos] {
            pos += 1;
            let typ = get_arg_typ(tokens, &mut pos, funcs, structs);
            add_to_tree(args_pos, &mut tree, Ast::new(AstNode::Identifier(word.clone())));
            tree.last_mut().unwrap().typ = Some(typ);
        } else {
            // TODO check if works
            add_to_tree(args_pos, &mut tree, Ast::new(AstNode::Identifier(word.clone())));
        }
        if let SolidToken::NewLine = tokens[pos] {} else {
            return (pos, tree)
        }
        pos += 1;
        let mut exited = false;
        for i in 0..=indent {
            if let SolidToken::Tab = tokens[pos + i] { } else {
                exited = true;
                break;
            }
        }
        if exited {
            return (pos - 1, tree)
        } else {
            pos += indent + 1;
        }
    }
    let mut struct_funcs: FuncTypes = HashMap::new();

    let mut vars = HashMap::new(); // todo insert the properties of the struct
    for child in unwrap_u(&tree[args_pos].children) {
        let arg = &tree[*child];
        vars.insert(
            if let AstNode::Identifier(n) = &arg.value { n.clone() } else { panic!() },
            child.clone()
        );
    }
    let mut vars = vec![vars];
    while let SolidToken::Def | SolidToken::Static = tokens[pos] {
        vars.push(HashMap::new());
        let temp = make_func(
            tokens, pos, tree, body_pos, indent + 1,
            &mut vars, &mut struct_funcs, structs, ppt
        );
        vars.pop();
        pos = temp.0; tree = temp.1;

        let func = &tree[*unwrap_u(&tree[body_pos].children).last().unwrap()];
        if let AstNode::Function(_) = func.value {
            let args_def_pos = unwrap_u(&func.children)[0];
            tree.push(Ast {
                children: None,
                parent: Some(args_def_pos),
                value: AstNode::Identifier(String::from("self")),
                typ: Some(Type {
                    kind: TypeKind::MutPointer,
                    children: Some(vec![Type {
                        kind: TypeKind::Struct(TypName::Str(name.clone())),
                        children: None,
                    }])
                }),
            });
            let self_pos = tree.len() - 1;
            let args_def = &mut tree[args_def_pos];
            if let Some(children) = &mut args_def.children {
                children.insert(0, self_pos);
            } else {
                args_def.children = Some(vec![self_pos]);
            }
        }
        pos += indent + 2;
        if pos > tokens.len() { break; }
    }
    pos -= indent + 2;
    (pos, tree)
}

// returns where pos = the index of the closing brace
// e.g.     x, y: bool | int ) -> int:
//                           ^
fn get_params(
    tokens: &Vec<SolidToken>, mut pos: &mut usize, funcs: &mut FuncTypes, structs: &StructTypes
) -> Vec<Param> {
    let mut params = Vec::new();
    loop {
        match &tokens[*pos] {
            SolidToken::Word(wrd) => {
                params.push(Param {
                    name: wrd.clone(),
                    typ:
                        if let SolidToken::Colon = &tokens[*pos + 1] {
                            *pos += 2;
                            get_arg_typ(tokens, &mut pos, funcs, structs)
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
//                       ^              ^        ^
fn get_arg_typ(
    tokens: &Vec<SolidToken>, pos: &mut usize, funcs: &mut FuncTypes, structs: &StructTypes
) -> Type {
    if structs.len() == 0 {
        panic!()
    }
    let mut amount_of_open = 1;
    let mut res: Option<Type> = None;
    loop {
        match &tokens[*pos] {
            SolidToken::Bracket(IsOpen::True) => {
                amount_of_open += 1;
                if let Some(typ) = res {
                    *pos += 1;
                    let mut children = vec![typ, get_arg_typ(tokens, pos, funcs, structs)];
                    while let SolidToken::Comma = &tokens[*pos] {
                        *pos += 1;
                        children.push(get_arg_typ(tokens, pos, funcs, structs));
                    }
                    *pos += 1;
                    res = Some(Type {
                        kind: TypeKind::TypWithSubTypes,
                        children: Some(children)
                    });
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
            | SolidToken::NewLine
            | SolidToken::Operator(OperatorType::Eq)=> {
                if amount_of_open == 1 { break }
                else {
                    panic!("unexpected token {:?}, at {}", tokens[*pos], *pos)
                }
            },
            SolidToken::Operator(OperatorType::BinOr) => {
                if let Some(typ) = res {
                    *pos += 1;
                    res = Some(typ.add_option(get_arg_typ(tokens, pos, funcs, structs)));
                    break
                } else {
                    panic!("need a value before |")
                }
            }
            SolidToken::Word(wrd) => {
                if let Some(_) = res {
                    panic!("unexpected type. res: {:?}, wrd: {wrd}", res)
                }
                res = if structs.contains_key(wrd) {
                    Some(Type {
                        kind: TypeKind::Struct(TypName::Str(wrd.clone())),
                        children: None,
                    })
                } else {
                    Some(Type::new(wrd.clone()))
                };
            },
            _ => panic!("unexpected token {:?}, at {}", tokens[*pos], *pos)
        }
        *pos += 1;
    }
    res.unwrap_or_else(|| panic!("expected arg but no arg found"))
}
