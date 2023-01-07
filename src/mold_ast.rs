use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use crate::ast_structure::{Ast, AstNode, Param};
use crate::{IS_COMPILED, typ_with_child, unwrap_enum, some_vec, print_tree};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{clean_type, GenericType, MUT_STR_TYPE, Type, TypeKind, TypName, UNKNOWN_TYPE, unwrap, unwrap_u};
use crate::ast_add_types::add_types;
use crate::built_in_funcs::BuiltIn;

pub type TraitTypes = HashMap<String, TraitType>;
pub type StructTypes = HashMap<String, StructType>;
pub type FuncTypes = HashMap<String, FuncType>;
pub type VarTypes = Vec<HashMap<String, usize>>;

// todo I think it allows to use any type of closing )}]

#[derive(Clone, Debug)]
pub struct StructType {
    pub generics: Option<Vec<String>>,
    pub pos: usize
}
#[derive(Clone, Debug)]
pub struct TraitType {
    pub generics: Option<Vec<String>>,
    pub pos: usize
}
#[derive(Clone, Debug)]
pub struct FuncType {
    pub input: Option<Vec<Type>>,
    pub output: Option<Type>
}
const UNKNOWN_FUNC_TYPE: FuncType = FuncType{ input: None, output: None };

pub fn construct_ast(
tokens: &Vec<SolidToken>, pos: usize,
built_ins: &HashMap<&str, Box<dyn BuiltIn>>
) -> (usize, Vec<Ast>) {
    let (mut structs, mut funcs, mut traits) = get_struct_and_func_names(tokens);
    let mut res = make_ast_statement(
        tokens, pos, vec![Ast::new(AstNode::Module)], 0, 0,
        &mut vec![HashMap::new()], &mut funcs, &mut structs, &mut traits
    );
    print_tree((res.1.clone(), 0));
    if unsafe { IS_COMPILED } {
        add_types(
            &mut res.1, 0, &mut vec![HashMap::new()],
            &funcs, &structs, &traits, &None, &built_ins
        );
        print_tree((res.1.clone(), 0));
    }
    res
}

fn get_struct_and_func_names(tokens: &Vec<SolidToken>) -> (StructTypes, FuncTypes, TraitTypes){
    let mut funcs = HashMap::new();
    let mut structs = HashMap::new();
    let mut traits = HashMap::new();

    for (i, tok) in tokens.iter().enumerate() {
        match tok {
            SolidToken::Def =>
                //1 if isnt part of a struct\trait
                if i == 0 || !(matches!(tokens[i - 1], SolidToken::Tab) ||
                    i > 1 && (matches!(tokens[i - 1], SolidToken::Static) && matches!(tokens[i - 2], SolidToken::Tab))
                ) {
                    if let SolidToken::Word(name) = &tokens[i + 1] {
                        funcs.insert(name.clone(), UNKNOWN_FUNC_TYPE);
                    }
                },
            SolidToken::Struct | SolidToken::Trait =>
                if let SolidToken::Word(name) = &tokens[i + 1] {
                    if let SolidToken::Operator(OperatorType::Smaller) = &tokens[i + 2] {
                        let mut pos = i + 3;
                        let mut generics = vec![unwrap_enum!(&tokens[pos], SolidToken::Word(w), w.clone())];
                        while let SolidToken::Comma = &tokens[pos + 1] {
                            pos += 2;
                            generics.push(unwrap_enum!(&tokens[pos], SolidToken::Word(w), w.clone()))
                        }
                        if let SolidToken::Struct = tok {
                            structs.insert(name.clone(), StructType { generics: Some(generics), pos: 0 });
                        } else {
                            traits.insert(name.clone(), TraitType { generics: Some(generics), pos: 0 });
                        }
                    } else if let SolidToken::Struct = tok {
                        structs.insert(name.clone(), StructType { generics: None, pos: 0 });
                    } else {
                        traits.insert(name.clone(), TraitType { generics: None, pos: 0 });
                    }
                },
            _ => ()
        }
    }

    //2 structs.insert(String::from("String"), (None, 0));
    (structs, funcs, traits)
}

fn make_ast_statement(
    tokens: &Vec<SolidToken>, mut pos: usize, mut ast: Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> (usize, Vec<Ast>) {
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Struct => {
                vars.push(HashMap::new());
                let temp = make_struct(tokens, pos + 1, ast, parent, indent, funcs, structs, traits);
                pos = temp.0; ast = temp.1;
                vars.pop();
            },
            SolidToken::Def | SolidToken::Static  => {
                vars.push(HashMap::new());
                let temp = make_func(tokens, pos, ast, parent, indent, vars, funcs, structs, traits);
                pos = temp.0 - 1; ast = temp.1;
                vars.pop();
            },
            SolidToken::Trait => {
                let temp = make_trait(tokens, pos + 1, ast, parent, indent, funcs, structs, traits);
                pos = temp.0; ast = temp.1;
            },
            SolidToken::For => {
                let (p, t) = for_statement(tokens, pos + 1, ast, parent, indent, vars, funcs, structs, traits);
                pos = p; ast = t;
            },
            SolidToken::While => {
                let (p, t) = if_while_statement(false, tokens, pos + 1, ast, parent, indent, vars, funcs, structs, traits);
                pos = p; ast = t;
            },
            SolidToken::If => {
                let (p, t) = if_while_statement(true, tokens, pos + 1, ast, parent, indent, vars, funcs, structs, traits);
                pos = p; ast = t;
            },
            SolidToken::Elif => {
                let last = get_last(&mut ast[parent].children);
                if let AstNode::IfStatement = ast[last].value {
                    let (p, t) = if_while_statement(true, tokens, pos + 1, ast, last, indent, vars, funcs, structs, traits);
                    pos = p; ast = t;
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
                let last = get_last(&mut ast[parent].children);
                if let AstNode::IfStatement = ast[last].value {
                    add_to_tree(last, &mut ast, Ast::new(AstNode::Body));
                    vars.push(HashMap::new());
                    let len = ast.len();
                    let (p, t) = make_ast_statement(
                        tokens, pos + 1, ast, len - 1, indent + 1, vars, funcs, structs, traits
                    );
                    pos = p - 1; ast = t;
                    vars.pop();
                } else {
                    panic!("else to unknown if")
                }
            },
            SolidToken::Pass => {
                add_to_tree(parent, &mut ast, Ast::new(AstNode::Pass));
            },
            SolidToken::Word(st) => {
                let (p, t) = word_tok(
                    tokens, pos, ast, parent, indent, vars, funcs, structs, traits, st, false
                );
                pos = p; ast = t;
            }
            SolidToken::NewLine => {
                let tabs = tokens.iter().skip(pos + 1)
                    .take_while(|&x| matches!(x, SolidToken::Tab)).count();
                if tabs < indent {
                    return (pos, ast)
                } else if tabs > indent {
                    panic!("unexpected indentation, expected `{indent}` found `{tabs}`")
                }
                pos += tabs;
            },
            SolidToken::Return => {
                add_to_tree(parent, &mut ast, Ast::new(AstNode::Return));
                if let SolidToken::NewLine = tokens[pos + 1] {

                } else {
                    let len = ast.len();
                    let (p, t) = make_ast_expression(
                        tokens, pos + 1, ast, len - 1, vars, funcs, structs, traits
                    );
                    pos = p - 1;
                    ast = t;
                }
            },
            SolidToken::Continue => {
                add_to_tree(parent, &mut ast, Ast::new(AstNode::Continue));
            },
            SolidToken::Break => {
                add_to_tree(parent, &mut ast, Ast::new(AstNode::Break));
            }
            SolidToken::IMut => {
                let st = unwrap_enum!(&tokens[pos + 1], SolidToken::Word(st), st);
                let (p, t) = word_tok(
                    tokens, pos + 1, ast, parent, indent, vars, funcs, structs, traits, st, false
                );
                pos = p; ast = t;
                let assignment = unwrap_u(&ast[parent].children).last().unwrap().clone();
                let assignment = &mut ast[assignment];
                assignment.is_mut = false;
            }
            _ => panic!("unexpected token `{:?}`", token)
        }
        pos += 1;
    }
    (pos, ast)
}

fn make_ast_expression(
    tokens: &Vec<SolidToken>, mut pos: usize, mut ast: Vec<Ast>, parent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> (usize, Vec<Ast>) {
    // println!("{}\n".to_str(&(tree.clone(), 0)));
    let mut amount_of_open = 0;
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Parenthesis(IsOpen::True) => {
                amount_of_open += 1;
                add_to_tree(parent, &mut ast, Ast::new(AstNode::Parentheses));
                let par_pos = ast.len() - 1;
                let (p, t) = make_ast_expression(tokens, pos + 1, ast, par_pos, vars, funcs, structs, traits);
                pos = p - 1; ast = t;
            },
            SolidToken::Bracket(IsOpen::True) =>{
                amount_of_open += 1;
                if let SolidToken::Parenthesis(IsOpen::False)
                | SolidToken::Bracket(IsOpen::False)
                | SolidToken::Brace(IsOpen::False)
                | SolidToken::Str { .. }
                | SolidToken::Word(_) = &tokens[pos - 1] {
                    //1 index
                    let index = insert_as_parent_of_prev(&mut ast, parent, AstNode::Index);
                    let (p, t) = make_ast_expression(tokens, pos + 1, ast, index, vars, funcs, structs, traits);
                    pos = p - 1; ast = t;
                } else {
                    //1 list-literal or comprehension
                    add_to_tree(parent, &mut ast, Ast::new(AstNode::ListLiteral));
                    let list_parent = ast.len() - 1;
                    while let SolidToken::Comma | SolidToken::Bracket(IsOpen::True) = &tokens[pos] {
                        let (p, t) = make_ast_expression(tokens, pos + 1, ast, list_parent, vars, funcs, structs, traits);
                        pos = p; ast = t;
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
                add_to_tree(parent, &mut ast, Ast::new(AstNode::Number(num.clone())));
            },
            SolidToken::Str { val: str, mutable } => {
                add_to_tree(parent, &mut ast, Ast::new(AstNode::String{ val: str.clone(), mutable: *mutable }));
            },
            SolidToken::Char(ch) => {
                add_to_tree(parent, &mut ast, Ast::new(AstNode::Char(ch.clone())));
            },
            SolidToken::Bool(bl) => {
                add_to_tree(parent, &mut ast, Ast::new(AstNode::Bool(bl.clone())));
            },
            SolidToken::Word(wrd) => {
                let (p, t) = word_tok(
                    tokens, pos, ast, parent, 0, vars, funcs, structs, traits, wrd, true
                );
                pos = p; ast = t;
            },
            SolidToken::UnaryOperator(op) => {
                add_to_tree(parent, &mut ast, Ast::new(AstNode::UnaryOp(op.clone())));
                let index = ast.len() - 1;
                let (p, t) = make_ast_expression(tokens, pos + 1, ast, index, vars, funcs, structs, traits);
                pos = p - 1; ast = t;
            },
            SolidToken::Operator(op) => {
                let mut parent = parent;
                while let AstNode::Operator(prev_op) | AstNode::UnaryOp(prev_op) = &ast[parent].value {
                    if !matches!(&ast[parent].value, AstNode::UnaryOp(_))
                        && prev_op.get_priority() < op.get_priority() { break }
                    parent = ast[parent].parent.unwrap();
                }
                let index = insert_as_parent_of_prev(&mut ast, parent, AstNode::Operator(op.clone()));
                let (p, t) = make_ast_expression(tokens, pos + 1, ast, index, vars, funcs, structs, traits);
                pos = p - 1; ast = t;
            },
            SolidToken::Period => {
                let (p, t) = add_property(&tokens, pos, ast, 0, vars, funcs, structs, traits, true, parent);
                pos = p; ast = t;
            }
            _ => panic!("unexpected token {:?}", token)
        }
        pos += 1;
    }
    (pos, ast)
}

fn word_tok(
    tokens: &Vec<SolidToken>, mut pos: usize, mut ast: Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes, st: &String,
    is_expression: bool
) -> (usize, Vec<Ast>) {
    add_to_tree(
        parent, &mut ast,
        Ast::new(AstNode::Identifier(st.clone()))
    );
    let mut identifier_pos = ast.len() - 1;
    loop {
        pos += 1;
        match &tokens[pos] {
            SolidToken::NewLine => { return (pos - 1, ast) },
            SolidToken::Operator(OperatorType::Eq) => {
                if is_expression { return (pos - 1, ast) }

                let mut parent = parent;
                while ast[parent].value.is_expression() {
                    parent = ast[parent].parent.unwrap();
                }

                let index = insert_as_parent_of_prev(&mut ast, parent, AstNode::Assignment);
                let (p, t) = make_ast_expression(
                    tokens, pos + 1, ast, index, vars, funcs, structs, traits
                );
                return (p - 1, t);
            },
            SolidToken::Colon => /*4 first assignment*/{
                if is_expression { return (pos - 1, ast) }
                let index = insert_as_parent_of_prev(&mut ast, parent, AstNode::FirstAssignment);
                identifier_pos += 1;
                vars.last_mut().unwrap().insert(st.clone(), identifier_pos);
                if let SolidToken::Operator(OperatorType::Eq) = tokens[pos + 1] {
                    pos += 1;
                } else {
                    pos -= 1;
                    let param = get_params(&tokens, &mut pos, funcs, structs, traits).remove(0); // 5 for now only taking the first
                    ast[index].typ = Some(param.typ);
                }
                let (p, t) = make_ast_expression(
                    tokens, pos + 1, ast, index, vars, funcs, structs, traits
                );
                return (p - 1, t);
            },
            SolidToken::Brace(IsOpen::True)
            | SolidToken::Parenthesis(IsOpen::True) => {
                let type_call = if let SolidToken::Brace(_) = tokens[pos] {
                    AstNode::StructInit
                } else { AstNode::FunctionCall(false) };
                let index = insert_as_parent_of_prev(&mut ast, parent, type_call);
                add_to_tree(index, &mut ast, Ast::new(AstNode::Args));
                let last = ast.len() - 1;
                let (p, t) = make_ast_expression(
                    tokens, pos + 1, ast, last, vars, funcs, structs, traits
                );
                pos = p; ast = t;
                while let SolidToken::Comma = tokens[pos] {
                    let (p, t) = make_ast_expression(
                        tokens, pos + 1, ast, last, vars, funcs, structs, traits
                    );
                    pos = p; ast = t;
                }
            },
            SolidToken::Bracket(IsOpen::True) => {
                let index = insert_as_parent_of_prev(&mut ast, parent, AstNode::Index);
                let (p, t) = make_ast_expression(tokens, pos + 1, ast, index, vars, funcs, structs, traits);
                pos = p; ast = t;
            },
            SolidToken::Period => {
                let (p, t) = add_property(&tokens, pos, ast, indent, vars, funcs, structs, traits, is_expression, parent);
                pos = p; ast = t;
            },
            _ if is_expression => return (pos - 1, ast),
            _ => panic!("Unexpected token {:?}", tokens[pos]),
        }
    }
}

fn add_property(
    tokens: &Vec<SolidToken>, pos: usize, mut ast: Vec<Ast>, indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes, is_expression: bool, mut parent: usize
) -> (usize, Vec<Ast>){
    while let AstNode::Property = ast[parent].value {
        parent = ast[parent].parent.unwrap();
    }
    let index = insert_as_parent_of_prev(&mut ast, parent, AstNode::Property);
    let st = unwrap_enum!(&tokens[pos + 1], SolidToken::Word(st), st, "expected word after period");
    return word_tok(tokens, pos + 1, ast, index, indent, vars, funcs, structs, traits, st, is_expression);
}

fn get_last(arr: &mut Option<Vec<usize>>) -> usize{
    let arr = unwrap_enum!(arr);
    let last = arr.pop().unwrap();
    arr.push(last);
    last
}

fn if_while_statement(
    is_if: bool, tokens: &Vec<SolidToken>, mut pos: usize, mut ast: Vec<Ast>, parent: usize,
    indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut ast, Ast::new(
        if is_if { AstNode::IfStatement } else { AstNode::WhileStatement }
    ));
    let len = ast.len();
    //4 condition:
    add_to_tree(len - 1, &mut ast, Ast::new(AstNode::ColonParentheses));
    let (p, t) = make_ast_expression(
        tokens, pos, ast, len, vars, funcs, structs, traits
    );
    pos = p; ast = t;
    //4 body:
    add_to_tree(len - 1, &mut ast, Ast::new(AstNode::Body));
    vars.push(HashMap::new());
    let len = ast.len();
    let (p, t) = make_ast_statement(
        tokens, pos + 1, ast, len - 1, indent + 1, vars, funcs, structs, traits
    );
    vars.pop();
    (p - 1, t)
}

fn for_statement(
    tokens: &Vec<SolidToken>, mut pos: usize, mut ast: Vec<Ast>, parent: usize,
    indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> (usize, Vec<Ast>) {
    add_to_tree(parent, &mut ast, Ast::new(AstNode::ForStatement));
    let loop_pos = ast.len() - 1;
    let pars_pos = loop_pos + 1;
    let vars_pos = loop_pos + 2;
    let iter_pos = loop_pos + 3;
    let body_pos = loop_pos + 4;
    add_to_tree(loop_pos, &mut ast, Ast::new(AstNode::ColonParentheses));
    add_to_tree(pars_pos, &mut ast, Ast::new(AstNode::ForVars));
    add_to_tree(pars_pos, &mut ast, Ast::new(AstNode::ForIter));
    add_to_tree(loop_pos, &mut ast, Ast::new(AstNode::Body));
    vars.push(HashMap::new());
    //4 vars:
    loop {
        let name = unwrap_enum!(&tokens[pos], SolidToken::Word(x), x, "expected identifier");
        add_to_tree(
            vars_pos, &mut ast,
            Ast::new(AstNode::Identifier(name.clone()))
        );
        vars.last_mut().unwrap().insert(name.clone(), ast.len() - 1);
        pos += 2;
        match &tokens[pos - 1] {
            SolidToken::Comma => continue,
            SolidToken::In => break,
            _ => panic!("expected `in` or `,`")
        }
    }
    //4 iters:
    let (p, t) = make_ast_expression(
        tokens, pos, ast, iter_pos, vars, funcs, structs, traits
    );
    pos = p; ast = t;
    //4 body:
    let (p, t) = make_ast_statement(
        tokens, pos + 1, ast, body_pos, indent + 1, vars, funcs, structs, traits
    );
    vars.pop();
    (p - 1, t)
}

fn insert_as_parent_of_prev(ast: &mut Vec<Ast>, parent: usize, value: AstNode) -> usize {
    let index = get_last(&mut ast[parent].children);
    ast[index].parent = Some(index);
    ast.insert(index, Ast {
        value,
        children: some_vec![index + 1],
        parent: Some(parent),
        typ: None,
        is_mut: true
    });
    for i in index + 1..ast.len() {
        if let Some(children) = &ast[i].children {
            ast[i].children = Some(children.iter().map(|x| x+1).collect())
        }
        if let Some(parent) = &ast[i].parent {
            if *parent >= index {
                ast[i].parent = Some(parent + 1);
            }
        }
    }
    index
}

pub fn add_to_tree(parent: usize, ast: &mut Vec<Ast>, mut new_node: Ast) {
    new_node.parent = Some(parent);
    ast.push(new_node);
    let pos = ast.len() - 1;
    if let Some(children) = &mut ast[parent].children {
        children.push(pos)
    } else {
        ast[parent].children = some_vec![pos]
    }
}

fn make_func(
    tokens: &Vec<SolidToken>, mut pos: usize, mut ast: Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> (usize, Vec<Ast>) {
    let is_static = matches!(&tokens[pos], SolidToken::Static);
    pos += if is_static { 2 } else { 1 };
    let mut name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };
    pos += 1;

    // todo can there be more than one? (if so make it while instead of if)
    if matches!(tokens[pos], SolidToken::Colon) && matches!(tokens[pos + 1], SolidToken::Colon) {
        // TODO
        pos += 2; //1 skip the ::
        write!(name, "::{}", unwrap_enum!(&tokens[pos], SolidToken::Word(w), w)).unwrap();
        pos += 1; //1 skip the next word
        let parent_struct = ast[parent].parent.unwrap();
        let traits = unwrap_u(&ast[parent_struct].children)[3];
        add_to_tree(traits, &mut ast, Ast::new(
            AstNode::Identifier(String::from(name.split("::").next().unwrap()))
        ));
    }
    add_to_tree(parent, &mut ast, Ast::new(
        if is_static {
            AstNode::StaticFunction(name.clone())
        } else {
            AstNode::Function(name.clone())
        }
    ));
    let index = ast.len() - 1;

    let generics_names = get_generics(&mut pos, tokens, index, &mut ast);
    let mut generics_hs = HashSet::from_iter(generics_names.iter().map(|x| x.clone()));
    if let Some(struct_parent) = ast[parent].parent {
        if let AstNode::Struct(_) = ast[struct_parent].value {
            let struct_generics = &ast[unwrap_u(&ast[struct_parent].children)[0]];
            let stct_g_typ = unwrap_enum!(&struct_generics.typ);
            for child in unwrap(&stct_g_typ.children) {
                unwrap_enum!(
                    child,
                    Type { kind: TypeKind::Generic(GenericType::Declaration(name)), .. },
                    generics_hs.insert(name.clone())
                );
            }
        }
    }

    let SolidToken::Parenthesis(IsOpen::True) = tokens[pos] else {
        panic!("expected `(` or `::`, found {:?}", tokens[pos])
    };
    pos += 1;
    let params = get_params(tokens, &mut pos, funcs, structs, traits);
    add_to_tree(index, &mut ast, Ast::new(AstNode::ArgsDef));
    add_to_tree(index, &mut ast, Ast::new(AstNode::ReturnType));
    add_to_tree(index, &mut ast, Ast::new(AstNode::Body));
    let args_pos = ast.len() - 3;
    let return_pos = args_pos + 1;
    let body_pos = args_pos + 2;

    let mut input = vec![];
    for param in &params {
        //1 checks if it is actually a generic and if so makes it a generic type
        let typ = Some(
            is_generic(&param.typ, &generics_hs)
        );
        input.push(typ.clone().unwrap());
        add_to_tree(args_pos, &mut ast, Ast {
            children: None, parent: Some(args_pos),
            value: AstNode::Identifier(param.name.clone()),
            typ,
            is_mut: param.is_mut
        });
        let identifier_pos = ast.len() - 1;
        vars.last_mut().unwrap().insert(param.name.clone(), identifier_pos);
    }

    pos += 1;
    if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
        pos += 1;
        let typ = get_arg_typ(tokens, &mut pos, funcs, structs, traits);
        ast[return_pos].typ = Some(find_generics_in_typ(&typ, &generics_hs));
    }
    if let SolidToken::Colon = tokens[pos] {} else {
        panic!("expected colon, found `{:?}`", tokens[pos])
    }
    pos += 1;
    funcs.insert(name, FuncType{
        input: if input.len() == 0 { None } else { Some(input) },
        output: ast[return_pos].typ.clone(),
    });
    make_ast_statement(tokens, pos, ast, body_pos, indent + 1, vars, funcs, structs, traits)
}

fn make_struct(
    tokens: &Vec<SolidToken>, mut pos: usize, mut ast: Vec<Ast>, parent: usize, indent: usize,
    funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> (usize, Vec<Ast>) {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };

    add_to_tree(parent, &mut ast, Ast::new(AstNode::Struct(name.clone())));
    let index = ast.len() - 1;

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, &mut ast);
    let generics_hs = HashSet::from_iter(generics_names.iter().map(|x| x.clone()));
    *structs.get_mut(&name).unwrap() = StructType { generics: Some(generics_names), pos: index};
    let mut trait_names = vec![];
    if let SolidToken::Parenthesis(IsOpen::True) = tokens[pos] {
        pos += 1;
        while let SolidToken::Word(w) = &tokens[pos] {
            trait_names.push(w.clone());
            let SolidToken::Comma = tokens[pos + 1] else {
                break
            };
            pos += 2;
        }
        pos += 2;
    }

    add_to_tree(index, &mut ast, Ast::new(AstNode::ArgsDef));
    add_to_tree(index, &mut ast, Ast::new(AstNode::Module));
    add_to_tree(index, &mut ast, Ast::new(AstNode::Traits));
    let args_pos = ast.len() - 3;
    let body_pos = args_pos + 1;

    unwrap_enum!(tokens[pos], SolidToken::Colon, "expected colon");
    pos += 2 + indent;
    if let SolidToken::Tab = &tokens[pos] {
        pos += 1;
        while let SolidToken::Word(word) = &tokens[pos] {
            pos += 1;
            if let SolidToken::Colon = &tokens[pos] {
                pos += 1;
                let typ = is_generic(&get_arg_typ(tokens, &mut pos, funcs, structs, traits), &generics_hs);
                add_to_tree(args_pos, &mut ast, Ast::new(AstNode::Identifier(word.clone())));
                ast.last_mut().unwrap().typ = Some(typ);
            } else {
                if unsafe { IS_COMPILED } {
                    panic!("argument `{word}` needs a type")
                }
                // todo check if works
                add_to_tree(args_pos, &mut ast, Ast::new(AstNode::Identifier(word.clone())));
            }
            if let SolidToken::NewLine = tokens[pos] {} else {
                return (pos, ast)
            }
            pos += 1;
            let mut exited = false;
            for i in 0..=indent {
                if let SolidToken::Tab = tokens[pos + i] {} else {
                    exited = true;
                    break;
                }
            }
            if exited {
                return (pos - 1, ast)
            } else {
                pos += indent + 1;
            }
        }
    }
    let mut struct_funcs: FuncTypes = HashMap::new();

    let mut vars = HashMap::new();
    for child in unwrap_u(&ast[args_pos].children) {
        let arg = &ast[*child];
        vars.insert(
            unwrap_enum!(&arg.value, AstNode::Identifier(n), n).clone(),
            child.clone()
        );
    }
    let mut vars = vec![vars]; //1 stack
    while let SolidToken::Def | SolidToken::Static = tokens[pos] {
        vars.push(HashMap::new());
        let temp = make_func(
            tokens, pos, ast, body_pos, indent + 1,
            &mut vars, &mut struct_funcs, structs, traits
        );
        vars.pop();
        pos = temp.0; ast = temp.1;

        let func = &ast[*unwrap_u(&ast[body_pos].children).last().unwrap()];
        if let AstNode::Function(_) = func.value {
            let args_def_pos = unwrap_u(&func.children)[1];
            ast.push(Ast {
                children: None,
                parent: Some(args_def_pos),
                value: AstNode::Identifier(String::from("self")),
                typ: Some(typ_with_child! {
                    TypeKind::MutPointer,
                    Type {
                        kind: TypeKind::Struct(TypName::Static("Self")),
                        children: None
                    }
                }),
                is_mut: false
            });
            let self_pos = ast.len() - 1;
            let args_def = &mut ast[args_def_pos];
            if let Some(children) = &mut args_def.children {
                children.insert(0, self_pos);
            } else {
                args_def.children = some_vec![self_pos];
            }
        }
        pos += indent + 2;
        if pos > tokens.len() { break; }
    }
    pos -= indent + 2;
    (pos, ast)
}

fn make_trait(
    tokens: &Vec<SolidToken>, mut pos: usize, mut ast: Vec<Ast>, parent: usize, indent: usize,
    funcs: &mut FuncTypes, structs: &mut StructTypes, traits: &mut TraitTypes
) -> (usize, Vec<Ast>) {
    let name =
        if let SolidToken::Word(name) = &tokens[pos] { name.clone() }
        else { panic!("Invalid name for function {:?}", tokens[pos]) };

    add_to_tree(parent, &mut ast, Ast::new(AstNode::Trait(name.clone())));
    let index = ast.len() - 1;

    pos += 1;
    let generics_names = get_generics(&mut pos, tokens, index, &mut ast);
    let generics_hs = HashSet::from_iter(generics_names.iter().map(|x| x.clone()));


    *traits.get_mut(&name).unwrap() = TraitType { generics: Some(generics_names), pos: index};
    add_to_tree(index, &mut ast, Ast::new(AstNode::Module));
    let body_pos = ast.len() - 1;

    unwrap_enum!(tokens[pos], SolidToken::Colon, "expected colon");
    pos += 3 + indent;
    while let SolidToken::Def = &tokens[pos] {
        pos += 1;
        let func_name = unwrap_enum!(&tokens[pos], SolidToken::Word(n), n);
        add_to_tree(body_pos, &mut ast, Ast::new(AstNode::Function(func_name.clone())));
        let func_pos = ast.len() - 1;
        pos += 1;
        let func_generics = get_generics(&mut pos, tokens, func_pos, &mut ast);
        pos += 1;
        let params = get_params(tokens, &mut pos, funcs, structs, traits);
        let mut args: Vec<Param> = params.iter().map(|x| Param {
            typ: is_generic(&x.typ, &generics_hs),
            name: x.name.clone(),
            is_mut: x.is_mut
        }).collect();
        args.insert(0, Param {
            name: String::from("self"),
            typ: typ_with_child! {
                TypeKind::MutPointer,
                Type {
                    kind: TypeKind::Struct(TypName::Static("Self")),
                    children: None
                }
            },
            is_mut: true
        });
        pos += 1;
        let return_type = if let SolidToken::Operator(OperatorType::Returns) = tokens[pos] {
            pos += 1;
            let return_type = get_arg_typ(tokens, &mut pos, funcs, structs, traits);
            Some(find_generics_in_typ(&return_type, &generics_hs))
        } else { None };
        //1 args
        add_to_tree(func_pos, &mut ast, Ast::new(AstNode::ArgsDef));
        let args_pos = ast.len() - 1;
        for arg in args {
            add_to_tree(args_pos, &mut ast, Ast{
                value: AstNode::Identifier(arg.name.clone()),
                typ: Some(arg.typ),
                children: None,
                parent: None,
                is_mut: arg.is_mut,
            })
        }
        //1 return
        add_to_tree(func_pos, &mut ast, Ast::new_w_typ(AstNode::ReturnType, return_type));
        pos += indent + 2;
    }
    pos -= indent + 2;
    (pos, ast)
}

// should be passed pos = one after the opening parenthesis
// returns where pos = the index of the closing brace
// e.g.     x, y: bool | int ) -> int:
//                           ^
fn get_params(
    tokens: &Vec<SolidToken>, mut pos: &mut usize, funcs: &mut FuncTypes, structs: &StructTypes, traits: &TraitTypes
) -> Vec<Param> {
    let mut params = Vec::new();
    let mut is_mut = true;
    loop {
        match &tokens[*pos] {
            SolidToken::Word(wrd) => {
                params.push(Param {
                    name: wrd.clone(),
                    typ:
                        if let SolidToken::Colon = &tokens[*pos + 1] {
                            *pos += 2;
                            get_arg_typ(tokens, &mut pos, funcs, structs, traits)
                        } else { UNKNOWN_TYPE },
                    is_mut
                });
                is_mut = true;
                if let SolidToken::Parenthesis(IsOpen::False)
                    | SolidToken::Operator(OperatorType::Eq) = tokens[*pos] {
                    return params
                }
            },
            SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Operator(OperatorType::Eq) => return params,
            SolidToken::IMut => {
                is_mut = false;
            }
            _ => panic!("unexpected token {:?}", tokens[*pos])
        }
        *pos += 1;
    }
}


// returns where pos is the index of the token after the end of the type
// e.g.     x: int | bool, y: int | None) -> bool:
//                       ^              ^        ^
fn get_arg_typ(
    tokens: &Vec<SolidToken>, pos: &mut usize, funcs: &mut FuncTypes, structs: &StructTypes, traits: &TraitTypes
) -> Type {
    if structs.len() == 0 {
        panic!()
    }
    let mut res: Option<Type> = None;
    loop {
        match &tokens[*pos] {
            SolidToken::Bracket(IsOpen::True) => {
                unwrap_enum!(&mut res, Some(Type{
                    kind: TypeKind::Struct(struct_name),
                    children: Some(res_children)
                }));
                *pos += 1;
                let add_child = |t: Type, i: usize, struct_name: &str| {
                    if !structs.contains_key(struct_name) {
                        panic!("cannot find struct `{struct_name}`")
                    }
                    typ_with_child! {
                        TypeKind::Generic(GenericType::Of(
                            unwrap_enum!(&structs[struct_name].generics)[i].clone()
                        )),
                        t
                    }
                };
                let mut children = vec![add_child(
                    get_arg_typ(tokens, pos, funcs, structs, traits),
                    0,
                    struct_name.get_str()
                )];
                let mut generic_num = 1;
                while let SolidToken::Comma = &tokens[*pos] {
                    *pos += 1;
                    children.push(add_child(
                        get_arg_typ(tokens, pos, funcs, structs, traits),
                        generic_num,
                        struct_name.get_str()
                    ));
                    generic_num += 1;
                }
                *pos += 1;
                res_children[0].children = Some(children);
                // res = Some(typ_with_child! {
                //     TypeKind::Struct(struct_name),
                //     Type {
                //         kind: TypeKind::GenericsMap,
                //         children: Some(children),
                //     }
                // });
                break
            },
            SolidToken::Comma
            | SolidToken::Parenthesis(IsOpen::False)
            | SolidToken::Bracket(IsOpen::False)
            | SolidToken::Colon
            | SolidToken::NewLine
            | SolidToken::Operator(OperatorType::Eq)=> {
                break
            },
            SolidToken::Operator(OperatorType::BinOr) => {
                let typ = unwrap_enum!(res, Some(x), x, "need a value before |");
                *pos += 1;
                res = Some(typ.add_option(get_arg_typ(tokens, pos, funcs, structs, traits)));
                break
            }
            SolidToken::Word(wrd) => {
                if let Some(_) = res {
                    panic!("unexpected type. res: {:?}, wrd: {wrd}", res)
                }
                let wrd = clean_type(wrd.clone()).to_string();
                res = if wrd == "str" {
                    Some(MUT_STR_TYPE)
                } else if structs.contains_key(&wrd) {
                    Some(typ_with_child! {
                        TypeKind::Struct(TypName::Str(wrd)),
                        Type{
                            kind: TypeKind::GenericsMap,
                            children: None
                        }
                    })
                } else if traits.contains_key(&wrd) {
                    Some(typ_with_child! {
                        TypeKind::Trait(TypName::Str(wrd.clone())),
                        Type{
                            kind: TypeKind::GenericsMap,
                            children: None
                        }
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

fn is_generic(typ: &Type, generics_hs: &HashSet<String>) -> Type {
    if let Type { kind: TypeKind::Struct(TypName::Str(name)), children: None } = &typ {
        if generics_hs.contains(name) {
            return Type {
                kind: TypeKind::Generic(GenericType::Of(name.clone())),
                children: None,
            }
        }
    }
    typ.clone()
}

fn get_generics(pos: &mut usize, tokens: &Vec<SolidToken>, index: usize, ast: &mut Vec<Ast>) -> Vec<String> {
    let mut generics_vec = vec![];
    let mut generics_names = vec![];
    if let SolidToken::Operator(OperatorType::Smaller) = tokens[*pos] { //1 generics
        *pos += 1;
        while let SolidToken::Word(name) = &tokens[*pos] {
            generics_vec.push(Type {
                kind: TypeKind::Generic(GenericType::Declaration(name.clone())),
                children: None,
            });
            generics_names.push(name.clone());
            *pos += 2;
        }
    }
    add_to_tree(index, ast, Ast::new_w_typ(
        AstNode::GenericsDeclaration,
        Some(Type {
            kind: TypeKind::Generics,
            children: if generics_vec.len() == 0 { None } else { Some(generics_vec) }
        })
    ));
    generics_names
}

fn find_generics_in_typ(typ: &Type, generics_hs: &HashSet<String>) -> Type {
    let mut res = is_generic(typ, &generics_hs);
    res.children = if let Some(children) = res.children {
        Some(children.iter().map(|x| find_generics_in_typ(x, generics_hs)).collect())
    } else { None };
    res
}