use std::collections::{HashMap, HashSet};
use crate::construct_ast::ast_structure::{Ast, AstNode};
use crate::{IS_COMPILED, unwrap_enum};
use crate::add_types::ast_add_types::add_types;
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{Type, TypeKind, unwrap, unwrap_u};
use crate::built_in_funcs::BuiltIn;
use crate::construct_ast::get_functions_and_types::{get_struct_and_func_names};
use crate::construct_ast::get_typ::{get_params};
use crate::construct_ast::make_func_struct_trait::{make_struct, make_func, make_trait};
use crate::construct_ast::tree_utils::{add_to_tree, get_last, insert_as_parent_of_prev, print_tree};

type TraitFuncs = HashMap<String, (usize, FuncType)>;

pub type VarTypes = Vec<HashMap<String, usize>>;
pub type GenericTypes = Vec<HashSet<String>>;
pub type TraitTypes = HashMap<String, TraitType>;
pub type StructTypes = HashMap<String, StructType>;
pub type FuncTypes = HashMap<String, FuncType>;
pub type TypeTypes = HashMap<String, Type>;

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
#[derive(Clone, Debug, PartialEq)]
pub struct FuncType {
    pub input: Option<Vec<Type>>,
    pub output: Option<Type>
}


// todo I think it allows to use any type of closing )}]
pub struct Info<'a> {
    pub funcs: &'a mut FuncTypes,
    pub structs: &'a mut StructTypes,
    pub traits: &'a mut TraitTypes,
    pub types: &'a mut TypeTypes,
    pub generics: &'a mut GenericTypes,
    pub struct_inner_types: &'a mut HashSet<String>
}


pub fn construct_ast(
    tokens: &[SolidToken], pos: usize,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>
) -> Vec<Ast> {
    let (mut structs, mut funcs, mut traits, mut types)
        = get_struct_and_func_names(tokens);
    let mut ast = vec![Ast::new(AstNode::Module)];
    let mut info = Info {
        funcs: &mut funcs,
        structs: &mut structs,
        traits: &mut traits,
        types: &mut types,
        generics: &mut vec![],
        struct_inner_types: &mut HashSet::new()
    };
    make_ast_statement(
        tokens, pos, &mut ast, 0, 0,
        &mut vec![HashMap::new()], &mut info, built_ins
    );

    duck_type(&mut ast, info.traits, info.structs);
    print_tree((ast.clone(), 0));
    if unsafe { IS_COMPILED } {
        add_types(
            &mut ast, 0, &mut vec![HashMap::new()],
            &mut info, &None, built_ins
        );
        print_tree((ast.clone(), 0));
    }
    ast
}

fn duck_type(ast: &mut Vec<Ast>, traits: &TraitTypes, structs: &StructTypes) {
    let traits: Vec<(String, TraitFuncs)> = traits.iter().filter_map(
        |(trt_name, trt)| {
            let trt_def = &ast[trt.pos];
            if let AstNode::Trait { strict: true, .. } = trt_def.value {
                None
            } else {
                let trt_module = &ast[unwrap_u(&trt_def.children)[1]];
                Some((trt_name.clone(), get_trt_strct_functions(ast, trt_module)))
            }
        }
    ).collect();
    for strct in structs.values() {
        let strct_def = &ast[strct.pos];
        let strct_children = unwrap_u(&strct_def.children);

        let strct_module_pos = strct_children[2];
        let strct_traits_pos = strct_children[3];
        let funcs = get_trt_strct_functions(ast, &ast[strct_module_pos]);
        let strct_traits: HashSet<String> = HashSet::from_iter(
            unwrap_u(&ast[strct_traits_pos].children).iter().map(|trt| {
                unwrap_enum!(&ast[*trt].value, AstNode::Identifier(name), name.clone())
            })
        );
        fn struct_matches_trait(trt_funcs: &TraitFuncs, funcs: &TraitFuncs) -> Option<HashMap<String, Type>> {
            let mut hm = HashMap::new();
            'trait_func_loop: for (trt_f_name, (_, trt_f_types)) in trt_funcs {
                if let Some((_, func_types)) = funcs.get(trt_f_name) {
                    if func_types == trt_f_types {
                        continue 'trait_func_loop
                    }
                    if func_types.output.is_some() != trt_f_types.output.is_some()
                        || func_types.input.is_some() != trt_f_types.input.is_some()
                    { return None }
                    let mut func_all_types = unwrap(&func_types.input).clone();
                    let mut trt_f_all_types = unwrap(&trt_f_types.input).clone();
                    if let Some(x) = &func_types.output {
                        func_all_types.push(x.clone());
                        unsafe { //1 safe cuz already checked that they both have or both dont have a return typ
                            trt_f_all_types.push(trt_f_types.output.clone().unwrap_unchecked())
                        }
                    }
                    
                    for (trt_fnc, fnc) in trt_f_all_types.iter().zip(func_all_types) {
                        if *trt_fnc == fnc {
                            continue
                        }
                        if let TypeKind::Struct(name) = &trt_fnc.kind {
                            if let Some(typ_name) = name.get_str().strip_prefix("Self::") { // TODO find a better way
                                if let Some(expected_typ) = hm.get(typ_name) {
                                    if fnc == *expected_typ {
                                        continue
                                    }
                                } else {
                                    hm.insert(String::from(typ_name), fnc);
                                    continue
                                }
                            }
                        }
                        return None
                    }
                    continue 'trait_func_loop
                }
                return None
            }
            Some(hm)
        }
        for (trt, trt_funcs) in traits.iter() {
            if strct_traits.contains(trt) { continue }
            //1 if all the functions in the trait are implemented
            if let Some(types_hm) = struct_matches_trait(trt_funcs, &funcs) {
                add_trait_to_struct(
                    ast, strct_module_pos, strct_traits_pos, &funcs, trt, trt_funcs, types_hm
                )
            }
        }
    }
}

fn add_trait_to_struct(
    ast: &mut Vec<Ast>, strct_module_pos: usize, strct_traits_pos: usize,
    funcs: &TraitFuncs, trt: &String, trt_funcs: &TraitFuncs, types_hm: HashMap<String, Type>
) {
    add_to_tree(
        strct_traits_pos, ast,
        Ast::new(AstNode::Identifier(trt.clone()))
    );
    for (func_name, (_trt_func_pos, _)) in trt_funcs {
        let new_func_name = format!("{}::{}", trt, func_name);
        //1 if already contains func with that name
        if funcs.keys().any(|x| *x == new_func_name) { continue }
        let func_pos = add_to_tree(
            strct_module_pos, ast,
            Ast::new(AstNode::Function(new_func_name))
        );
        // let trt_func_parts = ast[*trt_func_pos].children.clone().unwrap();
        let (fp, _) = funcs.get(func_name).unwrap();
        let func_parts = ast[*fp].children.clone().unwrap();
        add_to_tree(func_pos, ast, ast[func_parts[0]].clone());
        add_to_tree(func_pos, ast, ast[func_parts[1]].clone());
        add_to_tree(func_pos, ast, ast[func_parts[2]].clone());
        let body_pos = add_to_tree(
            func_pos, ast, Ast::new(AstNode::Body)
        );
        if !types_hm.is_empty() {
            let types_pos = add_to_tree(
                func_pos, ast, Ast::new(AstNode::Types)
            );
            for (name, typ) in types_hm.iter() {
                add_to_tree(types_pos, ast, Ast {
                    value: AstNode::Type(name.clone()),
                    children: None,
                    parent: None,
                    typ: Some(typ.clone()),
                    is_mut: false,
                });
            }
        }
        let rtrn = add_to_tree(
            body_pos, ast, Ast::new(AstNode::Return)
        );

        let prop = add_to_tree(
            rtrn, ast, Ast::new(AstNode::Property)
        );
        add_to_tree(prop, ast, Ast::new(
            AstNode::Identifier(String::from("self"))
        ));
        let func_call = add_to_tree(prop, ast, Ast::new(
            AstNode::FunctionCall(false)
        ));
        add_to_tree(func_call, ast, Ast::new(
            AstNode::Identifier(func_name.clone())
        ));
        add_to_tree(func_call, ast, Ast::new(
            AstNode::Args
        ));
    }
}

fn get_trt_strct_functions(ast: &[Ast], module: &Ast) -> TraitFuncs {
    unwrap_u(&module.children).iter().filter_map(|func| {
        if let AstNode::Function(name) = &ast[*func].value {
            let func_children = unwrap_u(&ast[*func].children);
            let (args_pos, return_pos) = (func_children[1], func_children[2]);
            let args_children = unwrap_u(&ast[args_pos].children);
            let args = if args_children.is_empty() { None } else {
                Some(
                    args_children.iter()
                        .map(|x| ast[*x].typ.clone().unwrap())
                        .collect::<Vec<Type>>()
                )
            };
            let return_typ = ast[return_pos].typ.clone();
            Some((name.clone(), (*func, FuncType {
                input: args,
                output: return_typ
            })))
        } else { None }
    }).collect()
}

pub fn make_ast_statement(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, info: &mut Info,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>
) -> usize {
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Struct => {
                vars.push(HashMap::new());
                pos = make_struct(
                    tokens, pos + 1, ast, parent, indent, info, built_ins
                );
                vars.pop();
            },
            SolidToken::Def  => {
                vars.push(HashMap::new());
                pos = make_func(
                    tokens, pos, ast, parent, indent, vars, info, built_ins
                ) - 1;
                vars.pop();
            },
            SolidToken::Trait | SolidToken::StrictTrait => {
                pos = make_trait(tokens, pos + 1, ast, parent, indent, info);
            },
            SolidToken::For => {
                pos = for_statement(
                    tokens, pos + 1, ast, parent, indent,
                    vars, info, built_ins
                );
            },
            SolidToken::While => {
                pos = if_while_statement(
                    false, tokens, pos + 1, ast, parent, indent,
                    vars, info, built_ins
                );
            },
            SolidToken::If => {
                pos = if_while_statement(
                    true, tokens, pos + 1, ast, parent, indent,
                    vars, info, built_ins
                );
            },
            SolidToken::Elif => {
                let last = get_last(&mut ast[parent].children);
                if let AstNode::IfStatement = ast[last].value {
                    pos = if_while_statement(
                        true, tokens, pos + 1, ast, last, indent,
                        vars, info, built_ins
                    );
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
                    let body_pos = add_to_tree(last, ast, Ast::new(AstNode::Body));
                    vars.push(HashMap::new());
                    pos = make_ast_statement(
                        tokens, pos + 1, ast, body_pos, indent + 1,
                        vars, info, built_ins
                    ) - 1;
                    vars.pop();
                } else {
                    panic!("else to unknown if")
                }
            },
            SolidToken::Pass => {
                add_to_tree(parent, ast, Ast::new(AstNode::Pass));
            },
            SolidToken::Word(st) => {
                pos = word_tok(
                    tokens, pos, ast, parent, indent, vars,
                    info, st, false
                );
            }
            SolidToken::NewLine => {
                let tabs = tokens.iter().skip(pos + 1)
                    .take_while(|&x| matches!(x, SolidToken::Tab)).count();
                match tabs.cmp(&indent) {
                    std::cmp::Ordering::Less => return pos,
                    std::cmp::Ordering::Greater =>  panic!(
                        "unexpected indentation, expected `{indent}` found `{tabs}`"
                    ),
                    _ => ()
                }
                pos += tabs;
            },
            SolidToken::Return => {
                let return_pos = add_to_tree(
                    parent, ast, Ast::new(AstNode::Return)
                );
                if let SolidToken::NewLine = tokens[pos + 1] {

                } else {
                    pos = make_ast_expression(
                        tokens, pos + 1, ast, return_pos, vars, info
                    ) - 1;
                }
            },
            SolidToken::Continue => {
                add_to_tree(parent, ast, Ast::new(AstNode::Continue));
            },
            SolidToken::Break => {
                add_to_tree(parent, ast, Ast::new(AstNode::Break));
            }
            SolidToken::IMut => {
                let st = unwrap_enum!(&tokens[pos + 1], SolidToken::Word(st), st);
                pos = word_tok(
                    tokens, pos + 1, ast, parent, indent,
                    vars, info, st, false
                );
                let assignment = *unwrap_u(&ast[parent].children).last().unwrap();
                let assignment = &mut ast[assignment];
                assignment.is_mut = false;
            }
            SolidToken::UnaryOperator(OperatorType::Dereference) => {
                let deref_pos = add_to_tree(parent, ast, Ast::new(
                    AstNode::UnaryOp(OperatorType::Dereference)
                ));
                pos = make_ast_statement(
                    tokens, pos + 1, ast, deref_pos, indent,
                    vars, info, built_ins
                ) - 1;
            }
            SolidToken::Parenthesis(IsOpen::True) => {
                todo!()
            }
            SolidToken::Type => {
                while !matches!(tokens[pos], SolidToken::NewLine) {
                    pos += 1;
                }
            }
            _ => panic!("unexpected token `{:?}`", token)
        }
        pos += 1;
    }
    pos
}

fn make_ast_expression(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    vars: &mut VarTypes, info: &mut Info
) -> usize {
    // println!("{}\n".to_str(&(tree.clone(), 0)));
    let mut amount_of_open = 0;
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Parenthesis(IsOpen::True) => {
                amount_of_open += 1;
                let par_pos = add_to_tree(parent, ast, Ast::new(AstNode::Parentheses));
                pos = make_ast_expression(
                    tokens, pos + 1, ast, par_pos, vars, info
                ) - 1;
            },
            SolidToken::Bracket(IsOpen::True) =>{
                amount_of_open += 1;
                if let SolidToken::Parenthesis(IsOpen::False)
                | SolidToken::Bracket(IsOpen::False)
                | SolidToken::Brace(IsOpen::False)
                | SolidToken::Str { .. }
                | SolidToken::Word(_) = &tokens[pos - 1] {
                    //1 index
                    let index = insert_as_parent_of_prev(ast, parent, AstNode::Index);
                    pos = make_ast_expression(
                        tokens, pos + 1, ast, index, vars, info
                    ) - 1;
                } else {
                    //1 list-literal or comprehension
                    let list_parent = add_to_tree(parent, ast, Ast::new(AstNode::ListLiteral));
                    let starting_pos = pos;
                    while matches!(&tokens[pos], SolidToken::Comma) || pos == starting_pos {
                        pos = make_ast_expression(
                            tokens, pos + 1, ast, list_parent, vars, info
                        );
                    }
                    pos -= 1;
                }
            },
            SolidToken::Brace(IsOpen::True) => {
                amount_of_open += 1;
                //1 set\dict-literal or comprehension
                //3                                         | this is a placeholder |
                let list_parent = add_to_tree(parent, ast, Ast::new(AstNode::SetLiteral));
                let starting_pos = pos;
                while matches!(&tokens[pos], SolidToken::Comma | SolidToken::Colon) || pos == starting_pos {
                    if let SolidToken::Colon = tokens[pos] {
                        if let AstNode::DictLiteral = ast[list_parent].value {
                            if unwrap_u(&ast[list_parent].children).len() % 2 != 1 { panic!() }
                        } else {
                            if unwrap_u(&ast[list_parent].children).len() != 1 { panic!() }
                            ast[list_parent].value = AstNode::DictLiteral;
                        }
                    } else if let AstNode::DictLiteral = ast[list_parent].value {
                        if unwrap_u(&ast[list_parent].children).len() % 2 == 1 { panic!() }
                    }
                    pos = make_ast_expression(
                        tokens, pos + 1, ast, list_parent, vars, info
                    );
                }
                pos -= 1;
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
                add_to_tree(parent, ast, Ast::new(AstNode::Number(num.clone())));
            },
            SolidToken::Str { val: str, mutable } => {
                add_to_tree(
                    parent, ast, Ast::new(
                        AstNode::String{ val: str.clone(), mutable: *mutable }
                    )
                );
            },
            SolidToken::Char(ch) => {
                add_to_tree(parent, ast, Ast::new(AstNode::Char(ch.clone())));
            },
            SolidToken::Bool(bl) => {
                add_to_tree(parent, ast, Ast::new(AstNode::Bool(*bl)));
            },
            SolidToken::Word(wrd) => {
                pos = word_tok(
                    tokens, pos, ast, parent, 0, 
                    vars, info, wrd, true
                );
            },
            SolidToken::UnaryOperator(op) => {
                let index = add_to_tree(
                    parent, ast, Ast::new(AstNode::UnaryOp(op.clone()))
                );
                pos = make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                ) - 1;
            },
            SolidToken::Operator(op) => {
                let mut parent = parent;
                while let AstNode::Operator(prev_op) 
                | AstNode::UnaryOp(prev_op) = &ast[parent].value {
                    if !matches!(&ast[parent].value, AstNode::UnaryOp(_))
                        && prev_op.get_priority() < op.get_priority() { break }
                    parent = ast[parent].parent.unwrap();
                }
                let index = insert_as_parent_of_prev(ast, parent, AstNode::Operator(op.clone()));
                pos = make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                ) - 1;
            },
            SolidToken::Period => {
                pos = add_property(
                    tokens, pos, ast, 0, vars, info, true, parent
                );
            }
            _ => panic!("unexpected token {:?}", token)
        }
        pos += 1;
    }
    pos
}

fn word_tok(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, info: &mut Info, st: &str, is_expression: bool
) -> usize {
    let mut identifier_pos = add_to_tree(
        parent, ast,
        Ast::new(AstNode::Identifier(String::from(st)))
    );
    loop {
        pos += 1;
        match &tokens[pos] {
            SolidToken::NewLine => { return pos - 1 },
            SolidToken::Operator(OperatorType::Eq) => {
                if is_expression { return pos - 1 }

                let mut parent = parent;
                while ast[parent].value.is_expression() {
                    parent = ast[parent].parent.unwrap();
                }

                let index = insert_as_parent_of_prev(ast, parent, AstNode::Assignment);
                return make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                ) - 1;
            },
            SolidToken::Colon => /*4 first assignment*/{
                if is_expression { return pos - 1 }

                let mut parent = parent;
                while ast[parent].value.is_expression() {
                    parent = ast[parent].parent.unwrap();
                }

                let index = insert_as_parent_of_prev(ast, parent, AstNode::FirstAssignment);
                identifier_pos += 1;
                vars.last_mut().unwrap().insert(String::from(st), identifier_pos);
                if let SolidToken::Operator(OperatorType::Eq) = tokens[pos + 1] {
                    pos += 1;
                } else {
                    pos -= 1;
                    let param = get_params(tokens, &mut pos, info).remove(0); // 5 for now only taking the first
                    ast[index].typ = Some(param.typ);
                }
                return make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                ) - 1;
            },
            SolidToken::Brace(IsOpen::True)
            | SolidToken::Parenthesis(IsOpen::True) => {
                let word = unwrap_enum!(&tokens[pos - 1], SolidToken::Word(w), w);

                let index = if info.structs.contains_key(word) && word != "str" { // TODO !!!!!!!!!!!!!!1

                    let prop_pos = insert_as_parent_of_prev(
                        ast, parent, AstNode::Property
                    );
                    let index = add_to_tree(prop_pos, ast, Ast::new(AstNode::FunctionCall(true)));

                    add_to_tree(index, ast, Ast::new(
                        AstNode::Identifier(String::from("__init__"))
                    ));
                    index
                } else {
                    insert_as_parent_of_prev(ast, parent, AstNode::FunctionCall(false))
                };
                // let index = insert_as_parent_of_prev(ast, parent, type_call);
                let last = add_to_tree(index, ast, Ast::new(AstNode::Args));
                pos = make_ast_expression(
                    tokens, pos + 1, ast, last, vars, info
                );
                while let SolidToken::Comma = tokens[pos] {
                    pos = make_ast_expression(
                        tokens, pos + 1, ast, last, vars, info
                    );
                }
            },
            SolidToken::Bracket(IsOpen::True) => {
                let index = insert_as_parent_of_prev(ast, parent, AstNode::Index);
                pos = make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                );
            },
            SolidToken::Period => {
                pos = add_property(
                    tokens, pos, ast, indent, vars, info, is_expression, parent
                );
            },
            _ if is_expression => return pos - 1,
            _ => panic!("Unexpected token {:?}", tokens[pos]),
        }
    }
}

fn add_property(
    tokens: &[SolidToken], pos: usize, ast: &mut Vec<Ast>, indent: usize,
    vars: &mut VarTypes, info: &mut Info, is_expression: bool, mut parent: usize
) -> usize {
    while let AstNode::Property = ast[parent].value {
        parent = ast[parent].parent.unwrap();
    }
    let index = insert_as_parent_of_prev(ast, parent, AstNode::Property);
    let st = unwrap_enum!(&tokens[pos + 1], SolidToken::Word(st), st, "expected word after period");
    word_tok(
        tokens, pos + 1, ast, index, indent, vars, info, st, is_expression
    )
}

fn if_while_statement(
    is_if: bool, tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    indent: usize, vars: &mut VarTypes, info: &mut Info,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>
) -> usize {
    let stmt_pos = add_to_tree(parent, ast, Ast::new(
        if is_if { AstNode::IfStatement } else { AstNode::WhileStatement }
    ));
    //4 condition:
    let colon_par = add_to_tree(stmt_pos, ast, Ast::new(AstNode::ColonParentheses));
    pos = make_ast_expression(
        tokens, pos, ast, colon_par, vars, info
    );
    //4 body:
    let body_pos = add_to_tree(stmt_pos, ast, Ast::new(AstNode::Body));
    vars.push(HashMap::new());
    pos = make_ast_statement(
        tokens, pos + 1, ast, body_pos, indent + 1,
        vars, info, built_ins
    );
    vars.pop();
    pos - 1
}

fn for_statement(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    indent: usize, vars: &mut VarTypes, info: &mut Info,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>
) -> usize {
    let loop_pos = add_to_tree(parent, ast, Ast::new(AstNode::ForStatement));
    let pars_pos = add_to_tree(loop_pos, ast, Ast::new(AstNode::ColonParentheses));
    let vars_pos = add_to_tree(pars_pos, ast, Ast::new(AstNode::ForVars));
    let iter_pos = add_to_tree(pars_pos, ast, Ast::new(AstNode::ForIter));
    let body_pos = add_to_tree(loop_pos, ast, Ast::new(AstNode::Body));
    vars.push(HashMap::new());
    //4 vars:
    loop {
        let name = unwrap_enum!(&tokens[pos], SolidToken::Word(x), x, "expected identifier");
        let i = add_to_tree(
            vars_pos, ast,
            Ast::new(AstNode::Identifier(name.clone()))
        );
        vars.last_mut().unwrap().insert(name.clone(), i);
        pos += 2;
        match &tokens[pos - 1] {
            SolidToken::Comma => continue,
            SolidToken::In => break,
            _ => panic!("expected `in` or `,`")
        }
    }
    //4 iters:
    pos = make_ast_expression(
        tokens, pos, ast, iter_pos, vars, info
    );
    //4 body:
    pos = make_ast_statement(
        tokens, pos + 1, ast, body_pos, indent + 1,
        vars, info, built_ins
    );
    vars.pop();
    pos - 1
}
