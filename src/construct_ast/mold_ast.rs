use std::collections::{HashMap, HashSet};
use pretty_print_tree::Color;
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::{EMPTY_STR, IS_COMPILED, unwrap_enum};
use crate::add_types::ast_add_types::add_types;
use crate::add_types::utils::{add_to_stack, get_from_stack};
use crate::mold_tokens::{IsOpen, OperatorType, SolidToken};
use crate::types::{print_type_b, Type, TypeKind, unwrap, unwrap_u};
use crate::construct_ast::get_functions_and_types::{get_struct_and_func_names};
use crate::construct_ast::get_typ::{get_arg_typ, get_params};
use crate::construct_ast::make_func_struct_trait::{make_struct, make_func, make_trait, make_enum};
use crate::construct_ast::tree_utils::{add_to_tree, extend_tree, get_last, insert_as_parent_of_prev, print_tree};


type TraitFuncs = HashMap<String, (usize, FuncType)>;

pub type VarTypes = Vec<HashMap<String, usize>>;
pub type GenericTypes = Vec<HashSet<String>>;
pub type TraitTypes = HashMap<String, TraitType>;
pub type EnumTypes = HashMap<String, EnumType>;
pub type StructTypes = HashMap<String, StructType>;
pub type FuncTypes = HashMap<String, FuncType>;
pub type TypeTypes = HashMap<String, Type>;

pub trait STType {
    fn get_generics(&self) -> &Option<Vec<String>>;
    fn get_generics_mut(&mut self) -> &mut Option<Vec<String>>;
    fn get_pos(&self) -> usize;
}
#[derive(Clone, Debug)]
pub struct TraitType {
    pub generics: Option<Vec<String>>,
    pub pos: usize
}
#[derive(Clone, Debug)]
pub struct StructType {
    pub generics: Option<Vec<String>>,
    pub pos: usize
}
#[derive(Clone, Debug)]
pub struct EnumType {
    pub generics: Option<Vec<String>>,
    pub pos: usize
}
impl STType for StructType {
    fn get_generics(&self) -> &Option<Vec<String>> { &self.generics }
    fn get_generics_mut(&mut self) -> &mut Option<Vec<String>> { &mut self.generics }
    fn get_pos(&self) -> usize { self.pos }
}
impl STType for TraitType {
    fn get_generics(&self) -> &Option<Vec<String>> { &self.generics }
    fn get_generics_mut(&mut self) -> &mut Option<Vec<String>> { &mut self.generics }
    fn get_pos(&self) -> usize { self.pos }
}
impl STType for EnumType {
    fn get_generics(&self) -> &Option<Vec<String>> { &self.generics }
    fn get_generics_mut(&mut self) -> &mut Option<Vec<String>> { &mut self.generics }
    fn get_pos(&self) -> usize { self.pos }
}


#[derive(Clone, Debug, PartialEq)]
pub struct FuncType {
    pub input: Option<Vec<Param>>,
    pub output: Option<Type>
}


// todo I think it allows to use any type of closing )}]
#[derive(Debug)]
pub struct Info<'a> {
    pub funcs: &'a mut FuncTypes,
    pub structs: &'a mut StructTypes,
    pub traits: &'a mut TraitTypes,
    pub enums: &'a mut EnumTypes,
    pub one_of_enums: &'a mut HashMap<String, String>,
    pub types: &'a mut TypeTypes,
    pub generics: &'a mut GenericTypes,
    pub struct_inner_types: &'a mut HashSet<String>
}


pub fn construct_ast(tokens: &[SolidToken], pos: usize, info: &mut Info) -> Vec<Ast> {
    let (structs, funcs, traits, types, enums)
        = get_struct_and_func_names(tokens);
    let mut ast = vec![Ast::new(AstNode::Module)];
    *info.funcs = funcs;
    *info.structs = structs;
    *info.traits = traits;
    *info.types = types;
    *info.enums = enums;
    *info.generics = vec![];
    *info.struct_inner_types = HashSet::new();
    make_ast_statement(
        tokens, pos, &mut ast, 0, 0,
        &mut vec![HashMap::new()], info
    );
    duck_type(&mut ast, info.traits, info.structs);
    print_tree(&ast, 0);
    if unsafe { IS_COMPILED } {
        add_types(
            &mut ast, 0, &mut vec![HashMap::new()],
            info, &None
        );
        print_tree(&ast, 0);
    }
    ast
}

fn duck_type(ast: &mut Vec<Ast>, traits: &TraitTypes, structs: &StructTypes) {
    let all_traits: Vec<_> = traits.iter().map(
        |(trt_name, trt)| {
            let trt_def = &ast[trt.pos];
            let trt_module = &ast[unwrap_u(&trt_def.children)[1]];
            (
                unwrap_enum!(&trt_def.value, AstNode::Trait { strict, .. }, *strict),
                trt_name.clone(),
                get_trt_strct_functions(ast, trt_module)
            )
        }
    ).collect();
    let duck_traits: Vec<_> = all_traits.iter()
        .filter_map(|(strict, name, funcs)|
            if *strict { None } else { Some((name.clone(), funcs.clone())) } //3 prob expensive clone
        ).collect();

    for strct in structs.values() {
        let strct_def = &ast[strct.pos];
        let strct_children = unwrap_u(&strct_def.children);

        let strct_module_pos = strct_children[2];
        let strct_traits_pos = strct_children[3];
        let funcs = get_trt_strct_functions(ast, &ast[strct_module_pos]);
        let strct_traits: HashMap<String, usize> = HashMap::from_iter(
            unwrap_u(&ast[strct_traits_pos].children).iter().map(|trt| {
                (unwrap_enum!(&ast[*trt].value, AstNode::Identifier(name), name.clone()), *trt)
            })
        );
        #[inline] fn struct_matches_trait(
            trt_funcs: &TraitFuncs, funcs: &TraitFuncs
        ) -> Option<HashMap<String, Type>> {
            let mut hm = HashMap::new();
            'trait_func_loop: for (trt_f_name, (_, trt_f_types)) in trt_funcs {
                if let Some((_, func_types)) = funcs.get(trt_f_name) {
                    if func_types == trt_f_types {
                        continue 'trait_func_loop
                    }
                    if func_types.output.is_some() != trt_f_types.output.is_some()
                        || func_types.input.is_some() != trt_f_types.input.is_some()
                    { return None }
                    let mut func_all_types =
                        if let Some(v) = &func_types.input { v.clone() }
                        else { vec![] };
                    let mut trt_f_all_types =
                        if let Some(v) = &trt_f_types.input { v.clone() }
                        else { vec![] };
                    if let Some(x) = &func_types.output {
                        func_all_types.push(Param {
                            typ: x.clone(),
                            name: EMPTY_STR,
                            is_mut: false,
                            is_args: false,
                            is_kwargs: false,
                            pos: 99999
                        });
                        unsafe { //1 safe cuz already checked that they both have or both dont have a return typ
                            trt_f_all_types.push(Param {
                                typ: trt_f_types.output.clone().unwrap_unchecked(),
                                name: EMPTY_STR,
                                is_mut: false,
                                is_args: false,
                                is_kwargs: false,
                                pos: usize::MAX
                            })
                        }
                    }

                    for (trt_fnc, fnc) in trt_f_all_types.iter().zip(func_all_types) {
                        if trt_fnc.name == fnc.name && trt_fnc.typ == fnc.typ {
                            continue
                        }
                        if let TypeKind::InnerType(typ_name) = &trt_fnc.typ.kind {
                            if let Some(expected_typ) = hm.get(typ_name) {
                                if fnc.typ == *expected_typ {
                                    continue
                                }
                            } else {
                                hm.insert(typ_name.clone(), fnc.typ);
                                continue
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
        //5 this part isn't duck typing:
        for (trt_name, trt_pos) in &strct_traits {
            let trt_funcs = all_traits.iter().find(
                |(_, name, _)| name == trt_name
            ).unwrap().2.clone();
            let types_hm = HashMap::from_iter(
                unwrap_u(&ast[*trt_pos].children).iter().map(
                    |c| (
                        unwrap_enum!(&ast[*c].value, AstNode::Type(n), n.clone()),
                        ast[*c].typ.clone().unwrap(),
                    )
                )
            );
            add_trait_to_struct(
                ast, strct_module_pos, strct_traits_pos, &funcs,
                trt_name, &trt_funcs, types_hm, false
            );
        }

        for (trt, trt_funcs) in duck_traits.iter() {
            if strct_traits.contains_key(trt) { continue }
            //1 if all the functions in the trait are implemented
            // HashMap<String, (usize, HashMap<String, FuncType>)>
            if let Some(types_hm) = struct_matches_trait(trt_funcs, &funcs) {
                add_trait_to_struct(
                    ast, strct_module_pos, strct_traits_pos, &funcs,
                    trt, trt_funcs, types_hm, true
                )
            }
        }
    }
}

fn add_trait_to_struct(
    ast: &mut Vec<Ast>, strct_module_pos: usize, strct_traits_pos: usize,
    funcs: &TraitFuncs, trt: &String, trt_funcs: &TraitFuncs, types_hm: HashMap<String, Type>,
    add_identifier: bool
) {
    if add_identifier {
        add_to_tree(
            strct_traits_pos, ast,
            Ast::new(AstNode::Identifier(trt.clone()))
        );
    }
    for (func_name, (_trt_func_pos, _)) in trt_funcs {
        let new_func_name = format!("{}::{}", trt, func_name);
        //1 if already contains func with that name
        if funcs.keys().any(|x| *x == new_func_name) { continue }
        let func_pos = add_to_tree(
            strct_module_pos, ast,
            Ast::new(AstNode::Function(new_func_name))
        );
        // let trt_func_parts = ast[*trt_func_pos].children.clone().unwrap();
        let (fp, _) = funcs.get(func_name).unwrap_or_else(||panic!("expected implementation of `{}`", func_name));
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
        if let AstNode::Function(name) | AstNode::StaticFunction(name) = &ast[*func].value {
            let func_children = unwrap_u(&ast[*func].children);
            let (args_pos, return_pos) = (func_children[1], func_children[2]);
            let args_children = unwrap_u(&ast[args_pos].children);
            let args = if args_children.is_empty() { None } else {
                Some(
                    args_children.iter()
                        .map(|x| {
                            let (name, is_args, is_kwargs) = unwrap_enum!(
                                &ast[*x].value,
                                AstNode::Arg { name, is_arg, is_kwarg },
                                (name.clone(), *is_arg, *is_kwarg)
                            );
                            Param {
                                typ: ast[*x].typ.clone().unwrap(),
                                is_mut: ast[*x].is_mut,
                                name, is_args, is_kwargs,
                                pos: *x
                            }
                        })
                        .collect()
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
    vars: &mut VarTypes, info: &mut Info
) -> usize {
    while pos < tokens.len() {
        let token = &tokens[pos];
        match token {
            SolidToken::Struct => {
                vars.push(HashMap::new());
                pos = make_struct(
                    tokens, pos + 1, ast, parent, indent, info
                );
                vars.pop();
            },
            SolidToken::Def  => {
                vars.push(HashMap::new());
                pos = make_func(
                    tokens, pos, ast, parent, indent, vars, info
                ) - 1;
                vars.pop();
            },
            SolidToken::Trait | SolidToken::StrictTrait => {
                pos = make_trait(tokens, pos + 1, ast, parent, indent, info);
            },
            SolidToken::For => {
                pos = for_statement(
                    tokens, pos + 1, ast, parent, indent,
                    vars, info
                );
            },
            SolidToken::While => {
                pos = if_while_statement(
                    false, tokens, pos + 1, ast, parent, indent,
                    vars, info
                );
            },
            SolidToken::If => {
                pos = if_while_statement(
                    true, tokens, pos + 1, ast, parent, indent,
                    vars, info
                );
            },
            SolidToken::Elif => {
                let last = get_last(&ast[parent].children);
                if let AstNode::IfStatement = ast[last].value {
                    pos = if_while_statement(
                        true, tokens, pos + 1, ast, last, indent,
                        vars, info
                    );
                } else { panic!("elif to unknown if") }
            },
            SolidToken::Else => {
                if let SolidToken::Colon = tokens[pos + 1] {
                    pos += 1;
                } else {
                    panic!("expected colon")
                }
                let last = get_last(&ast[parent].children);
                if let AstNode::IfStatement = ast[last].value {
                    let body_pos = add_to_tree(last, ast, Ast::new(AstNode::Body));
                    vars.push(HashMap::new());
                    pos = make_ast_statement(
                        tokens, pos + 1, ast, body_pos, indent + 1,
                        vars, info
                    ) - 1;
                    vars.pop();
                } else { panic!("else to unknown if") }
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
                let assignment_pos = *unwrap_u(&ast[parent].children).last().unwrap();
                let assignment = &mut ast[assignment_pos];
                assignment.is_mut = false;
                let var_pos = ast[assignment_pos].children.as_ref().unwrap()[0];
                ast[var_pos].is_mut = false;
            }
            SolidToken::UnaryOperator(OperatorType::Dereference) => {
                let deref_pos = add_to_tree(parent, ast, Ast::new(
                    AstNode::UnaryOp(OperatorType::Dereference)
                ));
                pos = make_ast_statement(
                    tokens, pos + 1, ast, deref_pos, indent,
                    vars, info
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
            SolidToken::Enum => {
                pos = make_enum(tokens, pos + 1, ast, parent, indent, info);
            }
            _ => panic!("unexpected token `{:?}`", token)
        }
        pos += 1;
    }
    pos
}

pub fn make_ast_expression(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    vars: &mut VarTypes, info: &mut Info
) -> usize {
    // println!("{}\n".to_str(&(tree.clone(), 0)));
    let mut amount_of_open = 0;
    while pos < tokens.len() {
        let token = &tokens[pos];
        let token = if let SolidToken::In = token {
            &SolidToken::Operator(OperatorType::In)
        } else { token };
        match token {
            SolidToken::Parenthesis(IsOpen::True) => {
                amount_of_open += 1;
                let par_pos = add_to_tree(parent, ast, Ast::new(AstNode::Parentheses));
                pos = make_ast_expression(
                    tokens, pos + 1, ast, par_pos, vars, info
                ) - 1;
            },
            SolidToken::Bracket(IsOpen::True) => {
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
                } else if let ComprehensionType::ListSet = get_comprehension_typ(tokens, pos) {
                    //1 list comprehension
                    let lst_comp = comprehension(
                        tokens, &mut pos, vars, info, SolidToken::Bracket(IsOpen::True),
                        SolidToken::Bracket(IsOpen::False),
                        AstNode::ListComprehension
                    );
                    extend_tree(ast, parent, lst_comp);
                    pos -= 1;
                } else {
                    //1 list literal
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
                let comprehension_typ = get_comprehension_typ(tokens, pos);
                match comprehension_typ {
                    ComprehensionType::Dict => {
                        let lst_comp = comprehension(
                            tokens, &mut pos, vars, info, SolidToken::Brace(IsOpen::True),
                            SolidToken::Brace(IsOpen::False),
                            AstNode::DictComprehension
                        );
                        extend_tree(ast, parent, lst_comp);
                        pos -= 1;
                    }
                    ComprehensionType::ListSet => {
                        let lst_comp = comprehension(
                            tokens, &mut pos, vars, info, SolidToken::Brace(IsOpen::True),
                            SolidToken::Brace(IsOpen::False),
                            AstNode::SetComprehension
                        );
                        extend_tree(ast, parent, lst_comp);
                        pos -= 1;
                    },
                    ComprehensionType::None => {
                        //1 set\dict literal
                        //3                                                                | this is a placeholder |
                        let list_parent = add_to_tree(parent, ast, Ast::new(AstNode::SetLiteral));
                        let starting_pos = pos;
                        if let SolidToken::Brace(IsOpen::False) = tokens[pos + 1] {
                            ast[list_parent].value = AstNode::DictLiteral;
                        } else {
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
                        }
                    }
                }
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
            SolidToken::Cast => {
                pos += 1;
                let typ= get_arg_typ(tokens, &mut pos, info);
                let cast = insert_as_parent_of_prev(ast, parent, AstNode::Cast);
                ast[cast].typ = Some(typ);
                pos -= 1;
            }
            _ => panic!("unexpected token {:?}", token)
        }
        pos += 1;
    }
    pos
}

enum ComprehensionType {
    ListSet, Dict, None
}
fn get_comprehension_typ(tokens: &[SolidToken], pos: usize) -> ComprehensionType {
    let mut is_dict = false;
    let mut amount_of_open = 0;
    for tok in tokens.iter().skip(pos) {
        match tok {
            SolidToken::Bracket(IsOpen::True)
            | SolidToken::Brace(IsOpen::True)
            | SolidToken::Parenthesis(IsOpen::True)
            => amount_of_open += 1,
            SolidToken::Bracket(IsOpen::False)
            | SolidToken::Brace(IsOpen::False)
            | SolidToken::Parenthesis(IsOpen::False)
            => {
                if amount_of_open == 1 { return ComprehensionType::None }
                amount_of_open -= 1
            },
            SolidToken::Colon if amount_of_open == 1 => is_dict = true,
            SolidToken::For if amount_of_open == 1 =>
                return if is_dict { ComprehensionType::Dict } else { ComprehensionType::ListSet },
            SolidToken::Comma if amount_of_open == 1 => return ComprehensionType::None,
            _ => ()
        }
    }
    unreachable!()
}

fn comprehension(
    tokens: &[SolidToken], pos: &mut usize, vars: &mut VarTypes, info: &mut Info,
    open_b_b_p: SolidToken, close_b_b_p: SolidToken, comprehension_typ: AstNode
) -> Vec<Ast> {
    let mut amount_of_open = 0;
    let mut toks = vec![];
    let mut comprehension = vec![Ast::new(comprehension_typ)];
    let stmt_mod = add_to_tree(
        0, &mut comprehension, Ast::new(AstNode::Module)
    );
    vars.push(HashMap::new());
    //1 statement
    loop {
        *pos += 1;
        match tokens[*pos] {
            SolidToken::Bracket(IsOpen::True)
            | SolidToken::Brace(IsOpen::True)
            | SolidToken::Parenthesis(IsOpen::True) =>
                if tokens[*pos] == open_b_b_p { amount_of_open += 1 },
            SolidToken::Bracket(IsOpen::False)
            | SolidToken::Brace(IsOpen::False)
            | SolidToken::Parenthesis(IsOpen::False ) =>
                if tokens[*pos] == close_b_b_p {
                    if amount_of_open == 0 { break }
                    amount_of_open -= 1
                },
            SolidToken::Colon if amount_of_open == 0 => {
                toks.push(SolidToken::NewLine);
                make_ast_expression(
                    &toks, 0, &mut comprehension, stmt_mod, vars, info
                );
                toks.clear();
                continue
            }
            SolidToken::For if amount_of_open == 0 => {
                println!("TOKS: {toks:?}");
                toks.push(SolidToken::NewLine);
                make_ast_expression(
                    &toks, 0, &mut comprehension, stmt_mod, vars, info
                );
                break
            }
            _ => ()
        }
        toks.push(tokens[*pos].clone());
    }
    let loops_mod = add_to_tree(0, &mut comprehension, Ast::new(AstNode::Module));
    //1 loops
    while *pos < tokens.len() && matches!(tokens[*pos], SolidToken::For) { //1 loops
        *pos += 1;
        let loop_pos = add_to_tree(
            loops_mod, &mut comprehension, Ast::new(AstNode::ForStatement)
        );
        let pars_pos = add_to_tree(
            loop_pos, &mut comprehension, Ast::new(AstNode::ColonParentheses)
        );
        let vars_pos = add_to_tree(
            pars_pos, &mut comprehension, Ast::new(AstNode::ForVars)
        );
        let iter_pos = add_to_tree(
            pars_pos, &mut comprehension, Ast::new(AstNode::ForIter)
        );
        vars.push(HashMap::new());
        //4 vars:
        get_for_loop_vars(tokens, pos, &mut comprehension, vars, vars_pos);
        //4 iters:
        amount_of_open = 0;
        toks.clear();
        loop { //1 statement
            match tokens[*pos] {
                SolidToken::Bracket(IsOpen::True)
                | SolidToken::Brace(IsOpen::True)
                | SolidToken::Parenthesis(IsOpen::True) =>
                    if tokens[*pos] == open_b_b_p { amount_of_open += 1 },
                SolidToken::Bracket(IsOpen::False)
                | SolidToken::Brace(IsOpen::False)
                | SolidToken::Parenthesis(IsOpen::False) =>
                    if tokens[*pos] == close_b_b_p {
                        if amount_of_open == 0 {
                            toks.push(SolidToken::NewLine);
                            make_ast_expression(
                                &toks, 0, &mut comprehension, iter_pos, vars, info
                            );
                            break
                        }
                        amount_of_open -= 1
                    },
                SolidToken::For | SolidToken::If if amount_of_open == 0 => {
                    toks.push(SolidToken::NewLine);
                    make_ast_expression(
                        &toks, 0, &mut comprehension, iter_pos, vars, info
                    );
                    break
                }
                _ => ()
            }
            toks.push(tokens[*pos].clone());
            *pos += 1;
        }
        vars.pop();
    }
    //1 condition
    let condition_mod = add_to_tree(0, &mut comprehension, Ast::new(AstNode::Module));
    if *pos < tokens.len() && matches!(tokens[*pos], SolidToken::If) {
        amount_of_open = 0;
        toks.clear();
        loop {
            *pos += 1;
            match tokens[*pos] {
                SolidToken::Bracket(IsOpen::True)
                | SolidToken::Brace(IsOpen::True)
                | SolidToken::Parenthesis(IsOpen::True) =>
                    if tokens[*pos] == open_b_b_p { amount_of_open += 1 },
                SolidToken::Bracket(IsOpen::False)
                | SolidToken::Brace(IsOpen::False)
                | SolidToken::Parenthesis(IsOpen::False ) =>
                    if tokens[*pos] == close_b_b_p {
                        if amount_of_open == 0 { break }
                        amount_of_open -= 1
                    },
                _ => ()
            }
            toks.push(tokens[*pos].clone());
        }
        toks.push(SolidToken::NewLine);
        make_ast_expression(&toks, 0, &mut comprehension, condition_mod, vars, info);
    }
    vars.pop();
    comprehension
}

fn word_tok(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, info: &mut Info, st: &String, is_expression: bool
) -> usize {
    let mut identifier_pos = add_to_tree(
        parent, ast,
        Ast::new(AstNode::Identifier(st.clone()))
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
                if get_from_stack(vars, st).is_none() {
                    ast[index].value = AstNode::FirstAssignment;
                    add_to_stack(vars, st.clone(), usize::MAX)
                }
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
                add_to_stack(vars, st.clone(), identifier_pos);
                pos += 1;
                if !matches!(tokens[pos], SolidToken::Operator(OperatorType::Eq)) {
                    ast[index].typ = Some(get_arg_typ(tokens, &mut pos, info));
                }
                return make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                ) - 1;
            },
            SolidToken::Brace(IsOpen::True)
            | SolidToken::Parenthesis(IsOpen::True) => {
                let word = unwrap_enum!(&tokens[pos - 1], SolidToken::Word(w), w);
                // if unsafe { IS_COMPILED } && word == "len" {
                //     turn_len_func_to_method(tokens, &mut pos, ast, parent, vars, info, word);
                //     continue
                // }
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
                    if let SolidToken::Word(name) = &tokens[pos + 1] { //1 if is named arg
                        if let SolidToken::Operator(OperatorType::Eq) = &tokens[pos + 2] {
                            let named_arg_pos = add_to_tree(
                                last, ast,
                                Ast::new(AstNode::NamedArg(name.clone()))
                            );
                            pos = make_ast_expression(
                                tokens, pos + 3, ast, named_arg_pos, vars, info
                            );
                            continue
                        }
                    }
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
            SolidToken::Operator(OperatorType::OpEq(op)) => {
                if is_expression { return pos - 1 }

                let mut parent = parent;
                while ast[parent].value.is_expression() {
                    parent = ast[parent].parent.unwrap();
                }
                let index = insert_as_parent_of_prev(ast, parent, AstNode::OpAssignment(*op.clone()));

                return make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                ) - 1;
            }
            _ if is_expression => return pos - 1,
            _ => panic!("Unexpected token {:?}", tokens[pos]),
        }
    }
}

/*fn turn_len_func_to_method(
    tokens: &[SolidToken], pos: &mut usize, ast: &mut Vec<Ast>, parent: usize,
    vars: &mut VarTypes, info: &mut Info, word: &str
) {
    let is_property = matches!(&ast[parent].value, AstNode::Property);
    ast.pop();
    let parent_children = ast[parent].children.as_mut().unwrap();
    parent_children.pop();
    let prop = if is_property { parent } else {
        add_to_tree(parent, ast, Ast::new(AstNode::Property))
    };
    if !is_property {
        *pos = make_ast_expression(
            tokens, *pos + 1, ast, prop, vars, info
        );
    } else {
        *pos += 1;
    }
    let func_call_pos = add_to_tree(
        prop, ast, Ast::new(AstNode::FunctionCall(false))
    );
    add_to_tree(
        func_call_pos, ast,
        Ast::new(AstNode::Identifier(String::from(word)))
    );
    add_to_tree(
        func_call_pos, ast, Ast::new(AstNode::Args)
    );
}*/

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
        vars, info
    );
    vars.pop();
    pos - 1
}

fn for_statement(
    tokens: &[SolidToken], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    indent: usize, vars: &mut VarTypes, info: &mut Info
) -> usize {
    let loop_pos = add_to_tree(parent, ast, Ast::new(AstNode::ForStatement));
    let pars_pos = add_to_tree(loop_pos, ast, Ast::new(AstNode::ColonParentheses));
    let vars_pos = add_to_tree(pars_pos, ast, Ast::new(AstNode::ForVars));
    let iter_pos = add_to_tree(pars_pos, ast, Ast::new(AstNode::ForIter));
    let body_pos = add_to_tree(loop_pos, ast, Ast::new(AstNode::Body));
    vars.push(HashMap::new());
    //4 vars:
    get_for_loop_vars(tokens, &mut pos, ast, vars, vars_pos);
    //4 iters:
    pos = make_ast_expression(
        tokens, pos, ast, iter_pos, vars, info
    );
    //4 body:
    pos = make_ast_statement(
        tokens, pos + 1, ast, body_pos, indent + 1,
        vars, info
    );
    vars.pop();
    pos - 1
}

fn get_for_loop_vars(
    tokens: &[SolidToken], pos: &mut usize, ast: &mut Vec<Ast>,
    vars: &mut VarTypes, vars_pos: usize
) {
    loop {
        let name = unwrap_enum!(&tokens[*pos], SolidToken::Word(x), x, "expected identifier");
        let i = add_to_tree(
            vars_pos, ast,
            Ast::new(AstNode::Identifier(name.clone()))
        );
        add_to_stack(vars, name.clone(), i);
        *pos += 2;
        match &tokens[*pos - 1] {
            SolidToken::Comma => continue,
            SolidToken::In => break,
            _ => panic!("expected `in` or `,`")
        }
    }
}
