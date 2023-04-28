use std::collections::{HashMap, HashSet};
use std::env::{args, var};
use std::fs;
use std::fmt::Write;
use std::path::{Path, PathBuf};
use lazy_static::lazy_static;
use crate::construct_ast::ast_structure::{Ast, AstNode, Param};
use crate::{DONT_PRINT, EMPTY_STR, IS_COMPILED, PARSED_FILES, CUR_LINE, CUR_COL, SRC_CODE, CUR_PATH, MODULE_PATH, LINE_DIFF, StrToType};
use crate::{OneOfEnums, parse_file, unwrap_enum, throw};
use crate::add_types::ast_add_types::add_types;
use crate::add_types::utils::{add_new_line, add_to_stack, get_from_stack, join, update_pos_from_token};
use crate::mold_tokens::{IsOpen, OperatorType, Pos, SolidToken, SolidTokenWPos};
use crate::types::{Type, TypeKind, unwrap_u};
use crate::construct_ast::get_functions_and_types::{get_struct_and_func_names};
use crate::construct_ast::get_typ::{get_arg_typ};
use crate::construct_ast::make_func_struct_trait::{make_struct, make_func, make_trait, make_enum};
use crate::construct_ast::tree_utils::{add_to_tree, extend_tree, get_last, insert_as_parent_of_prev, print_tree, update_pos_from_tree_node};
use crate::to_rust::to_rust;

// todo this shouldn't work: a = [1 2 3 4]
//  (it works cuz add_expression for a num just adds it to parent and then continues to the next token)

pub type TraitFuncs = HashMap<String, (usize, FuncType)>;

//1 the name of the var to its position (it could have multiple if it has been overridden)
pub type VarTypes = Vec<HashMap<String, (usize, usize)>>;
pub type GenericTypes = Vec<HashSet<String>>;
pub type TraitTypes = HashMap<String, TraitType>;
pub type EnumTypes = HashMap<String, EnumType>;
pub type StructTypes = HashMap<String, StructType>;
pub type FuncTypes = HashMap<String, FuncType>;
pub type TypeTypes = StrToType;

pub trait STType {
    fn get_associated_types(&self) -> &Option<Vec<String>>;
    fn get_generics(&self) -> &Option<Vec<String>>;
    fn get_generics_mut(&mut self) -> &mut Option<Vec<String>>;
    fn get_pos(&self) -> usize;
}
#[derive(Clone, Debug)]
pub struct TraitType {
    pub generics: Option<Vec<String>>,
    pub associated_types: Option<Vec<String>>,
    pub pos: usize,
    pub parent_file: String,
}
#[derive(Clone, Debug)]
pub struct StructType {
    pub generics: Option<Vec<String>>,
    pub associated_types: Option<Vec<String>>,
    pub pos: usize,
    pub parent_file: String,
}
#[derive(Clone, Debug)]
pub struct EnumType {
    pub generics: Option<Vec<String>>,
    pub pos: usize,
    pub parent_file: String,
}
impl STType for StructType {
    fn get_associated_types(&self) -> &Option<Vec<String>> { &self.associated_types }
    fn get_generics(&self) -> &Option<Vec<String>> { &self.generics }
    fn get_generics_mut(&mut self) -> &mut Option<Vec<String>> { &mut self.generics }
    fn get_pos(&self) -> usize { self.pos }
}
impl STType for TraitType {
    fn get_associated_types(&self) -> &Option<Vec<String>> { &self.associated_types }
    fn get_generics(&self) -> &Option<Vec<String>> { &self.generics }
    fn get_generics_mut(&mut self) -> &mut Option<Vec<String>> { &mut self.generics }
    fn get_pos(&self) -> usize { self.pos }
}
impl STType for EnumType {
    fn get_associated_types(&self) -> &Option<Vec<String>> { unreachable!() }
    fn get_generics(&self) -> &Option<Vec<String>> { &self.generics }
    fn get_generics_mut(&mut self) -> &mut Option<Vec<String>> { &mut self.generics }
    fn get_pos(&self) -> usize { self.pos }
}


#[derive(Clone, Debug, PartialEq, Eq)]
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
    pub one_of_enums: &'a mut OneOfEnums,
    pub types: &'a mut TypeTypes,
    pub generics: &'a mut GenericTypes,
    pub struct_associated_types: &'a mut HashSet<String>,
    pub cur_file_path: &'a mut PathBuf,
}

#[derive(Debug)]
pub struct FileInfo {
    pub funcs: FuncTypes,
    pub types: TypeTypes,
    pub structs: HashMap<String, (StructType, Vec<Ast>)>, //3 not very efficient...
    pub traits: HashMap<String, (TraitType, Vec<Ast>)>,
    pub enums: HashMap<String, (EnumType, Vec<Ast>)>,
    // pub one_of_enums: &'a mut HashMap<String, String>,
}


pub fn construct_ast(tokens: &[SolidTokenWPos], pos: usize, info: &mut Info) -> Vec<Ast> {
    let (structs, funcs, traits, types, enums)
        = get_struct_and_func_names(tokens, info.cur_file_path.to_str().unwrap());
    let mut ast = vec![Ast::new(AstNode::Module, Pos::default())];
    *info.funcs = funcs;
    *info.structs = structs;
    *info.traits = traits;
    *info.types = types;
    *info.enums = enums;
    *info.generics = vec![];
    *info.struct_associated_types = HashSet::new();
    make_ast_statement(
        tokens, pos, &mut ast, 0, 0,
        &mut vec![HashMap::new()], info
    );
    // duck_type(&mut ast, info.traits, info.structs);
    if unsafe { !DONT_PRINT } {
        print_tree(&ast, 0);
    }
    if unsafe { IS_COMPILED } {
        add_types(
            &mut ast, 0, &mut vec![HashMap::new()],
            info, &None
        );
        if unsafe { !DONT_PRINT } {
            print_tree(&ast, 0);
        }
    }
    ast
}

pub fn add_trait_to_struct(
    ast: &[Ast], struct_name: &str, funcs: &TraitFuncs, trt_name: &str, trt_funcs: &TraitFuncs,
    trait_types: &Option<StrToType>, trait_generics: &Option<Vec<Type>>, info: &Info
) -> String {
    let expected_generics = &info.traits[trt_name].generics;
    let generics = if matches!(expected_generics, Some(vc) if !vc.is_empty()) {
        assert_eq!(
            trait_generics.as_ref().unwrap().len(),
            expected_generics.as_ref().unwrap().len()
        );
        format!("<{}>", join(
            trait_generics.as_ref().unwrap().iter().map(|x| x.to_string()),
            ","
        ))
    } else {
        assert!(trait_generics.is_none() || !trait_generics.as_ref().unwrap().iter().any(|x|
            matches!(&x.kind, TypeKind::Generic(_))
        ));
        EMPTY_STR
    };
    let associated_types = if let Some(a_types) = trait_types {
        join(
            a_types.iter().map(|(name, typ)| format!("type {name} = {typ};")),
            "\n"
        )
    } else {
        EMPTY_STR
    };
    format!(
        "impl {trt_name}{generics} for {struct_name} {{\n\t{associated_types}\n\t{}\n}}",
        join(trt_funcs.iter().map(
            |(func_name, _)| {
                let new_func_name = format!("{trt_name}::{func_name}");
                //1 if already contains func with that name
                if funcs.keys().any(|x| *x == new_func_name) { EMPTY_STR }
                else {
                    let func = funcs.get(func_name).unwrap();
                    to_rust(ast, func.0, 1, info).strip_prefix("pub ").unwrap().to_string()
                }
            }
        ), "\n\t")
    )
}

//1 gets all the functions of the struct or trait (i think)
pub fn get_trt_strct_functions(ast: &[Ast], module: &Ast) -> TraitFuncs {
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
                                default_val_pos: ast[*x].children.as_ref().map(|ch| ch[0])
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
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, info: &mut Info
) -> usize {
    while pos < tokens.len() {
        update_pos_from_token(&tokens[pos]);

        match &tokens[pos].tok {
            SolidToken::Struct => {
                vars.push(vars.last().unwrap().clone());
                pos = make_struct(
                    tokens, pos + 1, ast, parent, indent, info
                );
                vars.pop();
            },
            SolidToken::Def  => {
                vars.push(vars.last().unwrap().clone());
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
                } else { throw!("elif to unknown if") }
            },
            SolidToken::Else => {
                if let SolidToken::Colon = tokens[pos + 1].tok {
                    pos += 1;
                } else {
                    update_pos_from_token(&tokens[pos + 1]);
                    throw!("expected colon")
                }
                let last = get_last(&ast[parent].children);
                if let AstNode::IfStatement = ast[last].value {
                    let body_pos = add_to_tree(last, ast, Ast::new_no_pos(AstNode::Body));
                    vars.push(vars.last().unwrap().clone());
                    pos = make_ast_statement(
                        tokens, pos + 1, ast, body_pos, indent + 1,
                        vars, info
                    ) - 1;
                    vars.pop();
                } else { throw!("else to unknown if") }
            },
            SolidToken::Pass => {
                add_to_tree(parent, ast, Ast::new(AstNode::Pass, tokens[pos].pos.clone()));
            },
            SolidToken::Word(st) => {
                pos = word_tok(
                    tokens, pos, ast, parent, indent, vars,
                    info, st, false
                );
            }
            SolidToken::NewLine => {
                let tabs = tokens.iter().skip(pos + 1)
                    .take_while(|&x| matches!(x.tok, SolidToken::Tab)).count();
                match tabs.cmp(&indent) {
                    std::cmp::Ordering::Less => return pos,
                    std::cmp::Ordering::Greater => {
                        update_pos_from_token(&tokens[pos + 1 + indent]);
                        throw!("unexpected indentation, expected `{}` found `{}`", indent, tabs)
                    },
                    _ => ()
                }
                pos += tabs;
            },
            SolidToken::Return => {
                let return_pos = add_to_tree(
                    parent, ast, Ast::new(AstNode::Return, tokens[pos].pos.clone())
                );
                if !matches!(tokens[pos + 1].tok, SolidToken::NewLine) {
                    pos = make_ast_expression(
                        tokens, pos + 1, ast, return_pos, vars, info
                    ) - 1;
                }
            },
            SolidToken::Continue => {
                add_to_tree(parent, ast, Ast::new(AstNode::Continue, tokens[pos].pos.clone()));
            },
            SolidToken::Break => {
                add_to_tree(parent, ast, Ast::new(AstNode::Break, tokens[pos].pos.clone()));
            }
            SolidToken::IMut => {
                let st = unwrap_enum!(&tokens[pos + 1].tok, SolidToken::Word(st), st);
                pos = word_tok(
                    tokens, pos + 1, ast, parent, indent,
                    vars, info, st, false
                );
                let assignment_pos = *unwrap_u(&ast[parent].children).last().unwrap();
                let assignment = &mut ast[assignment_pos];
                assignment.is_mut = false;
                let var_pos = ast[assignment_pos].ref_children()[0];
                ast[var_pos].is_mut = false;
            }
            SolidToken::UnaryOperator(OperatorType::Dereference) => {
                let deref_pos = add_to_tree(parent, ast, Ast::new(
                    AstNode::UnaryOp(OperatorType::Dereference),
                    tokens[pos].pos.clone()
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
                while !matches!(tokens[pos].tok, SolidToken::NewLine) {
                    pos += 1;
                }
            }
            SolidToken::Enum => {
                pos = make_enum(tokens, pos + 1, ast, parent, indent, info);
            }
            SolidToken::From => {
                let import_pos = add_to_tree(parent, ast, Ast::new_no_pos(AstNode::Import));
                let from_pos = add_to_tree(import_pos, ast, Ast::new(AstNode::From, tokens[pos].pos.clone()));
                let mut module = vec![];
                while module.is_empty() || matches!(tokens[pos].tok, SolidToken::Period) {
                    pos += 1;
                    update_pos_from_token(&tokens[pos]);
                    let word = unwrap_enum!(
                        &tokens[pos].tok, SolidToken::Word(word), word,
                        "invalid import module, expected `word`"
                    );
                    module.push(word);
                    add_to_tree(from_pos, ast, Ast::new(
                        AstNode::Identifier(word.clone()),
                        tokens[pos].pos.clone()
                    ));
                    pos += 1;
                }
                let last = if unsafe { IS_COMPILED } {
                    format!("{}.mo", module.pop().unwrap())
                } else {
                    format!("{}.py", module.pop().unwrap())
                };
                module.push(&last);
                let module_name = join(module.iter(), ".");
                let module = find_module(
                    &module, info.cur_file_path.parent().unwrap()
                ).unwrap_or_else(|| throw!("couldn't find module `{}`", module_name));
                let module_path = module.to_str().unwrap().to_string();
                let file_info = if let Some(info) =  unsafe {
                    PARSED_FILES.get(&module_name)
                } {
                    info
                } else {
                    parse_file(&module_path, info.one_of_enums);
                    unsafe { PARSED_FILES.get(&module_path).unwrap() }
                };

                update_pos_from_token(&tokens[pos]);
                unwrap_enum!(tokens[pos].tok, SolidToken::Import, false, "expected `import`");
                ast[import_pos].pos = Some(tokens[pos].pos.clone());

                pos += 1;
                loop {
                    update_pos_from_token(&tokens[pos]);
                    let word = unwrap_enum!(
                        &tokens[pos].tok, SolidToken::Word(word), word,
                        "invalid import name, expected `word`"
                    );
                    add_to_tree(import_pos, ast, Ast::new(AstNode::Identifier(word.clone()), tokens[pos].pos.clone()));
                    pos += 1;
                    let name = if let SolidToken::As = tokens[pos].tok {
                        Some(unwrap_enum!(&tokens[pos + 1].tok, SolidToken::Word(word), word.clone()))
                    } else {
                        None
                    };

                    // todo not sure what this does...
                    if let Some(val) = file_info.funcs.get(word) {
                        info.funcs.insert(name.unwrap_or_else(|| word.clone()), val.clone());
                    } else if let Some(val) = file_info.structs.get(word) {
                        let ignore = add_to_tree(0, ast, Ast::new_no_pos(AstNode::Ignore));
                        let index = ast.len();
                        extend_tree(ast, ignore, val.clone().1);
                        // if let Some(name) = &name {
                        //     ast[index].value = AstNode::Struct(name);
                        // }
                        info.structs.insert(name.unwrap_or_else(|| word.clone()), StructType {
                            generics: val.0.generics.clone(),
                            associated_types: val.0.associated_types.clone(),
                            pos: index,
                            parent_file: module_path.clone()
                        });
                    } else if let Some(val) = file_info.traits.get(word) {
                        let index = ast.len();
                        extend_tree(ast, 0, val.clone().1);
                        info.traits.insert(name.unwrap_or_else(|| word.clone()), TraitType {
                            generics: val.0.generics.clone(),
                            associated_types: val.0.associated_types.clone(),
                            pos: index,
                            parent_file: module_path.clone()
                        });
                    } else if let Some(val) = file_info.enums.get(word) {
                        let index = ast.len();
                        extend_tree(ast, 0, val.clone().1);
                        info.enums.insert(name.unwrap_or_else(|| word.clone()), EnumType {
                            generics: val.0.generics.clone(),
                            pos: index,
                            parent_file: module_path.clone()
                        });
                    } else if let Some(val) = file_info.types.get(word) {
                        info.types.insert(name.unwrap_or_else(|| word.clone()), val.clone());
                    }
                    match &tokens[pos].tok {
                        SolidToken::Comma => pos += 1,
                        SolidToken::As => {
                            pos += 1; //1 the new name
                            insert_as_parent_of_prev(ast, import_pos, AstNode::As(
                                unwrap_enum!(&tokens[pos].tok, SolidToken::Word(w), w.clone())
                            ), Some(tokens[pos].pos.clone()));
                            pos += 1;
                            match tokens[pos].tok {
                                SolidToken::Comma => pos += 1,
                                SolidToken::NewLine => break,
                                _ => {
                                    update_pos_from_token(&tokens[pos]);
                                    throw!("expected ',' or '\n' but found `{}`", tokens[pos].tok)
                                }
                            }
                        },
                        SolidToken::NewLine => break,
                        _ => {
                            update_pos_from_token(&tokens[pos]);
                            throw!("expected ',' but found `{}`", tokens[pos].tok)
                        }
                    }
                }

                // ast[import_pos].pos = Some(Pos {
                //     start_line: ast[from_pos].pos.as_ref().unwrap().start_line,
                //     start_col: ast[from_pos].pos.as_ref().unwrap().start_col,
                //     end_line: ast.last().unwrap().pos.as_ref().unwrap().end_line,
                //     end_col: ast.last().unwrap().pos.as_ref().unwrap().end_col,
                // })
            }
            SolidToken::Import => todo!(),
            SolidToken::Match => {
                vars.push(vars.last().unwrap().clone());

                let match_pos = add_to_tree(parent, ast, Ast::new(AstNode::Match, tokens[pos].pos.clone()));
                pos = make_ast_expression(tokens, pos+1, ast, match_pos, vars, info);

                if let SolidToken::Colon = tokens[pos].tok {
                    pos += 1;
                } else {
                    update_pos_from_token(&tokens[pos]);
                    throw!("expected `:`, found `{}`", tokens[pos].tok)
                }

                pos = make_ast_statement(tokens, pos, ast, match_pos, indent + 1, vars, info) - 1;
                vars.pop();
            }
            SolidToken::Case => {
                let case = add_to_tree(parent, ast, Ast::new(AstNode::Case, tokens[pos].pos.clone()));
                let expression = add_to_tree(case, ast, Ast::new_no_pos(AstNode::Body));

                // pos = make_ast_expression(tokens, pos + 1, ast, case, vars, info);
                pos += 1;
                if let SolidToken::Word(wrd) = &tokens[pos].tok {
                    add_to_tree(expression, ast, Ast::new(AstNode::Identifier(wrd.clone()), tokens[pos].pos.clone()));
                } else if let SolidToken::Null = &tokens[pos].tok {
                    add_to_tree(expression, ast, Ast::new(AstNode::Null, tokens[pos].pos.clone()));
                } else {
                    update_pos_from_token(&tokens[pos]);
                    throw!("expected identifier")
                }
                // let idf = unwrap_enum!(&tokens[pos], SolidToken::Word(wrd), wrd.clone(), "expected identifier");
                // add_to_tree(expression, ast, Ast::new(AstNode::Identifier(idf)));

                pos += 1;
                while let SolidToken::Period = tokens[pos].tok {
                    pos += 1;
                    update_pos_from_token(&tokens[pos]);
                    let idf = unwrap_enum!(&tokens[pos].tok, SolidToken::Word(wrd), wrd.clone(), "expected identifier");
                    let prop = insert_as_parent_of_prev(
                        ast, expression, AstNode::Property,
                        Some(tokens[pos - 1].pos.clone())
                    );
                    add_to_tree(prop, ast, Ast::new(AstNode::Identifier(idf), tokens[pos].pos.clone()));
                    pos += 1;
                }
                if let SolidToken::Parenthesis(IsOpen::True) = tokens[pos].tok {
                    // todo allow this too: `case opt1(a, int(b)):`
                    let parn = add_to_tree(expression, ast, Ast::new(AstNode::Parentheses, tokens[pos].pos.clone()));
                    loop {
                        pos += 1;
                        update_pos_from_token(&tokens[pos]);
                        let name = unwrap_enum!(&tokens[pos].tok, SolidToken::Word(wrd), wrd.clone(), "expected identifier");
                        add_to_tree(parn, ast, Ast::new(AstNode::Identifier(name), tokens[pos].pos.clone()));
                        pos += 1;
                        if !matches!(tokens[pos].tok, SolidToken::Comma) { break }
                    }
                    if let SolidToken::Parenthesis(IsOpen::False) = tokens[pos].tok {
                        pos += 1;
                    } else {
                        update_pos_from_token(&tokens[pos]);
                        throw!("expected `)` but found `{}`", tokens[pos].tok)
                    }
                }
                if let SolidToken::Colon = tokens[pos].tok {
                    pos += 1;
                } else if let SolidToken::As = tokens[pos].tok {
                    pos += 1;
                    add_to_tree(
                        case, ast, Ast::new(
                            AstNode::Identifier(
                                unwrap_enum!(&tokens[pos].tok, SolidToken::Word(wrd), wrd.clone())
                            ),
                            tokens[pos].pos.clone()
                        )
                    );
                    pos += 1;
                    if let SolidToken::Colon = tokens[pos].tok {
                        pos += 1;
                    } else {
                        update_pos_from_token(&tokens[pos]);
                        throw!("expected `:` but found `{}`", tokens[pos].tok)
                    }
                } else {
                    update_pos_from_token(&tokens[pos]);
                    throw!("expected `:` but found `{}`", tokens[pos].tok)
                }

                let body = add_to_tree(case, ast, Ast::new_no_pos(AstNode::Body));
                pos = make_ast_statement(tokens, pos, ast, body, indent + 1, vars, info) - 1;
            }
            _ => {
                update_pos_from_token(&tokens[pos]);
                throw!("unexpected token `{}`", tokens[pos].tok)
            }
        }
        pos += 1;
    }
    pos
}

pub fn make_ast_expression(
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    vars: &mut VarTypes, info: &mut Info
) -> usize {
    // println!("{}\n".to_str(&(tree.clone(), 0)));
    // todo lets random tokens to go together e.g. 1 + 2 a 4
    let mut amount_of_open = 0;
    while pos < tokens.len() {
        update_pos_from_token(&tokens[pos]);

        let token = &tokens[pos].tok;
        let token = if let SolidToken::In = token {
            &SolidToken::Operator(OperatorType::In)
        } else { token };
        match token {
            SolidToken::Parenthesis(IsOpen::True) => {
                amount_of_open += 1;
                let par_pos = add_to_tree(parent, ast, Ast::new(AstNode::Parentheses, tokens[pos].pos.clone()));
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
                | SolidToken::Word(_) = &tokens[pos - 1].tok {
                    //1 index
                    let index = insert_as_parent_of_prev(
                        ast, parent, AstNode::Index, Some(tokens[pos].pos.clone())
                    );
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
                    let list_parent = add_to_tree(parent, ast, Ast::new(AstNode::ListLiteral, tokens[pos].pos.clone()));
                    let starting_pos = pos;
                    while matches!(&tokens[pos].tok, SolidToken::Comma) || pos == starting_pos {
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
                        let list_parent = add_to_tree(parent, ast, Ast::new(AstNode::SetLiteral, tokens[pos].pos.clone()));
                        let starting_pos = pos;
                        if let SolidToken::Brace(IsOpen::False) = tokens[pos + 1].tok {
                            ast[list_parent].value = AstNode::DictLiteral;
                        } else {
                            while matches!(&tokens[pos].tok, SolidToken::Comma | SolidToken::Colon) || pos == starting_pos {
                                if let SolidToken::Colon = tokens[pos].tok {
                                    if let AstNode::DictLiteral = ast[list_parent].value {
                                        if unwrap_u(&ast[list_parent].children).len() % 2 != 1 { throw!() } // todo throw message
                                    } else {
                                        if unwrap_u(&ast[list_parent].children).len() != 1 { throw!() } // todo throw message
                                        ast[list_parent].value = AstNode::DictLiteral;
                                    }
                                } else if let AstNode::DictLiteral = ast[list_parent].value {
                                    if unwrap_u(&ast[list_parent].children).len() % 2 == 1 { throw!() } // todo throw message
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
            SolidToken::Comma => {
                if matches!(ast[parent].value, AstNode::Parentheses | AstNode::Tuple) {
                    ast[parent].value = AstNode::Tuple;
                    while let SolidToken::Comma = tokens[pos].tok {
                        pos = make_ast_expression(tokens, pos + 1, ast, parent, vars, info) - 1;
                    }
                } else { break }
            }
            SolidToken::Colon => break,
            SolidToken::Num(num) => {
                add_to_tree(parent, ast, Ast::new(AstNode::Number(num.clone()), tokens[pos].pos.clone()));
            },
            SolidToken::Str { val: str, mutable } => {
                add_to_tree(
                    parent, ast, Ast::new(
                        AstNode::String{ val: str.clone(), mutable: *mutable },
                        tokens[pos].pos.clone()
                    )
                );
            },
            SolidToken::Char(ch) => {
                add_to_tree(parent, ast, Ast::new(AstNode::Char(ch.clone()), tokens[pos].pos.clone()));
            },
            SolidToken::Bool(bl) => {
                add_to_tree(parent, ast, Ast::new(AstNode::Bool(*bl), tokens[pos].pos.clone()));
            },
            SolidToken::Null => {
                add_to_tree(parent, ast, Ast::new(AstNode::Null, tokens[pos].pos.clone()));
            },
            SolidToken::Word(wrd) => {
                pos = word_tok(
                    tokens, pos, ast, parent, 0, 
                    vars, info, wrd, true
                );
            },
            SolidToken::UnaryOperator(op) => {
                let index = add_to_tree(
                    parent, ast, Ast::new(AstNode::UnaryOp(op.clone()), tokens[pos].pos.clone())
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
                let index = insert_as_parent_of_prev(
                    ast, parent, AstNode::Operator(op.clone()),
                    Some(tokens[pos].pos.clone())
                );
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
                let cast = insert_as_parent_of_prev(
                    ast, parent, AstNode::Cast, Some(tokens[pos].pos.clone())
                );
                ast[cast].typ = Some(typ);
                pos -= 1;
            }
            SolidToken::If => { //1 ternary
                let mut parent = parent;
                while ast[parent].value.is_expression() {
                    parent = ast[parent].parent.unwrap();
                }
                let index = insert_as_parent_of_prev(
                    ast, parent, AstNode::Ternary, Some(tokens[pos].pos.clone())
                );
                let mut amount_of_open = 0;
                for (i, tok) in tokens.iter().enumerate().skip(pos) {
                    match tok.tok {
                        SolidToken::Brace(IsOpen::True) | SolidToken::Bracket(IsOpen::True)
                        | SolidToken::Parenthesis(IsOpen::True) => amount_of_open += 1,
                        SolidToken::Brace(IsOpen::False) | SolidToken::Bracket(IsOpen::False)
                        | SolidToken::Parenthesis(IsOpen::False) => amount_of_open -= 1,
                        SolidToken::Else if amount_of_open == 0 => {
                            make_ast_expression(
                                &tokens[pos+1..i], 0, ast, index, vars, info
                            );
                            pos = i + 1;
                            break
                        },
                        _ => ()
                    }
                }
                make_ast_expression(tokens, pos, ast, index, vars, info);
            }
            _ => throw!("unexpected token {:?}", token)
        }
        pos += 1;
    }
    pos
}

enum ComprehensionType {
    ListSet, Dict, None
}
fn get_comprehension_typ(tokens: &[SolidTokenWPos], pos: usize) -> ComprehensionType {
    let mut is_dict = false;
    let mut amount_of_open = 0;
    for tok in tokens.iter().skip(pos) {
        match tok.tok {
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
    tokens: &[SolidTokenWPos], pos: &mut usize, vars: &mut VarTypes, info: &mut Info,
    open_b_b_p: SolidToken, close_b_b_p: SolidToken, comprehension_typ: AstNode
) -> Vec<Ast> {
    let mut amount_of_open = 0;
    let mut toks: Vec<SolidTokenWPos> = vec![];
    let mut comprehension = vec![Ast::new(comprehension_typ, tokens[*pos].pos.clone())];
    let stmt_mod = add_to_tree(
        0, &mut comprehension, Ast::new_no_pos(AstNode::Module)
    );
    vars.push(vars.last().unwrap().clone());
    //1 statement
    loop {
        *pos += 1;
        match tokens[*pos].tok {
            SolidToken::Bracket(IsOpen::True)
            | SolidToken::Brace(IsOpen::True)
            | SolidToken::Parenthesis(IsOpen::True) =>
                if tokens[*pos].tok == open_b_b_p { amount_of_open += 1 },
            SolidToken::Bracket(IsOpen::False)
            | SolidToken::Brace(IsOpen::False)
            | SolidToken::Parenthesis(IsOpen::False ) =>
                if tokens[*pos].tok == close_b_b_p {
                    if amount_of_open == 0 { break }
                    amount_of_open -= 1
                },
            SolidToken::Colon if amount_of_open == 0 => {
                add_new_line(&mut toks);
                make_ast_expression(
                    &toks, 0, &mut comprehension, stmt_mod, vars, info
                );
                toks.clear();
                continue
            }
            SolidToken::For if amount_of_open == 0 => {
                add_new_line(&mut toks);
                make_ast_expression(
                    &toks, 0, &mut comprehension, stmt_mod, vars, info
                );
                break
            }
            _ => ()
        }
        toks.push(tokens[*pos].clone());
    }
    let loops_mod = add_to_tree(0, &mut comprehension, Ast::new_no_pos(AstNode::Module));
    //1 loops
    while *pos < tokens.len() && matches!(tokens[*pos].tok, SolidToken::For) { //1 loops
        *pos += 1;
        let loop_pos = add_to_tree(
            loops_mod, &mut comprehension, Ast::new(AstNode::ForStatement, tokens[*pos - 1].pos.clone())
        );
        let pars_pos = add_to_tree(
            loop_pos, &mut comprehension, Ast::new_no_pos(AstNode::ColonParentheses)
        );
        let vars_pos = add_to_tree(
            pars_pos, &mut comprehension, Ast::new_no_pos(AstNode::ForVars)
        );
        let iter_pos = add_to_tree(
            pars_pos, &mut comprehension, Ast::new_no_pos(AstNode::ForIter)
        );
        vars.push(vars.last().unwrap().clone());
        //4 vars:
        get_for_loop_vars(tokens, pos, &mut comprehension, vars, vars_pos);
        //4 iters:
        amount_of_open = 0;
        toks.clear();
        loop { //1 statement
            match tokens[*pos].tok {
                SolidToken::Bracket(IsOpen::True)
                | SolidToken::Brace(IsOpen::True)
                | SolidToken::Parenthesis(IsOpen::True) =>
                    if tokens[*pos].tok == open_b_b_p { amount_of_open += 1 },
                SolidToken::Bracket(IsOpen::False)
                | SolidToken::Brace(IsOpen::False)
                | SolidToken::Parenthesis(IsOpen::False) =>
                    if tokens[*pos].tok == close_b_b_p {
                        if amount_of_open == 0 {
                            add_new_line(&mut toks);
                            make_ast_expression(
                                &toks, 0, &mut comprehension, iter_pos, vars, info
                            );
                            break
                        }
                        amount_of_open -= 1
                    },
                SolidToken::For | SolidToken::If if amount_of_open == 0 => {
                    add_new_line(&mut toks);
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
    let condition_mod = add_to_tree(0, &mut comprehension, Ast::new_no_pos(AstNode::Module));
    if *pos < tokens.len() && matches!(tokens[*pos].tok, SolidToken::If) {
        amount_of_open = 0;
        toks.clear();
        loop {
            *pos += 1;
            match tokens[*pos].tok {
                SolidToken::Bracket(IsOpen::True)
                | SolidToken::Brace(IsOpen::True)
                | SolidToken::Parenthesis(IsOpen::True) =>
                    if tokens[*pos].tok == open_b_b_p { amount_of_open += 1 },
                SolidToken::Bracket(IsOpen::False)
                | SolidToken::Brace(IsOpen::False)
                | SolidToken::Parenthesis(IsOpen::False ) =>
                    if tokens[*pos].tok == close_b_b_p {
                        if amount_of_open == 0 { break }
                        amount_of_open -= 1
                    },
                _ => ()
            }
            toks.push(tokens[*pos].clone());
        }
        add_new_line(&mut toks);
        make_ast_expression(&toks, 0, &mut comprehension, condition_mod, vars, info);
    }
    vars.pop();
    comprehension
}

fn word_tok(
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize, indent: usize,
    vars: &mut VarTypes, info: &mut Info, st: &String, is_expression: bool
) -> usize {
    let mut st_clone = st.clone();
    if let Some((_, nm)) = get_from_stack(vars, st) {
        for i in 1..=nm {
            write!(&mut st_clone, "___{i}").unwrap();
        }
    }
    let mut identifier_pos = add_to_tree(
        parent, ast,
        Ast::new(AstNode::Identifier(st_clone), tokens[pos].pos.clone())
    );
    loop {
        pos += 1;
        match &tokens[pos].tok {
            SolidToken::NewLine => { return pos - 1 },
            SolidToken::Operator(OperatorType::Eq) => {
                if is_expression { return pos - 1 }

                let mut parent = parent;
                while ast[parent].value.is_expression() {
                    parent = ast[parent].parent.unwrap();
                }
                let index = insert_as_parent_of_prev(
                    ast, parent, AstNode::Assignment,
                    Some(tokens[pos].pos.clone())
                );
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

                let index = insert_as_parent_of_prev(ast, parent, AstNode::FirstAssignment, None);

                // TODO-TODO
                pos += 1;
                if !matches!(tokens[pos].tok, SolidToken::Operator(OperatorType::Eq)) {
                    ast[index].typ = Some(get_arg_typ(tokens, &mut pos, info));
                    update_pos_from_token(&tokens[pos]);
                    unwrap_enum!(
                        tokens[pos].tok, SolidToken::Operator(OperatorType::Eq), None::<bool>,
                        "expected `=` but found `{}`", tokens[pos].tok
                    );
                }

                ast[index].pos = Some(tokens[pos].pos.clone());
                let res = make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                ) - 1;
                identifier_pos += 1;
                if let Some((_, num_override)) = get_from_stack(vars, st) {
                    vars.last_mut().unwrap().insert(st.clone(), (identifier_pos, num_override + 1));
                    let name = unwrap_enum!(&mut ast[identifier_pos].value, AstNode::Identifier(st), st);
                    write!(name, "___{}", num_override + 1).unwrap();
                } else {
                    add_to_stack(vars, st.clone(), identifier_pos);
                }
                return res
            },
            SolidToken::Parenthesis(IsOpen::True) => {
                let word = unwrap_enum!(&tokens[pos - 1].tok, SolidToken::Word(w), w);
                // if unsafe { IS_COMPILED } && word == "len" {
                //     turn_len_func_to_method(tokens, &mut pos, ast, parent, vars, info, word);
                //     continue
                // }
                let index = if info.structs.contains_key(word) && word != "str" {
                    let prop_pos = insert_as_parent_of_prev(
                        ast, parent, AstNode::Property, Some(tokens[pos].pos.clone())
                    );
                    let index = add_to_tree(prop_pos, ast, Ast::new(
                        AstNode::FunctionCall(true),
                        tokens[pos - 1].pos.clone()
                    ));

                    add_to_tree(index, ast, Ast::new(
                        AstNode::Identifier(String::from("__init__")),
                        tokens[pos].pos.clone()
                    ));
                    index
                } else {
                    insert_as_parent_of_prev(
                        ast, parent, AstNode::FunctionCall(false),
                        Some(tokens[pos - 1].pos.clone())
                    )
                };
                // let index = insert_as_parent_of_prev(ast, parent, type_call);
                let last = add_to_tree(index, ast, Ast::new(
                    AstNode::Args, tokens[pos].pos.clone()
                ));
                #[inline] fn get_arg_or_named_arg(tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, vars: &mut VarTypes, info: &mut Info, last: usize) -> usize {
                    let parent = if let (SolidToken::Word(name), SolidToken::Operator(OperatorType::Eq))
                        = (&tokens[pos + 1].tok, &tokens[pos + 2].tok) { //1 if is named arg
                        let named_arg_pos = add_to_tree(
                            last, ast,
                            Ast::new(
                                AstNode::NamedArg(name.clone()),
                                tokens[pos + 1].pos.clone()
                            )
                        );
                        pos += 2;
                        named_arg_pos
                    } else { last };
                    make_ast_expression(
                        tokens, pos + 1, ast, parent, vars, info
                    )
                }

                pos = get_arg_or_named_arg(tokens, pos, ast, vars, info, last);
                while let SolidToken::Comma = tokens[pos].tok {
                    pos = get_arg_or_named_arg(tokens, pos, ast, vars, info, last);
                }
            },
            SolidToken::Bracket(IsOpen::True) => {
                let index = insert_as_parent_of_prev(
                    ast, parent, AstNode::Index, Some(tokens[pos].pos.clone())
                );
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
                let index = insert_as_parent_of_prev(
                    ast, parent, AstNode::OpAssignment(*op.clone()),
                    Some(tokens[pos].pos.clone())
                );

                return make_ast_expression(
                    tokens, pos + 1, ast, index, vars, info
                ) - 1;
            }
            _ if is_expression => return pos - 1,
            _ => throw!("Unexpected token `{}`", tokens[pos].tok),
        }
    }
}


/*fn turn_len_func_to_method(
    tokens: &[SolidTokenWPos], pos: &mut usize, ast: &mut Vec<Ast>, parent: usize,
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
    tokens: &[SolidTokenWPos], pos: usize, ast: &mut Vec<Ast>, indent: usize,
    vars: &mut VarTypes, info: &mut Info, is_expression: bool, mut parent: usize
) -> usize {
    while let AstNode::Property = ast[parent].value {
        parent = ast[parent].parent.unwrap();
    }
    let index = insert_as_parent_of_prev(
        ast, parent, AstNode::Property, Some(tokens[pos].pos.clone())
    );
    update_pos_from_token(&tokens[pos]);
    let st = unwrap_enum!(&tokens[pos + 1].tok, SolidToken::Word(st), st, "expected word after period");
    word_tok(
        tokens, pos + 1, ast, index, indent, vars, info, st, is_expression
    )
}

fn if_while_statement(
    is_if: bool, tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    indent: usize, vars: &mut VarTypes, info: &mut Info,
) -> usize {
    let stmt_pos = add_to_tree(parent, ast, Ast::new(
        if is_if { AstNode::IfStatement } else { AstNode::WhileStatement },
        tokens[pos - 1].pos.clone()
    ));
    //4 condition:
    let colon_par = add_to_tree(
        stmt_pos, ast, Ast::new_no_pos(AstNode::ColonParentheses)
    );
    pos = make_ast_expression(
        tokens, pos, ast, colon_par, vars, info
    );
    // todo check that there is colon after the condition? (for statement too)
    //4 body:
    let body_pos = add_to_tree(stmt_pos, ast, Ast::new_no_pos(AstNode::Body));
    vars.push(vars.last().unwrap().clone());
    pos = make_ast_statement(
        tokens, pos + 1, ast, body_pos, indent + 1,
        vars, info
    );
    vars.pop();
    pos - 1
}

fn for_statement(
    tokens: &[SolidTokenWPos], mut pos: usize, ast: &mut Vec<Ast>, parent: usize,
    indent: usize, vars: &mut VarTypes, info: &mut Info
) -> usize {
    let loop_pos = add_to_tree(parent, ast, Ast::new(
        AstNode::ForStatement, tokens[pos - 1].pos.clone()
    ));
    let pars_pos = add_to_tree(loop_pos, ast, Ast::new_no_pos(AstNode::ColonParentheses));
    let vars_pos = add_to_tree(pars_pos, ast, Ast::new_no_pos(AstNode::ForVars));
    let iter_pos = add_to_tree(pars_pos, ast, Ast::new_no_pos(AstNode::ForIter));
    let body_pos = add_to_tree(loop_pos, ast, Ast::new_no_pos(AstNode::Body));
    vars.push(vars.last().unwrap().clone());
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
    tokens: &[SolidTokenWPos], pos: &mut usize, ast: &mut Vec<Ast>,
    vars: &mut VarTypes, vars_pos: usize
) {
    loop {
        update_pos_from_token(&tokens[*pos]);
        let name = unwrap_enum!(&tokens[*pos].tok, SolidToken::Word(x), x, "expected identifier");
        let i = add_to_tree(
            vars_pos, ast,
            Ast::new(AstNode::Identifier(name.clone()), tokens[*pos].pos.clone())
        );
        add_to_stack(vars, name.clone(), i);
        *pos += 2;
        match &tokens[*pos - 1].tok {
            SolidToken::Comma => continue,
            SolidToken::In => break,
            _ => {
                update_pos_from_token(&tokens[*pos - 1]);
                throw!("expected `in` or `,`")
            }
        }
    }
}

fn find_module(module: &[&String], parent: &Path) -> Option<PathBuf> {
    // println!("module: {module:?}");
    // println!("parent: {parent:?}");

    for path in fs::read_dir(parent).unwrap() {
        let path = path.unwrap().path();
        // println!("filename: {:?}", path.file_name());
        if matches!(path.file_name(), Some(st) if st == module[0].as_str()){
            // println!("ln: {}", module.len());
            if module.len() == 1 {
                return Some(path);
            }
            return find_module(&module[1..], path.as_path());
        }
    }
    None
}