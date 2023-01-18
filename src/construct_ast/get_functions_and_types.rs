use std::collections::{HashMap, HashSet};
use crate::construct_ast::get_typ::{try_get_arg_typ};
use crate::mold_tokens::{OperatorType, SolidToken};
use crate::{unwrap_enum};
use crate::construct_ast::mold_ast::{
    FuncType, FuncTypes,
    StructType, StructTypes,
    TraitType, TraitTypes,
    TypeTypes, Info
};

const UNKNOWN_FUNC_TYPE: FuncType = FuncType{ input: None, output: None };

pub fn get_struct_and_func_names(tokens: &[SolidToken]) -> (StructTypes, FuncTypes, TraitTypes, TypeTypes){
    let mut funcs = HashMap::new();
    let mut structs = HashMap::new();
    let mut traits = HashMap::new();
    let mut types = HashMap::new();

    // for (i, tok) in tokens.iter().enumerate() {
    //     if let SolidToken::Type = tok {
    //
    //     }
    // }
    for (i, tok) in tokens.iter().enumerate() {
        match tok {
            SolidToken::Def =>
            //1 if isnt part of a struct\trait
                if i == 0 || !matches!(tokens[i - 1], SolidToken::Tab) {
                    if let SolidToken::Word(name) = &tokens[i + 1] {
                        funcs.insert(name.clone(), UNKNOWN_FUNC_TYPE);
                    }
                },
            SolidToken::Struct | SolidToken::Trait | SolidToken::StrictTrait =>
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
            SolidToken::Type => {
                if let SolidToken::Operator(OperatorType::Eq) = tokens[i + 2] {
                    types.insert(unwrap_enum!(&tokens[i + 1], SolidToken::Word(w), w), i + 3);
                }
            }
            _ => ()
        }
    }
    let mut resolved_types = HashMap::new();
    //3 NOT EFFICIENT
    println!("{:?}", types);
    println!("structs: {:?}", structs);
    while resolved_types.len() < types.len() {
        let prev_ln = resolved_types.len();
        for (&typ, &pos) in &types {
            if resolved_types.contains_key(typ) {
                continue
            }
            let res = try_get_arg_typ(
                tokens, &mut pos.clone(),
                &Info {
                    funcs: &mut funcs,
                    structs: &mut structs,
                    traits: &mut traits,
                    types: &mut resolved_types,
                    generics: &mut vec![],
                    struct_inner_types: &mut HashSet::new(),
                }, false
            );
            println!("res: {res:?}");
            if let Some(res) = res {
                resolved_types.insert((*typ).clone(), res);
            }
        }
        println!("{:?}", resolved_types);
        if resolved_types.len() == prev_ln {
            panic!("circular type definition")
        }
    }
    (structs, funcs, traits, resolved_types)
}
