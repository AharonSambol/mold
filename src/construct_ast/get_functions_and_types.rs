use std::collections::HashMap;
use crate::mold_tokens::{OperatorType, SolidToken};
use crate::types::Type;
use crate::unwrap_enum;

pub type TraitTypes = HashMap<String, TraitType>;
pub type StructTypes = HashMap<String, StructType>;
pub type FuncTypes = HashMap<String, FuncType>;

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


pub fn get_struct_and_func_names(tokens: &[SolidToken]) -> (StructTypes, FuncTypes, TraitTypes){
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
            _ => ()
        }
    }
    (structs, funcs, traits)
}
