use std::fmt::{Display, Formatter};
use crate::ast_structure::join;
use crate::IS_COMPILED;

pub const UNKNOWN_TYPE: Type = Type {
    kind: TypeKind::Unknown,
    children: None
};

#[derive(Debug, Clone)]
pub enum TypeKind {
    TypWithGenerics,
    Typ(String),
    OneOf,
    Unknown
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub children: Option<Vec<Type>>
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Unknown => if unsafe { IS_COMPILED } { panic!("Unknown Type") } else { write!(f, "")},
            TypeKind::Typ(st) => {
                if unsafe { IS_COMPILED } {
                    write!(f, "{}", match st.as_str() {
                        "str" => "String",
                        "int" => "i32",
                        _ => st
                    })
                } else {
                    write!(f, "{}", st)
                }
            },
            TypeKind::OneOf => {
                let children = unwrap(&self.children);
                write!(f, "{}", join(&children, "-or-"))
            },
            TypeKind::TypWithGenerics => {
                let children = unwrap(&self.children);
                let parent_type = children.first().unwrap();
                let inner_types = join(
                    &children.iter()
                        .skip(1)
                        .collect()
                    , ","
                );
                if unsafe { IS_COMPILED } {
                    write!(f, "{}<{}>", parent_type, inner_types)
                } else {
                    write!(f, "{}[{}]", parent_type, inner_types)
                }
            }
        }
    }
}

impl Type {
    pub fn new(typ: String) -> Type {
        Type {
            kind: TypeKind::Typ(typ),
            children: None
        }
    }

    pub fn add_option(mut self, typ: Type) -> Type {
        if let TypeKind::OneOf = self.kind {
            if let Some(vc) = &mut self.children {
                vc.push(typ);
            } else {
                // pretty sure this will never happen
                self.children = Some(vec![typ])
            }
            self
        } else if let TypeKind::OneOf = typ.kind {
            typ.add_option(self)
        } else {
            Type {
                kind: TypeKind::OneOf,
                children: Some(vec![self, typ])
            }
        }
    }
}

static EMPTY_VEC: Vec<Type> = vec![];

pub fn unwrap(children: &Option<Vec<Type>>) -> &Vec<Type> {
    if let Some(c) = &children { c } else { &EMPTY_VEC }
}
static EMPTY_VEC_US: Vec<usize> = vec![];
pub fn unwrap_u(children: &Option<Vec<usize>>) -> &Vec<usize> {
    if let Some(c) = &children { c } else { &EMPTY_VEC_US }
}