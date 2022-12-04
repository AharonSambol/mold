use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::ast_structure::join;
use crate::IS_COMPILED;

pub const UNKNOWN_TYPE: Type = Type {
    kind: TypeKind::Unknown,
    children: None
};

pub const STR_TYPE: Type = Type {
    kind: TypeKind::Typ(TypName::Static("String")),
    children: None
};
pub const BOOL_TYPE: Type = Type {
    kind: TypeKind::Typ(TypName::Static("bool")),
    children: None
};
pub const INT_TYPE: Type = Type {
    kind: TypeKind::Typ(TypName::Static("i32")),
    children: None
};
pub const FLOAT_TYPE: Type = Type {
    kind: TypeKind::Typ(TypName::Static("f32")),
    children: None
};
pub const CHAR_TYPE: Type = Type {
    kind: TypeKind::Typ(TypName::Static("char")),
    children: None
};
pub const ITER_TYPE: Type = Type {
    kind: TypeKind::Typ(TypName::Static("iter")),
    children: None
};

#[derive(Debug, Clone)]
pub enum GenericType {
    IterInternal
}

#[derive(Debug, Clone)]
pub enum TypName {
    Str(String),
    Static(&'static str)
}

impl PartialEq for TypName {
    fn eq(&self, other: &Self) -> bool {
        let s1 = match self {
            TypName::Str(s) => s.as_str(),
            TypName::Static(s) => s
        };
        let s2 = match other {
            TypName::Str(s) => s.as_str(),
            TypName::Static(s) => s
        };
        s1 == s2
    }
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    TypWithSubTypes,
    Generic(GenericType),
    Typ(TypName),
    OneOf,
    Optional,
    Tuple,
    Implements,
    Args,
    Trait(String),
    Unknown,
    Struct(String),
    Class(String),
    Pointer,
    // Struct(HashMap<String, usize>),
    // Class(HashMap<String, usize>),
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
                    let st = match st {
                        TypName::Str(s) => s.as_str(),
                        TypName::Static(s) => s
                    };
                    write!(f, "{}", match st {
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
            TypeKind::TypWithSubTypes => {
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
            },
            TypeKind::Tuple => {
                write!(f, "({})", join(unwrap(&self.children), ","))
            },
            TypeKind::Generic(c) => write!(f, "GENERIC({:?})", c),
            TypeKind::Optional => {
                write!(f, "OPTIONAL({})", join(unwrap(&self.children), ","))
            },
            TypeKind::Implements => {
                write!(f, "IMPL({})", join(unwrap(&self.children), ","))
            },
            TypeKind::Args => {
                write!(f, "ARGS({})", unwrap(&self.children)[0])
            },
            TypeKind::Trait(trt) => {
                write!(f, "TRAIT({})", trt)
            },
            TypeKind::Struct(name) => write!(f, "{}", name),
            TypeKind::Class(_) => {
                todo!()
            }
            TypeKind::Pointer => write!(f, "&{}", unwrap(&self.children)[0])
        }
    }
}

impl Type {
    pub fn new(typ: String) -> Type {
        Type {
            kind: TypeKind::Typ(clean_type(typ)),
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

impl Display for TypName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let st = match self {
            TypName::Str(s) => s.as_str(),
            TypName::Static(s) => s
        };
        write!(f, "{}", st)
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

pub fn generify(types: &Vec<Type>) -> Type {
    // TODO !!
    types[0].clone()
}

pub fn clean_type(st: String) -> TypName {
    TypName::Static(
        match st.as_str() {
            "str" => "String",
            "int" => "i32",
            "float" => "f32",
            "list" => "Vec",
            _ => return TypName::Str(st)
        }
    )
}