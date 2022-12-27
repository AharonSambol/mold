use std::fmt::{Display, Formatter};
use crate::ast_structure::join;
use crate::IS_COMPILED;

pub const UNKNOWN_TYPE: Type = Type {
    kind: TypeKind::Unknown,
    children: None
};

pub const STR_TYPE: Type = Type {
    kind: TypeKind::Struct(TypName::Static("str")),
    children: None
};
pub const MUT_STR_TYPE: Type = Type {
    kind: TypeKind::Struct(TypName::Static("String")),
    children: None
};
pub const BOOL_TYPE: Type = Type {
    kind: TypeKind::Struct(TypName::Static("bool")),
    children: None
};
pub const INT_TYPE: Type = Type {
    kind: TypeKind::Struct(TypName::Static("i32")),
    children: None
};
pub const FLOAT_TYPE: Type = Type {
    kind: TypeKind::Struct(TypName::Static("f32")),
    children: None
};
pub const CHAR_TYPE: Type = Type {
    kind: TypeKind::Struct(TypName::Static("char")),
    children: None
};
pub const ITER_NAME: TypName = TypName::Static("Iter");
pub const ITER_TYPE: Type = Type {
    kind: TypeKind::Struct(ITER_NAME),
    children: None
};

#[derive(Debug, Clone)]
pub enum GenericType {
    Declaration(String),
    Of(String),
}

#[derive(Debug, Clone)]
pub enum TypName {
    Str(String),
    Static(&'static str)
}


impl PartialEq for TypName {
    fn eq(&self, other: &Self) -> bool {
        self.get_str() == other.get_str()
    }
}

impl TypName {
    pub fn get_str(&self) -> &str {
        match self {
            TypName::Str(s) => s.as_str(),
            TypName::Static(s) => s
        }
    }

}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Generic(GenericType),
    Generics,
    GenericsMap,
    OneOf,
    Optional,
    Tuple,
    Implements,
    Args,
    Trait(String),
    Unknown,
    Function(String),
    Struct(TypName), // child[0] = generics
    _Class(String),
    _Pointer,
    MutPointer,
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
            // TypeKind::Typ(st) => {
            //     if unsafe { IS_COMPILED } {
            //         let st = match st {
            //             TypName::Str(s) => s.as_str(),
            //             TypName::Static(s) => s
            //         };
            //         write!(f, "{}", match st {
            //             "str" => "String",
            //             "int" => "i32",
            //             _ => st
            //         })
            //     } else {
            //         write!(f, "{st}")
            //     }
            // },
            TypeKind::OneOf => {
                let children = unwrap(&self.children);
                write!(f, "{}", join(&children, "-or-"))
            },
            // TypeKind::TypWithSubTypes => {
            //     let children = unwrap(&self.children);
            //     let parent_type = children.first().unwrap();
            //     let inner_types = join(
            //         &children.iter()
            //             .skip(1)
            //             .collect()
            //         , ","
            //     );
            //     if unsafe { IS_COMPILED } {
            //         write!(f, "{parent_type}<{inner_types}>")
            //     } else {
            //         write!(f, "{parent_type}[{inner_types}]")
            //     }
            // },
            TypeKind::Tuple => {
                write!(f, "({})", join(unwrap(&self.children), ","))
            },
            TypeKind::Generic(c) => {
                if let GenericType::Of(name) = c {
                    write!(f, "{name}")
                } else {
                    write!(f, "GENERIC({c:?})")
                }
            },
            TypeKind::GenericsMap => write!(f, "GENERICS_MAP({})", join(unwrap(&self.children), ",")),
            TypeKind::Generics => write!(f, "GENERICS({})", join(unwrap(&self.children), ",")),
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
                write!(f, "TRAIT({trt})")
            },
            TypeKind::Struct(name) => {
                let mut gens = String::new();
                if let Some(children) = &self.children {
                    if children.len() != 0 {
                        if let TypeKind::GenericsMap = children[0].kind {
                            if let Some(generics) = &children[0].children {
                                gens = format!("::<{}>", join(&(generics.iter().map(|x| unwrap(&x.children)[0].clone()).collect()), ","));
                            }
                        } else {
                            // println!("{:?}", children[0]);
                            if let Some(generics) = &children[0].children {
                                gens = format!("<{}>", join(generics, ","));
                            }
                        }
                    }
                }
                write!(f, "{name}{gens}")
            },
            TypeKind::Function(name) => write!(f, "{name}"),
            TypeKind::_Class(_) => {
                todo!()
            }
            TypeKind::_Pointer => write!(f, "&{}", unwrap(&self.children)[0]),
            TypeKind::MutPointer => write!(f, "&mut {}", unwrap(&self.children)[0]),
        }
    }
}

impl Type {
    pub fn new(typ: String) -> Type {
        Type {
            kind: TypeKind::Struct(clean_type(typ)),
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
        write!(f, "{st}")
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
    // Type {
    //     kind: TypeKind::Generic(GenericType::Of(generic_name)),
    //     children: Some(vec![
            types[0].clone()
    //     ]),
    // }
}

pub fn clean_type(st: String) -> TypName {
    TypName::Static(
        match st.as_str() {
            "str" => "String",
            "int" => "i32",
            "float" => "f32",
            "List" => "Vec",
            _ => return TypName::Str(st)
        }
    )
}

