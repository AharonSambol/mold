use std::fmt::{Display, Formatter};
use pretty_print_tree::{Color, PrettyPrintTree};
use crate::construct_ast::ast_structure::join;
use crate::EMPTY_STR;

pub const UNKNOWN_TYPE: Type = Type {
    kind: TypeKind::Unknown,
    children: None
};

pub const STR_TYPE: TypeKind = TypeKind::Struct(TypName::Static("str"));
pub const MUT_STR_TYPE: TypeKind = TypeKind::Struct(TypName::Static("String"));
pub const BOOL_TYPE: TypeKind = TypeKind::Struct(TypName::Static("bool"));
pub const INT_TYPE: TypeKind = TypeKind::Struct(TypName::Static("i32"));
pub const FLOAT_TYPE: TypeKind = TypeKind::Struct(TypName::Static("f32"));
pub const CHAR_TYPE: TypeKind = TypeKind::Struct(TypName::Static("char"));
pub const ITER_TYPE: TypeKind = TypeKind::Struct(TypName::Static("Iter"));
// pub const ITER_TYPE: Type = Type {
//     kind: TypeKind::Struct(ITER_NAME),
//     children: None
// };

#[derive(Debug, Clone, PartialEq, Eq)]
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
    fn eq(&self, other: &Self) -> bool { self.get_str() == other.get_str() }
}
impl PartialEq<&'static str> for TypName {
    fn eq(&self, other: &&'static str) -> bool { self.get_str() == *other }
}
impl PartialEq<str> for TypName {
    fn eq(&self, other: &str) -> bool { self.get_str() == other }
}
impl PartialEq<String> for TypName {
    fn eq(&self, other: &String) -> bool { self.get_str() == *other }
}

impl TypName {
    pub fn get_str(&self) -> &str {
        match self {
            TypName::Str(s) => s.as_str(),
            TypName::Static(s) => s
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Generic(GenericType),
    Generics,
    GenericsMap,
    _OneOf,
    Optional,
    Tuple,
    InnerType(String), // e.g. Iterator[Inner=i32]
    Args,
    Trait(TypName),
    Enum(TypName),
    Unknown,
    Function(String),
    Struct(TypName), // child[0] = generics
    _Class(String),
    Pointer,
    MutPointer,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub children: Option<Vec<Type>>
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        if self.kind != other.kind {
            return false
        }
        self.children == other.children
    }
}


#[allow(dead_code)]
pub fn print_type(typ: &Option<Type>) {
    let ppt: PrettyPrintTree<Type> = {
        PrettyPrintTree::<Type>::new(
            Box::new(|typ| {
                format!("{:?}", typ.kind)
            }),
            Box::new(|typ| {
                unwrap(&typ.children).clone()
            }),
        )
    };
    if let Some(t) = typ {
        println!("{}", ppt.to_str(t));
    } else {
        println!("None");
    }
    println!("\n");
}

#[allow(dead_code)]
pub fn print_type_b(typ: &Option<Type>){
    let mut ppt: PrettyPrintTree<Type> = {
        PrettyPrintTree::<Type>::new(
            Box::new(|typ| {
                format!("{:?}", typ.kind)
            }),
            Box::new(|typ| {
                unwrap(&typ.children).clone()
            }),
        )
    };
    ppt.color = Color::Black;
    if let Some(t) = typ {
        println!("{}", ppt.to_str(t));
    } else {
        println!("None");
    }
    println!("\n");
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Unknown => write!(f, ""),
            TypeKind::_OneOf => {
                let children = unwrap(&self.children);
                write!(f, "{}", join(children.iter(), "-or-"))
            },
            TypeKind::Tuple => {
                write!(f, "({})", join(unwrap(&self.children).iter(), ","))
            },
            TypeKind::Generic(c) => {
                if let GenericType::Of(name) = c {
                    if let Some(v) = &self.children {
                        write!(f, "{}", v[0])
                    } else {
                        write!(f, "{name}")
                    }
                } else {
                    write!(f, "GENERIC({c:?})")
                }
            },
            TypeKind::GenericsMap => write!(f, "GENERICS_MAP({})", join(unwrap(&self.children).iter(), ",")),
            TypeKind::Generics => write!(f, "GENERICS({})", join(unwrap(&self.children).iter(), ",")),
            TypeKind::Optional => {
                write!(f, "OPTIONAL({})", join(unwrap(&self.children).iter(), ","))
            },
            TypeKind::Args => {
                write!(f, "ARGS({})", unwrap(&self.children)[0])
            },
            TypeKind::Trait(name) => {
                let mut gens = self.format_generics();
                write!(f, "Box<dyn {name}{gens}>")
            },
            TypeKind::Enum(name) => {
                let gens = self.format_generics();
                write!(f, "{name}{gens}")
            },
            TypeKind::Struct(name) => {
                let mut gens = self.format_generics();
                write!(f, "{name}{gens}")
            },
            TypeKind::Function(name) => write!(f, "{name}"),
            TypeKind::_Class(_) => {
                todo!()
            }
            TypeKind::Pointer => write!(f, "&{}", unwrap(&self.children)[0]),
            TypeKind::MutPointer => write!(f, "&mut {}", unwrap(&self.children)[0]),
            TypeKind::InnerType(name) => {
                if self.children.is_none() {
                    write!(f, "Self::{name}")
                } else {
                    write!(f, "{name}={}", unwrap(&self.children)[0])
                }
            },
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
        if let TypeKind::_OneOf = self.kind {
            if let Some(vc) = &mut self.children {
                vc.push(typ);
            } else {
                // pretty sure this will never happen
                self.children = Some(vec![typ])
            }
            self
        } else if let TypeKind::_OneOf = typ.kind {
            typ.add_option(self)
        } else {
            Type {
                kind: TypeKind::_OneOf,
                children: Some(vec![self, typ])
            }
        }
    }

    fn format_generics(&self) -> String {
        if let Some(children) = &self.children {
            if !children.is_empty() {
                if let TypeKind::GenericsMap = children[0].kind {
                    if let Some(generics) = &children[0].children {
                        return format!("::<{}>", join(generics.iter(), ","));
                    }
                } else if let Some(generics) = &children[0].children {
                    return format!("<{}>", join(generics.iter(), ","));
                }
            }
        }
        EMPTY_STR
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

pub fn clean_type(st: String) -> TypName {
    TypName::Static(
        match st.as_str() {
            "str" => "String",
            "int" => "i32",
            "float" => "f32",
            "List" => "Vec",
            "Set" => "HashSet",
            "Dict" => "HashMap",
            _ => return TypName::Str(st)
        }
    )
}

