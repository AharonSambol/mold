use std::fmt::{Display, Formatter, write};

pub const UNKNOWN_TYPE: Type = Type {
    kind: TypeKind::Unknown,
    children: None
};

#[derive(Debug, Clone)]
pub enum TypeKind {
    Typ(String),
    OneOf,
    Unknown
}

#[derive(Debug, Clone)]
pub struct Type {
    kind: TypeKind,
    children: Option<Vec<Type>>
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Unknown => write!(f, "Unknown"),
            TypeKind::Typ(st) => write!(f, "{}", st),
            TypeKind::OneOf => write!(f, "({})", self.children
                .clone()
                .unwrap_or(vec![])
                .iter().map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("|"))
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