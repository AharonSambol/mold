use crate::types::{Type, UNKNOWN_TYPE};
use std::fmt::{Display, Formatter, write};

#[derive(Clone, Debug)]
pub struct Ast {
    pub children: Option<Vec<usize>>,
    pub parent: Option<usize>,
    pub value: AstNode,
}

impl Ast {
    pub fn new(typ: AstNode) -> Ast {
        Ast {
            children: None,
            parent: None,
            value: typ
        }
    }
}

#[derive(Clone, Debug)]
pub enum AstNode {
    Statement,
    Expression,
    Module,
    Function(Function),
    Class, Enum,

}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            AstNode::Statement => write!(f, "Statement"),
            AstNode::Expression => write!(f, "Expression"),
            AstNode::Module => write!(f, "Module"),
            AstNode::Class => write!(f, "Class"),
            AstNode::Enum => write!(f, "Enum"),
            AstNode::Function(func) => write!(f, "Func({})", func.to_string())
        }
    }
}
impl AstNode {
    pub fn new_func() -> AstNode {
        AstNode::Function(Function {
            name: "".to_string(),
            params: vec![],
            return_type: Some(UNKNOWN_TYPE),
        })
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let params = self.params.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ");
        if let Some(rt) = &self.return_type {
            write!(f, "\n\tname: {}\n\tparam({})\n\treturn type: {}\n", self.name, params, rt)
        } else {
            write!(f, "\n\tname: {}\n\tparam: {}\n\tno return\n", self.name, params)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub typ: Type,
    pub name: String
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.typ)
    }
}