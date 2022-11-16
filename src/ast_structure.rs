use crate::types::{Type, UNKNOWN_TYPE};
use std::fmt::{Display, Formatter};
use crate::mold_tokens::OperatorType;

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
    Module,             // children = all the functions/classes/enums..
    Function(Function),
    Class, Enum,
    Identifier(String), // no children
    Assignment,         // children[0] = var, children[1] = val
    FunctionCall,       // children[0] = func, children[1..] = param,
    Property,           // children[0] = obj, children[1] = prop
    Number(String),     // no children
    Operator(OperatorType),   // children[0] = elem1, children[1] = elem2
    UnaryOp(OperatorType),    // children[0] = elem
    Parentheses,        // children[0] = inside
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            AstNode::Statement => write!(f, "Statement"),
            AstNode::Expression => write!(f, "Expression"),
            AstNode::Module => write!(f, "Module"),
            AstNode::Class => write!(f, "Class"),
            AstNode::Enum => write!(f, "Enum"),
            AstNode::Function(func) => write!(f, "Func({})", func.to_string()),
            AstNode::FunctionCall =>
                write!(f, "FuncCall()"),
            AstNode::Assignment =>
                write!(f, "Assignment"),
            AstNode::Property =>
                write!(f, "Property"),
            AstNode::Identifier(st) => write!(f, "{}", st),
            AstNode::Number(num) => write!(f, "{}", num),
            AstNode::Operator(op) => write!(f, "{}", op),
            AstNode::UnaryOp(op) => write!(f, "Unary({})", op),
            AstNode::Parentheses => write!(f, "()"),
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
        let params = join(&self.params, ", ");
        if let Some(rt) = &self.return_type {
            write!(f, "\n\tname: {}\n\tparam({})\n\treturn type: {}\n", self.name, params, rt)
        } else {
            write!(f, "\n\tname: {}\n\tparam: {}\n\tno return\n", self.name, params)
        }
    }
}

pub fn join<T: Display>(lst: &Vec<T>, sep: &str) -> String {
    lst.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(sep)
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