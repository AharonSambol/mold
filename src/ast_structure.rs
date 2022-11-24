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
    Body,
    Module,             // children = all the functions/classes/enums..
    Function(Function), // children = the body
    Identifier(String), // no children
    Assignment,         // children[0] = var, children[1] = val
    FunctionCall,       // children[0] = func, children[1..] = param,
    Property,           // children[0] = obj, children[1] = prop
    Number(String),     // no children
    Operator(OperatorType),   // children[0] = elem1, children[1] = elem2
    UnaryOp(OperatorType),    // children[0] = elem
    Parentheses,        // children[0] = inside
    ColonParentheses,   // children[0] = inside
    IfStatement, WhileStatement, // children[0] = condition, children[1] = body, children[2] = else?
    Pass,
    ListLiteral, // children = elements
    Type(Type),
    Index,      // child[0] = item, child[1] = index
    Args,       // children = args
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            AstNode::Body => write!(f, "BODY"),
            AstNode::Module => write!(f, "MODULE"),
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
            AstNode::ColonParentheses => write!(f, "():"),
            AstNode::IfStatement => write!(f, "if"),
            AstNode::WhileStatement => write!(f, "while"),
            AstNode::Pass => write!(f, "pass"),
            AstNode::ListLiteral => write!(f, "[LIST]"),
            AstNode::Type(typ) => write!(f, "{}", typ),
            AstNode::Index => write!(f, "[INDEX]"),
            AstNode::Args => write!(f, "(ARGS)"),
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
        let params = join(&self.params, "\n\t\t");
        if let Some(rt) = &self.return_type {
            write!(f, "\n\tname: {}\n\tparam:\n\t\t{}\n\treturn type:\n\t\t{}\n", self.name, params, rt)
        } else {
            write!(f, "\n\tname: {}\n\tparam:\n\t\t{}\n\tno return\n", self.name, params)
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