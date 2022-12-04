use crate::mold_ast::{FuncType, FuncTypes};
use crate::mold_tokens::OperatorType;
use crate::types::{Type, TypeKind};
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct Ast {
    pub children: Option<Vec<usize>>,
    pub parent: Option<usize>,
    pub value: AstNode,
    pub typ: Option<Type>,
}

impl Ast {
    pub fn new(typ: AstNode) -> Ast {
        Ast {
            children: None,
            parent: None,
            typ: None,
            value: typ,
        }
    }
}

#[derive(Clone, Debug)]
pub enum AstNode {
    Body,
    Module,           // children = all the functions/classes/enums..
    Function(String), // children[0] = args, children[1] = returnType, children[2] = body
    Struct(String),   // children[0] = args, children[1] = functions, children[2] = body
    Functions(Vec<(String, FuncType)>),
    Identifier(String), // children[0] = type
    FirstAssignment,
    Assignment,   // children[0] = var, children[1] = val
    FunctionCall, // children[0] = func, children[1] = Args,
    StructInit,   // children[0] = struct, children[1] = Args,
    Property,     // children[0] = obj, children[1] = prop
    Number(String),
    Char(char),
    String(String),
    Bool(bool),             // no children
    Operator(OperatorType), // children[0] = elem1, children[1] = elem2
    UnaryOp(OperatorType),  // children[0] = elem
    Parentheses,            // children[0] = inside
    ColonParentheses,       // children[0] = inside
    IfStatement,
    WhileStatement, // children[0] = condition, children[1] = body, children[2] = else?
    ForStatement,   // children[0] = colon_parentheses(ForVars, ForIter)
    ForVars,        // children = vars
    ForIter,        // children[0] = iter
    Pass,
    ListLiteral, // children = elements
    Index,       // child[0] = item, child[1] = index
    ArgsDef,
    Args,       // children = args
    ReturnType, // children[0] = type
    Return,     // children[0] = return val
}

impl AstNode {
    pub fn is_expression(&self) -> bool {
        match self {
            AstNode::Identifier(_)
            | AstNode::FunctionCall
            | AstNode::StructInit
            | AstNode::Property
            | AstNode::Number(_)
            | AstNode::Char(_)
            | AstNode::String(_)
            | AstNode::Bool(_)
            | AstNode::Operator(_)
            | AstNode::UnaryOp(_)
            | AstNode::Parentheses
            | AstNode::Pass
            | AstNode::ListLiteral
            | AstNode::Index => true,
            AstNode::FirstAssignment
            | AstNode::Assignment
            | AstNode::Body
            | AstNode::Functions(_)
            | AstNode::Module
            | AstNode::Function(_)
            | AstNode::Struct(_)
            | AstNode::ColonParentheses
            | AstNode::IfStatement
            | AstNode::WhileStatement
            | AstNode::ForStatement
            | AstNode::ForVars
            | AstNode::ForIter
            | AstNode::ArgsDef
            | AstNode::Args
            | AstNode::ReturnType
            | AstNode::Return => false,
        }
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            AstNode::Body => write!(f, "BODY"),
            AstNode::Module => write!(f, "MODULE"),
            AstNode::Function(func) => write!(f, "FUNC({})", func.to_string()),
            AstNode::FunctionCall => write!(f, "FUNC_CALL"),
            AstNode::StructInit => write!(f, "STRUCT_INIT"),
            AstNode::Assignment => write!(f, "="),
            AstNode::FirstAssignment => write!(f, ":="),
            AstNode::Property => write!(f, "PROPERTY"),
            AstNode::Identifier(st) => write!(f, "{}", st),
            AstNode::Number(num) => write!(f, "{}", num),
            AstNode::String(str) => write!(f, "{}", str),
            AstNode::Char(chr) => write!(f, "{}", chr),
            AstNode::Operator(op) => write!(f, "{}", op),
            AstNode::UnaryOp(op) => write!(f, "UNARY({})", op),
            AstNode::Parentheses => write!(f, "()"),
            AstNode::ColonParentheses => write!(f, "():"),
            AstNode::IfStatement => write!(f, "IF"),
            AstNode::WhileStatement => write!(f, "WHILE"),
            AstNode::ForStatement => write!(f, "FOR"),
            AstNode::ForIter => write!(f, "ITER"),
            AstNode::ForVars => write!(f, "VARS"),
            AstNode::Pass => write!(f, "PASS"),
            AstNode::ListLiteral => write!(f, "[LIST]"),
            // AstNode::Type(typ) => write!(f, "{}", typ),
            AstNode::Index => write!(f, "[INDEX]"),
            AstNode::Args => write!(f, "(ARGS)"),
            AstNode::ArgsDef => write!(f, "(ARGS_DEF)"),
            AstNode::Return => write!(f, "RETURN"),
            AstNode::ReturnType => write!(f, "RETURNS"),
            AstNode::Struct(name) => write!(f, "STRUCT({})", name),
            AstNode::Functions(funcs) => write!(f, "FUNCS({:?})", funcs),
            AstNode::Bool(b) => write!(f, "{}", b),
        }
    }
}

pub fn join<T: Display>(lst: &Vec<T>, sep: &str) -> String {
    lst.iter()
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Debug, Clone)]
pub struct Param {
    pub typ: Type,
    pub name: String,
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Type {
            kind: TypeKind::Unknown,
            ..
        } = self.typ
        {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}: {}", self.name, self.typ)
        }
    }
}
