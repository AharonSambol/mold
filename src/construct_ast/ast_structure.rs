use crate::mold_tokens::OperatorType;
use crate::types::{Type, TypeKind};
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct Ast {
    pub value: AstNode,
    pub children: Option<Vec<usize>>,
    pub parent: Option<usize>,
    pub typ: Option<Type>,
    pub is_mut: bool
}

impl Ast {
    pub fn new(value: AstNode) -> Ast {
        Ast {
            value,
            typ: None,
            children: None,
            parent: None,
            is_mut: true
        }
    }
    pub fn new_w_typ(value: AstNode, typ: Option<Type>) -> Ast {
        Ast {
            value,
            typ,
            children: None,
            parent: None,
            is_mut: true
        }
    }
}

#[derive(Clone, Debug)]
pub enum AstNode {
    Args,       // children = args
    ArgsDef,
    Assignment,         // children[0] = var, children[1] = val
    Body,
    Bool(bool),             // no children
    Char(String),
    ColonParentheses,       // children[0] = inside
    DictLiteral, // children = elements
    Enum(String),
    FirstAssignment,
    ForIter,        // children[0] = iter
    ForStatement,   // children[0] = colon_parentheses(ForVars, ForIter), children[1] = body
    ForVars,        // children = vars
    Function(String), // children[0] = args, children[1] = returnType, children[2] = body
    FunctionCall(bool), // bool = is_static, children[0] = func, children[1] = Args,
    GenericsDeclaration,
    Identifier(String), // children[0] = type
    IfStatement,
    Index,       // child[0] = item, child[1] = index
    ListLiteral, // children = elements
    Module, // children = all the functions/classes/enums..
    Number(String),
    Operator(OperatorType), // children[0] = elem1, children[1] = elem2
    Parentheses,            // children[0] = inside
    Pass, Continue, Break,
    Property,           // children[0] = obj, children[1] = prop
    Return,     // children[0] = return val
    ReturnType,
    SetLiteral, // children = elements
    StaticFunction(String), // children[0] = args, children[1] = returnType, children[2] = body
    String { val: String, mutable: bool, },
    Struct(String), // children[0] = args, children[1] = functions, children[2] = body, children[3] = traits
    StructInit,         // children[0] = struct, children[1] = Args,
    Trait { name: String, strict: bool },  // children[0] = Module (functions)
    Traits,
    Type(String), // e.g.  struct A: \n type Inner
    Types,
    UnaryOp(OperatorType),  // children[0] = elem
    WhileStatement, // children[0] = condition, children[1] = body, children[2] = else?
}

impl AstNode {
    pub fn is_expression(&self) -> bool {
        match self {
            AstNode::Identifier(_)
            | AstNode::FunctionCall(_)
            | AstNode::StructInit
            | AstNode::Property
            | AstNode::Number(_)
            | AstNode::Char(_)
            | AstNode::String { .. }
            | AstNode::Bool(_)
            | AstNode::Operator(_)
            | AstNode::UnaryOp(_)
            | AstNode::Parentheses
            | AstNode::ListLiteral
            | AstNode::SetLiteral
            | AstNode::DictLiteral
            | AstNode::Index => true,
            AstNode::FirstAssignment
            | AstNode::Pass
            | AstNode::Continue
            | AstNode::Break
            | AstNode::Assignment
            | AstNode::Body
            | AstNode::Module
            | AstNode::Function { .. }
            | AstNode::StaticFunction { .. }
            | AstNode::Struct(_)
            | AstNode::Enum(_)
            | AstNode::Trait { .. }
            | AstNode::Traits
            | AstNode::Type(_)
            | AstNode::Types
            | AstNode::ColonParentheses
            | AstNode::IfStatement
            | AstNode::WhileStatement
            | AstNode::ForStatement
            | AstNode::ForVars
            | AstNode::ForIter
            | AstNode::ArgsDef
            | AstNode::Args
            | AstNode::ReturnType
            | AstNode::GenericsDeclaration
            | AstNode::Return => false,
        }
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            AstNode::Types => write!(f, "TYPES"),
            AstNode::Type(name) => write!(f, "TYPE({name})"),
            AstNode::Traits => write!(f, "TRAITS"),
            AstNode::Body => write!(f, "BODY"),
            AstNode::Module => write!(f, "MODULE"),
            AstNode::Function(func) => write!(f, "FUNC({func})"),
            AstNode::StaticFunction(func) => {
                write!(f, "STATIC_FUNC({})", func)
            }
            AstNode::FunctionCall(s) => {
                write!(f, "{}", if *s { "STATIC_FUNC_CALL" } else { "FUNC_CALL" })
            }
            AstNode::StructInit => write!(f, "STRUCT_INIT"),
            AstNode::Assignment => write!(f, "="),
            AstNode::FirstAssignment => write!(f, ":="),
            AstNode::Property => write!(f, "PROPERTY"),
            AstNode::Identifier(st) => write!(f, "{st}"),
            AstNode::Number(num) => write!(f, "{num}"),
            AstNode::String { val, mutable } => write!(f, "{val} [mut: {mutable}]"),
            AstNode::Char(chr) => write!(f, "{chr}"),
            AstNode::Operator(op) => write!(f, "{op}"),
            AstNode::UnaryOp(op) => write!(f, "UNARY({op})"),
            AstNode::Parentheses => write!(f, "()"),
            AstNode::ColonParentheses => write!(f, "():"),
            AstNode::IfStatement => write!(f, "IF"),
            AstNode::WhileStatement => write!(f, "WHILE"),
            AstNode::ForStatement => write!(f, "FOR"),
            AstNode::ForIter => write!(f, "ITER"),
            AstNode::ForVars => write!(f, "VARS"),
            AstNode::Pass => write!(f, "PASS"),
            AstNode::Continue => write!(f, "CONTINUE"),
            AstNode::Break => write!(f, "BREAK"),
            AstNode::ListLiteral => write!(f, "[LIST]"),
            AstNode::SetLiteral => write!(f, "[SET]"),
            AstNode::DictLiteral => write!(f, "[DICT]"),
            AstNode::Index => write!(f, "[INDEX]"),
            AstNode::Args => write!(f, "(ARGS)"),
            AstNode::ArgsDef => write!(f, "(ARGS_DEF)"),
            AstNode::Return => write!(f, "RETURN"),
            AstNode::ReturnType => write!(f, "RETURNS"),
            AstNode::GenericsDeclaration => write!(f, "GENERIC_DECLARATION"),
            AstNode::Struct(name) => write!(f, "STRUCT({name})"),
            AstNode::Enum(name) => write!(f, "ENUM({name})"),
            AstNode::Trait { name, strict } => write!(f, "TRAIT({name}, strict: {strict})"),
            AstNode::Bool(b) => write!(f, "{b}"),
        }
    }
}

pub fn join<T: Display, I: Iterator<Item=T>>(lst: I, sep: &str) -> String {
    lst.map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Debug, Clone)]
pub struct Param {
    pub typ: Type,
    pub name: String,
    pub is_mut: bool,
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Type { kind: TypeKind::Unknown, .. } = self.typ {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}: {}", self.name, self.typ)
        }
    }
}
