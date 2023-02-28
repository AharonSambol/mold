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
    Arg { name: String, is_arg: bool, is_kwarg: bool },
    Args, // children = args
    ArgsDef,
    As(String),
    Assignment, // children[0] = var, children[1] = val
    OpAssignment(OperatorType), // children[0] = var, children[1] = val
    Body,
    Bool(bool), // no children
    Cast, // children[0] = thing that's being cast
    Char(String),
    ColonParentheses, // children[0] = inside
    DictComprehension,
    DictLiteral, // children = elements
    Enum(String),
    FirstAssignment,
    ForIter, // children[0] = iter
    ForStatement, // children[0] = colon_parentheses(ForVars, ForIter), children[1] = body
    ForVars, // children = vars
    From, // children all the words (e.g. A.B => [Identifier(A), Identifier(B)])
    Function(String), // children[0] = args, children[1] = returnType, children[2] = body
    FunctionCall(bool), // bool = is_static, children[0] = func, children[1] = Args,
    GenericsDeclaration,
    Identifier(String),
    IfStatement,
    Ignore,
    Import,
    Index, // child[0] = item, child[1] = index
    ListComprehension, // children[0] = statement, children[1] = loops, children[2] = if (optional)
    ListLiteral, // children = elements
    Module, // children = all the functions/classes/enums..
    Number(String),
    Operator(OperatorType), // children[0] = elem1, children[1] = elem2
    Parentheses, // children[0] = inside
    Pass, Continue, Break,
    Property, // children[0] = obj, children[1] = prop
    Return, // children[0] = return val
    ReturnType,
    SetComprehension,
    SetLiteral, // children = elements
    StaticFunction(String), // children[0] = args, children[1] = returnType, children[2] = body
    String { val: String, mutable: bool, },
    Struct(String), // children[0] = generics, children[1] = args, children[2] = body, children[3] = traits
    StructInit, // children[0] = struct, children[1] = Args, // TODO dont think this is right...
    Ternary, // children[0] = val if true, children[1] = condition, children[2] = val if false
    Trait { name: String, strict: bool },  // children[0] = Module (functions)
    // Traits,
    Type(String), // e.g.  struct A: \n type Inner
    Types,
    UnaryOp(OperatorType),  // children[0] = elem
    WhileStatement, // children[0] = condition, children[1] = body, children[2] = else?
    NamedArg(String), // children[0] = value
}

impl AstNode {
    pub fn is_expression(&self) -> bool {
        match self {
            AstNode::Identifier(_)
            | AstNode::FunctionCall(_)
            | AstNode::StructInit
            | AstNode::Property
            | AstNode::Ternary
            | AstNode::Number(_)
            | AstNode::Char(_)
            | AstNode::String { .. }
            | AstNode::Bool(_)
            | AstNode::Operator(_)
            | AstNode::UnaryOp(_)
            | AstNode::Parentheses
            | AstNode::Cast
            | AstNode::DictComprehension
            | AstNode::ListComprehension
            | AstNode::ListLiteral
            | AstNode::SetComprehension
            | AstNode::SetLiteral
            | AstNode::DictLiteral
            | AstNode::Index => true,
            AstNode::FirstAssignment
            | AstNode::Import
            | AstNode::From
            | AstNode::As(_)
            | AstNode::Assignment
            | AstNode::OpAssignment(_)
            | AstNode::NamedArg(_)
            | AstNode::Arg { .. }
            | AstNode::Pass
            | AstNode::Ignore
            | AstNode::Continue
            | AstNode::Break
            | AstNode::Body
            | AstNode::Module
            | AstNode::Function { .. }
            | AstNode::StaticFunction { .. }
            | AstNode::Struct(_)
            | AstNode::Enum(_)
            | AstNode::Trait { .. }
            // | AstNode::Traits
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
            AstNode::Ignore => write!(f, "[IGNORE]"),
            AstNode::Ternary => write!(f, "[TERNARY]"),
            AstNode::Cast => write!(f, "[CAST]"),
            AstNode::DictComprehension => write!(f, "{{DICT-COMPREHENSION}}"),
            AstNode::SetComprehension => write!(f, "{{SET-COMPREHENSION}}"),
            AstNode::ListComprehension => write!(f, "[COMPREHENSION]"),
            AstNode::NamedArg(name) => write!(f, "{name}="),
            AstNode::Arg { name, is_arg, is_kwarg } =>
                write!(f, "{name}\n[{is_arg}, {is_kwarg}]"),
            AstNode::Types => write!(f, "TYPES"),
            AstNode::Type(name) => write!(f, "TYPE({name})"),
            // AstNode::Traits => write!(f, "TRAITS"),
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
            AstNode::OpAssignment(op) => write!(f, "{op}="),
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
            AstNode::As(name) => write!(f, "[AS: {name}]"),
            AstNode::From => write!(f, "[FROM]"),
            AstNode::Import => write!(f, "[IMPORT]"),
        }
    }
}

// 2 probably a bit faster if for iters of &str of String didnt do this unnecessary map
#[inline]
pub fn join<T: Display, I: Iterator<Item=T>>(lst: I, sep: &str) -> String {
    lst.map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub typ: Type,
    pub name: String,
    pub is_mut: bool,
    pub is_args: bool,
    pub is_kwargs: bool,
    pub pos: usize,
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
