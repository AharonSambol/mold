use crate::mold_tokens::SolidToken;

enum AstNode {
    Statement,
    Expression,
}

pub struct Ast<'a> {
    children: Vec<Ast<'a>>,
    parent: Option<&'a Ast<'a>>,
    value: AstNode,
}

pub fn maks_ast<'a>(input_code: Vec<SolidToken>) -> Ast<'a> {
    Ast{
        children: vec![],
        parent: None,
        value: AstNode::Statement
    }
}