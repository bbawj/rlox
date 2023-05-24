use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
    Conditional(Conditional),
    Variable(Variable),
    Assignment(Assignment),
    Logical(Logical),
    Call(Call),
    Get(Get),
    Set(Set),
    This(This),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub right: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub expression: Box<Expr>,
    pub then_part: Box<Expr>,
    pub else_part: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub distance: Option<u64>,
    pub name: Token,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub var: Variable,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Logical {
    pub token: Token,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    // used to report error location
    pub paren: Token,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Get {
    pub name: Token,
    pub object: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub name: Token,
    pub object: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct This {
    pub var: Variable,
}
