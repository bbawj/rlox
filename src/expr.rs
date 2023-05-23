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
    pub name: Token,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Token,
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
