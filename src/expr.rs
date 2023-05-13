use crate::token::Token;

#[derive(Debug)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
    Conditional(Conditional),
    Variable(Variable),
    Assignment(Assignment),
    Logical(Logical),
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Debug)]
pub struct Binary {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

#[derive(Debug)]
pub struct Unary {
    pub right: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug)]
pub struct Conditional {
    pub expression: Box<Expr>,
    pub then_part: Box<Expr>,
    pub else_part: Box<Expr>,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Token,
}

#[derive(Debug)]
pub struct Assignment {
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub struct Logical {
    pub token: Token,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}
