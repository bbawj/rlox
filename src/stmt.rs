use crate::{expr::Expr, token::Token};

#[derive(Debug)]
pub enum Stmt {
    Variable(Variable),
    ExprStmt(Expr),
    PrintStmt(Expr),
    Block(Vec<Stmt>),
    If(If),
}

#[derive(Debug)]
pub struct Variable {
    pub name: Token,
    pub initializer: Option<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub expr: Expr,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}
