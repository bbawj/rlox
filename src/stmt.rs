use crate::{expr::Expr, token::Token};

#[derive(Debug)]
pub enum Stmt {
    Variable(Variable),
    ExprStmt(Expr),
    PrintStmt(Expr),
    Block(Vec<Stmt>),
    If(If),
    While(While),
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

#[derive(Debug)]
pub struct While {
    pub condition: Expr,
    pub body: Box<Stmt>,
}
