use crate::{expr::Expr, token::Token};

#[derive(Debug)]
pub enum Stmt {
    Variable(Variable),
    ExprStmt(Expr),
    PrintStmt(Expr),
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub struct Variable {
    pub name: Token,
    pub initializer: Option<Expr>,
}
