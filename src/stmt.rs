use crate::{expr::Expr, token::Token};

#[derive(Debug, Clone)]
pub enum Stmt {
    Variable(Variable),
    ExprStmt(Expr),
    PrintStmt(Expr),
    Block(Vec<Stmt>),
    If(If),
    While(While),
    Function(Function),
    Return(Return),
    Class(Class),
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Token,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub expr: Expr,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub keyword: Token,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Token,
    pub methods: Vec<Function>,
}
