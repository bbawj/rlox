use std::collections::HashMap;

use crate::{expr::Expr, stmt::Stmt, token::Token, Rlox, RloxError};

#[derive(Clone)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Clone)]
enum ClassType {
    None,
    Class,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve_stmts(&mut self, stmts: &mut Vec<Stmt>) -> Result<(), RloxError> {
        for mut stmt in stmts {
            self.resolve_stmt(&mut stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> Result<(), RloxError> {
        match stmt {
            Stmt::Variable(v) => {
                self.declare(v.name.clone())?;
                if let Some(initializer) = &mut v.initializer {
                    self.resolve_expr(initializer)?;
                }
                self.define(v.name.clone());
                Ok(())
            }
            Stmt::ExprStmt(s) => self.resolve_expr(s),
            Stmt::PrintStmt(s) => self.resolve_expr(s),
            Stmt::Block(s) => {
                self.begin_scope();
                self.resolve_stmts(s)?;
                self.end_scope();
                Ok(())
            }
            Stmt::If(s) => {
                self.resolve_expr(&mut s.expr)?;
                self.resolve_stmt(&mut s.then_stmt)?;
                if let Some(else_branch) = &mut s.else_stmt {
                    self.resolve_stmt(else_branch)?;
                }
                Ok(())
            }
            Stmt::While(s) => {
                self.resolve_expr(&mut s.condition)?;
                self.resolve_stmt(&mut s.body)?;
                Ok(())
            }
            Stmt::Function(s) => {
                self.declare(s.name.clone())?;
                self.define(s.name.clone());
                self.resolve_function(s, FunctionType::Function)?;
                Ok(())
            }
            Stmt::Return(s) => {
                match self.current_function {
                    FunctionType::None => {
                        return Rlox::syntax_error(
                            &s.keyword.line,
                            "Can't return from top-level code.",
                        )
                    }
                    FunctionType::Initializer => {
                        return Rlox::syntax_error(
                            &s.keyword.line,
                            "Can't return a value from an initializer.",
                        )
                    }
                    _ => {}
                };
                if let Some(v) = &mut s.value {
                    self.resolve_expr(v)?;
                }
                Ok(())
            }
            Stmt::Class(s) => {
                let enclosing_class = &self.current_class.clone();
                self.current_class = ClassType::Class;

                self.declare(s.name.clone())?;
                self.define(s.name.clone());

                self.begin_scope();
                if let Some(cur_scope) = self.get_cur_scope() {
                    cur_scope.insert("this".to_string(), true);
                }

                for method in &mut s.methods {
                    let mut declaration = FunctionType::Method;
                    if method.name.lexeme == "init" {
                        declaration = FunctionType::Initializer;
                    }
                    self.resolve_function(method, declaration)?;
                }

                self.end_scope();
                self.current_class = enclosing_class.clone();

                Ok(())
            }
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) -> Result<(), RloxError> {
        match expr {
            Expr::Binary(e) => {
                self.resolve_expr(&mut e.left)?;
                self.resolve_expr(&mut e.right)?;
                Ok(())
            }
            Expr::Grouping(e) => self.resolve_expr(&mut e.expression),
            Expr::Literal(_) => Ok(()),
            Expr::Unary(e) => self.resolve_expr(&mut e.right),
            Expr::Conditional(e) => {
                self.resolve_expr(&mut e.expression)?;
                self.resolve_expr(&mut e.then_part)?;
                self.resolve_expr(&mut e.else_part)?;
                Ok(())
            }
            Expr::Variable(e) => {
                if let Some(scope) = self.get_cur_scope() {
                    match scope.get(&e.name.lexeme) {
                        Some(ready) => {
                            if !ready {
                                return Rlox::syntax_error(
                                    &e.name.line,
                                    "Can't read local variable in its own initializer.",
                                );
                            }
                        }
                        None => {}
                    };
                }
                self.resolve_local(e, e.name.clone());
                Ok(())
            }
            Expr::Assignment(e) => {
                self.resolve_expr(&mut *e.value)?;
                let name = e.var.name.clone();
                self.resolve_local(&mut e.var, name);
                Ok(())
            }
            Expr::Logical(e) => {
                self.resolve_expr(&mut e.left)?;
                self.resolve_expr(&mut e.right)?;
                Ok(())
            }
            Expr::Call(e) => {
                self.resolve_expr(&mut e.callee)?;
                for arg in &mut e.arguments {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
            Expr::Get(e) => {
                self.resolve_expr(&mut e.object)?;
                Ok(())
            }
            Expr::Set(e) => {
                self.resolve_expr(&mut e.value)?;
                self.resolve_expr(&mut e.object)?;
                Ok(())
            }
            Expr::This(e) => {
                match self.current_class {
                    ClassType::None => {
                        return Rlox::syntax_error(
                            &e.var.name.line,
                            "Can't use 'this' outside of a class.",
                        )
                    }
                    _ => (),
                }
                let name = e.var.name.clone();
                self.resolve_local(&mut e.var, name);
                Ok(())
            }
        }
    }

    fn resolve_local(&self, expr: &mut crate::expr::Variable, name: Token) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                expr.distance = Some(self.scopes.len() as u64 - 1 - i as u64);
            }
        }
    }

    fn resolve_function(
        &mut self,
        stmt: &mut crate::stmt::Function,
        func_type: FunctionType,
    ) -> Result<(), RloxError> {
        let enclosing_func = self.current_function.clone();
        self.current_function = func_type;

        self.begin_scope();
        for param in &stmt.params {
            self.declare(param.clone())?;
            self.define(param.clone());
        }
        self.resolve_stmts(&mut stmt.body)?;
        self.end_scope();

        self.current_function = enclosing_func;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_cur_scope(&mut self) -> Option<&mut HashMap<String, bool>> {
        if self.scopes.len() == 0 {
            return None;
        }
        let last_idx = self.scopes.len() - 1;
        self.scopes.get_mut(last_idx)
    }

    fn declare(&mut self, name: Token) -> Result<(), RloxError> {
        if let Some(s) = self.get_cur_scope() {
            if s.contains_key(&name.lexeme) {
                return Rlox::syntax_error(
                    &name.line,
                    "Already a variable with this name in this scope.",
                );
            }
            s.insert(name.lexeme, false);
        }
        Ok(())
    }

    fn define(&mut self, name: Token) {
        if self.scopes.len() == 0 {
            return;
        }

        if let Some(s) = self.get_cur_scope() {
            s.insert(name.lexeme, true);
        }
    }
}
