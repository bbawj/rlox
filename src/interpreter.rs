use std::collections::HashMap;

use crate::{
    expr::{Expr, Literal},
    stmt::Stmt,
    token::{Token, TokenType},
    Rlox, RloxError,
};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone)]
pub struct Environment {
    environment: HashMap<String, Value>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(enclosing: Option<Box<Environment>>) -> Self {
        Self {
            environment: HashMap::new(),
            enclosing,
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.environment.insert(name.to_string(), value);
    }

    pub fn get_variable(&self, identifier: &str) -> Option<&Value> {
        let mut value = self.environment.get(identifier);
        if value.is_none() && self.enclosing.is_some() {
            value = self.enclosing.as_ref().unwrap().get_variable(identifier);
        }

        value
    }

    pub fn assign(&mut self, token: Token, value: Value) -> Result<(), RloxError> {
        if !self.environment.contains_key(&token.lexeme) {
            match &mut self.enclosing {
                Some(e) => return e.assign(token, value),
                None => {
                    return Rlox::syntax_error(
                        &token.line,
                        &format!("Undefined variable '{}'", &token.lexeme),
                    );
                }
            }
        }
        self.environment.insert(token.lexeme, value);
        Ok(())
    }
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(None),
        }
    }

    fn equals(left: Value, right: Value) -> bool {
        match (left, right) {
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }

    pub fn interpret_expr(&mut self, expression: &Expr) -> Result<Value, RloxError> {
        match expression {
            Expr::Binary(binary) => {
                let left = self.interpret_expr(&binary.left)?;
                let right = self.interpret_expr(&binary.right)?;
                match (&left, binary.operator.token_type, &right) {
                    (Value::Number(n1), TokenType::Minus, Value::Number(n2)) => {
                        Ok(Value::Number(n1 - n2))
                    }
                    (Value::Number(n1), TokenType::Plus, Value::Number(n2)) => {
                        Ok(Value::Number(n1 + n2))
                    }
                    (Value::String(s1), TokenType::Plus, Value::String(s2)) => {
                        Ok(Value::String(s1.to_string() + s2))
                    }
                    (Value::Number(n1), TokenType::Slash, Value::Number(n2)) => {
                        Ok(Value::Number(n1 / n2))
                    }
                    (Value::Number(n1), TokenType::Star, Value::Number(n2)) => {
                        Ok(Value::Number(n1 * n2))
                    }
                    (_, TokenType::BangEqual, _) => {
                        Ok(Value::Bool(!Interpreter::equals(left, right)))
                    }
                    (_, TokenType::EqualEqual, _) => {
                        Ok(Value::Bool(Interpreter::equals(left, right)))
                    }
                    (Value::Number(n1), TokenType::Greater, Value::Number(n2)) => {
                        Ok(Value::Bool(n1 > n2))
                    }
                    (Value::Number(n1), TokenType::GreaterEqual, Value::Number(n2)) => {
                        Ok(Value::Bool(n1 >= n2))
                    }
                    (Value::Number(n1), TokenType::Less, Value::Number(n2)) => {
                        Ok(Value::Bool(n1 < n2))
                    }
                    (Value::Number(n1), TokenType::LessEqual, Value::Number(n2)) => {
                        Ok(Value::Bool(n1 <= n2))
                    }
                    _ => Rlox::syntax_error(
                        &binary.operator.line,
                        &format!(
                            "invalid operands {:?} and {:?} for binary operation {:?}",
                            left, right, binary.operator
                        ),
                    ),
                }
            }
            Expr::Grouping(grouping) => Ok(self.interpret_expr(&grouping.expression)?),
            Expr::Literal(literal) => match literal {
                Literal::Number(n) => Ok(Value::Number(*n)),
                Literal::String(s) => Ok(Value::String(s.to_owned())),
                Literal::True => Ok(Value::Bool(true)),
                Literal::False => Ok(Value::Bool(false)),
                Literal::Nil => Ok(Value::Nil),
            },
            Expr::Unary(unary) => {
                let right = self.interpret_expr(&unary.right)?;
                match (&unary.operator.token_type, &right) {
                    (TokenType::Bang, _) => Ok(Value::Bool(!self.is_truthy(right))),
                    (TokenType::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
                    _ => Rlox::syntax_error(
                        &unary.operator.line,
                        &format!(
                            "invalid operands {:?} for unary operation {:?}",
                            &right, &unary.operator
                        ),
                    ),
                }
            }
            Expr::Conditional(conditional) => {
                let value = self.interpret_expr(&conditional.expression)?;
                if self.is_truthy(value) {
                    Ok(self.interpret_expr(&conditional.then_part)?)
                } else {
                    Ok(self.interpret_expr(&conditional.else_part)?)
                }
            }
            Expr::Variable(v) => match self.environment.get_variable(&v.name.lexeme) {
                Some(v) => Ok(v.clone()),
                None => Rlox::runtime_error(&v.name.line, &format!("Undefined variable {:?}.", v)),
            },
            Expr::Assignment(assignment) => {
                let value = self.interpret_expr(&assignment.value)?;
                self.environment
                    .assign(assignment.name.clone(), value.clone())?;
                Ok(value)
            }
        }
    }

    pub fn interpret_stmt(&mut self, statement: Stmt) -> Result<(), RloxError> {
        match statement {
            Stmt::Variable(v) => {
                let mut value = Value::Nil;
                if let Some(initializer) = v.initializer {
                    value = self.interpret_expr(&initializer)?;
                }
                self.environment.define(&v.name.lexeme, value);
                Ok(())
            }
            Stmt::ExprStmt(expr) => {
                self.interpret_expr(&expr)?;
                Ok(())
            }
            Stmt::PrintStmt(expr) => {
                let value = self.interpret_expr(&expr)?;
                println!("{:?}", value);
                Ok(())
            }
            Stmt::Block(statements) => {
                self.execute_block(
                    statements,
                    Environment::new(Some(Box::new(self.environment.clone()))),
                )?;
                Ok(())
            }
        }
    }

    fn execute_block(
        &mut self,
        statements: Vec<Stmt>,
        environment: Environment,
    ) -> Result<(), RloxError> {
        let previous_env = self.environment.clone();
        self.environment = environment;
        for stmt in statements {
            match self.interpret_stmt(stmt) {
                Ok(_) => (),
                Err(e) => {
                    self.environment = previous_env;
                    return Err(e);
                }
            }
        }
        self.environment = previous_env;
        Ok(())
    }

    fn is_truthy(&self, value: Value) -> bool {
        match value {
            Value::Bool(boolean) => boolean,
            Value::Nil => false,
            _ => true,
        }
    }
}
