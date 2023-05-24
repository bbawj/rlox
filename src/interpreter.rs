use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    NativeFunction(NativeFunction),
    LoxFunction(u64),
}

pub trait LoxCallable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, RloxError>;
    fn arity(&self) -> u8;
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub callable:
        fn(interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, RloxError>,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl LoxCallable for NativeFunction {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, RloxError> {
        (self.callable)(interpreter, arguments)
    }

    fn arity(&self) -> u8 {
        self.arity
    }
}

#[derive(Debug, Clone)]
pub struct LoxFunction {
    pub id: u64,
    pub declaration: crate::stmt::Function,
    pub closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn new(
        id: u64,
        declaration: crate::stmt::Function,
        closure: &Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            id,
            declaration,
            closure: Rc::clone(closure),
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, RloxError> {
        let mut env = Environment::new(Some(Rc::clone(&self.closure)));
        for (i, param) in self.declaration.params.iter().enumerate() {
            env.define(&param.lexeme, arguments.get(i).unwrap().clone());
        }
        let x = match interpreter.execute_block(&self.declaration.body, env) {
            Ok(_) => {
                return Ok(Value::Nil);
            }
            Err(e) => match e {
                RloxError::ReturnValue(v) => {
                    return Ok(v);
                }
                _ => Err(e),
            },
        };
        x
    }

    fn arity(&self) -> u8 {
        self.declaration.params.len().try_into().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    environment: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        match enclosing {
            Some(en) => Self {
                environment: HashMap::new(),
                enclosing: Some(en),
            },
            None => Self {
                environment: HashMap::new(),
                enclosing: None,
            },
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.environment.insert(name.to_string(), value);
    }

    pub fn get_variable_at(&self, identifier: &str, distance: u64) -> Option<Value> {
        if distance == 0 {
            return self.get_variable(identifier).clone();
        } else {
            if let Some(enc) = &self.enclosing {
                return enc.borrow().get_variable_at(identifier, distance - 1);
            }
            None
        }
    }

    pub fn get_variable(&self, identifier: &str) -> Option<Value> {
        return self.environment.get(identifier).cloned();
    }

    pub fn assign_at(
        &mut self,
        token: Token,
        value: Value,
        distance: u64,
    ) -> Result<(), RloxError> {
        if distance == 0 {
            self.assign(token, value)?;
        } else {
            match &mut self.enclosing {
                Some(e) => return e.borrow_mut().assign_at(token, value, distance - 1),
                None => {
                    return Rlox::syntax_error(
                        &token.line,
                        &format!("Undefined variable '{}'", &token.lexeme),
                    );
                }
            }
        }
        Ok(())
    }

    pub fn assign(&mut self, token: Token, value: Value) -> Result<(), RloxError> {
        self.define(&token.lexeme, value);
        Ok(())
    }
}

#[derive(Clone)]
pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
    counter: u64,
    functions: HashMap<u64, LoxFunction>,
    locals: HashMap<Expr, u64>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));
        globals.borrow_mut().define(
            "clock",
            Value::NativeFunction(NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                callable: |_, _| {
                    let now = std::time::SystemTime::now()
                        .duration_since(std::time::SystemTime::UNIX_EPOCH)
                        .unwrap();
                    Ok(Value::Number(now.as_millis() as f64))
                },
            }),
        );
        Self {
            globals: Rc::clone(&globals),
            environment: Rc::clone(&globals),
            // environment: Rc::new(RefCell::new(Environment::new(None))),
            counter: 0,
            functions: HashMap::new(),
            locals: HashMap::new(),
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
                    (TokenType::Bang, _) => Ok(Value::Bool(!self.is_truthy(&right))),
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
                if self.is_truthy(&value) {
                    Ok(self.interpret_expr(&conditional.then_part)?)
                } else {
                    Ok(self.interpret_expr(&conditional.else_part)?)
                }
            }
            Expr::Variable(v) => {
                let distance = v.distance;
                match distance {
                    Some(d) => match self.environment.borrow().get_variable_at(&v.name.lexeme, d) {
                        Some(v) => Ok(v.clone()),
                        None => Rlox::runtime_error(
                            &v.name.line,
                            &format!("Undefined variable {:?}.", v),
                        ),
                    },
                    None => match self.globals.borrow().get_variable(&v.name.lexeme) {
                        Some(v) => Ok(v),
                        None => Rlox::runtime_error(
                            &v.name.line,
                            &format!("Undefined variable {:?}.", v),
                        ),
                    },
                }
            }
            Expr::Assignment(assignment) => {
                let value = self.interpret_expr(&assignment.value)?;
                match assignment.var.distance {
                    Some(d) => self.environment.borrow_mut().assign_at(
                        assignment.var.name.clone(),
                        value.clone(),
                        d,
                    )?,
                    None => self
                        .globals
                        .borrow_mut()
                        .assign(assignment.var.name.clone(), value.clone())?,
                }
                Ok(value)
            }
            Expr::Logical(expr) => {
                let left = self.interpret_expr(&expr.left)?;
                if expr.token.token_type == TokenType::Or {
                    if self.is_truthy(&left) {
                        return Ok(left);
                    }
                } else {
                    if !self.is_truthy(&left) {
                        return Ok(left);
                    }
                }
                let right = self.interpret_expr(&expr.right)?;
                return Ok(right);
            }
            Expr::Call(call) => {
                let callee = self.interpret_expr(&call.callee)?;
                let mut arguments = Vec::new();
                for argument in &call.arguments {
                    arguments.push(self.interpret_expr(argument)?);
                }

                match callee {
                    Value::NativeFunction(func) => func.call(self, arguments),
                    Value::LoxFunction(func_id) => {
                        let interpreter_clone = self.clone();
                        let func =
                            interpreter_clone.get_lox_function(&func_id, &call.paren.line)?;
                        func.call(self, arguments)
                    }
                    _ => {
                        return Rlox::syntax_error(
                            &call.paren.line,
                            "Can only call functions and classes.",
                        )
                    }
                }
            }
        }
    }

    fn get_lox_function(&self, func_id: &u64, line: &usize) -> Result<&LoxFunction, RloxError> {
        match self.functions.get(&func_id) {
            Some(func) => Ok(func),
            None => return Rlox::syntax_error(line, "Function not defined yet."),
        }
    }

    pub fn interpret_stmt(&mut self, statement: &Stmt) -> Result<(), RloxError> {
        match statement {
            Stmt::Variable(v) => {
                let mut value = Value::Nil;
                if let Some(initializer) = &v.initializer {
                    value = self.interpret_expr(&initializer)?;
                }
                self.environment.borrow_mut().define(&v.name.lexeme, value);
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
                let e = Environment::new(Some(self.environment.clone()));
                self.execute_block(statements, e)?;
                Ok(())
            }
            Stmt::If(statement) => {
                let expr = self.interpret_expr(&statement.expr)?;
                if self.is_truthy(&expr) {
                    self.interpret_stmt(&statement.then_stmt)?;
                } else if let Some(else_stmt) = &statement.else_stmt {
                    self.interpret_stmt(&else_stmt)?;
                }
                Ok(())
            }
            Stmt::While(while_stmt) => {
                let mut condition = self.interpret_expr(&while_stmt.condition)?;
                while self.is_truthy(&condition) {
                    self.interpret_stmt(&while_stmt.body)?;
                    condition = self.interpret_expr(&while_stmt.condition)?;
                }
                Ok(())
            }
            Stmt::Function(function) => {
                let func_id = self.alloc_id();
                self.environment
                    .borrow_mut()
                    .define(&function.name.lexeme, Value::LoxFunction(func_id));
                let func = LoxFunction::new(func_id, function.clone(), &self.environment);
                self.functions.insert(func_id, func);
                Ok(())
            }
            Stmt::Return(return_stmt) => {
                let mut value = Value::Nil;
                if let Some(value_expr) = &return_stmt.value {
                    value = self.interpret_expr(&value_expr)?;
                }
                Err(RloxError::ReturnValue(value))
            }
        }
    }

    fn execute_block(&mut self, statements: &Vec<Stmt>, env: Environment) -> Result<(), RloxError> {
        let prev = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(env));
        for stmt in statements {
            match self.interpret_stmt(&stmt) {
                Ok(_) => (),
                Err(e) => {
                    self.environment = prev;
                    return Err(e);
                }
            };
        }
        self.environment = prev;
        Ok(())
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match *value {
            Value::Bool(boolean) => boolean,
            Value::Nil => false,
            _ => true,
        }
    }

    fn alloc_id(&mut self) -> u64 {
        let id = self.counter;
        self.counter += 1;

        id
    }
}
