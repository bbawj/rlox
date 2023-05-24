use crate::interpreter::Environment;
use crate::lox_class::LoxInstance;
use crate::RloxError;
use crate::Value;
use std::cell::RefCell;
use std::rc::Rc;

use crate::Interpreter;

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
    pub declaration: crate::stmt::Function,
    pub closure: Rc<RefCell<Environment>>,
    pub is_initializer: bool,
}

impl LoxFunction {
    pub fn new(
        declaration: crate::stmt::Function,
        closure: &Rc<RefCell<Environment>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            declaration,
            closure: Rc::clone(closure),
            is_initializer,
        }
    }

    pub fn bind(&self, instance: Rc<LoxInstance>) -> LoxFunction {
        let mut env = Environment::new(Some(Rc::clone(&self.closure)));
        env.define("this", Value::LoxInstance(instance));
        return LoxFunction::new(
            self.declaration.clone(),
            &Rc::new(RefCell::new(env)),
            self.is_initializer,
        );
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
                    if self.is_initializer {
                        return Ok(self
                            .closure
                            .borrow()
                            .get_variable_at("this", 0)
                            .expect("Initializer must have been bound."));
                    }
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
