use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    interpreter::Value,
    lox_function::{LoxCallable, LoxFunction},
    token::Token,
    Rlox, RloxError,
};

#[derive(Debug, Clone)]
pub struct LoxClass {
    pub name: String,
    pub methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, LoxFunction>) -> Self {
        Self { name, methods }
    }

    pub fn find_method(&self, name: &str) -> Option<&LoxFunction> {
        self.methods.get(name)
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        interpreter: &mut crate::interpreter::Interpreter,
        arguments: Vec<crate::interpreter::Value>,
    ) -> Result<crate::interpreter::Value, crate::RloxError> {
        let instance = Rc::new(LoxInstance::new(self.clone()));
        if let Some(initializer) = self.find_method("init") {
            initializer
                .bind(Rc::clone(&instance))
                .call(interpreter, arguments)?;
        }
        Ok(crate::interpreter::Value::LoxInstance(instance))
    }

    fn arity(&self) -> u8 {
        if let Some(initializer) = self.find_method("init") {
            return initializer.arity();
        }
        0
    }
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    class: LoxClass,
    fields: RefCell<HashMap<String, Value>>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, name: Token) -> Result<Value, RloxError> {
        if let Some(field) = self.fields.borrow().get(&name.lexeme) {
            return Ok(field.clone());
        }

        if let Some(method) = self.class.methods.get(&name.lexeme) {
            return Ok(Value::LoxFunction(method.bind(Rc::new(self.clone()))));
        }
        Rlox::runtime_error(&name.line, &format!("Undefined property {}.", name.lexeme))
    }

    pub fn set(&self, name: Token, value: Value) {
        self.fields.borrow_mut().insert(name.lexeme, value);
    }
}
