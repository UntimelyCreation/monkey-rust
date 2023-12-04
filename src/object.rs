use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast::{BlockStatement, IdentifierExpression, Node};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    String(StringObj),
    ReturnValue(ReturnValue),
    Function(Function),
    Builtin(Builtin),
    Error(Error),
    Null,
}

type BuiltinFn = fn(Vec<Object>) -> Object;

impl Object {
    pub fn get_type_str(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::String(_) => "STRING".to_string(),
            Object::ReturnValue(_) => "RETURN".to_string(),
            Object::Function(_) => "FUNCTION".to_string(),
            Object::Builtin(_) => "BUILTIN".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(integer) => format!("{}", integer.value),
            Object::Boolean(boolean) => format!("{}", boolean.value),
            Object::String(string) => string.value.clone(),
            Object::ReturnValue(return_value) => return_value.value.inspect(),
            Object::Function(function) => [
                "fn(".to_string(),
                function
                    .parameters
                    .iter()
                    .map(|stmt| stmt.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                ") ".to_string(),
                function.body.to_string(),
            ]
            .join(""),
            Object::Builtin(_) => "builtin function".to_string(),
            Object::Error(error) => format!("ERROR: {}", error.message),
            Object::Null => "null".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Integer {
    pub value: i32,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Boolean {
    pub value: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringObj {
    pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Builtin {
    pub function: BuiltinFn,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub message: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    map: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            map: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(self) -> Self {
        let mut env = Environment::new();
        env.outer = Some(Rc::new(RefCell::new(self)));
        env
    }

    pub fn get(&self, identifier: &String) -> Option<Object> {
        match self.map.get(identifier) {
            None => match &self.outer {
                Some(outer) => outer.borrow().get(identifier),
                None => None,
            },
            result => result.cloned(),
        }
    }

    pub fn set(&mut self, identifier: String, val: Object) {
        self.map.insert(identifier, val);
    }
}
