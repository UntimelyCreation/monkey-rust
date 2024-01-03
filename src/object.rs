use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, BTreeMap, HashMap},
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::ast::{BlockStatement, IdentifierExpression, Node};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    String(StringObj),
    ReturnValue(ReturnValue),
    Function(Function),
    Builtin(Builtin),
    Array(Array),
    Hash(HashObj),
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
            Object::Array(_) => "ARRAY".to_string(),
            Object::Hash(_) => "HASH".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }

    pub fn get_hash_key(&self) -> Option<HashKey> {
        match self {
            Object::Integer(integer) => Some(integer.get_hash_key()),
            Object::Boolean(boolean) => Some(boolean.get_hash_key()),
            Object::String(string) => Some(string.get_hash_key()),
            _ => None,
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
            Object::Array(array) => [
                "[".to_string(),
                array
                    .elements
                    .iter()
                    .map(|stmt| stmt.inspect())
                    .collect::<Vec<String>>()
                    .join(", "),
                "]".to_string(),
            ]
            .join(""),
            Object::Hash(hash) => [
                "{".to_string(),
                hash.pairs
                    .values()
                    .map(|pair| [pair.key.inspect(), pair.value.inspect()].join(": "))
                    .collect::<Vec<String>>()
                    .join(", "),
                "}".to_string(),
            ]
            .join(""),
            Object::Error(error) => format!("ERROR: {}", error.message),
            Object::Null => "null".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Integer {
    pub value: i32,
}

impl Hashable for Integer {
    fn get_hash_key(&self) -> HashKey {
        HashKey {
            kind: "INTEGER".to_string(),
            value: self.value as u64,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Hashable for Boolean {
    fn get_hash_key(&self) -> HashKey {
        HashKey {
            kind: "BOOLEAN".to_string(),
            value: match self.value {
                false => 0,
                true => 1,
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringObj {
    pub value: String,
}

impl Hashable for StringObj {
    fn get_hash_key(&self) -> HashKey {
        let mut hasher = DefaultHasher::new();
        Hash::hash(&self.value, &mut hasher);
        HashKey {
            kind: "STRING".to_string(),
            value: hasher.finish(),
        }
    }
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
pub struct Array {
    pub elements: Vec<Object>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[derive(Debug, PartialEq, Clone)]
pub struct HashObj {
    pub pairs: BTreeMap<HashKey, HashPair>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub message: String,
}

trait Hashable {
    fn get_hash_key(&self) -> HashKey;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct HashKey {
    kind: String,
    value: u64,
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
