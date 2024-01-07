use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, BTreeMap, HashMap},
    fmt::Display,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::ast::{fmt_identifier_expressions, BlockStatement, IdentifierExpression};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    ReturnValue(Box<Object>),
    Function {
        parameters: Vec<IdentifierExpression>,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    },
    Builtin(BuiltinFn),
    Array(Vec<Object>),
    Hash(BTreeMap<HashKey, HashPair>),
    Error(String),
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
            Object::Function { .. } => "FUNCTION".to_string(),
            Object::Builtin(_) => "BUILTIN".to_string(),
            Object::Array(_) => "ARRAY".to_string(),
            Object::Hash(_) => "HASH".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }

    pub fn get_hash_key(&self) -> Option<HashKey> {
        match self {
            Object::Integer(integer) => Some(HashKey {
                kind: "INTEGER".to_string(),
                value: *integer as u64,
            }),
            Object::Boolean(boolean) => Some(HashKey {
                kind: "BOOLEAN".to_string(),
                value: match boolean {
                    false => 0,
                    true => 1,
                },
            }),
            Object::String(string) => {
                let mut hasher = DefaultHasher::new();
                Hash::hash(&string, &mut hasher);
                Some(HashKey {
                    kind: "STRING".to_string(),
                    value: hasher.finish(),
                })
            }
            _ => None,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(integer) => write!(f, "{}", integer),
            Self::Boolean(boolean) => write!(f, "{}", boolean),
            Self::String(string) => write!(f, "{}", string),
            Self::ReturnValue(value) => write!(f, "{}", value),
            Self::Function {
                parameters, body, ..
            } => {
                write!(
                    f,
                    "fn({}) {{ {} }}",
                    fmt_identifier_expressions(parameters, ", "),
                    body
                )
            }
            Self::Builtin(_) => write!(f, "builtin function"),
            Self::Array(elements) => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(|obj| obj.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Hash(pairs) => write!(
                f,
                "[{}]",
                pairs
                    .iter()
                    .map(|(_, v)| format!("{}: {}", v.key, v.value))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Error(msg) => write!(f, "{}", msg),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct HashKey {
    kind: String,
    value: u64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
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
