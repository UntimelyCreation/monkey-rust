use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, BTreeMap},
    fmt::Display,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::parser::ast::{fmt_identifier_expressions, BlockStatement, IdentifierExpression};
use crate::{code::Instructions, evaluator::environment::Environment};

use self::builtins::BuiltinFn;

pub mod builtins;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    ReturnValue(Rc<Object>),
    Function {
        parameters: Vec<IdentifierExpression>,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    },
    CompiledFn(CompiledFn),
    Closure(Closure),
    BuiltinFn(BuiltinFn),
    Array(Vec<Rc<Object>>),
    Hash(BTreeMap<HashKey, HashPair>),
    Error(String),
    Null,
}

impl Object {
    pub fn get_type_str(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::String(_) => "STRING".to_string(),
            Object::ReturnValue(_) => "RETURN".to_string(),
            Object::Function { .. } => "FUNCTION".to_string(),
            Object::CompiledFn { .. } => "COMPILED_FUNCTION".to_string(),
            Object::Closure(..) => "CLOSURE".to_string(),
            Object::BuiltinFn(_) => "BUILTIN".to_string(),
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

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(value) => *value,
            Object::Null => false,
            _ => true,
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
            Self::CompiledFn(_) => {
                write!(f, "compiled function")
            }
            Self::Closure(_) => {
                write!(f, "closure")
            }
            Self::BuiltinFn(_) => write!(f, "builtin function"),
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

#[derive(Debug, PartialEq, Clone)]
pub struct CompiledFn {
    pub instructions: Instructions,
    pub num_locals: usize,
    pub num_parameters: usize,
}

impl CompiledFn {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            num_locals: 0,
            num_parameters: 0,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub function: Rc<CompiledFn>,
    pub free_vars: Vec<Rc<Object>>,
}

impl Closure {
    pub fn new() -> Self {
        Self {
            function: Rc::new(CompiledFn::new()),
            free_vars: Vec::new(),
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
    pub key: Rc<Object>,
    pub value: Rc<Object>,
}

pub fn new_error(message: String) -> Object {
    Object::Error(message)
}

pub fn is_truthy(object: &Object) -> bool {
    !matches!(object, Object::Boolean(false) | Object::Null)
}

pub fn get_bool_object(expr: bool) -> Rc<Object> {
    Rc::new(Object::Boolean(expr))
}
