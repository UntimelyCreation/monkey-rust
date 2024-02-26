use std::rc::Rc;

use super::{new_error, Object};

pub type BuiltinFn = fn(&[Rc<Object>]) -> Rc<Object>;

pub fn get_builtin_fn(name: &str) -> Option<BuiltinFn> {
    match name {
        "len" => Some(LEN_BUILTIN),
        "first" => Some(FIRST_BUILTIN),
        "last" => Some(LAST_BUILTIN),
        "rest" => Some(REST_BUILTIN),
        "push" => Some(PUSH_BUILTIN),
        "puts" => Some(PUTS_BUILTIN),
        _ => None,
    }
}

pub static BUILTINS: [(&str, BuiltinFn); 6] = [
    ("len", LEN_BUILTIN),
    ("first", FIRST_BUILTIN),
    ("last", LAST_BUILTIN),
    ("rest", REST_BUILTIN),
    ("push", PUSH_BUILTIN),
    ("puts", PUTS_BUILTIN),
];

static LEN_BUILTIN: BuiltinFn = |objs| {
    if objs.len() != 1 {
        return Rc::new(new_error(format!(
            "wrong number of arguments: expected 1, found {}",
            objs.len()
        )));
    }

    match &objs[0].as_ref() {
        Object::String(string) => Rc::new(Object::Integer(string.len() as i32)),
        Object::Array(array) => Rc::new(Object::Integer(array.len() as i32)),
        _ => Rc::new(new_error(format!(
            "argument to 'len' not supported, found {}",
            objs[0].get_type_str()
        ))),
    }
};

static FIRST_BUILTIN: BuiltinFn = |objs| {
    if objs.len() != 1 {
        return Rc::new(new_error(format!(
            "wrong number of arguments: expected 1, found {}",
            objs.len()
        )));
    }

    match &objs[0].as_ref() {
        Object::Array(elements) => {
            if !elements.is_empty() {
                elements[0].clone()
            } else {
                Rc::new(Object::Null)
            }
        }
        _ => Rc::new(new_error(format!(
            "argument to 'first' must be ARRAY, found {}",
            objs[0].get_type_str()
        ))),
    }
};

static LAST_BUILTIN: BuiltinFn = |objs| {
    if objs.len() != 1 {
        return Rc::new(new_error(format!(
            "wrong number of arguments: expected 1, found {}",
            objs.len()
        )));
    }

    match &objs[0].as_ref() {
        Object::Array(elements) => elements.last().unwrap_or(&Rc::new(Object::Null)).clone(),
        _ => Rc::new(new_error(format!(
            "argument to 'last' must be ARRAY, found {}",
            objs[0].get_type_str()
        ))),
    }
};

static REST_BUILTIN: BuiltinFn = |objs| {
    if objs.len() != 1 {
        return Rc::new(new_error(format!(
            "wrong number of arguments: expected 1, found {}",
            objs.len()
        )));
    }

    match &objs[0].as_ref() {
        Object::Array(elements) => {
            if !elements.is_empty() {
                Rc::new(Object::Array(elements[1..].to_vec()))
            } else {
                Rc::new(Object::Null)
            }
        }
        _ => Rc::new(new_error(format!(
            "argument to 'rest' must be ARRAY, found {}",
            objs[0].get_type_str()
        ))),
    }
};

static PUSH_BUILTIN: BuiltinFn = |objs| {
    if objs.len() != 2 {
        return Rc::new(new_error(format!(
            "wrong number of arguments: expected 2, found {}",
            objs.len()
        )));
    }

    match &objs[0].as_ref() {
        Object::Array(elements) => {
            let mut elements = elements.to_vec();
            elements.push(objs[1].clone());
            Rc::new(Object::Array(elements))
        }
        _ => Rc::new(new_error(format!(
            "argument to 'push' must be ARRAY, found {}",
            objs[0].get_type_str()
        ))),
    }
};

static PUTS_BUILTIN: BuiltinFn = |objs| {
    for obj in objs.iter() {
        println!("{}", obj);
    }

    Rc::new(Object::Null)
};
