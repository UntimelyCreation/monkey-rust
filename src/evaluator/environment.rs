use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    map: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
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
