use crate::{code::Instructions, object::Closure};

#[derive(Debug, Clone)]
pub struct Frame {
    pub closure: Closure,
    pub ip: i32,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            closure: Closure::new(),
            ip: -1,
            base_pointer: 0,
        }
    }

    pub fn from_function(closure: Closure, base_pointer: usize) -> Self {
        Self {
            closure,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> Instructions {
        self.closure.function.instructions.clone()
    }
}
