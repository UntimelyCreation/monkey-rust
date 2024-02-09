use crate::{code::Instructions, object::CompiledFn};

#[derive(Debug, Clone)]
pub struct Frame {
    pub function: CompiledFn,
    pub ip: i32,
    pub base_pointer: usize,
}

impl Default for Frame {
    fn default() -> Self {
        Self::new()
    }
}

impl Frame {
    pub fn new() -> Self {
        Self {
            function: CompiledFn::new(),
            ip: -1,
            base_pointer: 0,
        }
    }

    pub fn from_function(function: CompiledFn, base_pointer: usize) -> Self {
        Self {
            function,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> Instructions {
        self.function.instructions.clone()
    }
}
