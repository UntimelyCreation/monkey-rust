use crate::{code::Instructions, evaluator::object::CompiledFn};

#[derive(Debug, Clone)]
pub struct Frame {
    pub function: CompiledFn,
    pub ip: i32,
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
        }
    }

    pub fn from_function(function: CompiledFn) -> Self {
        Self { function, ip: -1 }
    }

    pub fn instructions(&self) -> Instructions {
        self.function.instructions.clone()
    }
}
