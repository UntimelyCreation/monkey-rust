use crate::{code::Instructions, evaluator::object::Object, parser::ast::Node};

mod test_compiler;

struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile(&mut self, node: Node) -> Result<(), String> {
        Err("".to_owned())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}
