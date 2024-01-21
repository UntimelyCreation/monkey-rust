use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    evaluator::object::Object,
};

mod test_vm;

type RuntimeError = String;

const STACK_SIZE: usize = 2048;

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize,
}

impl Vm {
    pub fn from_bytecode(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        let mut ip = 0;
        while ip < self.instructions.stream.len() {
            let instr = &self.instructions.stream[ip];
            let op = &instr.0;
            let operands = &instr.1;

            match op {
                Opcode::OpConstant => match operands[..].try_into() {
                    Ok(bytes) => {
                        let const_index = u16::from_be_bytes(bytes) as usize;
                        self.push(self.constants[const_index].clone())?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_owned());
                    }
                },
                Opcode::OpAdd => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    match (&lhs, &rhs) {
                        (Object::Integer(lhs_value), Object::Integer(rhs_value)) => {
                            let result = lhs_value + rhs_value;
                            self.push(Object::Integer(result))?;
                        }
                        _ => {
                            return Err(format!(
                                "unknown operator: {} + {}",
                                lhs.get_type_str(),
                                rhs.get_type_str(),
                            ));
                        }
                    }
                }
            }
            ip += 1;
        }
        Ok(())
    }

    pub fn stack_top(&self) -> Option<Object> {
        match self.sp {
            0 => None,
            sp => Some(self.stack[sp - 1].clone()),
        }
    }

    fn push(&mut self, obj: Object) -> Result<(), RuntimeError> {
        if self.sp > STACK_SIZE {
            return Err("stack overflow".to_owned());
        }

        self.stack[self.sp] = obj;
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Object {
        let obj = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        obj
    }
}
