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
            let instr = self.instructions.stream[ip].to_owned();
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
                Opcode::OpPop => {
                    self.pop();
                }
                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                    self.exec_binary_operation(op)?;
                }
                Opcode::OpTrue => {
                    self.push(Object::Boolean(true))?;
                }
                Opcode::OpFalse => {
                    self.push(Object::Boolean(false))?;
                }
                Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
                    self.exec_comparison(op)?;
                }
                Opcode::OpMinus => {
                    self.exec_minus_operator()?;
                }
                Opcode::OpBang => {
                    self.exec_bang_operator()?;
                }
            }
            ip += 1;
        }
        Ok(())
    }

    fn exec_binary_operation(&mut self, op: &Opcode) -> Result<(), RuntimeError> {
        let rhs = self.pop();
        let lhs = self.pop();
        match (&lhs, &rhs) {
            (Object::Integer(lhs_value), Object::Integer(rhs_value)) => {
                self.exec_integer_binary_operation(op, *lhs_value, *rhs_value)
            }
            _ => Err(format!(
                "unsupported types for binary operation: {} {}",
                lhs.get_type_str(),
                rhs.get_type_str(),
            )),
        }
    }

    fn exec_integer_binary_operation(
        &mut self,
        op: &Opcode,
        lhs: i32,
        rhs: i32,
    ) -> Result<(), RuntimeError> {
        let result = match op {
            Opcode::OpAdd => Some(lhs + rhs),
            Opcode::OpSub => Some(lhs - rhs),
            Opcode::OpMul => Some(lhs * rhs),
            Opcode::OpDiv => Some(lhs / rhs),
            _ => None,
        };

        match result {
            Some(r) => self.push(Object::Integer(r)),
            None => Err(format!("unknown INTEGER operator: {:?}", op)),
        }
    }

    fn exec_comparison(&mut self, op: &Opcode) -> Result<(), RuntimeError> {
        let rhs = self.pop();
        let lhs = self.pop();
        match (&lhs, &rhs) {
            (Object::Integer(lhs_value), Object::Integer(rhs_value)) => {
                self.exec_integer_comparison(op, *lhs_value, *rhs_value)
            }
            (Object::Boolean(lhs_value), Object::Boolean(rhs_value)) => {
                self.exec_boolean_comparison(op, *lhs_value, *rhs_value)
            }
            _ => Err(format!(
                "unsupported types for comparison: {} {}",
                lhs.get_type_str(),
                rhs.get_type_str(),
            )),
        }
    }

    fn exec_integer_comparison(
        &mut self,
        op: &Opcode,
        lhs: i32,
        rhs: i32,
    ) -> Result<(), RuntimeError> {
        let result = match op {
            Opcode::OpEqual => Some(lhs == rhs),
            Opcode::OpNotEqual => Some(lhs != rhs),
            Opcode::OpGreaterThan => Some(lhs > rhs),
            _ => None,
        };

        match result {
            Some(r) => self.push(Object::Boolean(r)),
            None => Err(format!("unknown INTEGER operator: {:?}", op)),
        }
    }

    fn exec_boolean_comparison(
        &mut self,
        op: &Opcode,
        lhs: bool,
        rhs: bool,
    ) -> Result<(), RuntimeError> {
        let result = match op {
            Opcode::OpEqual => Some(lhs == rhs),
            Opcode::OpNotEqual => Some(lhs != rhs),
            _ => None,
        };

        match result {
            Some(r) => self.push(Object::Boolean(r)),
            None => Err(format!("unknown BOOLEAN operator: {:?}", op)),
        }
    }

    fn exec_minus_operator(&mut self) -> Result<(), RuntimeError> {
        let operand = self.pop();
        match operand {
            Object::Integer(value) => self.push(Object::Integer(-value)),
            _ => Err(format!(
                "unsupported type for negation: {}",
                operand.get_type_str()
            )),
        }
    }

    fn exec_bang_operator(&mut self) -> Result<(), RuntimeError> {
        match self.pop() {
            Object::Boolean(value) => self.push(Object::Boolean(!value)),
            _ => self.push(Object::Boolean(false)),
        }
    }

    pub fn last_popped(&self) -> Object {
        self.stack[self.sp].clone()
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
