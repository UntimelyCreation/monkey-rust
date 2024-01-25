use std::collections::BTreeMap;

use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    evaluator::object::{HashKey, HashPair, Object},
};

mod test_vm;

type RuntimeError = String;

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65535;

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize,
    globals: Vec<Object>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            instructions: Instructions::new(),
            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
            globals: vec![Object::Null; GLOBALS_SIZE],
        }
    }

    pub fn from_bytecode(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
            globals: vec![Object::Null; GLOBALS_SIZE],
        }
    }

    pub fn update(&mut self, bytecode: Bytecode) {
        self.constants = bytecode.constants;
        self.instructions = bytecode.instructions;
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        let mut ip = 0;
        while ip < self.instructions.stream.len() {
            let instr = self.instructions.stream[ip].clone();
            let op = &instr.0;
            let operands = &instr.1;

            match op {
                Opcode::OpConstant => match operands[..].try_into() {
                    Ok(bytes) => {
                        let const_index = u16::from_be_bytes(bytes) as usize;
                        self.push(self.constants[const_index].clone())?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpNull => {
                    self.push(Object::Null)?;
                }
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
                Opcode::OpJump => match operands[..].try_into() {
                    Ok(bytes) => {
                        let jump_pos = u16::from_be_bytes(bytes) as usize;
                        ip = jump_pos - 1;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpJumpCond => match operands[..].try_into() {
                    Ok(bytes) => {
                        let jump_pos = u16::from_be_bytes(bytes) as usize;

                        let condition = self.pop();
                        if !condition.is_truthy() {
                            ip = jump_pos - 1;
                        }
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpGetGlobal => match operands[..].try_into() {
                    Ok(bytes) => {
                        let global_index = u16::from_be_bytes(bytes) as usize;

                        self.push(self.globals[global_index].clone())?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpSetGlobal => match operands[..].try_into() {
                    Ok(bytes) => {
                        let global_index = u16::from_be_bytes(bytes) as usize;

                        self.globals[global_index] = self.pop();
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpArray => match operands[..].try_into() {
                    Ok(bytes) => {
                        let num_elements = u16::from_be_bytes(bytes) as usize;

                        let new_sp = self.sp - num_elements;
                        let array = self.build_array(new_sp, self.sp);
                        self.sp = new_sp;
                        self.push(array)?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpHash => match operands[..].try_into() {
                    Ok(bytes) => {
                        let num_elements = u16::from_be_bytes(bytes) as usize;

                        let new_sp = self.sp - num_elements;
                        let hash = self.build_hash(new_sp, self.sp)?;
                        self.sp = new_sp;
                        self.push(hash)?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpIndex => {
                    let index = self.pop();
                    let identifier = self.pop();

                    self.execute_index_expression(&identifier, &index)?;
                }
                _ => todo!(),
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
            (Object::String(lhs_value), Object::String(rhs_value)) => {
                self.exec_string_binary_operation(op, lhs_value, rhs_value)
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

    fn exec_string_binary_operation(
        &mut self,
        op: &Opcode,
        lhs: &str,
        rhs: &str,
    ) -> Result<(), RuntimeError> {
        let result = match op {
            Opcode::OpAdd => Some([lhs, rhs].join("")),
            _ => None,
        };

        match result {
            Some(r) => self.push(Object::String(r)),
            None => Err(format!("unknown STRING operator: {:?}", op)),
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
            Object::Null => self.push(Object::Boolean(true)),
            _ => self.push(Object::Boolean(false)),
        }
    }

    fn build_array(&self, start: usize, end: usize) -> Object {
        let mut elements = vec![Object::Null; end - start];
        elements.clone_from_slice(&self.stack[start..end]);

        Object::Array(elements)
    }

    fn build_hash(&self, start: usize, end: usize) -> Result<Object, RuntimeError> {
        let mut pairs = BTreeMap::new();
        for i in (start..end).step_by(2) {
            let key = self.stack[i].clone();
            match key.get_hash_key() {
                Some(hash_key) => {
                    let value = self.stack[i + 1].clone();
                    pairs.insert(hash_key, HashPair { key, value });
                }
                None => {
                    return Err(format!("unusable as hash key: {}", key.get_type_str()));
                }
            }
        }

        Ok(Object::Hash(pairs))
    }

    fn execute_index_expression(
        &mut self,
        identifier: &Object,
        index: &Object,
    ) -> Result<(), RuntimeError> {
        match (&identifier, &index) {
            (Object::Array(array), Object::Integer(integer)) => {
                self.execute_array_index_expression(array, *integer as usize)
            }
            (Object::Hash(hash), index) => self.execute_hash_index_expression(hash, index),
            _ => Err(format!(
                "index operator not supported: {}",
                identifier.get_type_str()
            )),
        }
    }

    fn execute_array_index_expression(
        &mut self,
        array: &[Object],
        index: usize,
    ) -> Result<(), RuntimeError> {
        if index >= array.len() {
            return self.push(Object::Null);
        }

        self.push(array[index].clone())
    }

    fn execute_hash_index_expression(
        &mut self,
        hash: &BTreeMap<HashKey, HashPair>,
        index: &Object,
    ) -> Result<(), RuntimeError> {
        if let Some(hash_key) = index.get_hash_key() {
            if let Some(pair) = hash.get(&hash_key) {
                self.push(pair.clone().value)
            } else {
                self.push(Object::Null)
            }
        } else {
            Err(format!("unusable as hash key: {}", index.get_type_str()))
        }
    }

    pub fn last_popped(&self) -> Object {
        self.stack[self.sp].clone()
    }

    fn push(&mut self, obj: Object) -> Result<(), RuntimeError> {
        if self.sp > STACK_SIZE {
            return Err("stack overflow".to_string());
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
