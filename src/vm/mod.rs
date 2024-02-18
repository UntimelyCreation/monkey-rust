use std::{collections::BTreeMap, rc::Rc};

use crate::{
    code::Opcode,
    compiler::Bytecode,
    object::{
        builtins::{BuiltinFn, BUILTINS},
        Closure, CompiledFn, HashKey, HashPair, Object,
    },
};

use self::frame::Frame;

pub mod frame;
mod test_vm;

type VmError = String;

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65535;
const MAX_FRAMES: usize = 1024;

pub struct Vm {
    constants: Vec<Rc<Object>>,
    stack: Vec<Rc<Object>>,
    sp: usize,
    globals: Vec<Rc<Object>>,

    frames: Vec<Frame>,
    frames_idx: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),

            stack: {
                let rc = Rc::new(Object::Null);
                vec![rc; STACK_SIZE]
            },
            sp: 0,

            globals: {
                let rc = Rc::new(Object::Null);
                vec![rc; GLOBALS_SIZE]
            },

            frames: Vec::new(),
            frames_idx: 1,
        }
    }

    pub fn from_bytecode(bytecode: Bytecode) -> Self {
        let main_function = CompiledFn {
            instructions: bytecode.instructions.clone(),
            num_locals: 0,
            num_parameters: 0,
        };
        let main_closure = Closure {
            function: Rc::new(main_function),
            free_vars: Vec::new(),
        };
        let main_frame = Frame::from_function(main_closure, 0);

        let mut frames = vec![Frame::new(); MAX_FRAMES];
        frames[0] = main_frame;

        Self {
            constants: bytecode.constants.to_vec(),

            stack: {
                let rc = Rc::new(Object::Null);
                vec![rc; STACK_SIZE]
            },
            sp: 0,

            globals: {
                let rc = Rc::new(Object::Null);
                vec![rc; GLOBALS_SIZE]
            },

            frames,
            frames_idx: 1,
        }
    }

    pub fn update(&mut self, bytecode: Bytecode) {
        self.constants = bytecode.constants.to_vec();

        let main_function = CompiledFn {
            instructions: bytecode.instructions.clone(),
            num_locals: 0,
            num_parameters: 0,
        };
        let main_closure = Closure {
            function: Rc::new(main_function),
            free_vars: Vec::new(),
        };
        let main_frame = Frame::from_function(main_closure, 0);

        let mut frames = vec![Frame::new(); MAX_FRAMES];
        frames[0] = main_frame;

        self.frames = frames;
        self.frames_idx = 1;
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        while self.current_frame().ip
            < (self.current_frame().instructions().stream.len() - 1) as i32
        {
            self.current_frame_mut().ip += 1;

            let ip = self.current_frame().ip as usize;
            let instr = &self.current_frame().instructions().stream;
            let op = Opcode::from(instr[ip]);

            match op {
                Opcode::OpConstant => match instr[ip + 1..ip + 3].try_into() {
                    Ok(bytes) => {
                        let const_index = u16::from_be_bytes(bytes) as usize;
                        self.current_frame_mut().ip += 2;
                        self.push_stack(self.constants[const_index].clone())?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpNull => {
                    self.push_stack(Rc::new(Object::Null))?;
                }
                Opcode::OpPop => {
                    self.pop_stack();
                }
                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                    self.exec_binary_operation(&op)?;
                }
                Opcode::OpTrue => {
                    self.push_stack(Rc::new(Object::Boolean(true)))?;
                }
                Opcode::OpFalse => {
                    self.push_stack(Rc::new(Object::Boolean(false)))?;
                }
                Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
                    self.exec_comparison(&op)?;
                }
                Opcode::OpMinus => {
                    self.exec_minus_operator()?;
                }
                Opcode::OpBang => {
                    self.exec_bang_operator()?;
                }
                Opcode::OpJump => match instr[ip + 1..ip + 3].try_into() {
                    Ok(bytes) => {
                        let jump_pos = u16::from_be_bytes(bytes) as usize;
                        self.current_frame_mut().ip = (jump_pos - 1) as i32;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpJumpCond => match instr[ip + 1..ip + 3].try_into() {
                    Ok(bytes) => {
                        let jump_pos = u16::from_be_bytes(bytes) as usize;
                        self.current_frame_mut().ip += 2;

                        let condition = self.pop_stack();
                        if !condition.is_truthy() {
                            self.current_frame_mut().ip = (jump_pos - 1) as i32;
                        }
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpGetGlobal => match instr[ip + 1..ip + 3].try_into() {
                    Ok(bytes) => {
                        let global_index = u16::from_be_bytes(bytes) as usize;
                        self.current_frame_mut().ip += 2;

                        self.push_stack(self.globals[global_index].clone())?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpSetGlobal => match instr[ip + 1..ip + 3].try_into() {
                    Ok(bytes) => {
                        let global_index = u16::from_be_bytes(bytes) as usize;
                        self.current_frame_mut().ip += 2;

                        self.globals[global_index] = self.pop_stack();
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpArray => match instr[ip + 1..ip + 3].try_into() {
                    Ok(bytes) => {
                        let num_elements = u16::from_be_bytes(bytes) as usize;
                        self.current_frame_mut().ip += 2;

                        let new_sp = self.sp - num_elements;
                        let array = self.build_array(new_sp, self.sp);
                        self.sp = new_sp;
                        self.push_stack(Rc::new(array))?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpHash => match instr[ip + 1..ip + 3].try_into() {
                    Ok(bytes) => {
                        let num_elements = u16::from_be_bytes(bytes) as usize;
                        self.current_frame_mut().ip += 2;

                        let new_sp = self.sp - num_elements;
                        let hash = self.build_hash(new_sp, self.sp)?;
                        self.sp = new_sp;
                        self.push_stack(Rc::new(hash))?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpIndex => {
                    let index = self.pop_stack();
                    let identifier = self.pop_stack();

                    self.execute_index_expression(&identifier, &index)?;
                }
                Opcode::OpCall => {
                    let num_args = instr[ip + 1] as usize;
                    self.current_frame_mut().ip += 1;

                    self.execute_call(num_args)?;
                }
                Opcode::OpReturnValue => {
                    let return_value = self.pop_stack();

                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push_stack(return_value)?;
                }
                Opcode::OpReturn => {
                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push_stack(Rc::new(Object::Null))?;
                }
                Opcode::OpGetLocal => {
                    let local_index = instr[ip + 1] as usize;
                    self.current_frame_mut().ip += 1;

                    let base_pointer = self.current_frame().base_pointer;
                    self.push_stack(self.stack[base_pointer + local_index].clone())?;
                }
                Opcode::OpSetLocal => {
                    let local_index = instr[ip + 1] as usize;
                    self.current_frame_mut().ip += 1;

                    let base_pointer = self.current_frame().base_pointer;
                    self.stack[base_pointer + local_index] = self.pop_stack();
                }
                Opcode::OpGetBuiltin => {
                    let builtin_index = instr[ip + 1] as usize;
                    self.current_frame_mut().ip += 1;

                    let definition = BUILTINS[builtin_index].1;

                    self.push_stack(Rc::new(Object::BuiltinFn(definition)))?;
                }
                Opcode::OpClosure => match instr[ip + 1..ip + 3].try_into() {
                    Ok(bytes) => {
                        let const_index = u16::from_be_bytes(bytes) as usize;
                        let num_free = instr[ip + 3] as usize;
                        self.current_frame_mut().ip += 3;

                        self.push_closure(const_index, num_free)?;
                    }
                    Err(..) => {
                        return Err("error in instruction".to_string());
                    }
                },
                Opcode::OpGetFree => {
                    let free_index = instr[ip + 1] as usize;
                    self.current_frame_mut().ip += 1;

                    let current_closure = &self.current_frame().closure;
                    self.push_stack(current_closure.free_vars[free_index].clone())?;
                }
                Opcode::OpCurrentClosure => {
                    let current_closure = self.current_frame().closure.clone();
                    self.push_stack(Rc::new(Object::Closure(current_closure)))?;
                }
            }
        }
        Ok(())
    }

    fn exec_binary_operation(&mut self, op: &Opcode) -> Result<(), VmError> {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        match (lhs.as_ref(), rhs.as_ref()) {
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
    ) -> Result<(), VmError> {
        let result = match op {
            Opcode::OpAdd => Some(lhs + rhs),
            Opcode::OpSub => Some(lhs - rhs),
            Opcode::OpMul => Some(lhs * rhs),
            Opcode::OpDiv => Some(lhs / rhs),
            _ => None,
        };

        match result {
            Some(r) => self.push_stack(Rc::new(Object::Integer(r))),
            None => Err(format!("unknown INTEGER operator: {:?}", op)),
        }
    }

    fn exec_string_binary_operation(
        &mut self,
        op: &Opcode,
        lhs: &str,
        rhs: &str,
    ) -> Result<(), VmError> {
        let result = match op {
            Opcode::OpAdd => Some([lhs, rhs].join("")),
            _ => None,
        };

        match result {
            Some(r) => self.push_stack(Rc::new(Object::String(r))),
            None => Err(format!("unknown STRING operator: {:?}", op)),
        }
    }

    fn exec_comparison(&mut self, op: &Opcode) -> Result<(), VmError> {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        match (lhs.as_ref(), rhs.as_ref()) {
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

    fn exec_integer_comparison(&mut self, op: &Opcode, lhs: i32, rhs: i32) -> Result<(), VmError> {
        let result = match op {
            Opcode::OpEqual => Some(lhs == rhs),
            Opcode::OpNotEqual => Some(lhs != rhs),
            Opcode::OpGreaterThan => Some(lhs > rhs),
            _ => None,
        };

        match result {
            Some(r) => self.push_stack(Rc::new(Object::Boolean(r))),
            None => Err(format!("unknown INTEGER operator: {:?}", op)),
        }
    }

    fn exec_boolean_comparison(
        &mut self,
        op: &Opcode,
        lhs: bool,
        rhs: bool,
    ) -> Result<(), VmError> {
        let result = match op {
            Opcode::OpEqual => Some(lhs == rhs),
            Opcode::OpNotEqual => Some(lhs != rhs),
            _ => None,
        };

        match result {
            Some(r) => self.push_stack(Rc::new(Object::Boolean(r))),
            None => Err(format!("unknown BOOLEAN operator: {:?}", op)),
        }
    }

    fn exec_minus_operator(&mut self) -> Result<(), VmError> {
        let operand = self.pop_stack();
        match operand.as_ref() {
            Object::Integer(value) => self.push_stack(Rc::new(Object::Integer(-value))),
            _ => Err(format!(
                "unsupported type for negation: {}",
                operand.get_type_str()
            )),
        }
    }

    fn exec_bang_operator(&mut self) -> Result<(), VmError> {
        match self.pop_stack().as_ref() {
            Object::Boolean(value) => self.push_stack(Rc::new(Object::Boolean(!value))),
            Object::Null => self.push_stack(Rc::new(Object::Boolean(true))),
            _ => self.push_stack(Rc::new(Object::Boolean(false))),
        }
    }

    fn build_array(&self, start: usize, end: usize) -> Object {
        let mut elements = {
            let rc = Rc::new(Object::Null);
            vec![rc; end - start]
        };
        elements.clone_from_slice(&self.stack[start..end]);

        Object::Array(elements)
    }

    fn build_hash(&self, start: usize, end: usize) -> Result<Object, VmError> {
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
    ) -> Result<(), VmError> {
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
        array: &[Rc<Object>],
        index: usize,
    ) -> Result<(), VmError> {
        if index >= array.len() {
            return self.push_stack(Rc::new(Object::Null));
        }

        self.push_stack(array[index].clone())
    }

    fn execute_hash_index_expression(
        &mut self,
        hash: &BTreeMap<HashKey, HashPair>,
        index: &Object,
    ) -> Result<(), VmError> {
        if let Some(hash_key) = index.get_hash_key() {
            if let Some(pair) = hash.get(&hash_key) {
                self.push_stack(pair.value.clone())
            } else {
                self.push_stack(Rc::new(Object::Null))
            }
        } else {
            Err(format!("unusable as hash key: {}", index.get_type_str()))
        }
    }

    fn execute_call(&mut self, num_args: usize) -> Result<(), VmError> {
        match self.stack[self.sp - 1 - num_args].as_ref() {
            Object::Closure(closure) => self.call_closure(closure.clone(), num_args),
            Object::BuiltinFn(builtin_fn) => self.call_builtin(*builtin_fn, num_args),
            _ => Err("calling non-function".to_string()),
        }
    }

    fn call_closure(&mut self, closure: Closure, num_args: usize) -> Result<(), VmError> {
        if num_args != closure.function.num_parameters {
            return Err(format!(
                "wrong number of arguments: expected {}, found {}",
                closure.function.num_parameters, num_args
            ));
        }

        let frame = Frame::from_function(closure.clone(), self.sp - num_args);
        self.sp = frame.base_pointer + closure.function.num_locals;
        self.push_frame(frame);

        Ok(())
    }

    fn call_builtin(&mut self, builtin_fn: BuiltinFn, num_args: usize) -> Result<(), VmError> {
        let args = &self.stack[self.sp - num_args..self.sp];

        let result = builtin_fn(args);
        self.sp -= num_args + 1;

        self.push_stack(result)
    }

    pub fn last_popped(&self) -> Rc<Object> {
        self.stack[self.sp].clone()
    }

    fn push_stack(&mut self, obj: Rc<Object>) -> Result<(), VmError> {
        if self.sp > STACK_SIZE {
            return Err("stack overflow".to_string());
        }

        self.stack[self.sp] = obj;
        self.sp += 1;
        Ok(())
    }

    fn pop_stack(&mut self) -> Rc<Object> {
        let obj = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        obj
    }

    fn current_frame(&self) -> &Frame {
        &self.frames[self.frames_idx - 1]
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_idx - 1]
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames[self.frames_idx] = frame;
        self.frames_idx += 1;
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames_idx -= 1;
        self.frames[self.frames_idx].clone()
    }

    fn push_closure(&mut self, const_index: usize, num_free: usize) -> Result<(), VmError> {
        let constant = &self.constants[const_index];
        match constant.as_ref() {
            Object::CompiledFn(compiled_fn) => {
                let mut free_vars = {
                    let rc = Rc::new(Object::Null);
                    vec![rc; num_free]
                };
                free_vars.clone_from_slice(&self.stack[self.sp - num_free..self.sp]);
                self.sp -= num_free;

                let closure = Object::Closure(Closure {
                    function: Rc::new(compiled_fn.clone()),
                    free_vars,
                });
                self.push_stack(Rc::new(closure))
            }
            _ => Err(format!("not a function: {}", constant)),
        }
    }
}
