use crate::{
    code::{make, Instructions, Opcode},
    evaluator::object::Object,
    lexer::token::Token,
    parser::ast::{Expression, Node, Statement},
};

mod test_compiler;

type CompileError = String;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile(&mut self, node: &Node) -> Result<Bytecode, CompileError> {
        match node {
            Node::Program(prgm) => {
                for stmt in prgm.0.iter() {
                    self.compile_stmt(stmt)?;
                }
            }
            _ => todo!(),
        }

        Ok(self.bytecode())
    }

    pub fn compile_stmt(&mut self, stmt: &Statement) -> Result<(), CompileError> {
        match stmt {
            Statement::Expression(expr_stmt) => {
                self.compile_expr(&expr_stmt.expr)?;
                self.emit(Opcode::OpPop, &[]);
                Ok(())
            }
            _ => todo!(),
        }
    }

    pub fn compile_expr(&mut self, expr: &Expression) -> Result<(), CompileError> {
        match expr {
            Expression::Integer(integer) => {
                let int_obj = Object::Integer(integer.value);
                let int_pos = self.add_constant(int_obj) as i32;
                self.emit(Opcode::OpConstant, &[int_pos]);
                Ok(())
            }
            Expression::Boolean(boolean) => {
                match boolean.value {
                    true => {
                        self.emit(Opcode::OpTrue, &[]);
                    }
                    false => {
                        self.emit(Opcode::OpFalse, &[]);
                    }
                }
                Ok(())
            }
            Expression::Prefix(prefix) => {
                self.compile_expr(&prefix.operand)?;

                match prefix.operator {
                    Token::Minus => self.emit(Opcode::OpMinus, &[]),
                    Token::Bang => self.emit(Opcode::OpBang, &[]),
                    _ => {
                        return Err(format!(
                            "unknown operator: {}",
                            prefix.operator.get_literal()
                        ))
                    }
                };
                Ok(())
            }
            Expression::Infix(infix) => {
                if infix.operator == Token::LessThan {
                    self.compile_expr(&infix.rhs)?;
                    self.compile_expr(&infix.lhs)?;
                    self.emit(Opcode::OpGreaterThan, &[]);
                    return Ok(());
                }

                self.compile_expr(&infix.lhs)?;
                self.compile_expr(&infix.rhs)?;

                match infix.operator {
                    Token::Plus => self.emit(Opcode::OpAdd, &[]),
                    Token::Minus => self.emit(Opcode::OpSub, &[]),
                    Token::Asterisk => self.emit(Opcode::OpMul, &[]),
                    Token::Slash => self.emit(Opcode::OpDiv, &[]),
                    Token::GreaterThan => self.emit(Opcode::OpGreaterThan, &[]),
                    Token::Equal => self.emit(Opcode::OpEqual, &[]),
                    Token::NotEqual => self.emit(Opcode::OpNotEqual, &[]),
                    _ => {
                        return Err(format!(
                            "unknown operator: {}",
                            infix.operator.get_literal()
                        ))
                    }
                };
                Ok(())
            }
            _ => todo!(),
        }
    }

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> usize {
        let instruction = make(op, operands);
        self.add_instruction(instruction)
    }

    fn add_instruction(&mut self, instruction: (Opcode, Vec<u8>)) -> usize {
        let pos_new_instr = self.instructions.stream.len();
        self.instructions.stream.push(instruction);
        pos_new_instr
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}
