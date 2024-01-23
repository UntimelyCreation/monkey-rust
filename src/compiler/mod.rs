use crate::{
    code::{make, Instructions, Opcode},
    evaluator::object::Object,
    lexer::token::Token,
    parser::ast::{BlockStatement, Expression, Node, Statement},
};

mod test_compiler;

type CompileError = String;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,

    last_op: Opcode,
    prev_op: Opcode,
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
            last_op: Opcode::OpPop,
            prev_op: Opcode::OpPop,
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

    pub fn compile_block_statement(&mut self, stmts: &BlockStatement) -> Result<(), CompileError> {
        for stmt in stmts.statements.iter() {
            self.compile_stmt(stmt)?;
        }
        Ok(())
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
            Expression::If(if_expr) => {
                self.compile_expr(&if_expr.condition)?;

                // Placeholder operand value
                let jump_cond_pos = self.emit(Opcode::OpJumpCond, &[-1]);

                self.compile_block_statement(&if_expr.consequence)?;
                if self.last_op == Opcode::OpPop {
                    self.remove_last_instr();
                }

                // Placeholder operand value
                let jump_pos = self.emit(Opcode::OpJump, &[-1]);

                let after_cons_pos = self.instructions.stream.len();
                self.update_operand(jump_cond_pos, after_cons_pos as i32);

                match &if_expr.alternative {
                    Some(alternative) => {
                        self.compile_block_statement(alternative)?;
                        if self.last_op == Opcode::OpPop {
                            self.remove_last_instr();
                        }
                    }
                    None => {
                        self.emit(Opcode::OpNull, &[]);
                    }
                }

                let after_alt_pos = self.instructions.stream.len();
                self.update_operand(jump_pos, after_alt_pos as i32);

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
        let instruction = make(op.clone(), operands);
        let position = self.add_instruction(instruction);

        self.set_last_op(op);

        position
    }

    fn add_instruction(&mut self, instruction: (Opcode, Vec<u8>)) -> usize {
        let pos_new_instr = self.instructions.stream.len();
        self.instructions.stream.push(instruction);
        pos_new_instr
    }

    fn set_last_op(&mut self, op: Opcode) {
        self.prev_op = self.last_op.clone();
        self.last_op = op;
    }

    fn remove_last_instr(&mut self) {
        self.instructions.stream.pop();
        self.last_op = self.prev_op.clone();
    }

    fn update_operand(&mut self, position: usize, operand: i32) {
        let op = self.instructions.stream[position].0.clone();
        let new_instr = make(op, &[operand]);

        self.instructions.stream[position] = new_instr;
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
