use crate::{
    code::{make, Instructions, Opcode},
    evaluator::object::Object,
    lexer::token::Token,
    parser::ast::{BlockStatement, Expression, Node, Statement},
};

use self::symbol::SymbolTable;

pub mod symbol;
mod test_compiler;
mod test_symbol;

type CompileError = String;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    symbol_table: SymbolTable,

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
        Self {
            instructions: Instructions::new(),
            constants: Vec::new(),
            symbol_table: SymbolTable::new(),
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

    fn compile_block_statement(&mut self, stmts: &BlockStatement) -> Result<(), CompileError> {
        for stmt in stmts.statements.iter() {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Statement) -> Result<(), CompileError> {
        match stmt {
            Statement::Let(stmt) => {
                self.compile_expression(&stmt.value)?;
                let symbol = self.symbol_table.define(stmt.identifier.name.clone());
                self.emit(Opcode::OpSetGlobal, &[symbol.index as i32]);
                Ok(())
            }
            Statement::Expression(stmt) => {
                self.compile_expression(&stmt.expr)?;
                self.emit(Opcode::OpPop, &[]);
                Ok(())
            }
            _ => todo!(),
        }
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<(), CompileError> {
        match expr {
            Expression::Identifier(expr) => {
                match self.symbol_table.resolve(&expr.name) {
                    Some(symbol) => self.emit(Opcode::OpGetGlobal, &[symbol.index as i32]),
                    None => return Err(format!("undefined variable: {}", expr.name)),
                };
                Ok(())
            }
            Expression::Integer(expr) => {
                let int_obj = Object::Integer(expr.value);
                let int_pos = self.add_constant(int_obj) as i32;
                self.emit(Opcode::OpConstant, &[int_pos]);
                Ok(())
            }
            Expression::String(expr) => {
                let string_obj = Object::String(expr.value.clone());
                let string_pos = self.add_constant(string_obj) as i32;
                self.emit(Opcode::OpConstant, &[string_pos]);
                Ok(())
            }
            Expression::Prefix(expr) => {
                self.compile_expression(&expr.operand)?;

                match expr.operator {
                    Token::Minus => self.emit(Opcode::OpMinus, &[]),
                    Token::Bang => self.emit(Opcode::OpBang, &[]),
                    _ => return Err(format!("unknown operator: {}", expr.operator.get_literal())),
                };
                Ok(())
            }
            Expression::Infix(expr) => {
                if expr.operator == Token::LessThan {
                    self.compile_expression(&expr.rhs)?;
                    self.compile_expression(&expr.lhs)?;
                    self.emit(Opcode::OpGreaterThan, &[]);
                    return Ok(());
                }

                self.compile_expression(&expr.lhs)?;
                self.compile_expression(&expr.rhs)?;

                match expr.operator {
                    Token::Plus => self.emit(Opcode::OpAdd, &[]),
                    Token::Minus => self.emit(Opcode::OpSub, &[]),
                    Token::Asterisk => self.emit(Opcode::OpMul, &[]),
                    Token::Slash => self.emit(Opcode::OpDiv, &[]),
                    Token::GreaterThan => self.emit(Opcode::OpGreaterThan, &[]),
                    Token::Equal => self.emit(Opcode::OpEqual, &[]),
                    Token::NotEqual => self.emit(Opcode::OpNotEqual, &[]),
                    _ => return Err(format!("unknown operator: {}", expr.operator.get_literal())),
                };
                Ok(())
            }
            Expression::Boolean(expr) => {
                match expr.value {
                    true => {
                        self.emit(Opcode::OpTrue, &[]);
                    }
                    false => {
                        self.emit(Opcode::OpFalse, &[]);
                    }
                }
                Ok(())
            }
            Expression::If(expr) => {
                self.compile_expression(&expr.condition)?;

                // Placeholder operand value
                let jump_cond_pos = self.emit(Opcode::OpJumpCond, &[-1]);

                self.compile_block_statement(&expr.consequence)?;
                if self.last_op == Opcode::OpPop {
                    self.remove_last_instr();
                }

                // Placeholder operand value
                let jump_pos = self.emit(Opcode::OpJump, &[-1]);

                let after_cons_pos = self.instructions.stream.len();
                self.update_operand(jump_cond_pos, after_cons_pos as i32);

                match &expr.alternative {
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
            Expression::ArrayLiteral(expr) => {
                for el in expr.elements.iter() {
                    self.compile_expression(el)?;
                }
                self.emit(Opcode::OpArray, &[expr.elements.len() as i32]);
                Ok(())
            }
            Expression::HashLiteral(expr) => {
                for (k, v) in expr.pairs.iter() {
                    self.compile_expression(k)?;
                    self.compile_expression(v)?;
                }
                self.emit(Opcode::OpHash, &[(expr.pairs.len() * 2) as i32]);
                Ok(())
            }
            Expression::Index(expr) => {
                self.compile_expression(&expr.identifier)?;
                self.compile_expression(&expr.index)?;
                self.emit(Opcode::OpIndex, &[]);
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
