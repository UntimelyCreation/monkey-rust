use crate::{
    code::{make, Instructions, Opcode},
    evaluator::object::{CompiledFn, Object},
    lexer::token::Token,
    parser::ast::{BlockStatement, Expression, Node, Statement},
};

use self::symbol::{SymbolScope, SymbolTable};

pub mod symbol;
mod test_compiler;
mod test_symbol;

type CompileError = String;

pub struct CompilationScope {
    instructions: Instructions,
    last_op: Opcode,
    prev_op: Opcode,
}

impl Default for CompilationScope {
    fn default() -> Self {
        Self::new()
    }
}

impl CompilationScope {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            last_op: Opcode::OpNull,
            prev_op: Opcode::OpNull,
        }
    }
}

pub struct Compiler {
    constants: Vec<Object>,
    symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope::new();
        Self {
            constants: Vec::new(),
            symbol_table: SymbolTable::new(),

            scopes: vec![main_scope],
            scope_index: 0,
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
                self.emit(
                    match &symbol.scope {
                        SymbolScope::Global => Opcode::OpSetGlobal,
                        SymbolScope::Local => Opcode::OpSetLocal,
                    },
                    &[symbol.index as i32],
                );
                Ok(())
            }
            Statement::Return(stmt) => {
                self.compile_expression(&stmt.value)?;
                self.emit(Opcode::OpReturnValue, &[]);
                Ok(())
            }
            Statement::Expression(stmt) => {
                self.compile_expression(&stmt.expr)?;
                self.emit(Opcode::OpPop, &[]);
                Ok(())
            }
        }
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<(), CompileError> {
        match expr {
            Expression::Identifier(expr) => {
                match self.symbol_table.resolve(&expr.name) {
                    Some(symbol) => self.emit(
                        match &symbol.scope {
                            SymbolScope::Global => Opcode::OpGetGlobal,
                            SymbolScope::Local => Opcode::OpGetLocal,
                        },
                        &[symbol.index as i32],
                    ),
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
                if self.last_instruction_is(Opcode::OpPop) {
                    self.remove_last_instr();
                }

                // Placeholder operand value
                let jump_pos = self.emit(Opcode::OpJump, &[-1]);

                let after_cons_pos = self.current_instructions().stream.len();
                self.update_operand(jump_cond_pos, after_cons_pos as i32);

                match &expr.alternative {
                    Some(alternative) => {
                        self.compile_block_statement(alternative)?;
                        if self.last_instruction_is(Opcode::OpPop) {
                            self.remove_last_instr();
                        }
                    }
                    None => {
                        self.emit(Opcode::OpNull, &[]);
                    }
                }

                let after_alt_pos = self.current_instructions().stream.len();
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
            Expression::FnLiteral(expr) => {
                self.enter_scope();
                self.compile_block_statement(&expr.body)?;

                if self.last_instruction_is(Opcode::OpPop) {
                    self.remove_last_instr();
                    self.emit(Opcode::OpReturnValue, &[]);
                }
                if !self.last_instruction_is(Opcode::OpReturnValue) {
                    self.emit(Opcode::OpReturn, &[]);
                }

                let num_locals = self.symbol_table.num_definitions;

                let instrs = self.leave_scope();

                let compiled_fn_obj = Object::CompiledFn(CompiledFn {
                    instructions: instrs,
                    num_locals,
                });
                let compiled_fn_pos = self.add_constant(compiled_fn_obj);
                self.emit(Opcode::OpConstant, &[compiled_fn_pos as i32]);
                Ok(())
            }
            Expression::Call(expr) => {
                self.compile_expression(&expr.function)?;
                self.emit(Opcode::OpCall, &[]);
                Ok(())
            }
            _ => todo!(),
        }
    }

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
            constants: self.constants.clone(),
        }
    }

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> usize {
        let instruction = make(op.clone(), operands);
        let position = self.add_instruction(instruction);

        self.set_last_op(op);

        position
    }

    fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
    }

    fn current_instructions_mut(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
    }

    fn last_instruction_is(&self, op: Opcode) -> bool {
        if self.current_instructions().stream.is_empty() {
            return false;
        }

        self.scopes[self.scope_index].last_op == op
    }

    fn add_instruction(&mut self, instruction: (Opcode, Vec<u8>)) -> usize {
        let pos_new_instr = self.current_instructions().stream.len();
        self.current_instructions_mut().stream.push(instruction);
        pos_new_instr
    }

    fn set_last_op(&mut self, op: Opcode) {
        let prev = self.scopes[self.scope_index].last_op.clone();

        self.scopes[self.scope_index].prev_op = prev;
        self.scopes[self.scope_index].last_op = op;
    }

    fn remove_last_instr(&mut self) {
        let prev = self.scopes[self.scope_index].prev_op.clone();

        self.current_instructions_mut().stream.pop();
        self.scopes[self.scope_index].last_op = prev;
    }

    fn update_operand(&mut self, position: usize, operand: i32) {
        let op = self.current_instructions().stream[position].0.clone();
        let new_instr = make(op, &[operand]);

        self.current_instructions_mut().stream[position] = new_instr;
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::new();
        self.scopes.push(scope);
        self.scope_index += 1;

        self.symbol_table = SymbolTable::new_enclosed(self.symbol_table.clone());
    }

    fn leave_scope(&mut self) -> Instructions {
        let instrs = self.current_instructions().clone();

        self.scopes.pop();
        self.scope_index -= 1;

        if let Some(outer) = &self.symbol_table.outer {
            self.symbol_table = outer.as_ref().clone();
        }

        instrs
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}
