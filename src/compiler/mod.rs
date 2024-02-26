use std::rc::Rc;

use crate::{
    code::{make, Instructions, Opcode},
    lexer::token::Token,
    object::{builtins::BUILTINS, CompiledFn, Object},
    parser::ast::{BlockStatement, Expression, Node, Statement},
};

use self::symbol::{Symbol, SymbolScope, SymbolTable};

pub mod symbol;
mod test_compiler;
mod test_symbol;

type CompileError = String;

pub struct CompilationScope {
    instructions: Instructions,
    last_instruction: EmittedInstruction,
    prev_instruction: EmittedInstruction,
}

impl CompilationScope {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            last_instruction: EmittedInstruction::new(),
            prev_instruction: EmittedInstruction::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub position: usize,
}

impl EmittedInstruction {
    pub fn new() -> Self {
        Self {
            opcode: Opcode::OpNull,
            position: 0,
        }
    }
}

pub struct Compiler {
    constants: Vec<Rc<Object>>,
    symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope::new();

        let mut symbol_table = SymbolTable::new();
        for (i, (name, _)) in BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i, name);
        }

        Self {
            constants: Vec::new(),
            symbol_table,

            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn compile(&mut self, node: &Node) -> Result<Bytecode, CompileError> {
        match node {
            Node::Program(prgm) => {
                for stmt in prgm.0.iter() {
                    self.compile_statement(stmt)?;
                }
            }
            Node::Statement(stmt) => {
                self.compile_statement(stmt)?;
            }
            Node::Expression(expr) => {
                self.compile_expression(expr)?;
            }
        }

        Ok(self.bytecode())
    }

    fn compile_block_statement(&mut self, stmts: &BlockStatement) -> Result<(), CompileError> {
        for stmt in stmts.statements.iter() {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompileError> {
        match stmt {
            Statement::Let(stmt) => {
                let symbol = self.symbol_table.define(&stmt.identifier.name);
                self.compile_expression(&stmt.value)?;

                self.emit(
                    match &symbol.scope {
                        SymbolScope::Global => Opcode::OpSetGlobal,
                        SymbolScope::Local => Opcode::OpSetLocal,
                        _ => unreachable!(),
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
                    Some(symbol) => self.load_symbol(&symbol),
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
                let string_obj = Object::String(expr.value.to_string());
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

                if !expr.name.is_empty() {
                    self.symbol_table.define_function(&expr.name);
                }

                for par in expr.parameters.iter() {
                    self.symbol_table.define(&par.name);
                }

                self.compile_block_statement(&expr.body)?;

                if self.last_instruction_is(Opcode::OpPop) {
                    self.remove_last_instr();
                    self.emit(Opcode::OpReturnValue, &[]);
                }
                if !self.last_instruction_is(Opcode::OpReturnValue) {
                    self.emit(Opcode::OpReturn, &[]);
                }

                let free_symbols = self.symbol_table.free_symbols.to_vec();
                let num_locals = self.symbol_table.num_definitions;
                let instrs = self.leave_scope();

                for symbol in &free_symbols {
                    self.load_symbol(symbol);
                }

                let compiled_fn_obj = Object::CompiledFn(CompiledFn {
                    instructions: instrs,
                    num_locals,
                    num_parameters: expr.parameters.len(),
                });
                let compiled_fn_pos = self.add_constant(compiled_fn_obj);
                self.emit(
                    Opcode::OpClosure,
                    &[compiled_fn_pos as i32, free_symbols.len() as i32],
                );
                Ok(())
            }
            Expression::Call(expr) => {
                self.compile_expression(&expr.function)?;

                for arg in expr.arguments.iter() {
                    self.compile_expression(arg)?;
                }

                self.emit(Opcode::OpCall, &[expr.arguments.len() as i32]);
                Ok(())
            }
        }
    }

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions(),
            constants: &self.constants,
        }
    }

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> usize {
        let mut instruction = make(op.clone(), operands);
        let position = self.add_instruction(&mut instruction);

        self.set_last_instruction(op, position);

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

        self.scopes[self.scope_index].last_instruction.opcode == op
    }

    fn add_instruction(&mut self, instr: &mut Vec<u8>) -> usize {
        let pos_new_instr = self.current_instructions().stream.len();
        self.current_instructions_mut().stream.append(instr);
        pos_new_instr
    }

    fn replace_instruction(&mut self, position: usize, new_instr: Vec<u8>) {
        self.current_instructions_mut().stream[position..(position + new_instr.len())]
            .copy_from_slice(&new_instr[..]);
    }

    fn set_last_instruction(&mut self, op: Opcode, position: usize) {
        let previous = self.scopes[self.scope_index].last_instruction.clone();
        let last_instruction = EmittedInstruction {
            opcode: op,
            position,
        };

        self.scopes[self.scope_index].prev_instruction = previous;
        self.scopes[self.scope_index].last_instruction = last_instruction;
    }

    fn remove_last_instr(&mut self) {
        let previous = self.scopes[self.scope_index].prev_instruction.clone();
        let last = self.scopes[self.scope_index].last_instruction.clone();

        let old = self.current_instructions().stream.to_vec();
        let new = old[..last.position].to_vec();

        self.scopes[self.scope_index].instructions.stream = new;
        self.scopes[self.scope_index].last_instruction = previous;
    }

    fn update_operand(&mut self, position: usize, operand: i32) {
        let op = Opcode::from(self.current_instructions().stream[position]);
        let new_instr = make(op, &[operand]);

        self.replace_instruction(position, new_instr);
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(Rc::new(obj));
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
            let outer = outer.as_ref().borrow().clone();
            self.symbol_table = outer;
        }

        instrs
    }

    fn load_symbol(&mut self, symbol: &Symbol) {
        match symbol.scope {
            SymbolScope::Global => self.emit(Opcode::OpGetGlobal, &[symbol.index as i32]),
            SymbolScope::Local => self.emit(Opcode::OpGetLocal, &[symbol.index as i32]),
            SymbolScope::Builtin => self.emit(Opcode::OpGetBuiltin, &[symbol.index as i32]),
            SymbolScope::Free => self.emit(Opcode::OpGetFree, &[symbol.index as i32]),
            SymbolScope::Function => self.emit(Opcode::OpCurrentClosure, &[]),
        };
    }
}

pub struct Bytecode<'a> {
    pub instructions: &'a Instructions,
    pub constants: &'a [Rc<Object>],
}
