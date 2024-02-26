#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        code::{make, Instructions, Opcode},
        compiler::{Compiler, EmittedInstruction},
        object::{CompiledFn, Object},
        parser::parse,
    };

    fn concat_instructions(instrs: Vec<Vec<u8>>) -> Instructions {
        Instructions {
            stream: instrs.into_iter().flatten().collect(),
        }
    }

    fn test_compiling(input: &str, expected_constants: &[Object], expected_instrs: Vec<Vec<u8>>) {
        let program = parse(input).expect("error occurred while parsing program");

        let mut compiler = Compiler::new();
        let bytecode = compiler
            .compile(&program)
            .expect("error occurred while compiling program");

        assert_eq!(
            concat_instructions(expected_instrs),
            bytecode.instructions.clone()
        );
        assert_eq!(
            expected_constants
                .iter()
                .map(|obj| Rc::new(obj.clone()))
                .collect::<Vec<_>>(),
            bytecode.constants
        );
    }

    #[test]
    fn test_integer_arithmetic() {
        let inputs = ["1; 2", "1 + 2", "1 - 2", "1 * 2", "2 / 1", "-1"];
        let expected_constants = [
            vec![Object::Integer(1), Object::Integer(2)],
            vec![Object::Integer(1), Object::Integer(2)],
            vec![Object::Integer(1), Object::Integer(2)],
            vec![Object::Integer(1), Object::Integer(2)],
            vec![Object::Integer(2), Object::Integer(1)],
            vec![Object::Integer(1)],
        ];
        let expected_instrs = [
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpSub, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpMul, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpDiv, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpMinus, &[]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let inputs = [
            "true",
            "false",
            "1 > 2",
            "1 < 2",
            "1 == 2",
            "1 != 2",
            "true == false",
            "true != false",
            "!true",
        ];
        let expected_constants = [
            vec![],
            vec![],
            vec![Object::Integer(1), Object::Integer(2)],
            vec![Object::Integer(2), Object::Integer(1)],
            vec![Object::Integer(1), Object::Integer(2)],
            vec![Object::Integer(1), Object::Integer(2)],
            vec![],
            vec![],
            vec![],
        ];
        let expected_instrs = [
            vec![make(Opcode::OpTrue, &[]), make(Opcode::OpPop, &[])],
            vec![make(Opcode::OpFalse, &[]), make(Opcode::OpPop, &[])],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpGreaterThan, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpGreaterThan, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpNotEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpFalse, &[]),
                make(Opcode::OpEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpFalse, &[]),
                make(Opcode::OpNotEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpBang, &[]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_conditionals() {
        let inputs = [
            "if (true) { 10 }; 3333;",
            "if (true) { 10 } else { 20 }; 3333;",
        ];
        let expected_constants = [
            vec![Object::Integer(10), Object::Integer(3333)],
            vec![
                Object::Integer(10),
                Object::Integer(20),
                Object::Integer(3333),
            ],
        ];
        let expected_instrs = [
            vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpJumpCond, &[10]),
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpJump, &[11]),
                make(Opcode::OpNull, &[]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpJumpCond, &[10]),
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpJump, &[13]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_global_let_statements() {
        let inputs = [
            "let one = 1; let two = 2;",
            "let one = 1; one;",
            "let one = 1; let two = one; two;",
        ];
        let expected_constants = [
            vec![Object::Integer(1), Object::Integer(2)],
            vec![Object::Integer(1)],
            vec![Object::Integer(1)],
        ];
        let expected_instrs = [
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpSetGlobal, &[1]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpSetGlobal, &[1]),
                make(Opcode::OpGetGlobal, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_string_expressions() {
        let inputs = ["\"monkey\"", "\"mon\" + \"key\""];
        let expected_constants = [
            vec![Object::String("monkey".to_string())],
            vec![
                Object::String("mon".to_string()),
                Object::String("key".to_string()),
            ],
        ];
        let expected_instrs = [
            vec![make(Opcode::OpConstant, &[0]), make(Opcode::OpPop, &[])],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_array_literals() {
        let inputs = ["[]", "[1, 2, 3]", "[1 + 2, 3 - 4, 5 * 6]"];
        let expected_constants = [
            vec![],
            vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
                Object::Integer(5),
                Object::Integer(6),
            ],
        ];
        let expected_instrs = [
            vec![make(Opcode::OpArray, &[0]), make(Opcode::OpPop, &[])],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpArray, &[3]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpSub, &[]),
                make(Opcode::OpConstant, &[4]),
                make(Opcode::OpConstant, &[5]),
                make(Opcode::OpMul, &[]),
                make(Opcode::OpArray, &[3]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_hash_literals() {
        let inputs = ["{}", "{1: 2, 3: 4, 5: 6}", "{1: 2 + 3, 4: 5 * 6}"];
        let expected_constants = [
            vec![],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
                Object::Integer(5),
                Object::Integer(6),
            ],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
                Object::Integer(5),
                Object::Integer(6),
            ],
        ];
        let expected_instrs = [
            vec![make(Opcode::OpHash, &[0]), make(Opcode::OpPop, &[])],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpConstant, &[4]),
                make(Opcode::OpConstant, &[5]),
                make(Opcode::OpHash, &[6]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpConstant, &[4]),
                make(Opcode::OpConstant, &[5]),
                make(Opcode::OpMul, &[]),
                make(Opcode::OpHash, &[4]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_index_expressions() {
        let inputs = ["[1, 2, 3][1 + 1]", "{1: 2}[2 - 1]"];
        let expected_constants = [
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(1),
                Object::Integer(1),
            ],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(2),
                Object::Integer(1),
            ],
        ];
        let expected_instrs = [
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpArray, &[3]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpConstant, &[4]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpIndex, &[]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpHash, &[2]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpSub, &[]),
                make(Opcode::OpIndex, &[]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_compilation_scopes() {
        let mut compiler = Compiler::new();
        assert_eq!(compiler.scope_index, 0);

        let global_symbol_table = compiler.symbol_table.clone();

        compiler.emit(Opcode::OpMul, &[]);

        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1);

        compiler.emit(Opcode::OpSub, &[]);
        assert_eq!(compiler.current_instructions().stream.len(), 1);

        let last = &compiler.scopes[compiler.scope_index].last_instruction;
        assert_eq!(
            *last,
            EmittedInstruction {
                opcode: Opcode::OpSub,
                position: 0
            }
        );

        assert_eq!(
            compiler.symbol_table.outer,
            Some(Rc::new(RefCell::new(global_symbol_table.clone())))
        );

        compiler.leave_scope();
        assert_eq!(compiler.scope_index, 0);

        assert_eq!(compiler.symbol_table, global_symbol_table);
        assert_eq!(compiler.symbol_table.outer, None);

        compiler.emit(Opcode::OpAdd, &[]);
        assert_eq!(compiler.current_instructions().stream.len(), 2);

        let last = &compiler.scopes[compiler.scope_index].last_instruction;
        let prev = &compiler.scopes[compiler.scope_index].prev_instruction;
        assert_eq!(
            *last,
            EmittedInstruction {
                opcode: Opcode::OpAdd,
                position: 1
            }
        );
        assert_eq!(
            *prev,
            EmittedInstruction {
                opcode: Opcode::OpMul,
                position: 0
            }
        );
    }

    #[test]
    fn test_functions() {
        let inputs = [
            "fn() { return 5 + 10 }",
            "fn() { 5 + 10 }",
            "fn() { 1; 2 }",
            "fn() { }",
        ];
        let expected_constants = [
            vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
            vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
            vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpPop, &[]),
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
            vec![Object::CompiledFn(CompiledFn {
                instructions: concat_instructions(vec![make(Opcode::OpReturn, &[])]),
                num_locals: 0,
                num_parameters: 0,
            })],
        ];
        let expected_instrs = [
            vec![make(Opcode::OpClosure, &[2, 0]), make(Opcode::OpPop, &[])],
            vec![make(Opcode::OpClosure, &[2, 0]), make(Opcode::OpPop, &[])],
            vec![make(Opcode::OpClosure, &[2, 0]), make(Opcode::OpPop, &[])],
            vec![make(Opcode::OpClosure, &[0, 0]), make(Opcode::OpPop, &[])],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_function_calls() {
        let inputs = [
            "fn() { 24 }()",
            "let noArg = fn() { 24 }; noArg()",
            "let oneArg = fn(a) { a }; oneArg(24);",
            "let manyArg = fn(a, b, c) { a; b; c }; manyArg(24, 25, 26);",
        ];
        let expected_constants = [
            vec![
                Object::Integer(24),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
            vec![
                Object::Integer(24),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
            vec![
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 1,
                }),
                Object::Integer(24),
            ],
            vec![
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpPop, &[]),
                        make(Opcode::OpGetLocal, &[1]),
                        make(Opcode::OpPop, &[]),
                        make(Opcode::OpGetLocal, &[2]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 3,
                    num_parameters: 3,
                }),
                Object::Integer(24),
                Object::Integer(25),
                Object::Integer(26),
            ],
        ];
        let expected_instrs = [
            vec![
                make(Opcode::OpClosure, &[1, 0]),
                make(Opcode::OpCall, &[0]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpClosure, &[1, 0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpCall, &[0]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpClosure, &[0, 0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpCall, &[1]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpClosure, &[0, 0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpCall, &[3]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_let_statement_scopes() {
        let inputs = [
            "let num = 55; fn() { num }",
            "fn() { let num = 55; num }",
            "fn() { let a = 55; let b = 77; a + b }",
        ];
        let expected_constants = [
            vec![
                Object::Integer(55),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpGetGlobal, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                }),
            ],
            vec![
                Object::Integer(55),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpSetLocal, &[0]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 0,
                }),
            ],
            vec![
                Object::Integer(55),
                Object::Integer(77),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpSetLocal, &[0]),
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpSetLocal, &[1]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpGetLocal, &[1]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 2,
                    num_parameters: 0,
                }),
            ],
        ];
        let expected_instrs = [
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpClosure, &[1, 0]),
                make(Opcode::OpPop, &[]),
            ],
            vec![make(Opcode::OpClosure, &[1, 0]), make(Opcode::OpPop, &[])],
            vec![make(Opcode::OpClosure, &[2, 0]), make(Opcode::OpPop, &[])],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_builtin_functions() {
        let inputs = ["len([]); push([], 1);", "fn() { len([]) };"];
        let expected_constants = [
            vec![Object::Integer(1)],
            vec![Object::CompiledFn(CompiledFn {
                instructions: concat_instructions(vec![
                    make(Opcode::OpGetBuiltin, &[0]),
                    make(Opcode::OpArray, &[0]),
                    make(Opcode::OpCall, &[1]),
                    make(Opcode::OpReturnValue, &[]),
                ]),
                num_locals: 0,
                num_parameters: 0,
            })],
        ];
        let expected_instrs = [
            vec![
                make(Opcode::OpGetBuiltin, &[0]),
                make(Opcode::OpArray, &[0]),
                make(Opcode::OpCall, &[1]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpGetBuiltin, &[4]),
                make(Opcode::OpArray, &[0]),
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpCall, &[2]),
                make(Opcode::OpPop, &[]),
            ],
            vec![make(Opcode::OpClosure, &[0, 0]), make(Opcode::OpPop, &[])],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_closures() {
        let inputs = [
            "fn(a) { fn (b) { a + b } }",
            "fn(a) { fn (b) { fn (c) { a + b + c } } }",
            "let global = 55; fn() { let a = 66; fn() { let b = 77; fn() { let c = 88; global + a + b + c; } } }"
        ];
        let expected_constants = [
            vec![
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpGetFree, &[0]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 1,
                }),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpClosure, &[0, 1]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 1,
                }),
            ],
            vec![
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpGetFree, &[0]),
                        make(Opcode::OpGetFree, &[1]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 1,
                }),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpGetFree, &[0]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpClosure, &[0, 2]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 1,
                }),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpClosure, &[1, 1]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 1,
                }),
            ],
            vec![
                Object::Integer(55),
                Object::Integer(66),
                Object::Integer(77),
                Object::Integer(88),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[3]),
                        make(Opcode::OpSetLocal, &[0]),
                        make(Opcode::OpGetGlobal, &[0]),
                        make(Opcode::OpGetFree, &[0]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpGetFree, &[1]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 0,
                }),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[2]),
                        make(Opcode::OpSetLocal, &[0]),
                        make(Opcode::OpGetFree, &[0]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpClosure, &[4, 2]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 0,
                }),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpSetLocal, &[0]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpClosure, &[5, 1]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 0,
                }),
            ],
        ];
        let expected_instrs = [
            vec![make(Opcode::OpClosure, &[1, 0]), make(Opcode::OpPop, &[])],
            vec![make(Opcode::OpClosure, &[2, 0]), make(Opcode::OpPop, &[])],
            vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpClosure, &[6, 0]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }

    #[test]
    fn test_recursive_functions() {
        let inputs = [
            "let countDown = fn(x) { countDown(x - 1); }; countDown(1);",
            "let wrapper = fn() { let countDown = fn(x) { countDown(x - 1); }; countDown(1); }; wrapper();",
        ];
        let expected_constants = [
            vec![
                Object::Integer(1),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpCurrentClosure, &[]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpSub, &[]),
                        make(Opcode::OpCall, &[1]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 1,
                }),
                Object::Integer(1),
            ],
            vec![
                Object::Integer(1),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpCurrentClosure, &[]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpSub, &[]),
                        make(Opcode::OpCall, &[1]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 1,
                }),
                Object::Integer(1),
                Object::CompiledFn(CompiledFn {
                    instructions: concat_instructions(vec![
                        make(Opcode::OpClosure, &[1, 0]),
                        make(Opcode::OpSetLocal, &[0]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpConstant, &[2]),
                        make(Opcode::OpCall, &[1]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 0,
                }),
            ],
        ];
        let expected_instrs = [
            vec![
                make(Opcode::OpClosure, &[1, 0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpCall, &[1]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpClosure, &[3, 0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpCall, &[0]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(input, &expected_constants[i], expected_instrs[i].clone());
        }
    }
}
