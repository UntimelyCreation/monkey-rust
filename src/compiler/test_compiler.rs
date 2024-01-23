#[cfg(test)]
mod tests {
    use crate::{
        code::{make, Instructions, Opcode},
        compiler::Compiler,
        evaluator::object::Object,
        parser::parse,
    };

    fn test_compiling(input: &str, expected_constants: Vec<Object>, expected_instrs: Instructions) {
        let program = parse(input).expect("error occurred while parsing program");

        let mut compiler = Compiler::new();
        let bytecode = compiler
            .compile(&program)
            .expect("error occurred while compiling program");

        assert_eq!(expected_instrs, bytecode.instructions);
        assert_eq!(expected_constants, bytecode.constants);
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
            test_compiling(
                input,
                expected_constants[i].clone(),
                Instructions {
                    stream: expected_instrs[i].clone(),
                },
            );
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
            test_compiling(
                input,
                expected_constants[i].clone(),
                Instructions {
                    stream: expected_instrs[i].clone(),
                },
            );
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
                make(Opcode::OpJumpCond, &[4]),
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpJump, &[5]),
                make(Opcode::OpNull, &[]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
            ],
            vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpJumpCond, &[4]),
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpJump, &[5]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpPop, &[]),
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_compiling(
                input,
                expected_constants[i].clone(),
                Instructions {
                    stream: expected_instrs[i].clone(),
                },
            );
        }
    }
}
