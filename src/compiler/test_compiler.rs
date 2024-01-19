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
        let _ = compiler.compile(&program);

        let bytecode = compiler.bytecode();
        assert_eq!(expected_instrs, bytecode.instructions);
        assert_eq!(expected_constants, bytecode.constants);
    }

    #[test]
    fn test_compiler() {
        let input = "1 + 2";
        let expected_constants = vec![Object::Integer(1), Object::Integer(2)];
        let expected_instrs = Instructions {
            stream: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
            ],
        };

        test_compiling(input, expected_constants, expected_instrs);
    }
}
