#[cfg(test)]
mod tests {
    use crate::{compiler::Compiler, evaluator::object::Object, parser::parse, vm::Vm};

    fn test_running(input: &str, expected: Object) {
        let program = parse(input).expect("error occurred while parsing program");

        let mut compiler = Compiler::new();
        let bytecode = compiler
            .compile(&program)
            .expect("error occurred while compiling program");

        let mut vm = Vm::from_bytecode(bytecode);
        vm.run().expect("error occurred while running vm");

        let stack_el = vm.last_popped();
        assert_eq!(expected, stack_el);
    }

    #[test]
    fn test_integer_arithmetic() {
        let inputs = [
            "1",
            "2",
            "1 + 2",
            "1 - 2",
            "1 * 2",
            "4 / 2",
            "50 / 2 * 2 + 10 - 5",
            "5 + 5 + 5 + 5 - 10",
            "2 * 2 * 2 * 2 * 2",
            "5 * 2 + 10",
            "5 + 2 * 10",
            "5 * (2 + 10)",
            "-5",
            "-10",
            "-50 + 100 + -50",
            "(5 + 10 * 2 + 15 / 3) * 2 - 10",
        ];
        let expected_objs = vec![
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
            Object::Integer(-1),
            Object::Integer(2),
            Object::Integer(2),
            Object::Integer(55),
            Object::Integer(10),
            Object::Integer(32),
            Object::Integer(20),
            Object::Integer(25),
            Object::Integer(60),
            Object::Integer(-5),
            Object::Integer(-10),
            Object::Integer(0),
            Object::Integer(50),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let inputs = [
            "true",
            "false",
            "1 < 2",
            "1 > 2",
            "1 < 1",
            "1 > 1",
            "1 == 1",
            "1 != 1",
            "1 == 2",
            "1 != 2",
            "true == true",
            "false == false",
            "true == false",
            "true != false",
            "false != true",
            "(1 < 2) == true",
            "(1 < 2) == false",
            "(1 > 2) == true",
            "(1 > 2) == false",
            "!true",
            "!false",
            "!5",
            "!!true",
            "!!false",
            "!!5",
        ];
        let expected_objs = vec![
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(false),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(true),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(true),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(true),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }
}
