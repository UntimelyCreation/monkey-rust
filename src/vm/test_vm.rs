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

        let stack_el = vm.stack_top().expect("no element found in stack");
        assert_eq!(expected, stack_el);
    }

    #[test]
    fn test_vm() {
        let inputs = ["1", "2", "1 + 2"];
        let expected_objs = vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)];

        for (i, input) in inputs.iter().enumerate() {
            let expected = expected_objs[i].clone();
            test_running(input, expected);
        }
    }
}
