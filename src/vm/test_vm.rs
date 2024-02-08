#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        compiler::Compiler,
        evaluator::object::{HashPair, Object},
        parser::parse,
        vm::Vm,
    };

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
            "!(if (false) { 5; })",
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
            Object::Boolean(true),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_conditionals() {
        let inputs = [
            "if (true) { 10 }",
            "if (true) { 10 } else { 20 }",
            "if (false) { 10 } else { 20 }",
            "if (1) { 10 }",
            "if (1 < 2) { 10 }",
            "if (1 < 2) { 10 } else { 20 }",
            "if (1 > 2) { 10 } else { 20 }",
            "if (1 > 2) { 10 }",
            "if (false) { 10 }",
            "if ((if (false) { 10 })) { 10 } else { 20 }",
        ];
        let expected_objs = vec![
            Object::Integer(10),
            Object::Integer(10),
            Object::Integer(20),
            Object::Integer(10),
            Object::Integer(10),
            Object::Integer(10),
            Object::Integer(20),
            Object::Null,
            Object::Null,
            Object::Integer(20),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_global_let_statements() {
        let inputs = [
            "let one = 1; one",
            "let one = 1; let two = 2; one + two",
            "let one = 1; let two = one + one; one + two",
        ];
        let expected_objs = vec![Object::Integer(1), Object::Integer(3), Object::Integer(3)];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_string_expressions() {
        let inputs = [
            "\"monkey\"",
            "\"mon\" + \"key\"",
            "\"mon\" + \"key\" + \"banana\"",
        ];
        let expected_objs = vec![
            Object::String("monkey".to_string()),
            Object::String("monkey".to_string()),
            Object::String("monkeybanana".to_string()),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_array_literals() {
        let inputs = ["[]", "[1, 2, 3]", "[1 + 2, 3 * 4, 5 + 6]"];
        let expected_objs = vec![
            Object::Array(vec![]),
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
            ]),
            Object::Array(vec![
                Object::Integer(3),
                Object::Integer(12),
                Object::Integer(11),
            ]),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_hash_literals() {
        let inputs = ["{}", "{1: 2, 3: 4, 5: 6}", "{1: 2 + 3, 4: 5 * 6}"];

        let mut expected_map1 = BTreeMap::new();
        let key1 = Object::Integer(1);
        expected_map1.insert(
            key1.get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key1,
                value: Object::Integer(2),
            },
        );
        let key2 = Object::Integer(3);
        expected_map1.insert(
            key2.get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key2,
                value: Object::Integer(4),
            },
        );
        let key3 = Object::Integer(5);
        expected_map1.insert(
            key3.get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key3,
                value: Object::Integer(6),
            },
        );

        let mut expected_map2 = BTreeMap::new();
        let key1 = Object::Integer(1);
        expected_map2.insert(
            key1.get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key1,
                value: Object::Integer(5),
            },
        );
        let key2 = Object::Integer(4);
        expected_map2.insert(
            key2.get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key2,
                value: Object::Integer(30),
            },
        );

        let expected_objs = vec![
            Object::Hash(BTreeMap::new()),
            Object::Hash(expected_map1),
            Object::Hash(expected_map2),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_index_expressions() {
        let inputs = [
            "[1, 2, 3][1]",
            "[1, 2, 3][0 + 2]",
            "[[1, 1, 1]][0][0]",
            "[][0]",
            "[1, 2, 3][99]",
            "[1][-1]",
            "{1: 1, 2: 2}[1]",
            "{1: 1, 2: 2}[2]",
            "{1: 1}[0]",
            "{}[0]",
        ];
        let expected_objs = vec![
            Object::Integer(2),
            Object::Integer(3),
            Object::Integer(1),
            Object::Null,
            Object::Null,
            Object::Null,
            Object::Integer(1),
            Object::Integer(2),
            Object::Null,
            Object::Null,
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_functions_without_arguments() {
        let inputs = [
            "let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();",
            "let one = fn() { 1; }; let two = fn() { 2; }; one() + two()",
            "let a = fn() { 1; }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c();",
        ];
        let expected_objs = vec![Object::Integer(15), Object::Integer(3), Object::Integer(3)];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_functions_with_return_statement() {
        let inputs = [
            "let earlyExit = fn() { return 99; 100; }; earlyExit();",
            "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
        ];
        let expected_objs = vec![Object::Integer(99), Object::Integer(99)];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_functions_without_return_value() {
        let inputs = [
            "let noReturn = fn() { }; noReturn();",
            "let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();",
        ];
        let expected_objs = vec![Object::Null, Object::Null];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_first_class_functions() {
        let inputs = [
            "let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();",
            "let returnsOneReturner = fn() { let returnsOne = fn() { 1; }; returnsOne; }; returnsOneReturner()();"
        ];
        let expected_objs = vec![Object::Integer(1), Object::Integer(1)];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_function_calls_with_bindings() {
        let inputs = [
            "let one = fn() { let one = 1; one }; one();",
            "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();",
            "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; let threeAndFour = fn() { let three = 3; let four = 4; three + four; }; oneAndTwo() + threeAndFour();",
            "let firstFoobar = fn() { let foobar = 50; foobar; }; let secondFoobar = fn() { let foobar = 100; foobar; }; firstFoobar() + secondFoobar();",
            "let globalSeed = 50; let minusOne = fn() { let num = 1; globalSeed - num;}; let minusTwo = fn() { let num = 2; globalSeed - num; }; minusOne() + minusTwo();",
        ];
        let expected_objs = vec![
            Object::Integer(1),
            Object::Integer(3),
            Object::Integer(10),
            Object::Integer(150),
            Object::Integer(97),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_function_calls_with_arguments_and_bindings() {
        let inputs = [
            "let identity = fn(a) { a; }; identity(4);",
            "let sum = fn(a, b) { a + b; }; sum(1, 2);",
            "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2);",
            "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2) + sum(3, 4);",
            "let sum = fn(a, b) { let c = a + b; c; }; let outer = fn() { sum(1, 2) + sum(3, 4); }; outer();",
            "let globalNum = 10; let sum = fn(a, b) { let c = a + b; c + globalNum; }; let outer = fn() { sum(1, 2) + sum(3, 4) + globalNum; }; outer() + globalNum;",
        ];
        let expected_objs = vec![
            Object::Integer(4),
            Object::Integer(3),
            Object::Integer(3),
            Object::Integer(10),
            Object::Integer(10),
            Object::Integer(50),
        ];

        for (i, input) in inputs.iter().enumerate() {
            test_running(input, expected_objs[i].clone());
        }
    }

    #[test]
    fn test_function_calls_with_incorrect_arguments() {
        let inputs = [
            "fn() { 1; }(1);",
            "fn(a) { a; }();",
            "fn(a, b) { a + b; }(1);",
        ];
        let expected_errs = [(0, 1), (1, 0), (2, 1)];

        for (i, input) in inputs.iter().enumerate() {
            let program = parse(input).expect("error occurred while parsing program");

            let mut compiler = Compiler::new();
            let bytecode = compiler
                .compile(&program)
                .expect("error occurred while compiling program");

            let mut vm = Vm::from_bytecode(bytecode);
            assert_eq!(
                vm.run(),
                Err(format!(
                    "wrong number of arguments: expected {}, found {}",
                    expected_errs[i].0, expected_errs[i].1
                ))
            );
        }
    }
}
