#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

    use crate::{
        evaluator::environment::Environment,
        evaluator::eval,
        object::{HashPair, Object},
        parser::parse,
    };

    fn test_evaluating(input: &str, expected: Object) {
        let env = Rc::new(RefCell::new(Environment::new()));

        let program = parse(input).expect("error occurred while parsing program");
        let eval_input = eval(program, env);

        assert_eq!(eval_input, expected);
    }

    #[test]
    fn test_let_statements() {
        let inputs = [
            "let a = 5; a;",
            "let a = 5 * 5; a;",
            "let a = 5; let b = a; b;",
            "let a = 5; let b = a; let c = a + b + 5; c;",
        ];
        let expected_values = [5, 25, 5, 15];

        for (i, input) in inputs.iter().enumerate() {
            let expected = Object::Integer(expected_values[i]);
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_return_expressions() {
        let inputs = [
            "return 10;",
            "return 10; 9;",
            "return 2 * 5; 9;",
            "9; return 2 * 5; 9;",
            "if (10 > 1) { if (10 > 1) { return 10; }} return 1;",
        ];
        let expected_values = [10, 10, 10, 10, 10];

        for (i, input) in inputs.iter().enumerate() {
            let expected = Object::Integer(expected_values[i]);
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_integer_expressions() {
        let inputs = [
            "5",
            "10",
            "-5",
            "-10",
            "5 + 5 + 5 + 5 - 10",
            "2 * 2 * 2 * 2 * 2",
            "5 * 2 + 10",
            "5 + 2 * 10",
            "50 / 2 * 2 + 10",
            "(5 + 10 * 2 + 15 / 3) * 2 + -10",
        ];
        let expected_values = [5, 10, -5, -10, 10, 32, 20, 25, 60, 50];

        for (i, input) in inputs.iter().enumerate() {
            let expected = Object::Integer(expected_values[i]);
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_bool_expressions() {
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
            "true == false",
            "false == false",
            "true != false",
            "false != true",
        ];
        let expected_values = [
            true, false, true, false, false, false, true, false, false, true, true, false, true,
            true, true,
        ];

        for (i, input) in inputs.iter().enumerate() {
            let expected = Object::Boolean(expected_values[i]);
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_string_expressions() {
        let inputs = ["\"Hello World!\"", "\"Hello \" + \"World!\""];
        let expected_values = ["Hello World!", "Hello World!"];

        for (i, input) in inputs.iter().enumerate() {
            let expected = Object::String(expected_values[i].to_string());
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_prefix_expressions() {
        let inputs = ["!true", "!false"];
        let expected_values = [false, true];

        for (i, input) in inputs.iter().enumerate() {
            let expected = Object::Boolean(expected_values[i]);
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_if_else_expressions() {
        let inputs = [
            "if (true) { 10 }",
            "if (false) { 10 }",
            "if (1) { 10 }",
            "if (1 < 2) { 10 }",
            "if (1 > 2) { 10 } else { 20 }",
        ];
        let expected_values = [
            Object::Integer(10),
            Object::Null,
            Object::Integer(10),
            Object::Integer(10),
            Object::Integer(20),
        ];

        for (i, input) in inputs.iter().enumerate() {
            let expected = expected_values[i].clone();
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_function_applications() {
        let inputs = [
            "let identity = fn(x) { x; }; identity(5);",
            "let identity = fn(x) { return x; }; identity(5);",
            "let double = fn(x) { x * 2; }; double(5);",
            "let add = fn(x, y) { x + y; }; add(5, 5);",
            "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            "fn(x) { x; }(5)",
        ];
        let expected_values = [5, 5, 10, 10, 20, 5];

        for (i, input) in inputs.iter().enumerate() {
            let expected = Object::Integer(expected_values[i]);
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_builtin_functions() {
        let inputs = [
            "len(\"\")",
            "len(\"four\")",
            "len(\"hello world\")",
            "len(1)",
            "len(\"one\", \"two\")",
            "len([1, 2, 3, 4])",
            "len([1, 5 * 6])",
            "first([1, 2, 3, 4])",
            "first(2)",
            "last([1, 2, 3, 4])",
            "last(fn(x) {x;})",
            "rest([1, 2, 3, 4])",
            "rest(\"hi\")",
            "push([1, 2, 3, 4], 5)",
            "push(\"no\", 5)",
            "push([1])",
        ];
        let expected_values = [
            Object::Integer(0),
            Object::Integer(4),
            Object::Integer(11),
            Object::Error("argument to 'len' not supported, found INTEGER".to_string()),
            Object::Error("wrong number of arguments: expected 1, found 2".to_string()),
            Object::Integer(4),
            Object::Integer(2),
            Object::Integer(1),
            Object::Error("argument to 'first' must be ARRAY, found INTEGER".to_string()),
            Object::Integer(4),
            Object::Error("argument to 'last' must be ARRAY, found FUNCTION".to_string()),
            Object::Array(vec![
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
            ]),
            Object::Error("argument to 'rest' must be ARRAY, found STRING".to_string()),
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
                Object::Integer(5),
            ]),
            Object::Error("argument to 'push' must be ARRAY, found STRING".to_string()),
            Object::Error("wrong number of arguments: expected 2, found 1".to_string()),
        ];

        for (i, input) in inputs.iter().enumerate() {
            let expected = expected_values[i].clone();
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

        let expected = Object::Array(vec![
            Object::Integer(1),
            Object::Integer(4),
            Object::Integer(6),
        ]);
        test_evaluating(input, expected);
    }

    #[test]
    fn test_eval_hash_literal() {
        let input = "let two = \"two\";
            { 
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }";

        let mut expected_pairs = BTreeMap::new();
        let key_one = Object::String("one".to_string());
        expected_pairs.insert(
            key_one
                .get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key_one,
                value: Object::Integer(1),
            },
        );
        let key_two = Object::String("two".to_string());
        expected_pairs.insert(
            key_two
                .get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key_two,
                value: Object::Integer(2),
            },
        );
        let key_three = Object::String("three".to_string());
        expected_pairs.insert(
            key_three
                .get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key_three,
                value: Object::Integer(3),
            },
        );
        let key_four = Object::Integer(4);
        expected_pairs.insert(
            key_four
                .get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key_four.clone(),
                value: key_four,
            },
        );
        let key_five = Object::Boolean(true);
        expected_pairs.insert(
            key_five
                .get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key_five,
                value: Object::Integer(5),
            },
        );
        let key_six = Object::Boolean(false);
        expected_pairs.insert(
            key_six
                .get_hash_key()
                .expect("error occurred while getting hash key"),
            HashPair {
                key: key_six,
                value: Object::Integer(6),
            },
        );

        let expected = Object::Hash(expected_pairs);
        test_evaluating(input, expected);
    }

    #[test]
    fn test_eval_array_index_expressions() {
        let inputs = [
            "[1, 2, 3][0]",
            "[1, 2, 3][1]",
            "[1, 2, 3][2]",
            "let i = 0; [1][i];",
            "[1, 2, 3][1 + 1];",
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            "[1, 2, 3][3]",
            "[1, 2, 3][-1]",
        ];
        let expected_values = [
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
            Object::Integer(1),
            Object::Integer(3),
            Object::Integer(6),
            Object::Integer(2),
            Object::Null,
            Object::Null,
        ];

        for (i, input) in inputs.iter().enumerate() {
            let expected = expected_values[i].clone();
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_hash_index_expressions() {
        let inputs = [
            "{\"foo\": 5}[\"foo\"]",
            "{\"foo\": 5}[\"bar\"]",
            "let key = \"foo\"; {\"foo\": 5}[key]",
            "{}[\"foo\"]",
            "{5: 5}[5]",
            "{true: 5}[true]",
            "{false: 5}[false]",
        ];
        let expected_values = [
            Object::Integer(5),
            Object::Null,
            Object::Integer(5),
            Object::Null,
            Object::Integer(5),
            Object::Integer(5),
            Object::Integer(5),
        ];

        for (i, input) in inputs.iter().enumerate() {
            let expected = expected_values[i].clone();
            test_evaluating(input, expected);
        }
    }

    #[test]
    fn test_eval_error_handling() {
        let inputs = [
            "5 + true;",
            "5 + true; 5;",
            "-true",
            "true * false",
            "5; true / false; 5",
            "if (10 > 1) { true + false; }",
            "foobar",
            "{\"name\": \"Monkey\"}[fn(x) { x }]",
        ];
        let expected_values = [
            "unknown operator: INTEGER + BOOLEAN",
            "unknown operator: INTEGER + BOOLEAN",
            "unknown operator: -BOOLEAN",
            "unknown operator: BOOLEAN * BOOLEAN",
            "unknown operator: BOOLEAN / BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
            "identifier not found: foobar",
            "unusable as hash key: FUNCTION",
        ];

        for (i, input) in inputs.iter().enumerate() {
            let expected = Object::Error(expected_values[i].to_string());
            test_evaluating(input, expected);
        }
    }
}
