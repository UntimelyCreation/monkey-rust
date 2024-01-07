mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
pub mod repl;
mod token;

#[warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

    use crate::{
        ast::{
            ArrayLiteralExpression, BlockStatement, BooleanExpression, CallExpression, Expression,
            ExpressionStatement, FnLiteralExpression, HashLiteralExpression, IdentifierExpression,
            IfExpression, IndexExpression, InfixExpression, IntegerExpression, LetStatement, Node,
            PrefixExpression, Program, ReturnStatement, Statement, StringExpression,
        },
        evaluator::eval,
        lexer::Lexer,
        object::{Environment, HashPair, Object},
        parser::parse,
        token::Token,
    };

    #[test]
    fn test_lexer() {
        let input = "let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !*-/5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        \"foobar\"
        \"foo bar\"
        [1, 2]
        {\"foo\": \"bar\"};";

        let lexer = Lexer::new(input);
        let expected = vec![
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::LParen,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Asterisk,
            Token::Minus,
            Token::Slash,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::GreaterThan,
            Token::Integer(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Boolean(true),
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::Boolean(false),
            Token::Semicolon,
            Token::RBrace,
            Token::Integer(10),
            Token::Equal,
            Token::Integer(10),
            Token::Semicolon,
            Token::Integer(10),
            Token::NotEqual,
            Token::Integer(9),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::LBracket,
            Token::Integer(1),
            Token::Comma,
            Token::Integer(2),
            Token::RBracket,
            Token::LBrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::RBrace,
            Token::Semicolon,
            Token::Eof,
        ];
        assert_eq!(lexer.tokenize(), expected);
    }

    #[test]
    fn test_parse_let_statements() {
        let input = "let x = 5;
let y = true;
let foobar = y;";

        let expected = vec![
            Statement::Let(LetStatement {
                identifier: IdentifierExpression {
                    name: String::from("x"),
                },
                value: Expression::Integer(IntegerExpression { value: 5 }),
            }),
            Statement::Let(LetStatement {
                identifier: IdentifierExpression {
                    name: String::from("y"),
                },
                value: Expression::Boolean(BooleanExpression { value: true }),
            }),
            Statement::Let(LetStatement {
                identifier: IdentifierExpression {
                    name: String::from("foobar"),
                },
                value: Expression::Identifier(IdentifierExpression {
                    name: "y".to_string(),
                }),
            }),
        ];
        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_return_statements() {
        let input = "return 5;
return true;
return x;";

        let expected = vec![
            Statement::Return(ReturnStatement {
                value: Expression::Integer(IntegerExpression { value: 5 }),
            }),
            Statement::Return(ReturnStatement {
                value: Expression::Boolean(BooleanExpression { value: true }),
            }),
            Statement::Return(ReturnStatement {
                value: Expression::Identifier(IdentifierExpression {
                    name: "x".to_string(),
                }),
            }),
        ];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_expression_statements() {
        let input = "foobar;";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::Identifier(IdentifierExpression {
                name: String::from("foobar"),
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_integer_expression() {
        let input = "5;";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::Integer(IntegerExpression { value: 5 }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_string_expression() {
        let input = "\"hello world\";";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::String(StringExpression {
                value: "hello world".to_string(),
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_prefix_bang_expression() {
        let input = "!5;";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::Prefix(PrefixExpression {
                prefix: Token::Bang,
                operand: Box::new(Expression::Integer(IntegerExpression { value: 5 })),
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_prefix_minus_expression() {
        let input = "-12;";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::Prefix(PrefixExpression {
                prefix: Token::Minus,
                operand: Box::new(Expression::Integer(IntegerExpression { value: 12 })),
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_infix_expressions() {
        let inputs = ["5 + 6;", "5 - 6;", "5 == 6;", "5 != 6;"];
        let expected_operators = [Token::Plus, Token::Minus, Token::Equal, Token::NotEqual];

        for (i, input) in inputs.iter().enumerate() {
            let expected = vec![Statement::Expression(ExpressionStatement {
                expr: Expression::Infix(InfixExpression {
                    operator: expected_operators[i].clone(),
                    lhs: Box::new(Expression::Integer(IntegerExpression { value: 5 })),
                    rhs: Box::new(Expression::Integer(IntegerExpression { value: 6 })),
                }),
            })];

            let parsed_input = parse(input).expect("error occurred while parsing program");
            assert_eq!(parsed_input, Node::Program(Program(expected)));
            //assert_eq!(parsed_input.to_string(), input.to_string());
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = [
            "-a * b;",
            "!-a;",
            "a + b + c;",
            "true;",
            "false;",
            "3 > 5 == false;",
            "1 + (2 + 3) + 4;",
            "(5 + 5) * 2;",
            "2 / (5 + 5);",
            "-(5 + 5);",
            "!(true == true);",
            "a + add(b * c) + d;",
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
            "add(a + b + c * d / f + g);",
            "a * [1, 2, 3, 4][b * c] * d",
            "add(a * b[2], b[1], 2 * [1, 2][1])",
        ];
        let expected_strings = [
            "((-a) * b);",
            "(!(-a));",
            "((a + b) + c);",
            "true;",
            "false;",
            "((3 > 5) == false);",
            "((1 + (2 + 3)) + 4);",
            "((5 + 5) * 2);",
            "(2 / (5 + 5));",
            "(-(5 + 5));",
            "(!(true == true));",
            "((a + add((b * c))) + d);",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            "add((((a + b) + ((c * d) / f)) + g));",
            "((a * ([1, 2, 3, 4][(b * c)])) * d);",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])));",
        ];

        for (i, input) in inputs.iter().enumerate() {
            let parsed_input = parse(input).expect("error occurred while parsing program");
            assert_eq!(parsed_input.to_string(), expected_strings[i].to_string());
        }
    }

    #[test]
    fn test_parse_boolean_expression() {
        let inputs = ["true;", "false;"];
        let expected_values = [
            BooleanExpression { value: true },
            BooleanExpression { value: false },
        ];

        for (i, input) in inputs.iter().enumerate() {
            let expected = vec![Statement::Expression(ExpressionStatement {
                expr: Expression::Boolean(expected_values[i].clone()),
            })];

            let parsed_input = parse(input).expect("error occurred while parsing program");
            assert_eq!(parsed_input, Node::Program(Program(expected)));
            //assert_eq!(parsed_input.to_string(), input.to_string());
        }
    }

    #[test]
    fn test_parse_if_expression_no_alternative() {
        let input = "if (x > y) { x };";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::If(IfExpression {
                condition: Box::new(Expression::Infix(InfixExpression {
                    operator: Token::GreaterThan,
                    lhs: Box::new(Expression::Identifier(IdentifierExpression {
                        name: "x".to_string(),
                    })),
                    rhs: Box::new(Expression::Identifier(IdentifierExpression {
                        name: "y".to_string(),
                    })),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        expr: Expression::Identifier(IdentifierExpression {
                            name: "x".to_string(),
                        }),
                    })],
                },
                alternative: None,
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_if_expression_with_alternative() {
        let input = "if (x > y) { x } else { y };";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::If(IfExpression {
                condition: Box::new(Expression::Infix(InfixExpression {
                    operator: Token::GreaterThan,
                    lhs: Box::new(Expression::Identifier(IdentifierExpression {
                        name: "x".to_string(),
                    })),
                    rhs: Box::new(Expression::Identifier(IdentifierExpression {
                        name: "y".to_string(),
                    })),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        expr: Expression::Identifier(IdentifierExpression {
                            name: "x".to_string(),
                        }),
                    })],
                },
                alternative: Some(BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        expr: Expression::Identifier(IdentifierExpression {
                            name: "y".to_string(),
                        }),
                    })],
                }),
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::FnLiteral(FnLiteralExpression {
                parameters: vec![
                    IdentifierExpression {
                        name: "x".to_string(),
                    },
                    IdentifierExpression {
                        name: "y".to_string(),
                    },
                ],
                body: BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        expr: Expression::Infix(InfixExpression {
                            operator: Token::Plus,
                            lhs: Box::new(Expression::Identifier(IdentifierExpression {
                                name: "x".to_string(),
                            })),
                            rhs: Box::new(Expression::Identifier(IdentifierExpression {
                                name: "y".to_string(),
                            })),
                        }),
                    })],
                },
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_function_parameters() {
        let inputs = ["fn() {}", "fn(x) {};", "fn (x, y) {}"];
        let expected_parameters = [
            vec![],
            vec![IdentifierExpression {
                name: "x".to_string(),
            }],
            vec![
                IdentifierExpression {
                    name: "x".to_string(),
                },
                IdentifierExpression {
                    name: "y".to_string(),
                },
            ],
        ];

        for (i, input) in inputs.iter().enumerate() {
            let expected = vec![Statement::Expression(ExpressionStatement {
                expr: Expression::FnLiteral(FnLiteralExpression {
                    parameters: expected_parameters[i].clone(),
                    body: BlockStatement { statements: vec![] },
                }),
            })];

            let parsed_input = parse(input).expect("error occurred while parsing program");
            assert_eq!(parsed_input, Node::Program(Program(expected)));
            //assert_eq!(parsed_input.to_string(), input.to_string());
        }
    }

    #[test]
    fn test_parse_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5)";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::Call(CallExpression {
                function: Box::new(Expression::Identifier(IdentifierExpression {
                    name: "add".to_string(),
                })),
                arguments: vec![
                    Expression::Integer(IntegerExpression { value: 1 }),
                    Expression::Infix(InfixExpression {
                        operator: Token::Asterisk,
                        lhs: Box::new(Expression::Integer(IntegerExpression { value: 2 })),
                        rhs: Box::new(Expression::Integer(IntegerExpression { value: 3 })),
                    }),
                    Expression::Infix(InfixExpression {
                        operator: Token::Plus,
                        lhs: Box::new(Expression::Integer(IntegerExpression { value: 4 })),
                        rhs: Box::new(Expression::Integer(IntegerExpression { value: 5 })),
                    }),
                ],
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::ArrayLiteral(ArrayLiteralExpression {
                elements: vec![
                    Expression::Integer(IntegerExpression { value: 1 }),
                    Expression::Infix(InfixExpression {
                        operator: Token::Asterisk,
                        lhs: Box::new(Expression::Integer(IntegerExpression { value: 2 })),
                        rhs: Box::new(Expression::Integer(IntegerExpression { value: 2 })),
                    }),
                    Expression::Infix(InfixExpression {
                        operator: Token::Plus,
                        lhs: Box::new(Expression::Integer(IntegerExpression { value: 3 })),
                        rhs: Box::new(Expression::Integer(IntegerExpression { value: 3 })),
                    }),
                ],
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_hash_literal() {
        let input = "{ \"one\": 1, true: 2, 3: 16/4 }";

        let mut expected_map = BTreeMap::new();
        expected_map.insert(
            Expression::String(StringExpression {
                value: "one".to_string(),
            }),
            Expression::Integer(IntegerExpression { value: 1 }),
        );
        expected_map.insert(
            Expression::Boolean(BooleanExpression { value: true }),
            Expression::Integer(IntegerExpression { value: 2 }),
        );
        expected_map.insert(
            Expression::Integer(IntegerExpression { value: 3 }),
            Expression::Infix(InfixExpression {
                operator: Token::Slash,
                lhs: Box::new(Expression::Integer(IntegerExpression { value: 16 })),
                rhs: Box::new(Expression::Integer(IntegerExpression { value: 4 })),
            }),
        );

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::HashLiteral(HashLiteralExpression {
                pairs: expected_map,
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_empty_hash_literal() {
        let input = "{ }";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::HashLiteral(HashLiteralExpression {
                pairs: BTreeMap::new(),
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
    }

    #[test]
    fn test_parse_index_expression() {
        let input = "myArray[1 + 1]";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::Index(IndexExpression {
                identifier: Box::new(Expression::Identifier(IdentifierExpression {
                    name: "myArray".to_string(),
                })),
                index: Box::new(Expression::Infix(InfixExpression {
                    operator: Token::Plus,
                    lhs: Box::new(Expression::Integer(IntegerExpression { value: 1 })),
                    rhs: Box::new(Expression::Integer(IntegerExpression { value: 1 })),
                })),
            }),
        })];

        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
        //assert_eq!(parsed_input.to_string(), input.to_string());
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = Object::Integer(expected_values[i]);

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = Object::Boolean(expected_values[i]);

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
        }
    }

    #[test]
    fn test_eval_string_expressions() {
        let inputs = ["\"Hello World!\"", "\"Hello \" + \"World!\""];
        let expected_values = ["Hello World!", "Hello World!"];

        for (i, input) in inputs.iter().enumerate() {
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = Object::String(expected_values[i].to_string());

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
        }
    }

    #[test]
    fn test_eval_prefix_expressions() {
        let inputs = ["!true", "!false"];
        let expected_values = [false, true];

        for (i, input) in inputs.iter().enumerate() {
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = Object::Boolean(expected_values[i]);

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = expected_values[i].clone();

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = Object::Integer(expected_values[i]);

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
        }
    }

    #[test]
    fn test_error_handling() {
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = Object::Error(expected_values[i].to_string());

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
        }
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = Object::Integer(expected_values[i]);

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
        }
    }

    #[test]
    fn test_function_applications() {
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = Object::Integer(expected_values[i]);

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
        }
    }

    #[test]
    fn test_builtin_functions() {
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = expected_values[i].clone();

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
        }
    }

    #[test]
    fn test_eval_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

        let env = Rc::new(RefCell::new(Environment::new()));

        let expected = Object::Array(vec![
            Object::Integer(1),
            Object::Integer(4),
            Object::Integer(6),
        ]);

        let program = parse(input).expect("error occurred while parsing program");
        println!("{program}");
        let eval_input = eval(program, env);

        assert_eq!(eval_input, expected);
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

        let env = Rc::new(RefCell::new(Environment::new()));

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

        let program = parse(input).expect("error occurred while parsing program");
        println!("{program}");
        let eval_input = eval(program, env);

        assert_eq!(eval_input, expected);
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = expected_values[i].clone();

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
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
            let env = Rc::new(RefCell::new(Environment::new()));

            let expected = expected_values[i].clone();

            let program = parse(input).expect("error occurred while parsing program");
            println!("{program}");
            let eval_input = eval(program, env);

            assert_eq!(eval_input, expected);
        }
    }
}
