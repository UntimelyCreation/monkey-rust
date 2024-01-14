#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        lexer::token::Token,
        parser::ast::{
            ArrayLiteralExpression, BlockStatement, BooleanExpression, CallExpression, Expression,
            ExpressionStatement, FnLiteralExpression, HashLiteralExpression, IdentifierExpression,
            IfExpression, IndexExpression, InfixExpression, IntegerExpression, LetStatement, Node,
            PrefixExpression, Program, ReturnStatement, Statement, StringExpression,
        },
        parser::parse,
    };

    fn test_parsing(input: &str, expected: Vec<Statement>) {
        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input, Node::Program(Program(expected)));
    }

    fn test_parsing_to_string(input: &str, expected: &str) {
        let parsed_input = parse(input).expect("error occurred while parsing program");
        assert_eq!(parsed_input.to_string(), expected.to_string());
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
        test_parsing(input, expected);
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
        test_parsing(input, expected);
    }

    #[test]
    fn test_parse_expression_statements() {
        let input = "foobar; 5; true;";

        let expected = vec![
            Statement::Expression(ExpressionStatement {
                expr: Expression::Identifier(IdentifierExpression {
                    name: String::from("foobar"),
                }),
            }),
            Statement::Expression(ExpressionStatement {
                expr: Expression::Integer(IntegerExpression { value: 5 }),
            }),
            Statement::Expression(ExpressionStatement {
                expr: Expression::Boolean(BooleanExpression { value: true }),
            }),
        ];
        test_parsing(input, expected);
    }

    #[test]
    fn test_parse_integer_expression() {
        let input = "5; 69420;";

        let expected = vec![
            Statement::Expression(ExpressionStatement {
                expr: Expression::Integer(IntegerExpression { value: 5 }),
            }),
            Statement::Expression(ExpressionStatement {
                expr: Expression::Integer(IntegerExpression { value: 69420 }),
            }),
        ];
        test_parsing(input, expected);
    }

    #[test]
    fn test_parse_string_expression() {
        let input = "\"hello world\";";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::String(StringExpression {
                value: "hello world".to_string(),
            }),
        })];
        test_parsing(input, expected);
    }

    #[test]
    fn test_parse_prefix_expressions() {
        let input = "!true;";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::Prefix(PrefixExpression {
                prefix: Token::Bang,
                operand: Box::new(Expression::Boolean(BooleanExpression { value: true })),
            }),
        })];
        test_parsing(input, expected);
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
        test_parsing(input, expected);
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
            test_parsing(input, expected);
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
            test_parsing_to_string(input, expected_strings[i]);
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
            test_parsing(input, expected);
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
        test_parsing(input, expected);
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
        test_parsing(input, expected);
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
        test_parsing(input, expected);
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
            test_parsing(input, expected);
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
        test_parsing(input, expected);
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
        test_parsing(input, expected);
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
        test_parsing(input, expected);
    }

    #[test]
    fn test_parse_hash_literal_empty() {
        let input = "{ }";

        let expected = vec![Statement::Expression(ExpressionStatement {
            expr: Expression::HashLiteral(HashLiteralExpression {
                pairs: BTreeMap::new(),
            }),
        })];
        test_parsing(input, expected);
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
        test_parsing(input, expected);
    }
}
