use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{AstNode, Expression, IdentifierExpression, IfExpression, Statement};
use crate::object::{
    Array, Boolean, Builtin, Environment, Error, Function, Integer, Object, ReturnValue, StringObj,
};

pub fn eval(node: AstNode, env: Rc<RefCell<Environment>>) -> Option<Object> {
    match node {
        AstNode::Program(program) => eval_program(program.0, env),
        AstNode::Statement(stmt) => match stmt {
            Statement::Expression(expr_stmt) => eval(AstNode::ExpressionStatement(expr_stmt), env),
            Statement::Block(block_stmt) => eval(AstNode::BlockStatement(block_stmt), env),
            Statement::Return(return_stmt) => eval(AstNode::ReturnStatement(return_stmt), env),
            Statement::Let(let_stmt) => eval(AstNode::LetStatement(let_stmt), env),
        },
        AstNode::ExpressionStatement(expr_stmt) => eval(AstNode::Expression(expr_stmt.expr), env),
        AstNode::BlockStatement(block_stmt) => eval_block_statement(block_stmt.statements, env),
        AstNode::ReturnStatement(return_stmt) => {
            if let Some(val) = eval(AstNode::Expression(return_stmt.value), env) {
                match val {
                    Object::Error(_) => Some(val),
                    _ => Some(Object::ReturnValue(ReturnValue {
                        value: Box::new(val),
                    })),
                }
            } else {
                None
            }
        }
        AstNode::LetStatement(let_stmt) => {
            if let Some(val) = eval(AstNode::Expression(let_stmt.value), env.clone()) {
                match val {
                    Object::Error(_) => Some(val),
                    _ => {
                        env.borrow_mut().set(let_stmt.name.value, val);
                        None
                    }
                }
            } else {
                None
            }
        }
        AstNode::Expression(expr) => match expr {
            Expression::Integer(int_expr) => eval(AstNode::IntegerExpression(int_expr), env),
            Expression::Boolean(bool_expr) => eval(AstNode::BooleanExpression(bool_expr), env),
            Expression::String(string_expr) => eval(AstNode::StringExpression(string_expr), env),
            Expression::Prefix(prefix_expr) => eval(AstNode::PrefixExpression(prefix_expr), env),
            Expression::Infix(infix_expr) => eval(AstNode::InfixExpression(infix_expr), env),
            Expression::If(if_expr) => eval(AstNode::IfExpression(if_expr), env),
            Expression::Identifier(identifier_expr) => {
                eval(AstNode::IdentifierExpression(identifier_expr), env)
            }
            Expression::FnLiteral(fn_literal_expr) => {
                eval(AstNode::FnLiteralExpression(fn_literal_expr), env)
            }
            Expression::ArrayLiteral(array_literal_expr) => {
                eval(AstNode::ArrayLiteralExpression(array_literal_expr), env)
            }
            Expression::Call(call_expr) => eval(AstNode::CallExpression(call_expr), env),
            Expression::Index(index_expr) => eval(AstNode::IndexExpression(index_expr), env),
        },
        AstNode::IntegerExpression(expr) => Some(Object::Integer(Integer { value: expr.value })),
        AstNode::BooleanExpression(expr) => Some(get_bool_object(expr.value)),
        AstNode::StringExpression(expr) => Some(Object::String(StringObj { value: expr.value })),
        AstNode::PrefixExpression(expr) => {
            if let Some(rhs) = eval(AstNode::Expression(*expr.expr), env) {
                match rhs {
                    Object::Error(_) => Some(rhs),
                    _ => eval_prefix_expression(expr.prefix.literal, rhs),
                }
            } else {
                None
            }
        }
        AstNode::InfixExpression(expr) => {
            if let Some(lhs) = eval(AstNode::Expression(*expr.lhs), env.clone()) {
                match lhs {
                    Object::Error(_) => Some(lhs),
                    _ => {
                        if let Some(rhs) = eval(AstNode::Expression(*expr.rhs), env) {
                            match rhs {
                                Object::Error(_) => Some(lhs),
                                _ => eval_infix_expression(expr.operator.literal, lhs, rhs),
                            }
                        } else {
                            None
                        }
                    }
                }
            } else {
                None
            }
        }
        AstNode::IdentifierExpression(expr) => eval_identifier(expr, env),
        AstNode::IfExpression(expr) => eval_if_expression(expr, env),
        AstNode::FnLiteralExpression(expr) => Some(Object::Function(Function {
            parameters: expr.parameters,
            body: expr.body,
            env,
        })),
        AstNode::ArrayLiteralExpression(expr) => Some(Object::Array(Array {
            elements: eval_expressions(expr.elements, env),
        })),
        AstNode::CallExpression(expr) => {
            if let Some(function) = eval(AstNode::Expression(*expr.function), env.clone()) {
                match function {
                    Object::Error(_) => Some(function),
                    _ => {
                        let args = eval_expressions(expr.arguments, env);
                        if args.len() == 1 {
                            if let Object::Error(_) = args[0] {
                                return Some(args[0].clone());
                            }
                        }
                        apply_function(function, args)
                    }
                }
            } else {
                None
            }
        }
        AstNode::IndexExpression(expr) => {
            if let Some(identifier) = eval(AstNode::Expression(*expr.identifier), env.clone()) {
                match identifier {
                    Object::Error(_) => Some(identifier),
                    _ => {
                        if let Some(index) = eval(AstNode::Expression(*expr.index), env) {
                            match index {
                                Object::Error(_) => Some(index),
                                _ => eval_index_expression(identifier, index),
                            }
                        } else {
                            None
                        }
                    }
                }
            } else {
                None
            }
        }
    }
}

fn eval_program(stmts: Vec<Statement>, env: Rc<RefCell<Environment>>) -> Option<Object> {
    let mut result = None;

    for stmt in stmts.iter() {
        result = eval(AstNode::Statement(stmt.clone()), env.clone());

        if let Some(Object::ReturnValue(ReturnValue { value })) = result {
            return Some(*value);
        } else if let Some(Object::Error(_)) = result {
            return result;
        }
    }
    result
}

fn eval_block_statement(stmts: Vec<Statement>, env: Rc<RefCell<Environment>>) -> Option<Object> {
    let mut result = None;

    for stmt in stmts.iter() {
        result = eval(AstNode::Statement(stmt.clone()), env.clone());

        if let Some(Object::ReturnValue(_)) = result {
            return result;
        } else if let Some(Object::Error(_)) = result {
            return result;
        }
    }
    result
}

fn eval_expressions(exprs: Vec<Expression>, env: Rc<RefCell<Environment>>) -> Vec<Object> {
    let mut result = Vec::new();
    let mut evaluated;

    for expr in exprs.iter() {
        evaluated = eval(AstNode::Expression(expr.clone()), env.clone());
        match evaluated {
            Some(Object::Error(err)) => {
                return vec![Object::Error(err)];
            }
            Some(val) => {
                result.push(val);
            }
            _ => {}
        }
    }
    result
}

fn eval_prefix_expression(prefix: String, expr: Object) -> Option<Object> {
    match prefix {
        prefix if prefix == *"!" => eval_bang_operator_expression(expr),
        prefix if prefix == *"-" => eval_minus_operator_expression(expr),
        _ => Some(new_error(format!(
            "unknown operator: {}{}",
            prefix,
            expr.get_type_str()
        ))),
    }
}

fn eval_bang_operator_expression(expr: Object) -> Option<Object> {
    match expr {
        Object::Boolean(Boolean { value: true }) => Some(Object::Boolean(Boolean { value: false })),
        Object::Boolean(Boolean { value: false }) => Some(Object::Boolean(Boolean { value: true })),
        Object::Null => Some(Object::Boolean(Boolean { value: true })),
        _ => Some(Object::Boolean(Boolean { value: false })),
    }
}

fn eval_minus_operator_expression(expr: Object) -> Option<Object> {
    match expr {
        Object::Integer(Integer { value }) => Some(Object::Integer(Integer { value: -value })),
        _ => Some(new_error(format!(
            "unknown operator: -{}",
            expr.get_type_str()
        ))),
    }
}

fn eval_infix_expression(operator: String, lhs: Object, rhs: Object) -> Option<Object> {
    match (&lhs, &rhs) {
        (Object::Integer(lhs_value), Object::Integer(rhs_value)) => {
            eval_integer_infix_expression(operator, *lhs_value, *rhs_value)
        }
        (Object::Boolean(lhs_value), Object::Boolean(rhs_value)) => {
            eval_boolean_infix_expression(operator, *lhs_value, *rhs_value)
        }
        (Object::String(lhs_value), Object::String(rhs_value)) => {
            eval_string_infix_expression(operator, lhs_value.clone(), rhs_value.clone())
        }
        _ => Some(new_error(format!(
            "unknown operator: {} {} {}",
            lhs.get_type_str(),
            operator,
            rhs.get_type_str(),
        ))),
    }
}

fn eval_integer_infix_expression(operator: String, lhs: Integer, rhs: Integer) -> Option<Object> {
    let left_value = lhs.value;
    let right_value = rhs.value;

    match operator {
        operator if operator == *"+" => Some(Object::Integer(Integer {
            value: left_value + right_value,
        })),
        operator if operator == *"-" => Some(Object::Integer(Integer {
            value: left_value - right_value,
        })),
        operator if operator == *"*" => Some(Object::Integer(Integer {
            value: left_value * right_value,
        })),
        operator if operator == *"/" => Some(Object::Integer(Integer {
            value: left_value / right_value,
        })),
        operator if operator == *"<" => Some(get_bool_object(left_value < right_value)),
        operator if operator == *">" => Some(get_bool_object(left_value > right_value)),
        operator if operator == *"==" => Some(get_bool_object(left_value == right_value)),
        operator if operator == *"!=" => Some(get_bool_object(left_value != right_value)),
        _ => Some(new_error(format!(
            "unknown operator: INTEGER {} INTEGER",
            operator,
        ))),
    }
}

fn eval_boolean_infix_expression(operator: String, lhs: Boolean, rhs: Boolean) -> Option<Object> {
    let left_value = lhs.value;
    let right_value = rhs.value;

    match operator {
        operator if operator == *"==" => Some(get_bool_object(left_value == right_value)),
        operator if operator == *"!=" => Some(get_bool_object(left_value != right_value)),
        _ => Some(new_error(format!(
            "unknown operator: BOOLEAN {} BOOLEAN",
            operator,
        ))),
    }
}

fn eval_string_infix_expression(
    operator: String,
    lhs: StringObj,
    rhs: StringObj,
) -> Option<Object> {
    let left_value = lhs.value;
    let right_value = rhs.value;

    match operator {
        operator if operator == *"+" => Some(Object::String(StringObj {
            value: [left_value, right_value].join(""),
        })),
        _ => Some(new_error(format!(
            "unknown operator: STRING {} STRING",
            operator,
        ))),
    }
}

fn eval_if_expression(expr: IfExpression, env: Rc<RefCell<Environment>>) -> Option<Object> {
    let condition = eval(AstNode::Expression(*expr.condition), env.clone());

    match condition {
        Some(Object::Error(_)) => condition,
        Some(val) => {
            if is_truthy(val) {
                eval(AstNode::BlockStatement(expr.consequence), env)
            } else if let Some(alternative) = expr.alternative {
                eval(AstNode::BlockStatement(alternative), env)
            } else {
                Some(Object::Null)
            }
        }
        None => None,
    }
}

fn eval_identifier(
    identifier: IdentifierExpression,
    env: Rc<RefCell<Environment>>,
) -> Option<Object> {
    let value = identifier.value;
    match env.borrow().get(&value) {
        Some(val) => Some(val),
        None => match get_builtin_fn(value.clone()) {
            Some(builtin) => Some(builtin),
            None => Some(new_error(format!("identifier not found: {}", value))),
        },
    }
}

fn apply_function(function: Object, args: Vec<Object>) -> Option<Object> {
    match function {
        Object::Function(function) => {
            let extended_env = Rc::new(RefCell::new(extend_function_env(&function, args)));
            let evaluated = eval(AstNode::BlockStatement(function.body), extended_env);
            if let Some(Object::ReturnValue(ReturnValue { value })) = evaluated {
                return Some(*value);
            }
            evaluated
        }
        Object::Builtin(builtin) => Some((builtin.function)(args)),
        _ => Some(new_error(format!(
            "not a function: {}",
            function.get_type_str(),
        ))),
    }
}

fn extend_function_env(function: &Function, args: Vec<Object>) -> Environment {
    let mut env = function.env.borrow().clone().new_enclosed();

    for (i, param) in function.parameters.iter().enumerate() {
        env.set(param.value.clone(), args[i].clone());
    }
    env
}

fn eval_index_expression(identifier: Object, index: Object) -> Option<Object> {
    match (&identifier, &index) {
        (Object::Array(array), Object::Integer(integer)) => {
            eval_array_index_expression(array.clone(), integer.value as usize)
        }
        _ => Some(new_error(format!(
            "index operator not supported: {}",
            identifier.get_type_str()
        ))),
    }
}

fn eval_array_index_expression(array: Array, index: usize) -> Option<Object> {
    if index > array.elements.len() - 1 {
        return Some(Object::Null);
    }

    Some(array.elements[index].clone())
}

fn new_error(message: String) -> Object {
    Object::Error(Error { message })
}

fn is_truthy(object: Object) -> bool {
    !(object == Object::Boolean(Boolean { value: false }) || object == Object::Null)
}

fn get_bool_object(expr: bool) -> Object {
    if expr {
        Object::Boolean(Boolean { value: true })
    } else {
        Object::Boolean(Boolean { value: false })
    }
}

fn get_builtin_fn(name: String) -> Option<Object> {
    match name {
        str if &str == "len" => Some(Object::Builtin(Builtin {
            function: |objs| {
                if objs.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments: expected 1, found {}",
                        objs.len()
                    ));
                }

                match &objs[0] {
                    Object::String(string) => Object::Integer(Integer {
                        value: string.value.len() as i32,
                    }),
                    Object::Array(array) => Object::Integer(Integer {
                        value: array.elements.len() as i32,
                    }),
                    _ => new_error(format!(
                        "argument to 'len' not supported, found {}",
                        objs[0].get_type_str()
                    )),
                }
            },
        })),
        str if &str == "first" => Some(Object::Builtin(Builtin {
            function: |objs| {
                if objs.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments: expected 1, found {}",
                        objs.len()
                    ));
                }

                match &objs[0] {
                    Object::Array(array) => {
                        if !array.elements.is_empty() {
                            array.elements[0].clone()
                        } else {
                            Object::Null
                        }
                    }
                    _ => new_error(format!(
                        "argument to 'first' must be ARRAY, found {}",
                        objs[0].get_type_str()
                    )),
                }
            },
        })),
        str if &str == "last" => Some(Object::Builtin(Builtin {
            function: |objs| {
                if objs.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments: expected 1, found {}",
                        objs.len()
                    ));
                }

                match &objs[0] {
                    Object::Array(array) => {
                        if !array.elements.is_empty() {
                            array.elements.last().unwrap().clone()
                        } else {
                            Object::Null
                        }
                    }
                    _ => new_error(format!(
                        "argument to 'last' must be ARRAY, found {}",
                        objs[0].get_type_str()
                    )),
                }
            },
        })),
        str if &str == "rest" => Some(Object::Builtin(Builtin {
            function: |objs| {
                if objs.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments: expected 1, found {}",
                        objs.len()
                    ));
                }

                match &objs[0] {
                    Object::Array(array) => {
                        if !array.elements.is_empty() {
                            Object::Array(Array {
                                elements: array.elements[1..].to_vec(),
                            })
                        } else {
                            Object::Null
                        }
                    }
                    _ => new_error(format!(
                        "argument to 'rest' must be ARRAY, found {}",
                        objs[0].get_type_str()
                    )),
                }
            },
        })),
        str if &str == "push" => Some(Object::Builtin(Builtin {
            function: |objs| {
                if objs.len() != 2 {
                    return new_error(format!(
                        "wrong number of arguments: expected 2, found {}",
                        objs.len()
                    ));
                }

                match &objs[0] {
                    Object::Array(array) => {
                        let mut elements = array.elements.clone();
                        elements.push(objs[1].clone());
                        Object::Array(Array { elements })
                    }
                    _ => new_error(format!(
                        "argument to 'push' must be ARRAY, found {}",
                        objs[0].get_type_str()
                    )),
                }
            },
        })),
        _ => None,
    }
}
