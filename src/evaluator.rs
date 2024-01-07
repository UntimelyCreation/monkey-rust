use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast::{
    BlockStatement, Expression, HashLiteralExpression, IdentifierExpression, IfExpression, Node,
    Statement,
};
use crate::object::{Environment, HashKey, HashPair, Object};

type EvalError = String;

pub fn eval(node: Node, env: Rc<RefCell<Environment>>) -> Option<Object> {
    match node {
        Node::Program(prgm) => eval_program(&prgm.0, env),
        Node::Statement(stmt) => eval_statement(&stmt, env),
        Node::Expression(expr) => eval_expression(&expr, env),
    }
}

fn eval_program(stmts: &[Statement], env: Rc<RefCell<Environment>>) -> Option<Object> {
    let mut result = None;

    for stmt in stmts.iter() {
        result = eval_statement(stmt, env.clone());

        if let Some(Object::ReturnValue(value)) = result {
            return Some(*value);
        } else if let Some(Object::Error(_)) = result {
            return result;
        }
    }
    result
}

fn eval_block_statement(stmts: &BlockStatement, env: Rc<RefCell<Environment>>) -> Option<Object> {
    let mut result = None;

    for stmt in stmts.statements.iter() {
        result = eval_statement(stmt, env.clone());

        if let Some(Object::ReturnValue(_)) = result {
            return result;
        } else if let Some(Object::Error(_)) = result {
            return result;
        }
    }
    result
}

fn eval_statement(stmt: &Statement, env: Rc<RefCell<Environment>>) -> Option<Object> {
    match stmt {
        Statement::Let(stmt) => {
            if let Some(val) = eval_expression(&stmt.value, env.clone()) {
                match val {
                    Object::Error(_) => Some(val),
                    _ => {
                        env.borrow_mut().set(stmt.identifier.name.to_owned(), val);
                        None
                    }
                }
            } else {
                None
            }
        }
        Statement::Return(stmt) => {
            if let Some(val) = eval_expression(&stmt.value, env) {
                match val {
                    Object::Error(_) => Some(val),
                    _ => Some(Object::ReturnValue(Box::new(val))),
                }
            } else {
                None
            }
        }
        Statement::Expression(expr) => eval_expression(&expr.expr, env),
    }
}

fn eval_expression(expr: &Expression, env: Rc<RefCell<Environment>>) -> Option<Object> {
    match expr {
        Expression::Identifier(expr) => eval_identifier(expr, env),
        Expression::Integer(expr) => Some(Object::Integer(expr.value)),
        Expression::Boolean(expr) => Some(get_bool_object(expr.value)),
        Expression::String(expr) => Some(Object::String(expr.value.to_owned())),
        Expression::Prefix(expr) => {
            if let Some(rhs) = eval_expression(&expr.operand, env) {
                match rhs {
                    Object::Error(_) => Some(rhs),
                    _ => eval_prefix_expression(expr.prefix.get_literal(), &rhs),
                }
            } else {
                None
            }
        }
        Expression::Infix(expr) => {
            if let Some(lhs) = eval_expression(&expr.lhs, env.clone()) {
                match lhs {
                    Object::Error(_) => Some(lhs),
                    _ => {
                        if let Some(rhs) = eval_expression(&expr.rhs, env.clone()) {
                            match rhs {
                                Object::Error(_) => Some(lhs),
                                _ => eval_infix_expression(expr.operator.get_literal(), &lhs, &rhs),
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
        Expression::If(expr) => eval_if_expression(expr, env),
        Expression::FnLiteral(expr) => Some(Object::Function {
            parameters: expr.parameters.to_owned(),
            body: expr.body.to_owned(),
            env,
        }),
        Expression::ArrayLiteral(expr) => {
            Some(Object::Array(eval_expressions(&expr.elements, env)))
        }
        Expression::HashLiteral(expr) => eval_hash_literal(expr, env),
        Expression::Call(expr) => {
            if let Some(function) = eval_expression(&expr.function, env.clone()) {
                match function {
                    Object::Error(_) => Some(function),
                    _ => {
                        let args = eval_expressions(&expr.arguments, env.clone());
                        if args.len() == 1 {
                            if let Object::Error(_) = args[0] {
                                return Some(args[0].to_owned());
                            }
                        }
                        apply_function(&function, &args)
                    }
                }
            } else {
                None
            }
        }
        Expression::Index(expr) => {
            if let Some(identifier) = eval_expression(&expr.identifier, env.clone()) {
                match identifier {
                    Object::Error(_) => Some(identifier),
                    _ => {
                        if let Some(index) = eval_expression(&expr.index, env) {
                            match index {
                                Object::Error(_) => Some(index),
                                _ => eval_index_expression(&identifier, &index),
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

fn eval_expressions(exprs: &[Expression], env: Rc<RefCell<Environment>>) -> Vec<Object> {
    let mut result = Vec::new();
    let mut evaluated;

    for expr in exprs.iter() {
        evaluated = eval_expression(expr, env.clone());
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

fn eval_prefix_expression(prefix: String, expr: &Object) -> Option<Object> {
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

fn eval_bang_operator_expression(expr: &Object) -> Option<Object> {
    match expr {
        Object::Boolean(true) => Some(Object::Boolean(false)),
        Object::Boolean(false) => Some(Object::Boolean(true)),
        Object::Null => Some(Object::Boolean(true)),
        _ => Some(Object::Boolean(false)),
    }
}

fn eval_minus_operator_expression(expr: &Object) -> Option<Object> {
    match expr {
        Object::Integer(value) => Some(Object::Integer(-value)),
        _ => Some(new_error(format!(
            "unknown operator: -{}",
            expr.get_type_str()
        ))),
    }
}

fn eval_infix_expression(operator: String, lhs: &Object, rhs: &Object) -> Option<Object> {
    match (&lhs, &rhs) {
        (Object::Integer(lhs_value), Object::Integer(rhs_value)) => {
            eval_integer_infix_expression(operator, lhs_value, rhs_value)
        }
        (Object::Boolean(lhs_value), Object::Boolean(rhs_value)) => {
            eval_boolean_infix_expression(operator, lhs_value, rhs_value)
        }
        (Object::String(lhs_value), Object::String(rhs_value)) => {
            eval_string_infix_expression(operator, lhs_value, rhs_value)
        }
        _ => Some(new_error(format!(
            "unknown operator: {} {} {}",
            lhs.get_type_str(),
            operator,
            rhs.get_type_str(),
        ))),
    }
}

fn eval_integer_infix_expression(operator: String, lhs: &i32, rhs: &i32) -> Option<Object> {
    match operator {
        operator if operator == *"+" => Some(Object::Integer(lhs + rhs)),
        operator if operator == *"-" => Some(Object::Integer(lhs - rhs)),
        operator if operator == *"*" => Some(Object::Integer(lhs * rhs)),
        operator if operator == *"/" => Some(Object::Integer(lhs / rhs)),
        operator if operator == *"<" => Some(get_bool_object(lhs < rhs)),
        operator if operator == *">" => Some(get_bool_object(lhs > rhs)),
        operator if operator == *"==" => Some(get_bool_object(lhs == rhs)),
        operator if operator == *"!=" => Some(get_bool_object(lhs != rhs)),
        _ => Some(new_error(format!(
            "unknown operator: INTEGER {} INTEGER",
            operator,
        ))),
    }
}

fn eval_boolean_infix_expression(operator: String, lhs: &bool, rhs: &bool) -> Option<Object> {
    match operator {
        operator if operator == *"==" => Some(get_bool_object(lhs == rhs)),
        operator if operator == *"!=" => Some(get_bool_object(lhs != rhs)),
        _ => Some(new_error(format!(
            "unknown operator: BOOLEAN {} BOOLEAN",
            operator,
        ))),
    }
}

fn eval_string_infix_expression(operator: String, lhs: &str, rhs: &str) -> Option<Object> {
    match operator {
        operator if operator == *"+" => Some(Object::String([lhs, rhs].join(""))),
        _ => Some(new_error(format!(
            "unknown operator: STRING {} STRING",
            operator,
        ))),
    }
}

fn eval_if_expression(expr: &IfExpression, env: Rc<RefCell<Environment>>) -> Option<Object> {
    let condition = eval_expression(&expr.condition, env.clone());

    match condition {
        Some(Object::Error(_)) => condition,
        Some(val) => {
            if is_truthy(&val) {
                eval_block_statement(&expr.consequence, env)
            } else if let Some(alternative) = &expr.alternative {
                eval_block_statement(alternative, env)
            } else {
                Some(Object::Null)
            }
        }
        None => None,
    }
}

fn eval_identifier(
    identifier: &IdentifierExpression,
    env: Rc<RefCell<Environment>>,
) -> Option<Object> {
    let value = &identifier.name;
    match env.borrow().get(value) {
        Some(val) => Some(val),
        None => match get_builtin_fn(value) {
            Some(builtin) => Some(builtin),
            None => Some(new_error(format!("identifier not found: {}", value))),
        },
    }
}

fn apply_function(function: &Object, args: &Vec<Object>) -> Option<Object> {
    match function {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let extended_env = Rc::new(RefCell::new(extend_function_env(
                parameters,
                env.clone(),
                args,
            )));
            let evaluated = eval_block_statement(body, extended_env);
            if let Some(Object::ReturnValue(value)) = evaluated {
                return Some(*value);
            }
            evaluated
        }
        Object::Builtin(builtin) => Some(builtin(args.to_owned())),
        _ => Some(new_error(format!(
            "not a function: {}",
            function.get_type_str(),
        ))),
    }
}

fn extend_function_env(
    parameters: &[IdentifierExpression],
    env: Rc<RefCell<Environment>>,
    args: &[Object],
) -> Environment {
    let mut env = env.borrow().clone().new_enclosed();

    for (i, param) in parameters.iter().enumerate() {
        env.set(param.name.to_owned(), args[i].to_owned());
    }
    env
}

fn eval_index_expression(identifier: &Object, index: &Object) -> Option<Object> {
    match (&identifier, &index) {
        (Object::Array(array), Object::Integer(integer)) => {
            eval_array_index_expression(array, *integer as usize)
        }
        (Object::Hash(hash), index) => eval_hash_index_expression(hash, index),
        _ => Some(new_error(format!(
            "index operator not supported: {}",
            identifier.get_type_str()
        ))),
    }
}

fn eval_array_index_expression(array: &[Object], index: usize) -> Option<Object> {
    if index > array.len() - 1 {
        return Some(Object::Null);
    }

    Some(array[index].to_owned())
}

fn eval_hash_index_expression(
    hash: &BTreeMap<HashKey, HashPair>,
    index: &Object,
) -> Option<Object> {
    if let Some(hash_key) = index.get_hash_key() {
        if let Some(pair) = hash.get(&hash_key) {
            Some(pair.value.clone())
        } else {
            Some(Object::Null)
        }
    } else {
        Some(new_error(format!(
            "unusable as hash key: {}",
            index.get_type_str()
        )))
    }
}

fn eval_hash_literal(
    hash_literal: &HashLiteralExpression,
    env: Rc<RefCell<Environment>>,
) -> Option<Object> {
    let mut pairs = BTreeMap::new();

    for (key_expr, value_expr) in hash_literal.pairs.iter() {
        if let Some(key) = eval_expression(key_expr, env.clone()) {
            match key {
                Object::Error(_) => {
                    return Some(key);
                }
                _ => match key.get_hash_key() {
                    Some(hash_key) => {
                        if let Some(value) = eval_expression(value_expr, env.clone()) {
                            match value {
                                Object::Error(_) => {
                                    return Some(value);
                                }
                                _ => pairs.insert(hash_key, HashPair { key, value }),
                            };
                        }
                    }
                    None => {
                        return Some(new_error(format!(
                            "unusable as hash key: {}",
                            key.get_type_str()
                        )));
                    }
                },
            }
        }
    }

    Some(Object::Hash(pairs))
}

fn new_error(message: String) -> Object {
    Object::Error(message)
}

fn is_truthy(object: &Object) -> bool {
    !matches!(object, Object::Boolean(false) | Object::Null)
}

fn get_bool_object(expr: bool) -> Object {
    if expr {
        Object::Boolean(true)
    } else {
        Object::Boolean(false)
    }
}

fn get_builtin_fn(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin(|objs| {
            if objs.len() != 1 {
                return new_error(format!(
                    "wrong number of arguments: expected 1, found {}",
                    objs.len()
                ));
            }

            match &objs[0] {
                Object::String(string) => Object::Integer(string.len() as i32),
                Object::Array(array) => Object::Integer(array.len() as i32),
                _ => new_error(format!(
                    "argument to 'len' not supported, found {}",
                    objs[0].get_type_str()
                )),
            }
        })),
        "first" => Some(Object::Builtin(|objs| {
            if objs.len() != 1 {
                return new_error(format!(
                    "wrong number of arguments: expected 1, found {}",
                    objs.len()
                ));
            }

            match &objs[0] {
                Object::Array(elements) => {
                    if !elements.is_empty() {
                        elements[0].to_owned()
                    } else {
                        Object::Null
                    }
                }
                _ => new_error(format!(
                    "argument to 'first' must be ARRAY, found {}",
                    objs[0].get_type_str()
                )),
            }
        })),
        "last" => Some(Object::Builtin(|objs| {
            if objs.len() != 1 {
                return new_error(format!(
                    "wrong number of arguments: expected 1, found {}",
                    objs.len()
                ));
            }

            match &objs[0] {
                Object::Array(elements) => elements.last().unwrap_or(&Object::Null).clone(),
                _ => new_error(format!(
                    "argument to 'last' must be ARRAY, found {}",
                    objs[0].get_type_str()
                )),
            }
        })),
        "rest" => Some(Object::Builtin(|objs| {
            if objs.len() != 1 {
                return new_error(format!(
                    "wrong number of arguments: expected 1, found {}",
                    objs.len()
                ));
            }

            match &objs[0] {
                Object::Array(elements) => {
                    if !elements.is_empty() {
                        Object::Array(elements[1..].to_owned())
                    } else {
                        Object::Null
                    }
                }
                _ => new_error(format!(
                    "argument to 'rest' must be ARRAY, found {}",
                    objs[0].get_type_str()
                )),
            }
        })),
        "push" => Some(Object::Builtin(|objs| {
            if objs.len() != 2 {
                return new_error(format!(
                    "wrong number of arguments: expected 2, found {}",
                    objs.len()
                ));
            }

            match &objs[0] {
                Object::Array(elements) => {
                    let mut elements = elements.clone();
                    elements.push(objs[1].clone());
                    Object::Array(elements)
                }
                _ => new_error(format!(
                    "argument to 'push' must be ARRAY, found {}",
                    objs[0].get_type_str()
                )),
            }
        })),
        "puts" => Some(Object::Builtin(|objs| {
            for obj in objs.iter() {
                println!("{}", obj);
            }

            Object::Null
        })),
        _ => None,
    }
}
