use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::object::builtins::get_builtin_fn;
use crate::object::{get_bool_object, is_truthy, HashKey, HashPair, Object};
use crate::parser::ast::{
    BlockStatement, Expression, HashLiteralExpression, IdentifierExpression, IfExpression, Node,
    Statement,
};
use environment::Environment;

pub mod environment;
mod test_evaluator;

type EvalError = String;

pub fn eval(node: Node, env: Rc<RefCell<Environment>>) -> Rc<Object> {
    match node {
        Node::Program(prgm) => match eval_program(&prgm.0, env) {
            Ok(evaluated) => evaluated,
            Err(err) => Rc::new(Object::Error(err)),
        },
        Node::Statement(stmt) => match eval_statement(&stmt, env) {
            Ok(evaluated) => evaluated,
            Err(err) => Rc::new(Object::Error(err)),
        },
        Node::Expression(expr) => match eval_expression(&expr, env) {
            Ok(evaluated) => evaluated,
            Err(err) => Rc::new(Object::Error(err)),
        },
    }
}

fn eval_program(
    stmts: &[Statement],
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let mut result = Rc::new(Object::Null);

    for stmt in stmts.iter() {
        result = eval_statement(stmt, env.clone())?;

        if let Object::ReturnValue(value) = result.as_ref() {
            return Ok(value.clone());
        }
    }
    Ok(result)
}

fn eval_block_statement(
    stmts: &BlockStatement,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let mut result = Rc::new(Object::Null);

    for stmt in stmts.statements.iter() {
        result = eval_statement(stmt, env.clone())?;

        if let Object::ReturnValue(_) = result.as_ref() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_statement(
    stmt: &Statement,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    match stmt {
        Statement::Let(stmt) => {
            let val = eval_expression(&stmt.value, env.clone())?;
            env.borrow_mut().set(stmt.identifier.name.to_string(), val);
            Ok(Rc::new(Object::Null))
        }
        Statement::Return(stmt) => {
            let value = eval_expression(&stmt.value, env)?;
            Ok(Rc::new(Object::ReturnValue(value)))
        }
        Statement::Expression(expr) => eval_expression(&expr.expr, env),
    }
}

fn eval_expression(
    expr: &Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    match expr {
        Expression::Identifier(expr) => eval_identifier(expr, env),
        Expression::Integer(expr) => Ok(Rc::new(Object::Integer(expr.value))),
        Expression::Boolean(expr) => Ok(get_bool_object(expr.value)),
        Expression::String(expr) => Ok(Rc::new(Object::String(expr.value.to_owned()))),
        Expression::Prefix(expr) => {
            let rhs = eval_expression(&expr.operand, env)?;
            eval_prefix_expression(expr.operator.get_literal(), &rhs)
        }
        Expression::Infix(expr) => {
            let lhs = eval_expression(&expr.lhs, env.clone())?;
            let rhs = eval_expression(&expr.rhs, env.clone())?;
            eval_infix_expression(expr.operator.get_literal(), &lhs, &rhs)
        }
        Expression::If(expr) => eval_if_expression(expr, env),
        Expression::FnLiteral(expr) => Ok(Rc::new(Object::Function {
            parameters: expr.parameters.to_owned(),
            body: expr.body.to_owned(),
            env,
        })),
        Expression::ArrayLiteral(expr) => Ok(Rc::new(Object::Array(eval_expressions(
            &expr.elements,
            env,
        )?))),
        Expression::HashLiteral(expr) => eval_hash_literal(expr, env),
        Expression::Call(expr) => {
            let function = eval_expression(&expr.function, env.clone())?;
            let args = eval_expressions(&expr.arguments, env.clone())?;
            if args.len() == 1 {
                if let Object::Error(_) = args[0].as_ref() {
                    return Ok(args[0].clone());
                }
            }
            apply_function(&function, &args)
        }
        Expression::Index(expr) => {
            let identifier = eval_expression(&expr.identifier, env.clone())?;
            let index = eval_expression(&expr.index, env)?;
            eval_index_expression(&identifier, &index)
        }
    }
}

fn eval_expressions(
    exprs: &[Expression],
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Rc<Object>>, EvalError> {
    let mut result = Vec::new();

    for expr in exprs.iter() {
        let evaluated = eval_expression(expr, env.clone())?;
        result.push(evaluated);
    }
    Ok(result)
}

fn eval_prefix_expression(prefix: String, expr: &Object) -> Result<Rc<Object>, EvalError> {
    match prefix {
        prefix if prefix == *"!" => eval_bang_operator_expression(expr),
        prefix if prefix == *"-" => eval_minus_operator_expression(expr),
        _ => Err(format!(
            "unknown operator: {}{}",
            prefix,
            expr.get_type_str()
        )),
    }
}

fn eval_bang_operator_expression(expr: &Object) -> Result<Rc<Object>, EvalError> {
    Ok(Rc::new(match expr {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }))
}

fn eval_minus_operator_expression(expr: &Object) -> Result<Rc<Object>, EvalError> {
    match expr {
        Object::Integer(value) => Ok(Rc::new(Object::Integer(-value))),
        _ => Err(format!("unknown operator: -{}", expr.get_type_str())),
    }
}

fn eval_infix_expression(
    operator: String,
    lhs: &Object,
    rhs: &Object,
) -> Result<Rc<Object>, EvalError> {
    match (&lhs, &rhs) {
        (Object::Integer(lhs_value), Object::Integer(rhs_value)) => {
            eval_integer_infix_expression(&operator, lhs_value, rhs_value)
        }
        (Object::Boolean(lhs_value), Object::Boolean(rhs_value)) => {
            eval_boolean_infix_expression(&operator, lhs_value, rhs_value)
        }
        (Object::String(lhs_value), Object::String(rhs_value)) => {
            eval_string_infix_expression(&operator, lhs_value, rhs_value)
        }
        _ => Err(format!(
            "unknown operator: {} {} {}",
            lhs.get_type_str(),
            operator,
            rhs.get_type_str(),
        )),
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    lhs: &i32,
    rhs: &i32,
) -> Result<Rc<Object>, EvalError> {
    match operator {
        "+" => Ok(Rc::new(Object::Integer(lhs + rhs))),
        "-" => Ok(Rc::new(Object::Integer(lhs - rhs))),
        "*" => Ok(Rc::new(Object::Integer(lhs * rhs))),
        "/" => Ok(Rc::new(Object::Integer(lhs / rhs))),
        "<" => Ok(get_bool_object(lhs < rhs)),
        ">" => Ok(get_bool_object(lhs > rhs)),
        "==" => Ok(get_bool_object(lhs == rhs)),
        "!=" => Ok(get_bool_object(lhs != rhs)),
        _ => Err(format!("unknown operator: INTEGER {} INTEGER", operator,)),
    }
}

fn eval_boolean_infix_expression(
    operator: &str,
    lhs: &bool,
    rhs: &bool,
) -> Result<Rc<Object>, EvalError> {
    match operator {
        "==" => Ok(get_bool_object(lhs == rhs)),
        "!=" => Ok(get_bool_object(lhs != rhs)),
        _ => Err(format!("unknown operator: BOOLEAN {} BOOLEAN", operator,)),
    }
}

fn eval_string_infix_expression(
    operator: &str,
    lhs: &str,
    rhs: &str,
) -> Result<Rc<Object>, EvalError> {
    match operator {
        "+" => Ok(Rc::new(Object::String([lhs, rhs].join("")))),
        _ => Err(format!("unknown operator: STRING {} STRING", operator,)),
    }
}

fn eval_if_expression(
    expr: &IfExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let condition = eval_expression(&expr.condition, env.clone())?;

    if is_truthy(&condition) {
        eval_block_statement(&expr.consequence, env)
    } else if let Some(alternative) = &expr.alternative {
        eval_block_statement(alternative, env)
    } else {
        Ok(Rc::new(Object::Null))
    }
}

fn eval_identifier(
    identifier: &IdentifierExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let value = &identifier.name;
    match env.borrow().get(value) {
        Some(val) => Ok(val),
        None => match get_builtin_fn(value) {
            Some(builtin) => Ok(Rc::new(Object::BuiltinFn(builtin))),
            None => Err(format!("identifier not found: {}", value)),
        },
    }
}

fn apply_function(function: &Object, args: &[Rc<Object>]) -> Result<Rc<Object>, EvalError> {
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
            let evaluated = eval_block_statement(body, extended_env)?;
            if let Object::ReturnValue(value) = evaluated.as_ref() {
                return Ok(value.clone());
            }
            Ok(evaluated)
        }
        Object::BuiltinFn(builtin) => Ok(builtin(args)),
        _ => Err(format!("not a function: {}", function.get_type_str(),)),
    }
}

fn extend_function_env(
    parameters: &[IdentifierExpression],
    env: Rc<RefCell<Environment>>,
    args: &[Rc<Object>],
) -> Environment {
    let mut env = env.borrow().clone().new_enclosed();

    for (i, param) in parameters.iter().enumerate() {
        env.set(param.name.to_owned(), args[i].clone());
    }
    env
}

fn eval_index_expression(identifier: &Object, index: &Object) -> Result<Rc<Object>, EvalError> {
    match (&identifier, &index) {
        (Object::Array(array), Object::Integer(integer)) => {
            eval_array_index_expression(array, *integer as usize)
        }
        (Object::Hash(hash), index) => eval_hash_index_expression(hash, index),
        _ => Err(format!(
            "index operator not supported: {}",
            identifier.get_type_str()
        )),
    }
}

fn eval_array_index_expression(
    array: &[Rc<Object>],
    index: usize,
) -> Result<Rc<Object>, EvalError> {
    if index >= array.len() {
        return Ok(Rc::new(Object::Null));
    }

    Ok(array[index].clone())
}

fn eval_hash_index_expression(
    hash: &BTreeMap<HashKey, HashPair>,
    index: &Object,
) -> Result<Rc<Object>, EvalError> {
    if let Some(hash_key) = index.get_hash_key() {
        if let Some(pair) = hash.get(&hash_key) {
            Ok(pair.value.clone())
        } else {
            Ok(Rc::new(Object::Null))
        }
    } else {
        Err(format!("unusable as hash key: {}", index.get_type_str()))
    }
}

fn eval_hash_literal(
    hash_literal: &HashLiteralExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let mut pairs = BTreeMap::new();

    for (key_expr, value_expr) in hash_literal.pairs.iter() {
        let key = eval_expression(key_expr, env.clone())?;
        match key.get_hash_key() {
            Some(hash_key) => {
                let value = eval_expression(value_expr, env.clone())?;
                pairs.insert(hash_key, HashPair { key, value });
            }
            None => {
                return Err(format!("unusable as hash key: {}", key.get_type_str()));
            }
        }
    }

    Ok(Rc::new(Object::Hash(pairs)))
}
