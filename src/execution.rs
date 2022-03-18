use std::{collections::HashMap, fmt::Display, io, slice::SliceIndex};

use imperative_parser::{BoolOperation, CmpOperation, Expr, MathOperation, Statement};

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::List(l) => {
                for val in l {
                    write!(f, "{}", val).unwrap();
                }
                Ok(())
            }
        }
    }
}

fn num_eval(expr: Expr, vars: &HashMap<String, Value>) -> f64 {
    match eval(expr, vars) {
        Value::Number(n) => n,
        Value::String(s) => s.parse::<f64>().unwrap_or(s.len() as f64),
        Value::Boolean(b) => match b {
            true => 1.0,
            false => 0.0,
        },
        Value::List(l) => l.len() as f64,
    }
}

fn bool_eval(expr: Expr, vars: &HashMap<String, Value>) -> bool {
    match eval(expr, vars) {
        Value::Number(n) => n != 0.0,
        Value::String(s) => s != "",
        Value::Boolean(b) => b,
        Value::List(l) => l.len() != 0,
    }
}

fn eval(expr: Expr, vars: &HashMap<String, Value>) -> Value {
    match expr {
        Expr::Number(n) => Value::Number(n),
        Expr::String(s) => Value::String(s),
        Expr::Variable(var) => vars.get(&var).unwrap_or(&Value::Number(0.0)).clone(),
        Expr::Boolean(b) => Value::Boolean(b),
        Expr::Index { index, list } => {
            let index = num_eval(*index, vars) as usize;
            let evaled = eval(*list, vars);
            match evaled {
                // TODO: hashmaps
                Value::List(l) => l.get(index).unwrap_or(&Value::Number(0.0)).clone(),
                _ => {
                    let s = match evaled {
                        Value::Number(n) => n.to_string(),
                        Value::String(s) => s,
                        Value::Boolean(b) => b.to_string(),
                        Value::List(_) => todo!("this really isn't supposed to happen"),
                    };

                    Value::String(match s.chars().collect::<Vec<char>>().get(index) {
                        Some(c) => c.to_string(),
                        None => "".to_string(),
                    })
                }
            }
        }
        Expr::MathExpr { lhs, op, rhs } => {
            let [lhs, rhs] = [lhs, rhs].map(|hs| num_eval(*hs, vars));

            Value::Number(match op {
                MathOperation::Add => lhs + rhs,
                MathOperation::Sub => lhs - rhs,
                MathOperation::Mul => lhs * rhs,
                MathOperation::Div => lhs / rhs,
                MathOperation::Mod => lhs % rhs,
                MathOperation::Pow => lhs.powf(rhs),
                MathOperation::Rot => lhs.powf(1.0 / rhs),
            })
        }
        Expr::CmpExpr { lhs, op, rhs } => {
            // TODO: make this actually check other stuff than numbers
            let [lhs, rhs] = [lhs, rhs].map(|hs| num_eval(*hs, vars));

            Value::Boolean(match op {
                CmpOperation::Equal => lhs == rhs,
                CmpOperation::NotEqual => lhs != rhs,
                CmpOperation::Less => lhs < rhs,
                CmpOperation::Greater => lhs > rhs,
            })
        }
        Expr::BoolExpr { lhs, op, rhs } => {
            let [lhs, rhs] = [lhs, rhs].map(|hs| bool_eval(*hs, vars));

            Value::Boolean(match op {
                BoolOperation::And => lhs && rhs,
                BoolOperation::Or => lhs || rhs,
            })
        }
    }
}

fn exec_statement(
    statement: Statement,
    procs: &HashMap<String, Vec<Statement>>,
    vars: &mut HashMap<String, Value>,
) {
    match statement {
        Statement::Assign { var, expr } => {
            vars.insert(var, eval(expr, vars));
        }
        Statement::Add { var, expr } => todo!(),
        Statement::Input { expr } => {
            let mut buffer = String::new();
            let stdin = io::stdin();
            stdin.read_line(&mut buffer).unwrap();
            match expr {
                Expr::Variable(var) => {
                    vars.insert(var, Value::String(buffer[..buffer.len() - 1].to_string()));
                }
                _ => {}
            }
        }
        Statement::Output { expr } => println!("{}", eval(expr, vars)),
        Statement::If { condition, body } => {
            if bool_eval(condition, vars) {
                exec_statement(*body, procs, vars);
            }
        }
        Statement::Perform { proc } => match procs.get(&proc) {
            Some(proc) => exec_proc(proc.clone(), procs, vars),
            None => {}
        },
        Statement::Repeat { condition, body } => {
            while bool_eval(condition.clone().unwrap_or(Expr::Boolean(true)), vars) {
                exec_statement(*body.clone(), procs, vars);
            }
        }
    }
}

fn exec_proc(
    statements: Vec<Statement>,
    procs: &HashMap<String, Vec<Statement>>,
    vars: &mut HashMap<String, Value>,
) {
    for statement in statements {
        exec_statement(statement, procs, vars);
    }
}

pub fn execute(procedures: Vec<(String, Vec<Statement>)>) {
    let main_procedure = procedures.first().unwrap().clone().1;

    let procedure_map: HashMap<String, Vec<Statement>>;

    {
        let mut procedures_temp: HashMap<String, Vec<Statement>> = HashMap::new();
        for (indx, stmnts) in procedures {
            procedures_temp.insert(indx, stmnts);
        }
        procedure_map = procedures_temp;
    }

    let mut vars: HashMap<String, Value> = HashMap::new();

    exec_proc(main_procedure, &procedure_map, &mut vars);
}
