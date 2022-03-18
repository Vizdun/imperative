use chumsky::prelude::*;

#[derive(Debug, Clone)]
pub enum Statement {
    Assign {
        var: String,
        expr: Expr, // TODO: expr enum
    },
    Add {
        var: String,
        expr: Expr, // TODO: expr enum
    },
    Input {
        expr: Expr,
    },
    Output {
        expr: Expr,
    },
    If {
        condition: Expr,
        body: Box<Statement>, // TODO: else, else if
    },
    Perform {
        proc: String,
    },
    Repeat {
        condition: Option<Expr>,
        body: Box<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum MathOperation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Rot,
}

#[derive(Debug, Clone)]
pub enum BoolOperation {
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum CmpOperation {
    Equal,
    NotEqual,
    Less,
    Greater,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    String(String),
    Variable(String),
    Boolean(bool),
    Index {
        index: Box<Expr>,
        list: Box<Expr>,
    },
    MathExpr {
        lhs: Box<Expr>,
        op: MathOperation,
        rhs: Box<Expr>,
    },
    CmpExpr {
        lhs: Box<Expr>,
        op: CmpOperation,
        rhs: Box<Expr>,
    },
    BoolExpr {
        lhs: Box<Expr>,
        op: BoolOperation,
        rhs: Box<Expr>,
    },
}

fn ident_parser() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    let short_identifier = filter(|c: &char| c.is_ascii_alphabetic() | (*c == '-'))
        .repeated()
        .at_least(1)
        .collect();

    let long_identifier = filter(|c: &char| c.is_ascii_alphabetic() | (*c == '-') | (*c == ' '))
        .repeated()
        .at_least(1)
        .delimited_by(just('\''), just('\''))
        .collect();

    long_identifier.or(short_identifier)
}

// NOTE: if this breaks it's because whitespace between stuff
fn expr_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let number =
        chumsky::text::int::<_, Simple<char>>(10).map(|ns| Expr::Number(ns.parse().unwrap()));

    let string = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Expr::String);

    let boolean = choice((
        just("true").to(Expr::Boolean(true)),
        just("false").to(Expr::Boolean(false)),
    ));

    let variable = ident_parser().map(Expr::Variable);

    let primitive = number.or(string).or(variable).or(boolean);

    let index = primitive
        .clone()
        .then_ignore(chumsky::text::whitespace().then(just("of").then(chumsky::text::whitespace())))
        .then(primitive.clone())
        .map(|(index, list)| Expr::Index {
            index: Box::new(index),
            list: Box::new(list),
        });

    let so_far = index.or(primitive.clone());

    let math_op = choice((
        just("plus").to(MathOperation::Add),
        just("minus").to(MathOperation::Sub),
        just("multiplied")
            .then_ignore(chumsky::text::whitespace())
            .then(just("by"))
            .to(MathOperation::Mul),
        just("divided")
            .then_ignore(chumsky::text::whitespace())
            .then(just("by"))
            .to(MathOperation::Div),
        just("modulo").to(MathOperation::Mod),
        just("to")
            .then_ignore(chumsky::text::whitespace())
            .then(just("the"))
            .then_ignore(chumsky::text::whitespace())
            .then(just("power"))
            .then_ignore(chumsky::text::whitespace())
            .then(just("of"))
            .to(MathOperation::Pow),
        just("root")
            .then_ignore(chumsky::text::whitespace())
            .then(just("of"))
            .to(MathOperation::Rot),
    ));

    let math_expr = so_far
        .clone()
        .then_ignore(chumsky::text::whitespace())
        .then(math_op)
        .then_ignore(chumsky::text::whitespace())
        .then(so_far.clone())
        .map(|((lhs, op), rhs)| Expr::MathExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        });

    let so_far = math_expr.or(so_far);

    let cmp_op = choice((
        just("not")
            .then_ignore(chumsky::text::whitespace())
            .then(just("equal"))
            .then_ignore(chumsky::text::whitespace())
            .then(just("to"))
            .to(CmpOperation::NotEqual),
        just("equal")
            .then_ignore(chumsky::text::whitespace())
            .then(just("to"))
            .to(CmpOperation::Equal),
        just("greater")
            .then_ignore(chumsky::text::whitespace())
            .then(just("than"))
            .to(CmpOperation::Greater),
        just("less")
            .then_ignore(chumsky::text::whitespace())
            .then(just("than"))
            .to(CmpOperation::Less),
    ));

    // consider nesting cmprs / repeating ops
    let cmp_expr = so_far
        .clone()
        .then_ignore(chumsky::text::whitespace())
        .then_ignore(just("is"))
        .then_ignore(chumsky::text::whitespace())
        .then(cmp_op)
        .then_ignore(chumsky::text::whitespace())
        .then(so_far.clone())
        .map(|((lhs, op), rhs)| Expr::CmpExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        });

    let so_far = cmp_expr.or(so_far);

    let bool_op = choice((
        just("and").to(BoolOperation::And),
        just("or").to(BoolOperation::Or),
    ));

    let bool_expr = so_far
        .clone()
        .then_ignore(chumsky::text::whitespace())
        .then(bool_op)
        .then_ignore(chumsky::text::whitespace())
        .then(so_far.clone())
        .map(|((lhs, op), rhs)| Expr::BoolExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        });

    bool_expr.or(so_far)
}

// TODO: actually implement if statements properly lmfao
// TODO: OOP

// TODO: consider making long identifiers by taking until
fn statement_parser() -> impl Parser<char, Vec<Statement>, Error = Simple<char>> {
    let comment = just('(').then(take_until(just(')')));

    let statement = recursive(|statement| {
        let assign = just("assign")
            .then_ignore(chumsky::text::whitespace())
            .then(expr_parser())
            .then_ignore(
                chumsky::text::whitespace()
                    .then_ignore(just("to"))
                    .then_ignore(chumsky::text::whitespace()),
            )
            .then(ident_parser())
            .map(|((_, expr), var)| Statement::Assign { var, expr });

        let add = just("add")
            .then_ignore(chumsky::text::whitespace())
            .then(expr_parser())
            .then_ignore(
                chumsky::text::whitespace()
                    .then_ignore(just("to"))
                    .then_ignore(chumsky::text::whitespace()),
            )
            .then(ident_parser())
            .map(|((_, expr), var)| Statement::Add { var, expr });

        let input = just("input")
            .then_ignore(chumsky::text::whitespace())
            .then(expr_parser())
            .map(|(_, expr)| Statement::Input { expr });

        let output = just("output")
            .then_ignore(chumsky::text::whitespace())
            .then(expr_parser())
            .map(|(_, expr)| Statement::Output { expr });

        let r#if = just("if")
            .then_ignore(chumsky::text::whitespace())
            .then(expr_parser())
            .then_ignore(chumsky::text::whitespace())
            .then_ignore(just("then"))
            .then_ignore(chumsky::text::whitespace())
            .then(statement.clone())
            .map(|((_, expr), body)| Statement::If {
                condition: expr,
                body: Box::new(body),
            });

        let perform = just("perform")
            .then_ignore(chumsky::text::whitespace())
            .then(ident_parser())
            .map(|(_, proc)| Statement::Perform { proc });

        let repeat = just("repeat")
            .then_ignore(chumsky::text::whitespace())
            .then(statement)
            .then(
                chumsky::text::whitespace()
                    .then_ignore(just("while"))
                    .then_ignore(chumsky::text::whitespace())
                    .then(expr_parser())
                    .or_not(),
            )
            .map(|((_, body), condition)| Statement::Repeat {
                body: Box::new(body),
                condition: match condition {
                    Some((_, condition)) => Some(condition),
                    None => None
                },
            });

        assign
            .or(add)
            .or(input)
            .or(output)
            .or(r#if)
            .or(perform)
            .or(repeat)
    });

    statement
        .then_ignore(just('.'))
        .padded()
        .padded_by(comment.padded().repeated())
        .repeated()
}

pub fn main_parser() -> impl Parser<char, Vec<(String, Vec<Statement>)>, Error = Simple<char>> {
    just('#')
        .then_ignore(chumsky::text::whitespace())
        .then(ident_parser())
        .then_ignore(chumsky::text::newline())
        .then(statement_parser())
        .map(|((_, proc), body)| (proc, body))
        .repeated()
        .then_ignore(end())
}

pub fn case_parser(
) -> impl Parser<char, (Vec<(Vec<char>, Vec<char>)>, Vec<char>), Error = Simple<char>> {
    // TODO: handle comments
    take_until(just('"'))
        .map(|(s, _)| s)
        .then(take_until(just('"')).map(|(s, _)| s))
        .repeated()
        .then(take_until(end()).map(|(s, _)| s))
}
