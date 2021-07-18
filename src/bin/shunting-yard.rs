#![feature(bindings_after_at)]
use std::{fmt, io::BufRead};

#[derive(Debug, Clone)]
struct S(char, Option<Box<S>>, Option<Box<S>>);

impl fmt::Display for S {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self(op, None, None) => write!(f, "{}", op),
            Self(op, Some(left), None) => {
                write!(f, "({} {})", op, left)
            }
            Self(op, Some(left), Some(right)) => {
                write!(f, "({} {} {})", op, left, right)
            }
            Self(op, None, Some(right)) => {
                write!(f, "({} {})", op, right)
            }
        }
    }
}

impl From<S> for String {
    fn from(x: S) -> String {
        format!("{}", x)
    }
}

struct Lexer {
    tokens: Vec<char>,
}

impl Lexer {
    fn new(input: &str) -> Lexer {
        let mut tokens = input
            .chars()
            .filter(|it| !it.is_ascii_whitespace())
            .collect::<Vec<_>>();
        tokens.reverse();
        Lexer { tokens }
    }

    fn next(&mut self) -> Option<char> {
        self.tokens.pop()
    }
}

fn expr(input: &str) -> Result<S, &'static str> {
    let mut lexer = Lexer::new(input);

    expr_bp(&mut lexer)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Fixity {
    Prefix,
    Infix,
    Postfix,
    None,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Operator(char, Fixity);

impl Operator {
    pub fn new(
        token: Option<char>,
        prefix: bool,
    ) -> Option<Self> {
        let token = token?;
        let op = Operator(
            token,
            if prefix {
                Fixity::Prefix
            } else {
                Fixity::Infix
            },
        );

        if op.exist() {
            return Some(op);
        }

        let op = Operator(
            token,
            if prefix {
                Fixity::None
            } else {
                Fixity::Postfix
            },
        );

        if op.exist() {
            return Some(op);
        }

        None
    }

    fn exist(&self) -> bool {
        match self {
            Operator(
                '0'..='9' | 'a'..='z' | 'A'..='Z',
                Fixity::None,
            ) => true,
            Operator('(' | '+' | '-', Fixity::Prefix) => true,
            Operator(')' | '!', Fixity::Postfix) => true,
            Operator(
                '.' | '=' | '+' | '-' | '*' | '/',
                Fixity::Infix,
            ) => true,
            _ => false,
        }
    }

    fn bp(&self) -> Option<(u8, u8)> {
        Some(match self {
            Operator(
                '0'..='9' | 'a'..='z' | 'A'..='Z',
                Fixity::None,
            ) => (99, 100),
            Operator(token, Fixity::Prefix) => (
                99,
                match token {
                    '(' => 0,
                    '+' | '-' => 9,
                    _ => return None,
                },
            ),
            Operator(token, Fixity::Postfix) => (
                match token {
                    ')' => 0,
                    '!' => 11,
                    _ => return None,
                },
                100,
            ),
            Operator(token, Fixity::Infix) => match token {
                '=' => (2, 1),
                '+' | '-' => (5, 6),
                '*' | '/' => (7, 8),
                '.' => (14, 13),
                _ => return None,
            },
            _ => return None,
        })
    }
}

use std::cmp::Ordering;
impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Operator) -> Option<Ordering> {
        let (_, r_bp1) = self.bp()?;
        let (l_bp2, _) = other.bp()?;

        Some(match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
            (false, false) => Ordering::Equal,
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
            _ => return None,
        })
    }
}
struct Frame {
    operator: Option<Operator>,
    lhs: Option<S>,
}

fn expr_bp(lexer: &mut Lexer) -> Result<S, &'static str> {
    let mut top = Frame {
        lhs: None,
        operator: None,
    };
    let mut stack = Vec::new();

    loop {
        let token = lexer.next();
        let operator = loop {
            let operator =
                Operator::new(token, top.lhs.is_none());
            match operator {
                t @ Some(op) if top.operator <= t => break op,
                _ => {
                    let res = top;
                    if let Some(S('(', None, Some(_))) = res.lhs {
                        return Err(
                            "Expected closing parenthesis",
                        );
                    }

                    top = match stack.pop() {
                        Some(it) => it,
                        None => {
                            return res.lhs.ok_or("No expression")
                        }
                    };

                    top.lhs = Some(S(
                        res.operator.unwrap().0,
                        top.lhs.map(Box::new),
                        res.lhs.map(Box::new),
                    ));
                }
            };
        };

        if let Operator(')', Fixity::Postfix) = operator {
            if let Some(Operator('(', Fixity::Prefix)) =
                top.operator
            {
                let res = top;
                top = stack.pop().unwrap();
                top.lhs = res.lhs;
                continue;
            } else {
                return Err("Unexpected closing parenthesis");
            }
        }

        stack.push(top);
        top = Frame {
            lhs: None,
            operator: Some(operator),
        };
    }
}

#[test]
fn tests() {
    let s = expr("1").unwrap();
    assert_eq!(s.to_string(), "1");

    let s = expr("1 + 2 * 3").unwrap();
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))");

    let s = expr("a + b * c * d + e").unwrap();
    assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");

    let s = expr("f . g . h").unwrap();
    assert_eq!(s.to_string(), "(. f (. g h))");

    let s = expr(" 1 + 2 + f . g . h * 3 * 4").unwrap();
    assert_eq!(
        s.to_string(),
        "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))"
    );

    let s = expr("--1 * 2").unwrap();
    assert_eq!(s.to_string(), "(* (- (- 1)) 2)");

    let s = expr("--f . g").unwrap();
    assert_eq!(s.to_string(), "(- (- (. f g)))");

    let s = expr("-9!").unwrap();
    assert_eq!(s.to_string(), "(- (! 9))");

    let s = expr("f . g !").unwrap();
    assert_eq!(s.to_string(), "(! (. f g))");

    let s = expr("(((0)))").unwrap();
    assert_eq!(s.to_string(), "0");

    let s = expr("(1 + 2) * 3").unwrap();
    assert_eq!(s.to_string(), "(* (+ 1 2) 3)");

    let s = expr("1 + (2 * 3)").unwrap();
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))");

    let s = expr("(1 + (2 * 3)").unwrap_err();
    assert_eq!(s, "Expected closing parenthesis");

    let s = expr("1 + (2 * 3))").unwrap_err();
    assert_eq!(s, "Unexpected closing parenthesis");

    let s = expr("1 + 2 * 3)").unwrap_err();
    assert_eq!(s, "Unexpected closing parenthesis");

    let s = expr("1 + (2 * 3").unwrap_err();
    assert_eq!(s, "Expected closing parenthesis");
}

fn main() {
    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        let s = expr(&line);
        println!("{:?}", s)
    }
}
