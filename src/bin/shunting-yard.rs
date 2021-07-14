#![feature(bindings_after_at)]
use std::{fmt, io::BufRead};

#[derive(Debug)]
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

fn expr(input: &str) -> S {
    let mut lexer = Lexer::new(input);
    let res = expr_bp(&mut lexer).unwrap();
    res
}

// enum Fixity {
//     Prefix,
//     Infix,
//     Postfix,
//     None,
//   }

// struct Operator(char, Fixity);
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Operator(char, bool);

impl Operator {
    fn bp(&self) -> Option<(u8, u8)> {
        let &Operator(op, prefix) = self;

        Some(match op {
            '0'..='9' | 'a'..='z' | 'A'..='Z' => (99, 100),
            '(' => (99, 0),
            ')' => (0, 100),
            '=' => (2, 1),
            '+' | '-' if prefix => (99, 9),
            '+' | '-' => (5, 6),
            '*' | '/' => (7, 8),
            '!' => (11, 100),
            '.' => (14, 13),
            _ => return None,
        })
    }
}

use std::cmp::Ordering;
impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Operator) -> Option<Ordering> {
        let (_, r_bp1) = self.bp()?;
        let (l_bp2, _) = other.bp()?;
        let res = match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
            (false, false) => {
                println!("{:?} = {:?}", self, other);
                Some(Ordering::Equal)
            }
            (true, false) => {
                println!("{:?} < {:?}", self, other);
                Some(Ordering::Less)
            }
            (false, true) => {
                println!("{:?} > {:?}", self, other);
                Some(Ordering::Greater)
            }
            _ => None,
        };
        println!("{:?}", res);
        res
    }
}
struct Frame {
    operator: Option<Operator>,
    lhs: Option<S>,
}

fn expr_bp(lexer: &mut Lexer) -> Option<S> {
    let mut top = Frame {
        lhs: None,
        operator: None,
    };
    let mut stack = Vec::new();

    loop {
        let token = lexer.next();
        let operator = loop {
            let operator = token.map(|token| Operator(token, top.lhs.is_none()));
            match operator {
                t @ Some(op) if top.operator <= t => break op,
                _ => {
                    let res = top;
                    top = match stack.pop() {
                        Some(it) => it,
                        None => return res.lhs,
                    };

                    let token = res.operator.unwrap().0;
                    top.lhs = Some(S(
                        token,
                        top.lhs.map(Box::new),
                        res.lhs.map(Box::new),
                    ));
                }
            };
        };

        if operator.0 == ')' {
            assert_eq!(
                top.operator,
                Some(Operator('(', true))
            );
            let res = top;
            top = stack.pop().unwrap();
            top.lhs = res.lhs;
            continue;
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
    let s = expr("1");
    assert_eq!(s.to_string(), "1");

    let s = expr("1 + 2 * 3");
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))");

    let s = expr("a + b * c * d + e");
    assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");

    let s = expr("f . g . h");
    assert_eq!(s.to_string(), "(. f (. g h))");

    let s = expr(" 1 + 2 + f . g . h * 3 * 4");
    assert_eq!(
        s.to_string(),
        "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))"
    );

    let s = expr("--1 * 2");
    assert_eq!(s.to_string(), "(* (- (- 1)) 2)");

    let s = expr("--f . g");
    assert_eq!(s.to_string(), "(- (- (. f g)))");

    let s = expr("-9!");
    assert_eq!(s.to_string(), "(- (! 9))");

    let s = expr("f . g !");
    assert_eq!(s.to_string(), "(! (. f g))");

    let s = expr("(((0)))");
    assert_eq!(s.to_string(), "0");

    let s = expr("(1 + 2) * 3");
    assert_eq!(s.to_string(), "(* (+ 1 2) 3)");

    let s = expr("1 + (2 * 3)");
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))");
}

fn main() {
    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        let s = expr(&line);
        println!("{}", s)
    }
}
