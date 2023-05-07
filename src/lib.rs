mod lexer;

use lexer::Token;
use thiserror::Error;

use std::cmp::{max, min};
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("the dice type `d{0}` is invalid")]
    InvalidDiceType(u8),
    #[error("expression ended prematurely, expected `{0}`")]
    UnexpectedEndOfExpression(String),
    #[error("expected a `{0}`, found `{1}`")]
    InvalidConstant(&'static str, String),
    #[error("unexpected character `{0}`")]
    UnexpectedCharacter(char),
    #[error("expected `{0}`, found `{1}`")]
    UnexpectedToken(&'static str, Token),
}

pub trait DiceSource {
    fn roll(&mut self, n: u8, dice_type: DiceType) -> u16;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DiceType {
    D4,
    D6,
    D8,
    D10,
    D12,
    D20,
    D100,
}

impl DiceType {
    pub fn num_faces(self) -> u8 {
        match self {
            DiceType::D100 => 100,
            DiceType::D20 => 20,
            DiceType::D12 => 12,
            DiceType::D10 => 10,
            DiceType::D8 => 8,
            DiceType::D6 => 6,
            DiceType::D4 => 4,
        }
    }
}

impl Display for DiceType {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "d{}", self.num_faces())
    }
}

impl TryFrom<u8> for DiceType {
    type Error = ParseError;

    fn try_from(n: u8) -> Result<DiceType, Self::Error> {
        match n {
            100 => Ok(DiceType::D100),
            20 => Ok(DiceType::D20),
            12 => Ok(DiceType::D12),
            10 => Ok(DiceType::D10),
            8 => Ok(DiceType::D8),
            6 => Ok(DiceType::D6),
            4 => Ok(DiceType::D4),
            n => Err(ParseError::InvalidDiceType(n)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Term {
    Number(u8),
    Advantage(DiceType),
    Disadvantage(DiceType),
    Dice(u8, DiceType),
}

impl Term {
    fn eval<D: DiceSource>(self, src: &mut D) -> i32 {
        match self {
            Term::Number(n) => n as i32,
            Term::Dice(n, d) => src.roll(n, d) as i32,
            Term::Advantage(d) => max(src.roll(1, d), src.roll(1, d)) as i32,
            Term::Disadvantage(d) => min(src.roll(1, d), src.roll(1, d)) as i32,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expression {
    Term(Term),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
}

impl From<Term> for Expression {
    fn from(term: Term) -> Self {
        Expression::Term(term)
    }
}

impl Expression {
    fn eval<D: DiceSource>(self, src: &mut D) -> i32 {
        match self {
            Expression::Term(term) => term.eval(src),
            Expression::Plus(a, b) => a.eval(src) + b.eval(src),
            Expression::Minus(a, b) => a.eval(src) - b.eval(src),
        }
    }
}

pub fn eval<D: DiceSource>(input: &str, src: &mut D) -> Result<i32, ParseError> {
    let mut tokens = lexer::tokenize(input)?;
    let expr = parse_expr(&mut tokens)?;
    Ok(expr.eval(src))
}

fn parse_expr(tokens: &mut Vec<Token>) -> Result<Expression, ParseError> {
    let term = parse_term(tokens)?;
    match tokens.last().copied() {
        Some(Token::Plus) => {
            tokens.pop();
            Ok(Expression::Plus(
                Box::new(parse_expr(tokens)?),
                Box::new(term.into()),
            ))
        }
        Some(Token::Minus) => {
            tokens.pop();
            Ok(Expression::Minus(
                Box::new(parse_expr(tokens)?),
                Box::new(term.into()),
            ))
        }
        Some(token) => Err(ParseError::UnexpectedToken("+|-", token)),
        None => Ok(Expression::Term(term)),
    }
}

fn parse_term(tokens: &mut Vec<Token>) -> Result<Term, ParseError> {
    match tokens.pop() {
        Some(Token::Advantage(dice)) => Ok(Term::Advantage(dice)),
        Some(Token::Disadvantage(dice)) => Ok(Term::Disadvantage(dice)),
        Some(Token::Dice(n, dice)) => Ok(Term::Dice(n, dice)),
        Some(Token::Number(n)) => Ok(Term::Number(n)),
        Some(token) => Err(ParseError::UnexpectedToken("adN|ddN|dN|kdN", token)),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokenize;

    use super::*;

    struct TestDice {
        d4: u16,
        d6: u16,
        d8: u16,
        d10: u16,
        d12: u16,
        d20: u16,
        d100: u16,
    }

    impl TestDice {
        fn new() -> Self {
            Self {
                d4: 3,
                d6: 4,
                d8: 5,
                d10: 6,
                d12: 7,
                d20: 11,
                d100: 51,
            }
        }

        fn roll_one(&mut self, dice: DiceType) -> u16 {
            match dice {
                DiceType::D100 => self.roll_d100(),
                DiceType::D20 => self.roll_d20(),
                DiceType::D12 => self.roll_d12(),
                DiceType::D10 => self.roll_d10(),
                DiceType::D8 => self.roll_d8(),
                DiceType::D6 => self.roll_d6(),
                DiceType::D4 => self.roll_d4(),
            }
        }
        fn roll_d4(&mut self) -> u16 {
            let val = self.d4;
            self.d4 += 1;
            val
        }
        fn roll_d6(&mut self) -> u16 {
            let val = self.d6;
            self.d6 += 1;
            val
        }
        fn roll_d8(&mut self) -> u16 {
            let val = self.d8;
            self.d8 += 1;
            val
        }
        fn roll_d10(&mut self) -> u16 {
            let val = self.d10;
            self.d10 += 1;
            val
        }
        fn roll_d12(&mut self) -> u16 {
            let val = self.d12;
            self.d12 += 1;
            val
        }
        fn roll_d20(&mut self) -> u16 {
            let val = self.d20;
            self.d20 += 1;
            val
        }
        fn roll_d100(&mut self) -> u16 {
            let val = self.d100;
            self.d100 += 1;
            val
        }
    }

    impl DiceSource for TestDice {
        fn roll(&mut self, n: u8, dice: DiceType) -> u16 {
            (0..n).map(|_| self.roll_one(dice)).sum()
        }
    }

    #[test]
    fn test_parse() {
        use DiceType::*;
        use Expression::{Minus, Plus};
        use Term::{Advantage, Dice, Number};

        let expr = "2d4 + d8 + ad20 - 1d6 - 6";
        let mut tokens = tokenize(expr).unwrap();
        let result = parse_expr(&mut tokens).unwrap();
        assert_eq!(
            result,
            Minus(
                Box::new(Minus(
                    Box::new(Plus(
                        Box::new(Plus(
                            Box::new(Expression::Term(Dice(2, D4))),
                            Box::new(Expression::Term(Dice(1, D8))),
                        )),
                        Box::new(Expression::Term(Advantage(D20))),
                    )),
                    Box::new(Expression::Term(Dice(1, D6))),
                )),
                Box::new(Expression::Term(Number(6))),
            ),
        );
    }

    #[test]
    fn test_eval() {
        let result = eval("2d4 + 1d8 + ad20 - 1d6 - 6", &mut TestDice::new());
        assert_eq!(result, Ok(14));
    }

    #[test]
    fn test_invalid_constant() {
        let result = eval("1d4 - 987654", &mut TestDice::new());
        let expected = ParseError::InvalidConstant("u8", "987654".to_string());
        assert_eq!(result, Err(expected));
    }

    #[test]
    fn test_invalid_expression() {
        let result = eval("1d4++4", &mut TestDice::new());
        let expected = ParseError::UnexpectedToken("adN|ddN|dN|kdN", Token::Plus);
        assert_eq!(result, Err(expected));

        let result = eval("10 + d", &mut TestDice::new());
        let expected = ParseError::UnexpectedEndOfExpression("dN".to_string());
        assert_eq!(result, Err(expected));
    }
}
