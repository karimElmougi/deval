use crate::{DiceAmount, DiceSource, DiceType, ParseError};

use std::any::type_name;
use std::convert::TryFrom;
use std::fmt::Display;
use std::iter::{Iterator, Peekable};
use std::str::FromStr;

struct PeekingTakeWhile<'a, It, Pred>
where
    It: Iterator + 'a,
{
    iter: &'a mut Peekable<It>,
    predicate: Pred,
}

impl<'a, It, Pred> Iterator for PeekingTakeWhile<'a, It, Pred>
where
    It: Iterator + 'a,
    Pred: for<'b> FnMut(&'b It::Item) -> bool,
{
    type Item = It::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let &mut PeekingTakeWhile {
            ref mut iter,
            ref mut predicate,
        } = self;

        if iter.peek().map(predicate).unwrap_or(false) {
            iter.next()
        } else {
            None
        }
    }
}

struct CharacterStream<'a, It>
where
    It: Iterator<Item = (usize, char)>,
{
    expr: &'a str,
    stream: Peekable<It>,
}

fn new_char_stream(expr: &str) -> CharacterStream<impl Iterator<Item = (usize, char)> + '_> {
    CharacterStream {
        expr,
        stream: expr
            .chars()
            .enumerate()
            .filter(|(_, c)| !c.is_whitespace())
            .peekable(),
    }
}

impl<'a, It> CharacterStream<'a, It>
where
    It: Iterator<Item = (usize, char)>,
{
    fn is_at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn peek(&mut self) -> Option<char> {
        self.stream.peek().map(|&(_, c)| c)
    }

    fn consume_char(&mut self, expected: char) -> Result<()> {
        match self.stream.next() {
            Some((_, c)) if c == expected => Ok(()),
            Some((_, c)) => invalid_input(expected, c),
            None => end_of_expr(expected),
        }
    }

    fn parse_num<T: FromStr>(&mut self) -> Result<T> {
        match self.stream.peek() {
            Some(&(i, c)) if c.is_numeric() => {
                let (j, _) = self.take_while(|(_, c)| c.is_numeric()).last().unwrap();
                let str = &self.expr[i..j + 1];
                str.parse::<T>().map_err(|_| {
                    ParseError::InvalidConstant(type_name::<T>().to_string(), str.to_string())
                })
            }
            Some((_, c)) => invalid_input("digit", c),
            None => end_of_expr("constant"),
        }
    }

    fn parse_dice_type(&mut self) -> Result<DiceType> {
        self.consume_char('d')?;
        let num_faces = self.parse_num::<i32>()?;
        DiceType::try_from(num_faces)
    }

    fn take_while<'b, Pred>(&'b mut self, predicate: Pred) -> PeekingTakeWhile<'b, It, Pred>
    where
        Pred: for<'c> FnMut(&'c It::Item) -> bool,
    {
        PeekingTakeWhile {
            iter: &mut self.stream,
            predicate,
        }
    }
}

type Result<R> = std::result::Result<R, ParseError>;

pub fn eval<D: DiceSource>(expr: &str, dice: &mut D) -> Result<i32> {
    let mut stream = new_char_stream(expr);

    let result = eval_expr(&mut stream, dice)?;
    if stream.is_at_end() {
        Ok(result)
    } else {
        invalid_input("end of expression", stream.peek().unwrap())
    }
}

fn eval_term<It, D>(stream: &mut CharacterStream<'_, It>, dice: &mut D) -> Result<i32>
where
    It: Iterator<Item = (usize, char)>,
    D: DiceSource,
{
    match stream.peek() {
        Some('(') => {
            stream.consume_char('(')?;
            let value = eval_expr(stream, dice)?;
            stream.consume_char(')')?;
            Ok(value)
        }
        Some('a') => {
            stream.consume_char('a')?;
            let amount = DiceAmount::Advantage;
            let dice_type = stream.parse_dice_type()?;
            Ok(dice.roll(amount, dice_type) as i32)
        }
        Some('d') => {
            stream.consume_char('d')?;
            let amount = DiceAmount::Disadvantage;
            let dice_type = stream.parse_dice_type()?;
            Ok(dice.roll(amount, dice_type) as i32)
        }
        Some(_) => {
            let num = stream.parse_num::<u16>()?;
            match stream.peek() {
                Some('d') => {
                    let dice_type = stream.parse_dice_type()?;
                    Ok(dice.roll(DiceAmount::Number(num as u8), dice_type) as i32)
                }
                _ => Ok(num as i32),
            }
        }
        None => end_of_expr("term"),
    }
}

fn eval_expr<It, D>(stream: &mut CharacterStream<'_, It>, dice: &mut D) -> Result<i32>
where
    It: Iterator<Item = (usize, char)>,
    D: DiceSource,
{
    let mut result = eval_term(stream, dice)?;

    loop {
        match stream.peek() {
            Some('+') => {
                stream.consume_char('+')?;
                let right_hand_term = eval_term(stream, dice)?;
                result += right_hand_term;
            }
            Some('-') => {
                stream.consume_char('-')?;
                let right_hand_term = eval_term(stream, dice)?;
                result -= right_hand_term;
            }
            Some(_) => break,
            None => break,
        }
    }

    Ok(result)
}

fn invalid_input<E, F, R>(expected: E, found: F) -> Result<R>
where
    E: Display,
    F: Display,
{
    Err(ParseError::InvalidExpression(
        expected.to_string(),
        found.to_string(),
    ))
}

fn end_of_expr<E: Display, R>(expected: E) -> Result<R> {
    Err(ParseError::UnexpectedEndOfExpression(expected.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestDice;

    impl TestDice {
        fn new() -> Self {
            Self {}
        }
    }

    impl DiceSource for TestDice {
        fn roll(&mut self, amount: DiceAmount, dice_type: DiceType) -> u16 {
            let dice = match dice_type {
                DiceType::D20 => 11,
                DiceType::D12 => 7,
                DiceType::D10 => 6,
                DiceType::D8 => 5,
                DiceType::D6 => 4,
                DiceType::D4 => 3,
            };

            match amount {
                DiceAmount::Number(n) => n as u16 * dice as u16,
                DiceAmount::Advantage => dice as u16 + 1,
                DiceAmount::Disadvantage => dice as u16 - 1,
            }
        }
    }

    #[test]
    fn test_large_dice_num() {
        let result = eval("100d20", &mut TestDice::new());
        assert_eq!(result, Ok(1100));
    }

    #[test]
    fn test_eval() {
        let result = eval(
            "2d4 + 1d8 + ad20 - (1d6+(dd6-(4))) - 6",
            &mut TestDice::new(),
        );
        assert_eq!(result, Ok(14));
    }

    #[test]
    fn test_invalid_constant() {
        let result = eval("1d4 - 987654", &mut TestDice::new());
        let expected = ParseError::InvalidConstant("u16".to_string(), "987654".to_string());
        assert_eq!(result, Err(expected));
    }

    #[test]
    fn test_invalid_expression() {
        let result = eval("1d4++4", &mut TestDice::new());
        let expected = ParseError::InvalidExpression("digit".to_string(), "+".to_string());
        assert_eq!(result, Err(expected));
    }
}
