use crate::{DiceType, ParseError};

use std::iter::{Filter, Peekable};
use std::str::Chars;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    Number(u8),
    Advantage(DiceType),
    Disadvantage(DiceType),
    Plus,
    Minus,
    Dice(u8, DiceType),
}

impl Token {
    pub fn is_dice(&self) -> bool {
        matches!(self, Token::Dice(..))
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Token::Advantage(dice) => write!(f, "a{dice}"),
            Token::Disadvantage(dice) => write!(f, "d{dice}"),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Dice(n, dice) => write!(f, "{n}{dice}"),
            Token::Number(n) => n.fmt(f),
        }
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let inner = input
        .chars()
        .filter((|c| !c.is_whitespace()) as Pred)
        .peekable();
    TokenStream { inner }.collect()
}

type Pred = fn(&char) -> bool;

struct InnerStream<'a>(Peekable<Filter<Chars<'a>, Pred>>);

impl<'a> Iterator for InnerStream<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_char = self.0.next()?;
        Some(to_token(next_char, &mut self.0))
    }
}

struct TokenStream<'a> {
    inner: Peekable<Filter<Chars<'a>, Pred>>,
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_char = self.inner.next()?;
        Some(to_token(next_char, &mut self.inner))
    }
}

fn to_token<It>(c: char, stream: &mut Peekable<It>) -> Result<Token, ParseError>
where
    It: Iterator<Item = char>,
{
    match c {
        'a' => Ok(Token::Advantage(consume_dice(stream)?)),
        'd' => {
            if stream.peek().map(|c| c.is_numeric()).unwrap_or(false) {
                let value = take_while(stream, |c| c.is_ascii_digit()).collect::<String>();
                let n = value
                    .parse::<u8>()
                    .map_err(|_| ParseError::InvalidConstant("u8", value))?;
                Ok(Token::Dice(1, DiceType::try_from(n)?))
            } else {
                Ok(Token::Disadvantage(consume_dice(stream)?))
            }
        }
        '+' => Ok(Token::Plus),
        '-' => Ok(Token::Minus),
        n @ '0'..='9' => {
            let value = std::iter::once(n)
                .chain(take_while(stream, |c| c.is_ascii_digit()))
                .collect::<String>();
            let n = value
                .parse::<u8>()
                .map_err(|_| ParseError::InvalidConstant("u8", value))?;

            if stream.peek().map(|c| *c == 'd').unwrap_or(false) {
                let dice = consume_dice(stream)?;
                Ok(Token::Dice(n, dice))
            } else {
                Ok(Token::Number(n))
            }
        }
        _ => Err(ParseError::UnexpectedCharacter(c)),
    }
}

fn consume_dice<It>(stream: &mut Peekable<It>) -> Result<DiceType, ParseError>
where
    It: Iterator<Item = char>,
{
    match stream.next() {
        Some('d') => {
            let value = take_while(stream, |c| c.is_ascii_digit()).collect::<String>();
            value
                .parse::<u8>()
                .map_err(|_| ParseError::InvalidConstant("u8", value))
                .and_then(DiceType::try_from)
        }
        Some(c) => Err(ParseError::UnexpectedCharacter(c)),
        None => Err(ParseError::UnexpectedEndOfExpression("dN".to_string())),
    }
}

fn take_while<'a, It, Pred>(
    iter: &'a mut Peekable<It>,
    predicate: Pred,
) -> PeekingTakeWhile<'a, It, Pred>
where
    It: Iterator,
    Pred: for<'c> FnMut(&'c It::Item) -> bool,
{
    PeekingTakeWhile { iter, predicate }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let expr = "2d4 + d8 + ad20 - 1d6 - 6";
        let tokens = tokenize(expr).unwrap();
        use DiceType::*;
        use Token::*;
        assert_eq!(
            tokens,
            vec![
                Dice(2, D4),
                Plus,
                Dice(1, D8),
                Plus,
                Advantage(D20),
                Minus,
                Dice(1, D6),
                Minus,
                Number(6)
            ]
        );
    }
}
