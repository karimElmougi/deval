pub mod streaming_eval;

use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("the dice type `d{0}` is invalid")]
    InvalidDiceType(i32),
    #[error("invalid expression (expected `{0}`, found `{1}`)")]
    InvalidExpression(String, String),
    #[error("expression ended prematurely, expected `{0}`")]
    UnexpectedEndOfExpression(String),
    #[error("expected a `{0}`, found `{1}`")]
    InvalidConstant(String, String),
}

pub trait DiceSource {
    fn roll(&mut self, amount: DiceAmount, dice_type: DiceType) -> u16;
}

#[derive(Copy, Clone)]
pub enum DiceType {
    D20,
    D12,
    D10,
    D8,
    D6,
    D4,
}

impl DiceType {
    pub fn num_faces(self) -> u8 {
        match self {
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

impl TryFrom<i32> for DiceType {
    type Error = ParseError;

    fn try_from(n: i32) -> Result<DiceType, Self::Error> {
        match n {
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

pub enum DiceAmount {
    Advantage,
    Disadvantage,
    Number(u8),
}
