mod codegen;
mod parse;
mod tokenize;

use crate::codegen::*;
use crate::parse::*;
use crate::tokenize::*;

pub type StdResult<T, E> = std::result::Result<T, E>;
pub type Result<T> = StdResult<T, Error>;

/// The memory size of a running machine.
const MEMORY_SIZE: usize = (usize::BITS >> 3) as usize;

/// Executes the main logic.
///
/// * args - command-line arguments
///
/// # Return
///
/// Exit code.
pub fn main(mut args: Vec<String>) -> u8 {
    if args.len() != 2 {
        eprintln!("Only ONE argument is required.");
        return 1;
    }

    // This unwrapping never fails becaouse of checking.
    let input = args.pop().unwrap().leak();

    // catch any error
    let mut gen = Generator::new(input);
    match gen.codegen() {
        Err(e) => {
            e.show(input);
            return 1;
        }
        _ => {}
    };

    0
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents all errros.
pub enum ErrorKind {
    Error(&'static str),
}

type Error = Annot<ErrorKind>;

impl Error {
    /// Shows the error in an understandable way.
    pub fn show(&self, input: &str) {
        let msg = match self.value {
            ErrorKind::Error(msg) => msg,
        };
        eprintln!("{}", input);
        // inserts '^' at the error point
        eprintln!("{:spaces_num$}^ {}", "", msg, spaces_num = self.loc.start);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Represents a location as half interval [start, end).
pub struct Loc {
    start: usize,
    end: usize,
}

impl Loc {
    /// Merges two Locations.
    pub fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};

        Loc {
            start: min(self.start, other.start),
            end: max(self.end, other.end),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Wraps any type with information.
///
/// Value of type `T` with location info.
pub struct Annot<T> {
    value: T,
    loc: Loc,
}
