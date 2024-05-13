use std::{
    cmp,
    io::{self, Write},
    ops::{Add, AddAssign},
};

/// Represents location [`start`, `end`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc {
    pub start: usize,
    pub end: usize,
}

impl Loc {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Add for Loc {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: cmp::min(self.start, rhs.start),
            end: cmp::max(self.end, rhs.end),
        }
    }
}

impl AddAssign for Loc {
    fn add_assign(&mut self, rhs: Self) {
        self.start = cmp::min(self.start, rhs.start);
        self.end = cmp::max(self.end, rhs.end);
    }
}

/// Represents data and the location.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    pub data: T,
    pub loc: Loc,
}

/// Represents errors.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    Compile { message: String, input: String },
}

pub type Error = Annot<ErrorKind>;

impl Error {
    /// Constructs error that occurs in compilation.
    pub fn from_compilation(message: String, input: String, loc: Loc) -> Self {
        Self {
            data: ErrorKind::Compile { message, input },
            loc,
        }
    }

    /// Shows proper message with consuming the error.
    pub fn show(self) {
        match self.data {
            ErrorKind::Compile { message, input } => {
                let mut stderr = io::stderr().lock();
                writeln!(stderr, "{}", input).unwrap();
                writeln!(stderr, "{:num$}^ {}", "", message, num = self.loc.start).unwrap();
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
