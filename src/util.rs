use std::{
    cmp,
    fmt::Display,
    ops::{Add, AddAssign},
};

/// Represents a location [`start`, `end`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc {
    start: usize,
    end: usize,
}

impl Loc {
    pub fn at(pos: usize) -> Self {
        Self {
            start: pos,
            end: pos + 1,
        }
    }

    pub fn range(start: usize, end: usize) -> Self {
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

/// Represents data annotated with a location.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    pub data: T,
    pub loc: Loc,
}

pub type Result<T> = std::result::Result<T, Error>;

/// Represents a error occured in this program.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    ConfigError,
    CompileError {
        message: String,
        input: &'static str,
        loc: Loc,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ConfigError => Ok(()),
            Self::CompileError {
                message,
                input,
                loc,
            } => {
                f.write_str(*input)?;
                f.write_str("\n")?;
                write!(f, "{:num$}^ {}", "", message, num = loc.start)
            }
        }
    }
}
