use std::io::{self, Write as _};

use crate::util::{Annot, Loc};

pub type Result<T> = std::result::Result<T, Error>;

struct ErrorKind {
    message: String,
    input: String,
}

/// Represents an occurance of an error and include proper error message.
pub type Error = Annot<ErrorKind>;

impl Error {
    pub fn new(message: String, input: String, loc: Loc) -> Self {
        Self {
            kind: ErrorKind { message, input },
            loc,
        }
    }

    pub fn show(&self) {
        let mut stderr = io::stderr().lock();
        write!(stderr, "{}", self.kind.input).unwrap();
        write!(
            stderr,
            "{:num$}^ {}",
            "",
            self.kind.message,
            num = self.loc.start
        )
        .unwrap();
    }
}
