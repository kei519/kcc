use crate::util::{Annot, Loc};

pub type Result<T> = std::result::Result<T, Error>;

/// Represents an occurance of an error and include proper error message.
pub type Error = Annot<String>;

impl Error {
    pub fn new(message: String, loc: Loc) -> Self {
        Self { kind: message, loc }
    }
}
