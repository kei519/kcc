#[cfg(test)]
mod tests;

use crate::util::{Annot, Error, Loc, Result};

pub struct Tokenizer {
    input: &'static str,
    pos: usize,
}

impl Tokenizer {
    pub fn new(input: &'static str) -> Self {
        Self { input, pos: 0 }
    }

    pub fn tokenize(self) -> Result<Vec<Token>> {
        let mut ret = vec![];

        while !self.cur().is_empty() {
            return Err(Error::CompileError {
                message: "unrecoginized token".into(),
                input: self.input,
                loc: Loc::at(self.pos),
            });
        }

        ret.push(Token::with_eof(Loc::at(self.pos)));
        Ok(ret)
    }

    /// Returns the byte sequence starting from the current position.
    fn cur(&self) -> &'static [u8] {
        &self.input.as_bytes()[self.pos..]
    }

    /// Advances the cursor if the cursor will not exceed the input.
    /// Returns `true` if the cursor is advanced.
    fn next(&mut self) -> bool {
        if self.pos < self.input.len() {
            self.pos += 1;
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// Represents the end of file.
    Eof,
}

pub type Token = Annot<TokenKind>;

impl Token {
    pub fn with_eof(loc: Loc) -> Self {
        Self {
            data: TokenKind::Eof,
            loc,
        }
    }
}
