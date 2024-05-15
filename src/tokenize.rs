#[cfg(test)]
mod tests;

use crate::util::{Annot, Error, Loc, Result};

const KW: [&'static str; 6] = ["+", "-", "*", "/", "(", ")"];

pub struct Tokenizer {
    input: &'static str,
    pos: usize,
}

impl Tokenizer {
    pub fn new(input: &'static str) -> Self {
        Self { input, pos: 0 }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>> {
        let mut ret = vec![];

        'main: while !self.cur().is_empty() {
            if self.cur()[0].is_ascii_whitespace() {
                self.next();
                continue;
            }

            if self.cur()[0].is_ascii_digit() {
                let start = self.pos;
                let num = self.str_to_num();
                let end = self.pos;
                ret.push(Token::with_num(num, Loc::range(start, end)));
                continue;
            }

            for &kw in &KW {
                if self.cur().starts_with(kw.as_bytes()) {
                    let start = self.pos;
                    self.pos += kw.len();
                    let end = self.pos;
                    ret.push(Token::with_reserved(kw, Loc::range(start, end)));
                    continue 'main;
                }
            }

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

    /// Converts the byte sequence starting from the current position to a number.
    ///
    /// # Remarks
    ///
    /// This function assumes that the byte sequence starting with a digit.
    fn str_to_num(&mut self) -> usize {
        let mut num = 0;
        while !self.cur().is_empty() && self.cur()[0].is_ascii_digit() {
            num = num * 10;
            num += (self.cur()[0] - b'0') as usize;
            self.next();
        }
        num
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// Numeric literal.
    Num(usize),
    // Keyword.
    Reserved(&'static str),
    /// Represents the end of file.
    Eof,
}

pub type Token = Annot<TokenKind>;

impl Token {
    pub fn with_num(num: usize, loc: Loc) -> Self {
        Self {
            data: TokenKind::Num(num),
            loc,
        }
    }

    pub fn with_reserved(keyword: &'static str, loc: Loc) -> Self {
        Self {
            data: TokenKind::Reserved(keyword),
            loc,
        }
    }

    pub fn with_eof(loc: Loc) -> Self {
        Self {
            data: TokenKind::Eof,
            loc,
        }
    }
}
