use crate::{
    config::Config,
    error::{Error, Result},
    util::{Annot, Loc},
};

/// Represents token kinds.
pub enum TokenKind {
    /// Numeric literals
    Num(usize),
    /// Eod of file.
    Eof,
}

pub type Token = Annot<TokenKind>;

impl Token {
    pub fn from_num(num: usize, loc: Loc) -> Self {
        Self {
            kind: TokenKind::Num(num),
            loc,
        }
    }

    pub fn eof(loc: Loc) -> Self {
        Self {
            kind: TokenKind::Eof,
            loc,
        }
    }
}

pub struct Tokenizer {
    pub config: Config,
    pub pos: usize,
}

impl Tokenizer {
    pub fn new(config: Config) -> Self {
        Self { config, pos: 0 }
    }

    /// Returns byte array from the cursor.
    pub fn cur(&self) -> &[u8] {
        &self.config.input().as_bytes()[self.pos..]
    }

    /// Returns byte length of the input.
    pub fn len(&self) -> usize {
        self.config.input().len()
    }

    /// Returns the number of ramains of bytes.
    pub fn num_rem(&self) -> usize {
        self.len() - self.pos
    }

    /// Forwards the cursor one byte and returns `true`
    /// when the cursor does not reache the end of input.
    /// Otherwise, does nothing and returns `false`.
    pub fn forward(&mut self) -> bool {
        if self.pos < self.len() {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    /// Converts input as digit from head and returns read digit as [Token] if the head is digit
    /// and remains of input.
    /// Otherwise, returns error and the input.
    pub fn str_to_num(&mut self) -> Result<Token> {
        if self.num_rem() == 0 || !self.cur()[0].is_ascii_digit() {
            return Err(Error::new(
                "digit is required".into(),
                Loc::new(self.len() - 1, self.len()),
            ));
        }

        let start = self.pos;
        let mut ret = 0;
        while self.cur().len() != 0 && self.cur()[0].is_ascii_digit() {
            ret *= 10;
            ret += (self.cur()[0] - b'0') as usize;
            self.forward();
        }
        Ok(Token::from_num(ret, Loc::new(start, self.pos)))
    }
}
