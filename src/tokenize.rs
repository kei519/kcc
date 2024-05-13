use crate::{
    config::Config,
    util::{Annot, Error, Loc, Result},
};

pub struct Tokenizer {
    config: Config,
    pos: usize,
}

impl Tokenizer {
    pub fn new(config: Config) -> Self {
        Self { config, pos: 0 }
    }

    pub fn tokenize(mut self) -> Result<(Vec<Token>, Config)> {
        let mut ret = vec![];

        while !self.cur().is_empty() {
            let start = self.pos;

            if self.cur()[0].is_ascii_digit() {
                let num = self.str_to_num();
                let end = self.pos;
                ret.push(Token::from_num(num, Loc::new(start, end)));
                continue;
            } else {
                return Err(Error::from_compilation(
                    "unknown token".into(),
                    self.config.input,
                    Loc::new(self.pos, self.pos + 1),
                ));
            }
        }

        ret.push(Token::from_eof(Loc::new(self.pos, self.pos + 1)));
        Ok((ret, self.config))
    }

    /// Returns length of input bytes.
    pub fn len(&self) -> usize {
        self.config.input.len()
    }

    /// Forward the cursor if the cursor does not reach EOF.
    /// If forwards returns `true` otherwise returns `false`.
    fn forward(&mut self) -> bool {
        if self.pos < self.len() {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    /// Returns byte array from the cursor.
    fn cur(&self) -> &[u8] {
        &self.config.input.as_bytes()[self.pos..]
    }

    fn str_to_num(&mut self) -> usize {
        let mut ret = 0;
        while !self.cur().is_empty() && self.cur()[0].is_ascii_digit() {
            ret *= 10;
            ret += (self.cur()[0] - b'0') as usize;
            self.forward();
        }
        ret
    }
}

/// Represents token kinds.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// Numeric literals.
    Num(usize),
    Eof,
}

pub type Token = Annot<TokenKind>;

impl Token {
    fn from_num(num: usize, loc: Loc) -> Self {
        Self {
            data: TokenKind::Num(num),
            loc,
        }
    }

    pub fn from_eof(loc: Loc) -> Self {
        Self {
            data: TokenKind::Eof,
            loc,
        }
    }
}
