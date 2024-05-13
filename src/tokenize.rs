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

    pub fn tokenize(self) -> Result<(Vec<Token>, Config)> {
        let ret = vec![];

        while !self.cur().is_empty() {
            return Err(Error::from_compilation(
                "only empty string is accepted".into(),
                self.config.input,
                Loc::new(self.pos, self.pos + 1),
            ));
        }

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
}

/// Represents token kinds.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Eof,
}

pub type Token = Annot<TokenKind>;

impl Token {
    pub fn from_eof(loc: Loc) -> Self {
        Self {
            data: TokenKind::Eof,
            loc,
        }
    }
}
