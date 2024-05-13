use std::str::pattern::Pattern;

use crate::{
    config::Config,
    tokenize::{Token, TokenKind},
    util::{Annot, Error, Loc, Result},
};

pub struct Parser {
    tokens: Vec<Token>,
    /// Token position where the parser is reading.
    pos: usize,
    config: Config,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, config: Config) -> Self {
        Self {
            tokens,
            pos: 0,
            config,
        }
    }

    fn tok(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn forward(&mut self) -> bool {
        if self.tok().data != TokenKind::Eof {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    pub fn parse(self) -> Result<Node> {
        let loc = self.tok().loc;
        let TokenKind::Num(num) = self.tok().data else {
            return Err(Error::from_compilation(
                "integer is required".into(),
                self.config.input,
                self.tok().loc,
            ));
        };
        self.forward();
        if !matches!(self.tok().data, TokenKind::Eof) {
            return Err(Error::from_compilation(
                "only integer is accepted".into(),
                self.config.input,
                self.tok().loc,
            ));
        }
        Ok(Node::from_num(num, loc))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// Integer.
    Num(usize),
}

pub type Node = Annot<NodeKind>;

impl Node {
    fn from_num(num: usize, loc: Loc) -> Self {
        Self {
            data: NodeKind::Num(num),
            loc,
        }
    }
}
