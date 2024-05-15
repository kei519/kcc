#[cfg(test)]
mod tests;

use crate::{
    tokenize::{Token, TokenKind},
    util::{Annot, Error, Loc, Result},
};

pub struct Parser {
    input: &'static str,
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(input: &'static str, tokens: Vec<Token>) -> Self {
        Self {
            input,
            tokens,
            pos: 0,
        }
    }

    /// Returns the current token.
    pub fn tok(&self) -> &Token {
        &self.tokens[self.pos]
    }

    /// Advances the cursor if the cursor will not exceed the eof.
    /// Returns `true` if the cursor is advanced.
    pub fn next(&mut self) -> bool {
        if self.tok().data != TokenKind::Eof {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    pub fn parse(mut self) -> Result<Vec<Node>> {
        let mut ret = vec![];

        if let TokenKind::Num(num) = self.tok().data {
            ret.push(Node::with_num(num, self.tok().loc));
        } else {
            return Err(Error::CompileError {
                message: "number is required".into(),
                input: self.input,
                loc: self.tok().loc,
            });
        }
        self.next();

        if TokenKind::Eof != self.tok().data {
            return Err(Error::CompileError {
                message: "extra token".into(),
                input: self.input,
                loc: self.tok().loc,
            });
        }

        Ok(ret)
    }
}

/// Represents a node in AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// Integer.
    Num(usize),
}

pub type Node = Annot<NodeKind>;

impl Node {
    pub fn with_num(num: usize, loc: Loc) -> Self {
        Self {
            data: NodeKind::Num(num),
            loc,
        }
    }
}
