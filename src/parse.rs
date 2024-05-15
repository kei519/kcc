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

    /// ```text
    /// program = expr
    /// ```
    pub fn parse(mut self) -> Result<Vec<Node>> {
        let mut ret = vec![];

        ret.push(self.expr()?);

        if TokenKind::Eof != self.tok().data {
            return Err(Error::CompileError {
                message: "extra token".into(),
                input: self.input,
                loc: self.tok().loc,
            });
        }

        Ok(ret)
    }

    pub fn consume(&mut self, op: &str) -> bool {
        match self.tok().data {
            TokenKind::Reserved(kw) if kw == op => {
                self.next();
                true
            }
            _ => false,
        }
    }

    pub fn expect(&mut self, op: &str) -> Result<()> {
        if !self.consume(op) {
            return Err(Error::CompileError {
                message: format!("expected '{}'", op),
                input: self.input,
                loc: self.tok().loc,
            });
        }
        Ok(())
    }

    /// ```text
    /// primary = num
    /// ```
    pub fn primary(&mut self) -> Result<Node> {
        match self.tok().data {
            TokenKind::Num(num) => {
                let node = Node::with_num(num, self.tok().loc);
                self.next();
                Ok(node)
            }
            _ => Err(Error::CompileError {
                message: "number is required".into(),
                input: self.input,
                loc: self.tok().loc,
            }),
        }
    }

    /// ```text
    /// expr = primary ( "+" expr | "-" expr )?
    /// ```
    pub fn expr(&mut self) -> Result<Node> {
        let left = self.primary()?;

        if self.consume("+") {
            let right = self.expr()?;
            Ok(Node::with_binop(BinOpKind::Add, left, right))
        } else if self.consume("-") {
            let right = self.expr()?;
            Ok(Node::with_binop(BinOpKind::Sub, left, right))
        } else {
            Ok(left)
        }
    }
}

/// Represents a binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
}

/// Represents a node in AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// Integer.
    Num(usize),
    /// Binary operator.
    BinOp {
        op: BinOpKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}

pub type Node = Annot<NodeKind>;

impl Node {
    pub fn with_num(num: usize, loc: Loc) -> Self {
        Self {
            data: NodeKind::Num(num),
            loc,
        }
    }

    pub fn with_binop(op: BinOpKind, lhs: Node, rhs: Node) -> Self {
        // binop loc must be between lhs and rhs.
        let loc = lhs.loc + rhs.loc;

        Self {
            data: NodeKind::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            loc,
        }
    }
}
