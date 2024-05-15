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
    /// primary = "(" expr ")" | num
    /// ```
    pub fn primary(&mut self) -> Result<Node> {
        if self.consume("(") {
            let node = self.expr()?;
            self.expect(")")?;
            Ok(node)
        } else if let TokenKind::Num(num) = self.tok().data {
            let node = Node::with_num(num, self.tok().loc);
            self.next();
            Ok(node)
        } else {
            Err(Error::CompileError {
                message: "number is required".into(),
                input: self.input,
                loc: self.tok().loc,
            })
        }
    }

    /// ```text
    /// unary = ( ("+" | "-") unary ) | primary
    /// ```
    pub fn unary(&mut self) -> Result<Node> {
        let loc = self.tok().loc;

        let node = if self.consume("+") {
            let operand = self.unary()?;
            Node::with_unop(UnOpKind::Pos, operand, loc)
        } else if self.consume("-") {
            let operand = self.unary()?;
            Node::with_unop(UnOpKind::Neg, operand, loc)
        } else {
            self.primary()?
        };

        Ok(node)
    }

    /// ```text
    /// mul = unary ( "*" mul | "/" mul )?
    /// ```
    pub fn mul(&mut self) -> Result<Node> {
        let left = self.unary()?;

        let node = if self.consume("*") {
            let right = self.mul()?;
            Node::with_binop(BinOpKind::Mul, left, right)
        } else if self.consume("/") {
            let right = self.mul()?;
            Node::with_binop(BinOpKind::Div, left, right)
        } else {
            left
        };

        Ok(node)
    }

    /// ```text
    /// add = mul ( "+" add | "-" add )?
    /// ```
    pub fn add(&mut self) -> Result<Node> {
        let left = self.mul()?;

        let node = if self.consume("+") {
            let right = self.add()?;
            Node::with_binop(BinOpKind::Add, left, right)
        } else if self.consume("-") {
            let right = self.add()?;
            Node::with_binop(BinOpKind::Sub, left, right)
        } else {
            left
        };

        Ok(node)
    }

    /// ```text
    /// relational = add ( "<" relational | "<=" relational | ">" relational | ">=" relational)?
    /// ```
    pub fn relational(&mut self) -> Result<Node> {
        let left = self.add()?;

        let node = if self.consume("<") {
            let right = self.relational()?;
            Node::with_binop(BinOpKind::Lt, left, right)
        } else if self.consume("<=") {
            let right = self.relational()?;
            Node::with_binop(BinOpKind::Le, left, right)
        } else if self.consume(">") {
            let right = self.relational()?;
            Node::with_binop(BinOpKind::Gt, left, right)
        } else if self.consume(">=") {
            let right = self.relational()?;
            Node::with_binop(BinOpKind::Ge, left, right)
        } else {
            left
        };

        Ok(node)
    }

    /// ```text
    /// equality = relational ( "==" equality | "!=" equality )?
    /// ```
    pub fn equality(&mut self) -> Result<Node> {
        let left = self.relational()?;

        let node = if self.consume("==") {
            let right = self.equality()?;
            Node::with_binop(BinOpKind::Eq, left, right)
        } else if self.consume("!=") {
            let right = self.equality()?;
            Node::with_binop(BinOpKind::Ne, left, right)
        } else {
            left
        };

        Ok(node)
    }

    /// ```text
    /// expr = equality
    /// ```
    pub fn expr(&mut self) -> Result<Node> {
        self.equality()
    }
}

/// Represents a unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    /// +
    Pos,
    /// -
    Neg,
}

/// Represents a binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
    /// <
    Lt,
    /// <=
    Le,
    /// >
    Gt,
    /// >=
    Ge,
    /// ==
    Eq,
    /// !=
    Ne,
}

/// Represents a node in AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// Integer.
    Num(usize),
    /// Unary operator.
    UnOp { op: UnOpKind, operand: Box<Node> },
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

    pub fn with_unop(op: UnOpKind, operand: Node, op_loc: Loc) -> Self {
        let loc = op_loc + operand.loc;
        Self {
            data: NodeKind::UnOp {
                op,
                operand: Box::new(operand),
            },
            loc: loc,
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
