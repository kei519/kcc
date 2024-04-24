use crate::tokenize::*;
use crate::*;

/// Respresents binary operators.
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
    /// ==
    Eq,
    /// !=
    Ne,
    /// <
    Lt,
    /// <=
    Le,
}

type BinOp = Annot<BinOpKind>;

impl BinOp {
    /// Constructor.
    pub fn new(op: BinOpKind, loc: Loc) -> Self {
        Self { value: op, loc }
    }
}

/// Represents a node of the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// Integer
    Num(u64),
    /// Binary operator.
    BinOp {
        /// Operator type.
        op: BinOp,
        /// Left side of the operator.
        left: Box<Node>,
        /// Right side of the operator.
        right: Box<Node>,
    },
}

pub type Node = Annot<NodeKind>;

impl Node {
    pub fn with_num(num: u64, loc: Loc) -> Self {
        Self {
            value: NodeKind::Num(num),
            loc,
        }
    }

    pub fn with_binop(op: BinOp, left: Node, right: Node) -> Self {
        let loc = left.loc.merge(&op.loc).merge(&right.loc);
        Self {
            value: NodeKind::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
            loc,
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    /// Constructor.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    /// The shared reference of the current token.
    pub fn tok(&self) -> &Token {
        if self.pos >= self.tokens.len() {
            let eof_pos = self.tokens[self.tokens.len() - 1].loc.end;
            Box::leak(Box::new(Token {
                value: TokenKind::Eof,
                loc: Loc {
                    start: eof_pos,
                    end: eof_pos + 1,
                },
            }))
        } else {
            &self.tokens[self.pos]
        }
    }

    /// Skip one token.
    pub fn skip(&mut self) {
        self.pos += 1
    }

    /// Reads a token when it is the expected char,
    /// otherwise returns an error.
    pub fn expect(&mut self, op: &[u8]) -> Result<()> {
        use std::str::from_utf8;

        match self.tok().value {
            TokenKind::Reserved(kw) if op == kw => {
                self.skip();
                Ok(())
            }
            _ => Err(Error {
                value: ErrorKind::Error(format!("not '{}'", from_utf8(op).unwrap()).leak()),
                loc: self.tok().loc,
            }),
        }
    }

    /// Reads a token when it is a number,
    /// otherwise returns an error.
    pub fn expect_number(&mut self) -> Result<u64> {
        match self.tok().value {
            TokenKind::Num(n) => {
                self.skip();
                Ok(n)
            }
            _ => Err(Error {
                value: ErrorKind::Error("not a number"),
                loc: self.tok().loc,
            }),
        }
    }

    /// Read a token when it is the expected char and returns true,
    /// otherwise returns false.
    pub fn consume(&mut self, op: &[u8]) -> bool {
        match self.tok().value {
            TokenKind::Reserved(kw) if kw == op => {
                self.skip();
                true
            }
            _ => false,
        }
    }

    /// primary = num | "(" expr ")"
    pub fn primary(&mut self) -> Result<Node> {
        let tok_loc = self.tok().loc;
        if self.consume(b"(") {
            let ret = self.expr()?;
            self.expect(b")")?;
            Ok(ret)
        } else {
            let ret = Node::with_num(self.expect_number()?, tok_loc);
            Ok(ret)
        }
    }

    /// unary = ( "+" | "-" )? primary
    pub fn unary(&mut self) -> Result<Node> {
        // FIXME: Two operators, "+" and "-", are not binary operator.
        // So the loc of this function return value is NOT correct.
        if self.consume(b"+") {
            Ok(self.primary()?)
        } else if self.consume(b"-") {
            let img_pos = Loc { start: 0, end: 0 };
            Ok(Node::with_binop(
                BinOp::new(BinOpKind::Sub, img_pos),
                Node::with_num(0, img_pos),
                self.primary()?,
            ))
        } else {
            Ok(self.primary()?)
        }
    }

    /// mul = unary ( "*" unary | "/" unary)*
    pub fn mul(&mut self) -> Result<Node> {
        let mut ret = self.unary()?;

        loop {
            let tok_loc = self.tok().loc;
            if self.consume(b"*") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Mul, tok_loc), ret, self.unary()?);
            } else if self.consume(b"/") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Div, tok_loc), ret, self.unary()?);
            } else {
                return Ok(ret);
            }
        }
    }

    /// add = mul ( "+" mul | "-" mul)*
    pub fn add(&mut self) -> Result<Node> {
        let mut ret = self.mul()?;

        loop {
            let tok_loc = self.tok().loc;
            if self.consume(b"+") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Add, tok_loc), ret, self.mul()?);
            } else if self.consume(b"-") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Sub, tok_loc), ret, self.mul()?);
            } else {
                return Ok(ret);
            }
        }
    }

    /// relational = add ( "<" add | "<=" add | ">" add | ">=" add )*
    pub fn relational(&mut self) -> Result<Node> {
        let mut ret = self.add()?;

        loop {
            let tok_loc = self.tok().loc;
            if self.consume(b"<") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Lt, tok_loc), ret, self.add()?);
            } else if self.consume(b"<=") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Le, tok_loc), ret, self.add()?);
            } else if self.consume(b">") {
                // FIXME: This loc infos are not correct.
                ret = Node::with_binop(BinOp::new(BinOpKind::Lt, tok_loc), self.add()?, ret);
            } else if self.consume(b">=") {
                // FIXME: So are this ones
                ret = Node::with_binop(BinOp::new(BinOpKind::Le, tok_loc), self.add()?, ret);
            } else {
                return Ok(ret);
            }
        }
    }

    /// equality = relational ( "==" relational | "!=" relational)*
    pub fn equality(&mut self) -> Result<Node> {
        let mut ret = self.relational()?;

        loop {
            let tok_loc = self.tok().loc;
            if self.consume(b"==") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Eq, tok_loc), ret, self.relational()?);
            } else if self.consume(b"!=") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Ne, tok_loc), ret, self.relational()?);
            } else {
                return Ok(ret);
            }
        }
    }

    /// expr = equality
    pub fn expr(&mut self) -> Result<Node> {
        self.equality()
    }
}
