use crate::tokenize::*;
use crate::*;

/// Represents unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    /// +
    Pos,
    /// -
    Neg,
    /// expr
    ExprStmt,
}

pub type UnOp = Annot<UnOpKind>;

impl UnOp {
    pub fn new(op: UnOpKind, loc: Loc) -> Self {
        Self { value: op, loc }
    }
}

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
    /// >
    Gt,
    /// >=
    Ge,
    /// =
    Assign,
}

type BinOp = Annot<BinOpKind>;

impl BinOp {
    /// Constructor.
    pub fn new(op: BinOpKind, loc: Loc) -> Self {
        Self { value: op, loc }
    }
}

/// Condition statements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CondKind {
    /// while
    While,
}

pub type Cond = Annot<CondKind>;

impl Cond {
    pub fn new(kind: CondKind, loc: Loc) -> Self {
        Self { value: kind, loc }
    }
}
/// Represents a node of the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// Integer
    Num(u64),
    /// Variable
    Var {
        /// the name of the variable
        name: &'static str,
        /// the offset of the variable from rbp
        offset: usize,
    },
    /// "return"
    Return {
        /// returned value
        val: Box<Node>,
    },
    /// Unary operator.
    UnOp {
        /// Operator type.
        op: UnOp,
        /// Operand.
        arg: Box<Node>,
    },
    /// Binary operator.
    BinOp {
        /// Operator type.
        op: BinOp,
        /// Left side of the operator.
        left: Box<Node>,
        /// Right side of the operator.
        right: Box<Node>,
    },
    /// Condition statemens.
    Cond {
        /// Condition type.
        kind: Cond,
        /// Reperesents condition.
        cond: Box<Node>,
        /// Executed statement when condition is passed.
        then: Box<Node>,
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

    pub fn with_var(name: &'static str, offset: usize, loc: Loc) -> Self {
        Self {
            value: NodeKind::Var { name, offset },
            loc,
        }
    }

    pub fn with_return(val: Node, ret_loc: Loc) -> Self {
        let loc = ret_loc.merge(&val.loc);
        Self {
            value: NodeKind::Return { val: Box::new(val) },
            loc,
        }
    }

    pub fn with_unop(op: UnOp, arg: Node) -> Self {
        let loc = op.loc.merge(&arg.loc);
        Self {
            value: NodeKind::UnOp {
                op: op,
                arg: Box::new(arg),
            },
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

    pub fn with_cond(kind: Cond, cond: Node, then: Node) -> Self {
        let loc = kind.loc.merge(&cond.loc).merge(&then.loc);
        Self {
            value: NodeKind::Cond {
                kind,
                cond: Box::new(cond),
                then: Box::new(then),
            },
            loc,
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    /// holds variales names and the offsets of them
    vars: Vec<&'static str>,
}

impl Parser {
    /// Constructor.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            vars: vec![],
        }
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

    /// Adds a new variable and returns the offset.
    pub fn add_var(&mut self, name: &'static str) -> usize {
        self.vars.push(name);
        Self::offset(self.vars.len() - 1)
    }

    /// Finds the same name variable and returns the offset.
    pub fn find_var(&self, name: &'static str) -> Option<usize> {
        self.vars
            .iter()
            .position(|&var| var == name)
            .map(|i| Self::offset(i))
    }

    fn offset(index: usize) -> usize {
        (index + 1) * MEMORY_SIZE
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

    /// primary = num | ident | "(" expr ")"
    pub fn primary(&mut self) -> Result<Node> {
        let tok_loc = self.tok().loc;
        if self.consume(b"(") {
            let ret = self.expr()?;
            self.expect(b")")?;
            Ok(ret)
        } else if let TokenKind::Ident(name) = self.tok().value {
            self.skip();
            let offset = match self.find_var(name) {
                Some(o) => o,
                None => self.add_var(name),
            };
            let ret = Node::with_var(name, offset, tok_loc);
            Ok(ret)
        } else {
            let ret = Node::with_num(self.expect_number()?, tok_loc);
            Ok(ret)
        }
    }

    /// unary = ( "+" | "-" )? primary
    pub fn unary(&mut self) -> Result<Node> {
        let tok_loc = self.tok().loc;
        let node = if self.consume(b"+") {
            Node::with_unop(UnOp::new(UnOpKind::Pos, tok_loc), self.primary()?)
        } else if self.consume(b"-") {
            Node::with_unop(UnOp::new(UnOpKind::Neg, tok_loc), self.primary()?)
        } else {
            self.primary()?
        };
        Ok(node)
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
                ret = Node::with_binop(BinOp::new(BinOpKind::Gt, tok_loc), ret, self.add()?);
            } else if self.consume(b">=") {
                ret = Node::with_binop(BinOp::new(BinOpKind::Ge, tok_loc), ret, self.add()?);
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

    /// assign = equality ( "=" assign )?
    pub fn assign(&mut self) -> Result<Node> {
        let mut ret = self.equality()?;
        let eq_loc = self.tok().loc;
        if self.consume(b"=") {
            ret = Node::with_binop(BinOp::new(BinOpKind::Assign, eq_loc), ret, self.assign()?);
        }
        Ok(ret)
    }

    /// expr = assign
    pub fn expr(&mut self) -> Result<Node> {
        self.assign()
    }

    /// stmt = ( "return" )? expr ";"
    ///      | "while" "(" expr ")" stmt
    pub fn stmt(&mut self) -> Result<Node> {
        let ret = match self.tok().value {
            TokenKind::Reserved(b"return") => {
                let ret_loc = self.tok().loc;
                self.skip();
                let ret_val = self.expr()?;
                self.expect(b";")?;
                Node::with_return(ret_val, ret_loc)
            }
            TokenKind::Reserved(b"while") => {
                let while_loc = self.tok().loc;
                self.skip();
                self.expect(b"(")?;
                let cond = self.expr()?;
                self.expect(b")")?;
                let then = self.stmt()?;
                Node::with_cond(Cond::new(CondKind::While, while_loc), cond, then)
            }
            _ => {
                let expr = self.expr()?;
                self.expect(b";")?;
                Node::with_unop(UnOp::new(UnOpKind::ExprStmt, expr.loc), expr)
            }
        };
        Ok(ret)
    }

    /// program = stmt*
    pub fn program(&mut self) -> Result<Vec<Node>> {
        let mut nodes = vec![];
        while self.tok().value != TokenKind::Eof {
            nodes.push(self.stmt()?);
        }
        Ok(nodes)
    }

    /// Returns parse result, the nodes and the number of variables.
    pub fn parse(&mut self) -> Result<(Vec<Node>, usize)> {
        Ok((self.program()?, self.vars.len()))
    }
}
