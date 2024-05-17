#[cfg(test)]
mod tests;

use std::collections::HashSet;

use crate::{
    tokenize::{Token, TokenKind},
    util::{Annot, Error, Loc, Result},
};

pub struct Parser {
    input: &'static str,
    tokens: Vec<Token>,
    global_vars: HashSet<Var>,
    pos: usize,
}

impl Parser {
    pub fn new(input: &'static str, tokens: Vec<Token>) -> Self {
        Self {
            input,
            tokens,
            global_vars: HashSet::new(),
            pos: 0,
        }
    }

    /// Returns the current token.
    fn tok(&self) -> &Token {
        &self.tokens[self.pos]
    }

    /// Advances the cursor if the cursor will not exceed the eof.
    /// Returns `true` if the cursor is advanced.
    fn next(&mut self) -> bool {
        if self.tok().data != TokenKind::Eof {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    /// Parses the whole program.
    ///
    /// ```text
    /// program = stmt*
    /// ```
    pub fn parse(mut self) -> Result<Node> {
        let mut stmts = vec![];

        while TokenKind::Eof != self.tok().data {
            stmts.push(self.stmt()?);
        }

        Ok(Node::with_prog(stmts, self.global_vars))
    }

    fn consume(&mut self, op: &str) -> bool {
        match self.tok().data {
            TokenKind::Reserved(kw) if kw == op => {
                self.next();
                true
            }
            _ => false,
        }
    }

    fn expect(&mut self, op: &str) -> Result<()> {
        if !self.consume(op) {
            return Err(Error::CompileError {
                message: format!("expected '{}'", op),
                input: self.input,
                loc: self.tok().loc,
            });
        }
        Ok(())
    }

    fn expect_num(&mut self) -> Result<usize> {
        if let TokenKind::Num(num) = self.tok().data {
            self.next();
            Ok(num)
        } else {
            Err(Error::CompileError {
                message: "number is required".into(),
                input: self.input,
                loc: self.tok().loc,
            })
        }
    }

    fn consume_ident(&mut self) -> Option<&'static str> {
        if let TokenKind::Ident(ident) = self.tok().data {
            self.next();
            Some(ident)
        } else {
            None
        }
    }

    /// ```text
    /// primary = "(" expr ")" | ident | num
    /// ```
    fn primary(&mut self) -> Result<Node> {
        let loc = self.tok().loc;

        let node = if self.consume("(") {
            let mut node = self.expr()?;

            // The location of the node surrounded by parentheses is
            // the merge of the both surrounding parentheses locations.
            let loc = loc + self.tok().loc;
            self.expect(")")?;
            node.loc = loc;

            node
        } else if let Some(name) = self.consume_ident() {
            self.global_vars.insert(Var::new(name));
            Node::with_var(name, loc)
        } else {
            Node::with_num(self.expect_num()?, loc)
        };

        Ok(node)
    }

    /// ```text
    /// unary = ( ("+" | "-") unary ) | primary
    /// ```
    fn unary(&mut self) -> Result<Node> {
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
    fn mul(&mut self) -> Result<Node> {
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
    fn add(&mut self) -> Result<Node> {
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
    fn relational(&mut self) -> Result<Node> {
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
    fn equality(&mut self) -> Result<Node> {
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
    /// assign = equality ( "=" assign )?
    /// ```
    fn assign(&mut self) -> Result<Node> {
        let left = self.equality()?;

        let node = if self.consume("=") {
            let right = self.assign()?;
            Node::with_binop(BinOpKind::Assign, left, right)
        } else {
            left
        };

        Ok(node)
    }

    /// ```text
    /// expr = assign
    /// ```
    fn expr(&mut self) -> Result<Node> {
        self.assign()
    }

    /// ```text
    /// stmt = ( "return" )? expr ";"
    /// ```
    fn stmt(&mut self) -> Result<Node> {
        let loc = self.tok().loc;
        let node = if self.consume("return") {
            Node::with_unop(UnOpKind::Return, self.expr()?, loc)
        } else {
            Node::with_unop(UnOpKind::Expr, self.expr()?, loc)
        };
        self.expect(";")?;

        Ok(node)
    }
}

/// Represents a unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    /// +
    Pos,
    /// -
    Neg,
    /// return
    Return,
    /// Expression statement.
    Expr,
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
    /// =
    Assign,
}

/// Represents a node in AST.
#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// Variable.
    Var(&'static str),
    /// Whole program.
    Program {
        stmts: Vec<Node>,
        global_vars: HashSet<Var>,
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

    pub fn with_var(name: &'static str, loc: Loc) -> Self {
        Self {
            data: NodeKind::Var(name),
            loc,
        }
    }

    pub fn with_prog(stmts: Vec<Node>, global_vars: HashSet<Var>) -> Self {
        // If there are some statements, the location is the merge of the first and the last.
        // Otherwise, the location is at the begginning, 0.
        let loc = if let Some(first) = stmts.first() {
            if let Some(last) = stmts.last() {
                first.loc + last.loc
            } else {
                first.loc
            }
        } else {
            Loc::at(0)
        };

        Self {
            data: NodeKind::Program { stmts, global_vars },
            loc,
        }
    }
}

/// Represets variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    /// Variable name.
    pub name: &'static str,
    /// Variable offset from RBP.
    pub offset: usize,
}

impl Var {
    pub fn new(name: &'static str) -> Self {
        Self { name, offset: 0 }
    }
}
