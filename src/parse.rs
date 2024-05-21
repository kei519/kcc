#[cfg(test)]
mod tests;

use std::{collections::HashSet, mem};

use crate::{
    tokenize::{Token, TokenKind},
    typing::Type,
    util::{Annot, Error, Loc, Result},
};

pub struct Parser {
    input: &'static str,
    tokens: Vec<Token>,
    locals: HashSet<Var>,
    pos: usize,
}

impl Parser {
    pub fn new(input: &'static str, tokens: Vec<Token>) -> Self {
        Self {
            input,
            tokens,
            locals: HashSet::new(),
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
    /// program = function*
    /// ```
    pub fn parse(mut self) -> Result<Node> {
        let mut funcs = vec![];

        while TokenKind::Eof != self.tok().data {
            funcs.push(self.function()?);
        }

        Ok(Node::with_prog(funcs))
    }

    /// Consumes the current token if it matches a given string.
    fn consume(&mut self, s: &str) -> bool {
        if self.peek(s) {
            self.next();
            true
        } else {
            false
        }
    }

    /// Returns `true` if the current token matches a given string.
    fn peek(&self, s: &str) -> bool {
        match self.tok().data {
            TokenKind::Reserved(kw) if kw == s => true,
            _ => false,
        }
    }

    /// Ensures that the current token is a given string.
    fn expect(&mut self, s: &str) -> Result<()> {
        if !self.consume(s) {
            return Err(Error::CompileError {
                message: format!("expected '{}'", s),
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

    fn expect_ident(&mut self) -> Result<&'static str> {
        self.consume_ident().ok_or(Error::CompileError {
            message: "identifier is required".into(),
            input: self.input,
            loc: self.tok().loc,
        })
    }

    /// ```text
    /// basetype = "int" "*"*
    /// ```
    fn basetype(&mut self) -> Result<Type> {
        self.expect("int")?;
        let mut ty = Type::Int;

        while self.consume("*") {
            ty = Type::Ptr(Box::new(ty));
        }

        Ok(ty)
    }

    /// ```text
    /// func-args = ( assign ( "," assign )* )?
    /// ```
    fn func_args(&mut self) -> Result<Vec<Node>> {
        let Ok(arg) = self.assign() else {
            return Ok(vec![]);
        };
        let mut args = vec![arg];

        while self.consume(",") {
            args.push(self.assign()?);
        }

        Ok(args)
    }

    /// ```text
    /// param = basetype ident
    /// ```
    fn param(&mut self) -> Result<Var> {
        let ty = self.basetype()?;
        let name = self.expect_ident()?;
        // Pushes in locals because parameters are the same as locals.
        self.locals.insert(Var::new(name, ty.clone()));
        Ok(Var::new(name, ty))
    }

    /// ```text
    /// params = param ( "," param )*
    /// ```
    fn params(&mut self) -> Result<Vec<Var>> {
        let Ok(param) = self.param() else {
            return Ok(vec![]);
        };
        let mut params = vec![param];

        while self.consume(",") {
            params.push(self.param()?);
        }
        Ok(params)
    }

    /// ```text
    /// primary = "(" expr ")" | ident ( "(" func-args ")" )? | num
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
            if self.consume("(") {
                let args = self.func_args()?;

                // The location of the function call is between function name
                // and close parentesis.
                let loc = loc + self.tok().loc;
                self.expect(")")?;

                return Ok(Node::with_fn_call(name, args, loc));
            }

            if !self.locals.iter().any(|var| var.name == name) {
                return Err(Error::CompileError {
                    message: "this variable is not declared".into(),
                    input: self.input,
                    loc: loc,
                });
            }
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
    /// declaration = basetype ident ( "=" expr )? ";"
    /// ```
    fn decl(&mut self) -> Result<Node> {
        let loc = self.tok().loc;

        let ty = self.basetype()?;

        let name_loc = self.tok().loc;
        let Some(name) = self.consume_ident() else {
            return Err(Error::CompileError {
                message: "variable name is required".into(),
                input: self.input,
                loc: self.tok().loc,
            });
        };
        let var = Node::with_var(name, name_loc);

        let init = if self.consume("=") {
            Some(self.expr()?)
        } else {
            None
        };

        let loc = loc + self.tok().loc;
        self.expect(";")?;

        // Declaration of the same name variable multiple times is not allowed.
        if !self.locals.insert(Var::new(name, ty.clone())) {
            return Err(Error::CompileError {
                message: "this variable is already declared".into(),
                input: self.input,
                loc: name_loc,
            });
        };

        Ok(Node::with_var_decl(var, ty, init, loc))
    }

    /// ```text
    /// stmt = declaration
    ///      | block = "{" stmt* "}"
    ///      | ( "return" )? expr ";"
    ///      | "while" "(" expr ")" stmt
    ///      | "for" "(" expr? ";" expr? ";" expr ")" stmt
    ///      | "if" "(" expr ")" ( "else" "if" "(" expr ")" stmt )* ( "else" stmt )?
    /// ```
    fn stmt(&mut self) -> Result<Node> {
        if self.peek("int") {
            return self.decl();
        }

        let loc = self.tok().loc;

        // Recognizes a block.
        if self.consume("{") {
            let mut stmts = vec![];
            let brace_loc = loc;
            let mut loc = loc + self.tok().loc;

            while !self.consume("}") {
                if self.tok().data == TokenKind::Eof {
                    return Err(Error::CompileError {
                        message: "this brace is not closed".into(),
                        input: self.input,
                        loc: brace_loc,
                    });
                }
                stmts.push(self.stmt()?);
                loc += self.tok().loc;
            }
            return Ok(Node::with_block(stmts, loc));
        } else if self.consume("while") {
            self.expect("(")?;
            let cond = self.expr()?;
            self.expect(")")?;

            let stmt = self.stmt()?;
            let loc = loc + stmt.loc;

            return Ok(Node::with_while(cond, stmt, loc));
        } else if self.consume("for") {
            self.expect("(")?;

            let init = self.expr().ok();
            self.expect(";")?;

            let cond = self.expr().ok();
            self.expect(";")?;

            let inc = self.expr().ok();
            self.expect(")")?;

            let stmt = self.stmt()?;

            let loc = loc + stmt.loc;
            return Ok(Node::with_for(init, cond, inc, stmt, loc));
        } else if self.consume("if") {
            self.expect("(")?;
            let cond = self.expr()?;
            self.expect(")")?;
            let stmt = self.stmt()?;

            let mut elif_conds = vec![];
            let mut elif_stmts = vec![];
            let mut has_else = false;

            while self.consume("else") {
                has_else = true;
                if !self.consume("if") {
                    break;
                }
                has_else = false;

                self.expect("(")?;
                elif_conds.push(self.expr()?);
                self.expect(")")?;

                elif_stmts.push(self.stmt()?);
            }

            let else_stmt = if has_else { Some(self.stmt()?) } else { None };

            let loc = if else_stmt.is_some() {
                // This unwrapping always succeeds due to the condition.
                loc + else_stmt.as_ref().unwrap().loc
            } else if !elif_stmts.is_empty() {
                // This unwrapping also always succeeds due to the condition.
                loc + elif_stmts.last().unwrap().loc
            } else {
                loc + stmt.loc
            };

            return Ok(Node::with_if(
                cond, stmt, elif_conds, elif_stmts, else_stmt, loc,
            ));
        }

        let node = if self.consume("return") {
            Node::with_unop(UnOpKind::Return, self.expr()?, loc)
        } else {
            Node::with_unop(UnOpKind::Expr, self.expr()?, loc)
        };
        self.expect(";")?;

        Ok(node)
    }

    /// ```text
    /// function = basetype ident "(" params? ")" "{" stmt* "}"
    /// ```
    fn function(&mut self) -> Result<Node> {
        let loc = self.tok().loc;

        let ty = self.basetype()?;
        let name = self.expect_ident()?;

        self.expect("(")?;
        let params = self.params()?;
        self.expect(")")?;

        self.expect("{")?;
        let mut stmts = vec![];

        let loc = loop {
            let loc = loc + self.tok().loc;
            if self.consume("}") {
                break loc;
            }
            stmts.push(self.stmt()?);
        };

        for param in &params {
            self.locals.remove(param);
        }
        let locals = mem::replace(&mut self.locals, HashSet::new())
            .into_iter()
            .collect();
        Ok(Node::with_fn(name, ty, params, stmts, locals, loc))
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
    /// Variable declaration.
    VarDecl {
        var: Box<Node>,
        ty: Type,
        /// Initial value.
        init: Option<Box<Node>>,
    },
    /// Variable.
    Var(&'static str),
    /// Block.
    Block { stmts: Vec<Node> },
    /// while.
    While { cond: Box<Node>, stmt: Box<Node> },
    /// for.
    For {
        init: Option<Box<Node>>,
        cond: Option<Box<Node>>,
        inc: Option<Box<Node>>,
        stmt: Box<Node>,
    },
    /// if.
    If {
        cond: Box<Node>,
        stmt: Box<Node>,
        elif_conds: Vec<Node>,
        elif_stmts: Vec<Node>,
        else_stmt: Option<Box<Node>>,
    },
    /// Function call.
    FnCall { name: &'static str, args: Vec<Node> },
    /// Function.
    Fn {
        name: &'static str,
        return_ty: Type,
        params: Vec<Var>,
        stmts: Vec<Node>,
        locals: Vec<Var>,
    },
    /// Whole program.
    Program { funcs: Vec<Node> },
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

    pub fn with_var_decl(var: Node, ty: Type, init: Option<Node>, loc: Loc) -> Self {
        Self {
            data: NodeKind::VarDecl {
                var: Box::new(var),
                ty,
                init: init.map(|node| Box::new(node)),
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

    pub fn with_block(stmts: Vec<Node>, loc: Loc) -> Self {
        Self {
            data: NodeKind::Block { stmts },
            loc,
        }
    }

    pub fn with_while(cond: Node, stmt: Node, loc: Loc) -> Self {
        Self {
            data: NodeKind::While {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
            },
            loc,
        }
    }

    pub fn with_for(
        init: Option<Node>,
        cond: Option<Node>,
        inc: Option<Node>,
        stmt: Node,
        loc: Loc,
    ) -> Self {
        Self {
            data: NodeKind::For {
                init: init.map(|node| Box::new(node)),
                cond: cond.map(|node| Box::new(node)),
                inc: inc.map(|node| Box::new(node)),
                stmt: Box::new(stmt),
            },
            loc,
        }
    }

    /// # Remarks
    ///
    /// The size of `elif_conds` and the size of `elif_stmts` must be the same.
    pub fn with_if(
        cond: Node,
        stmt: Node,
        elif_conds: Vec<Node>,
        elif_stmts: Vec<Node>,
        else_stmt: Option<Node>,
        loc: Loc,
    ) -> Self {
        assert_eq!(elif_conds.len(), elif_stmts.len());

        Self {
            data: NodeKind::If {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
                elif_conds,
                elif_stmts,
                else_stmt: else_stmt.map(|node| Box::new(node)),
            },
            loc,
        }
    }

    pub fn with_fn_call(name: &'static str, args: Vec<Node>, loc: Loc) -> Self {
        Self {
            data: NodeKind::FnCall { name, args },
            loc,
        }
    }

    pub fn with_fn(
        name: &'static str,
        return_ty: Type,
        params: Vec<Var>,
        stmts: Vec<Node>,
        locals: Vec<Var>,
        loc: Loc,
    ) -> Self {
        Self {
            data: NodeKind::Fn {
                name,
                return_ty,
                params,
                stmts,
                locals,
            },
            loc,
        }
    }

    pub fn with_prog(funcs: Vec<Node>) -> Self {
        // If there are some functions, the location is the merge of the first and the last.
        // Otherwise, the location is at the begginning, 0.
        let loc = if let Some(first) = funcs.first() {
            if let Some(last) = funcs.last() {
                first.loc + last.loc
            } else {
                first.loc
            }
        } else {
            Loc::at(0)
        };

        Self {
            data: NodeKind::Program { funcs },
            loc,
        }
    }
}

/// Represets variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    /// Variable name.
    pub name: &'static str,
    /// Type.
    pub ty: Type,
    /// Variable offset from RBP.
    pub offset: usize,
}

impl Var {
    pub fn new(name: &'static str, ty: Type) -> Self {
        Self {
            name,
            ty,
            offset: 0,
        }
    }
}
