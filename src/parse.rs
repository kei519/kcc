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
    /// &
    Addr,
    /// *
    Deref,
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
    /// num + num
    Add,
    /// ptr + num or num + ptr
    PtrAdd,
    /// num - num
    Sub,
    /// ptr - num
    PtrSub,
    /// ptr - ptr
    PtrDiff,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CondKind {
    /// while
    While {
        /// Condition.
        cond: Box<Node>,
        /// Executed statement when condition is passed.
        then: Box<Node>,
    },
    /// if
    If {
        /// Condition.
        cond: Box<Node>,
        /// Executed statement when condition is passed.
        then: Box<Node>,
        /// Else statement.
        els: Option<Box<Node>>,
    },
    /// for
    For {
        /// Initialize statement.
        init: Option<Box<Node>>,
        /// Condition.
        cond: Option<Box<Node>>,
        /// Increment statement.
        inc: Option<Box<Node>>,
        /// Executed statement when condition is passed.
        then: Box<Node>,
    },
}

pub type Cond = Annot<CondKind>;

impl Cond {
    pub fn with_while(loc: Loc, cond: Node, then: Node) -> Self {
        let loc = loc.merge(&cond.loc).merge(&then.loc);
        Self {
            value: CondKind::While {
                cond: Box::new(cond),
                then: Box::new(then),
            },
            loc,
        }
    }

    pub fn with_if(loc: Loc, cond: Node, then: Node, els: Option<Node>) -> Self {
        let loc = loc.merge(&cond.loc).merge(&then.loc);
        let (loc, els) = match els {
            Some(e) => (loc.merge(&e.loc), Some(Box::new(e))),
            None => (loc, None),
        };

        Self {
            value: CondKind::If {
                cond: Box::new(cond),
                then: Box::new(then),
                els,
            },
            loc,
        }
    }

    pub fn with_for(
        loc: Loc,
        init: Option<Node>,
        cond: Option<Node>,
        inc: Option<Node>,
        then: Node,
    ) -> Self {
        // the executed statement always follow the init, cond and inc statement.
        let loc = loc.merge(&then.loc);
        Self {
            value: CondKind::For {
                init: init.map(|e| Box::new(e)),
                cond: cond.map(|e| Box::new(e)),
                inc: inc.map(|e| Box::new(e)),
                then: Box::new(then),
            },
            loc,
        }
    }
}

/// Represents variables names and types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarKind {
    /// The name of the variable.
    pub name: &'static str,
    /// The type of the variable.
    pub ty: Type,
}

type Var = Annot<VarKind>;

impl Var {
    /// Constructor.
    fn new(name: &'static str, ty: Type, loc: Loc) -> Self {
        Self {
            value: VarKind { name, ty },
            loc,
        }
    }
}

/// Represents a node of the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// Integer
    Num(u64),
    /// Variable
    Var(Var),
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
    Cond(Cond),
    /// Block.
    Block {
        /// Statements thet composes the block.
        stmts: Vec<Node>,
    },
    /// Call function.
    FnCall {
        /// The name of the function called.
        name: &'static str,
        /// The arguments passed to the function.
        args: Vec<Node>,
    },
    /// Define function.
    FnDef {
        /// Name of the defined function.
        name: &'static str,
        /// Parameters passed by a caller.
        params: Vec<VarKind>,
        /// Local variable defined in the function.
        locals: Vec<VarKind>,
        /// Statements
        stmts: Vec<Node>,
    },
    /// Empty statement.
    Null,
}

pub type Node = Annot<NodeKind>;

impl Node {
    pub fn with_num(num: u64, loc: Loc) -> Self {
        Self {
            value: NodeKind::Num(num),
            loc,
        }
    }

    pub fn with_var(var: Var) -> Self {
        let loc = var.loc;
        Self {
            value: NodeKind::Var(var),
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

    pub fn with_decl(loc: Loc) -> Self {
        Self {
            value: NodeKind::Null,
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

    pub fn with_cond(cond: Cond) -> Self {
        let loc = cond.loc;
        Self {
            value: NodeKind::Cond(cond),
            loc: loc,
        }
    }

    pub fn with_block(stmts: Vec<Node>, loc: Loc) -> Self {
        Self {
            value: NodeKind::Block { stmts },
            loc,
        }
    }

    pub fn with_fn_call(name: &'static str, args: Vec<Node>, loc: Loc) -> Self {
        Self {
            value: NodeKind::FnCall { name, args },
            loc,
        }
    }

    pub fn with_fn_def(
        name: &'static str,
        params: Vec<VarKind>,
        locals: Vec<VarKind>,
        stmts: Vec<Node>,
        loc: Loc,
    ) -> Self {
        Self {
            value: NodeKind::FnDef {
                name,
                params,
                locals,
                stmts,
            },
            loc,
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    /// Parameters of a function that is parsing.
    params: Vec<VarKind>,
    /// Local variables of a function that is parsing.
    locals: Vec<VarKind>,
}

impl Parser {
    /// Constructor.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            params: vec![],
            locals: vec![],
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

    /// Adds a new variable.
    pub fn add_var(&mut self, name: &'static str, ty: Type) {
        self.locals.push(VarKind { name, ty });
    }

    /// Finds the same name variable and returns the type.
    pub fn find_var(&self, name: &'static str) -> Option<Type> {
        self.params
            .iter()
            .chain(self.locals.iter())
            .find(|&v| v.name == name)
            .map(|v| v.ty.clone())
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

    /// Reads a token when it is the expected char and returns true,
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

    /// Reads an indentifier name when the token is ident,
    /// otherwise returns an Error.
    pub fn consume_ident(&mut self) -> Result<&'static str> {
        let TokenKind::Ident(name) = self.tok().value else {
            return Err(Error {
                value: ErrorKind::Error("some name is required"),
                loc: self.tok().loc,
            });
        };
        self.skip();
        Ok(name)
    }

    /// Reads parameters of a function followed by ")" and returns a list of them.
    /// When some other tokens is shown, returns Error.
    pub fn read_func_params(&mut self) -> Result<()> {
        if self.consume(b")") {
            return Ok(());
        }

        let ty = self.basetype()?;
        let name = self.consume_ident()?;
        self.params.push(VarKind { name, ty });

        while !self.consume(b")") {
            self.expect(b",")?;
            let ty = self.basetype()?;
            let name = self.consume_ident()?;
            self.params.push(VarKind { name, ty });
        }

        Ok(())
    }

    pub fn read_type_suffix(&mut self, base: Type) -> Result<Type> {
        if !self.consume(b"[") {
            return Ok(base);
        }
        let sz = self.expect_number()?;
        self.expect(b"]")?;
        Ok(Type::with_array(base, sz as usize))
    }

    /// basetype = "int" "*"*
    fn basetype(&mut self) -> Result<Type> {
        let loc = self.tok().loc;
        if let TokenKind::Ident(type_name) = self.tok().value {
            if type_name == "int" {
                self.skip();
                let mut ty = Type::new(TypeKind::Int);
                while self.consume(b"*") {
                    ty = Type::with_ptr(Type::new(TypeKind::Int));
                }
                return Ok(ty);
            }
        }
        return Err(Error {
            value: ErrorKind::Error("This is not type name"),
            loc,
        });
    }

    /// primary = num
    ///         | ident ( "(" params? ")" )?
    ///         | "(" expr ")"
    pub fn primary(&mut self) -> Result<Node> {
        let tok_loc = self.tok().loc;
        let ret = if self.consume(b"(") {
            let ret = self.expr()?;
            self.expect(b")")?;
            ret
        } else if let TokenKind::Ident(name) = self.tok().value {
            self.skip();
            if self.consume(b"(") {
                // function call
                let mut args = vec![];
                if let Ok(expr) = self.expr() {
                    args.push(expr);
                    while self.consume(b",") {
                        args.push(self.expr()?);
                    }
                }
                let loc = tok_loc.merge(&self.tok().loc);
                self.expect(b")")?;
                Node::with_fn_call(name, args, loc)
            } else {
                // variable
                let Some(ty) = self.find_var(name) else {
                    return Err(Error {
                        value: ErrorKind::Error("undefined variable"),
                        loc: tok_loc,
                    });
                };
                Node::with_var(Var::new(name, ty, tok_loc))
            }
        } else {
            let ret = Node::with_num(self.expect_number()?, tok_loc);
            ret
        };
        Ok(ret)
    }

    /// unary = ( "+" | "-" )? primary
    ///       | "*" unary
    ///       | "&" unary
    pub fn unary(&mut self) -> Result<Node> {
        let tok_loc = self.tok().loc;
        let node = if self.consume(b"+") {
            Node::with_unop(UnOp::new(UnOpKind::Pos, tok_loc), self.primary()?)
        } else if self.consume(b"-") {
            Node::with_unop(UnOp::new(UnOpKind::Neg, tok_loc), self.primary()?)
        } else if self.consume(b"*") {
            Node::with_unop(UnOp::new(UnOpKind::Deref, tok_loc), self.unary()?)
        } else if self.consume(b"&") {
            Node::with_unop(UnOp::new(UnOpKind::Addr, tok_loc), self.unary()?)
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

    /// declaration = basetype ident ( "[" num "]" )* ("=" expr)? ";"
    pub fn declaration(&mut self) -> Result<Node> {
        let loc = self.tok().loc;
        let ty = self.basetype()?;
        let name = self.consume_ident()?;
        let ty = self.read_type_suffix(ty)?;
        self.add_var(name, ty.clone());

        {
            let loc = loc.merge(&self.tok().loc);
            if self.consume(b";") {
                return Ok(Node::with_decl(loc));
            }
        }

        let left = Node::with_var(Var::new(name, ty, loc));
        self.expect(b"=")?;
        let right = self.expr()?;
        let loc = loc.merge(&self.tok().loc);
        self.expect(b";")?;

        let node = Node::with_binop(BinOp::new(BinOpKind::Assign, loc), left, right);
        Ok(Node::with_unop(UnOp::new(UnOpKind::ExprStmt, loc), node))
    }

    /// expr = assign
    pub fn expr(&mut self) -> Result<Node> {
        self.assign()
    }

    /// stmt = ( "return" )? expr ";"
    ///      | "{" stmt* "}"
    ///      | "while" "(" expr ")" stmt
    ///      | "if" "(" expr ")" stmt ( "else" stmt )?
    ///      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    ///      | declaration
    pub fn stmt(&mut self) -> Result<Node> {
        let loc = self.tok().loc;
        let ret = match self.tok().value {
            TokenKind::Reserved(b"return") => {
                self.skip();
                let ret_val = self.expr()?;
                self.expect(b";")?;
                Node::with_return(ret_val, loc)
            }
            TokenKind::Reserved(b"while") => {
                self.skip();
                self.expect(b"(")?;
                let cond = self.expr()?;
                self.expect(b")")?;
                let then = self.stmt()?;
                Node::with_cond(Cond::with_while(loc, cond, then))
            }
            TokenKind::Reserved(b"if") => {
                self.skip();
                self.expect(b"(")?;
                let cond = self.expr()?;
                self.expect(b")")?;
                let then = self.stmt()?;

                if self.consume(b"else") {
                    Node::with_cond(Cond::with_if(loc, cond, then, Some(self.stmt()?)))
                } else {
                    Node::with_cond(Cond::with_if(loc, cond, then, None))
                }
            }
            TokenKind::Reserved(b"for") => {
                self.skip();
                self.expect(b"(")?;
                let init = self.expr().ok();
                self.expect(b";")?;
                let cond = self.expr().ok();
                self.expect(b";")?;
                let inc = self.expr().ok();
                self.expect(b")")?;
                Node::with_cond(Cond::with_for(loc, init, cond, inc, self.stmt()?))
            }
            TokenKind::Reserved(b"{") => {
                self.skip();
                let mut stmts = vec![];
                while let Ok(stmt) = self.stmt() {
                    stmts.push(stmt);
                }
                // A block location should be between "{" and "}".
                let loc = loc.merge(&self.tok().loc);
                self.expect(b"}")?;
                Node::with_block(stmts, loc)
            }
            _ => {
                // checks whether it is the declaration of the variable.
                if let Ok(node) = self.declaration() {
                    node
                } else {
                    let expr = self.expr()?;
                    self.expect(b";")?;
                    Node::with_unop(UnOp::new(UnOpKind::ExprStmt, expr.loc), expr)
                }
            }
        };
        Ok(ret)
    }

    /// function = basetype ident "(" params? ")" "{" stmt* "}"
    /// params = basetype ( "," param )*
    /// param = basetype ident
    pub fn function(&mut self) -> Result<Node> {
        use std::mem::replace;

        let func_start_loc = self.tok().loc;
        self.basetype()?;

        let func_name = self.consume_ident()?;
        self.expect(b"(")?;
        self.read_func_params()?;
        self.expect(b"{")?;
        let mut stmts = vec![];
        while let Ok(node) = self.stmt() {
            stmts.push(node);
        }
        let loc = func_start_loc.merge(&self.tok().loc);
        self.expect(b"}")?;
        let params = replace(&mut self.params, vec![]);
        let locals = replace(&mut self.locals, vec![]);
        Ok(Node::with_fn_def(func_name, params, locals, stmts, loc))
    }

    /// program = function*
    pub fn program(&mut self) -> Result<Vec<Node>> {
        let mut nodes = vec![];
        while self.tok().value != TokenKind::Eof {
            nodes.push(self.function()?);
        }
        Ok(nodes)
    }
}
