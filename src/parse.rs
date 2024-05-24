use std::{cell::RefCell, mem, rc::Rc};

use crate::{
    tokenize::{Token, TokenKind},
    typing::{Member, Type, TypeKind},
    util::{Error, Loc, Result},
};

pub struct Parser {
    file_name: &'static str,
    input: &'static str,
    tokens: Vec<Token>,
    scope: Vec<Rc<RefCell<Var>>>,
    locals: Vec<Rc<RefCell<Var>>>,
    globals: Vec<Rc<RefCell<Var>>>,
    num_label: usize,
    pos: usize,
}

impl Parser {
    pub fn new(file_name: &'static str, input: &'static str, tokens: Vec<Token>) -> Self {
        Self {
            file_name,
            input,
            tokens,
            scope: vec![],
            locals: vec![],
            globals: vec![],
            num_label: 0,
            pos: 0,
        }
    }

    /// Returns the current token.
    fn tok(&self) -> &Token {
        &self.tokens[self.pos]
    }

    /// Returns the current token location.
    fn loc(&self) -> Loc {
        self.tok().loc
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

    /// Returns the current last position of scope.
    fn enter_scope(&self) -> usize {
        self.scope.len()
    }

    /// Reverts the scope previous one with return value of [enter_scope()].
    fn exit_scope(&mut self, prev: usize) {
        self.scope.truncate(prev);
    }

    fn new_var(&mut self, name: &'static str, ty: Rc<Type>, is_local: bool) -> Rc<RefCell<Var>> {
        let var = Rc::new(RefCell::new(Var::new(name, ty, is_local)));

        self.scope.push(var.clone());
        var
    }

    /// Pushes a given `var` into `self.locals`
    /// if it does not have the same name variable.
    ///
    /// Returns `true` if succeeds pushing.
    fn new_lvar(&mut self, name: &'static str, ty: Rc<Type>) -> Rc<RefCell<Var>> {
        let var = self.new_var(name, ty, true);
        self.locals.push(var.clone());
        var
    }

    /// Pushes a given `var` into `self.globals`
    /// if it does not have the same name variable.
    ///
    /// Returns `true` if succeeds pushing.
    fn new_gvar(&mut self, name: &'static str, ty: Rc<Type>) -> Rc<RefCell<Var>> {
        let var = self.new_var(name, ty, false);
        self.globals.push(var.clone());
        var
    }

    /// Finds declared variables,
    /// returns local one when the same name global and local oens are declared.
    fn find_var(&mut self, name: &'static str) -> Option<Rc<RefCell<Var>>> {
        self.scope
            .iter()
            .rev()
            .find(|var| var.borrow().name == name)
            .map(|var| var.clone())
    }

    /// Determines whether the next top-level item is a function
    /// of a global variable by looking ahead input tokens.
    fn is_function(&mut self) -> Result<bool> {
        // Saves current position to restore it after the operation below.
        let pos = self.pos;

        self.basetype()?;
        let is_func = self.consume_ident().is_some() && self.consume("(");

        self.pos = pos;
        Ok(is_func)
    }

    fn new_label(&mut self) -> &'static str {
        let buf = format!(".L.data.{}", self.num_label).leak();
        self.num_label += 1;
        buf
    }

    /// Parses the whole program.
    ///
    /// ```text
    /// program = ( global-var | function )*
    /// ```
    pub fn parse(mut self) -> Result<Node> {
        let mut funcs = vec![];

        while TokenKind::Eof != self.tok().data {
            if self.is_function()? {
                funcs.push(self.function()?);
            } else {
                self.global_var()?;
            }
        }

        Ok(Node::with_prog(funcs, self.globals))
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
            return self.comp_err(format!("expected '{}'", s), self.loc());
        }
        Ok(())
    }

    fn expect_num(&mut self) -> Result<usize> {
        if let TokenKind::Num(num) = self.tok().data {
            self.next();
            Ok(num)
        } else {
            self.comp_err("number is required", self.loc())
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
        match self.consume_ident() {
            Some(name) => Ok(name),
            None => self.comp_err("identifier is required", self.loc()),
        }
    }

    /// Returns true if the next token represents a type.
    fn is_typename(&self) -> bool {
        self.peek("char") || self.peek("int") || self.peek("struct")
    }

    /// ```text
    /// basetype = ( "char" | "int" | struct-decl ) "*"*
    /// ```
    fn basetype(&mut self) -> Result<Rc<Type>> {
        if !self.is_typename() {
            return self.comp_err("typename expected", self.loc());
        }

        let mut ty = if self.consume("char") {
            Type::char_type()
        } else if self.consume("int") {
            Type::int_type()
        } else {
            self.struct_decl()?
        };

        while self.consume("*") {
            ty = Type::with_ptr(ty);
        }

        Ok(ty)
    }

    fn read_type_suffix(&mut self, base: Rc<Type>) -> Result<Rc<Type>> {
        if !self.consume("[") {
            return Ok(base);
        }

        let sz = self.expect_num()?;
        self.expect("]")?;
        let base = self.read_type_suffix(base)?;
        Ok(Type::with_array(base, sz))
    }

    /// ```text
    /// struct-decl = "struct" "{" struct-member* "}"
    /// ```
    fn struct_decl(&mut self) -> Result<Rc<Type>> {
        self.expect("struct")?;
        self.expect("{")?;

        let mut members = vec![];

        while !self.consume("}") {
            members.push(self.struct_member()?);
        }

        let mut offset = 0;
        let members = members
            .into_iter()
            .map(|mut member| {
                member.offset = offset;
                offset += member.ty.size;
                Rc::new(member)
            })
            .collect();

        Ok(Type::with_struct(members))
    }

    /// ```text
    /// struct-member = basetype ident ( "[" num "]" )* ";"
    /// ```
    fn struct_member(&mut self) -> Result<Member> {
        let ty = self.basetype()?;
        let name = self.expect_ident()?;
        let ty = self.read_type_suffix(ty)?;
        self.expect(";")?;
        Ok(Member::new(name, ty))
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
    fn param(&mut self) -> Result<Rc<RefCell<Var>>> {
        let ty = self.basetype()?;

        let name = self.expect_ident()?;
        let ty = self.read_type_suffix(ty)?;

        // Pushes in locals because parameters are the same as locals.
        Ok(self.new_lvar(name, ty))
    }

    /// Returns the number of parameters.
    ///
    /// ```text
    /// params = param ( "," param )*
    /// ```
    fn params(&mut self) -> Result<Vec<Rc<RefCell<Var>>>> {
        let Ok(param) = self.param() else {
            return Ok(vec![]);
        };
        let mut params = vec![param];

        while self.consume(",") {
            let param = self.param()?;
            params.push(param);
        }
        Ok(params)
    }

    /// ```text
    /// primary = "(" "{" stmt-expr-tail
    ///         | "(" expr ")"
    ///         | "sizeof" unary
    ///         | ident ( "(" func-args ")" )?
    ///         | str
    ///         | num
    /// ```
    fn primary(&mut self) -> Result<Node> {
        let loc = self.loc();

        let node = if self.consume("(") {
            if self.consume("{") {
                self.stmt_expr()?
            } else {
                let mut node = self.expr()?;

                // The location of the node surrounded by parentheses is
                // the merge of the both surrounding parentheses locations.
                let loc = loc + self.loc();
                self.expect(")")?;
                node.loc = loc;

                node
            }
        } else if self.consume("sizeof") {
            let loc = loc + self.loc();
            let node = self.unary()?;
            Node::with_num(node.ty.size, loc)
        } else if let Some(name) = self.consume_ident() {
            if self.consume("(") {
                let args = self.func_args()?;

                // The location of the function call is between function name
                // and close parentesis.
                let loc = loc + self.loc();
                self.expect(")")?;

                return Ok(Node::with_fn_call(name, Type::int_type(), args, loc));
            }

            let Some(var) = self.find_var(name) else {
                return self.comp_err("undeclared variable", loc);
            };
            Node::with_var(var, loc)
        } else if let TokenKind::Str(s) = self.tok().data {
            self.next();

            let label = self.new_label();
            let var = self.new_gvar(label, Type::with_str(s.len()));
            {
                let mut var = var.borrow_mut();
                var.kind = VarKind::Str(s);
            }
            Node::with_var(var, loc)
        } else {
            Node::with_num(self.expect_num()?, loc)
        };

        Ok(node)
    }

    /// ```text
    /// unary = ( ("+" | "-" | "&" | "*" )? unary )
    ///       | postfix
    /// ```
    fn unary(&mut self) -> Result<Node> {
        let loc = self.loc();

        let node = if self.consume("+") {
            let operand = self.unary()?;
            Node::with_unop(UnOpKind::Pos, operand, loc)
        } else if self.consume("-") {
            let operand = self.unary()?;
            Node::with_unop(UnOpKind::Neg, operand, loc)
        } else if self.consume("&") {
            let operand = self.unary()?;
            Node::with_unop(UnOpKind::Addr, operand, loc)
        } else if self.consume("*") {
            let operand = self.unary()?;
            Node::with_unop(UnOpKind::Deref, operand, loc)
        } else {
            self.postfix()?
        };

        Ok(node)
    }

    fn struct_ref(&mut self, instance: Node) -> Result<Node> {
        if !instance.ty.is_struct() {
            return self.comp_err("not a struct", instance.loc);
        }

        let mem_loc = self.loc();
        let Some(mem) = find_member(&instance.ty, self.expect_ident()?) else {
            return self.comp_err("no such member", mem_loc);
        };

        let loc = instance.loc + mem_loc;
        Ok(Node::with_member(instance, mem, loc))
    }

    /// ```text
    /// postfix = primary ( "[" expr "]" | "." ident )*
    /// ```
    fn postfix(&mut self) -> Result<Node> {
        let loc = self.loc();
        let mut node = self.primary()?;

        loop {
            if self.consume("[") {
                let exp_loc = self.loc();

                // x[y] is short for *(x+y)
                let exp = Node::with_binop(BinOpKind::Add, node, self.expr()?)
                    .ok_or_else(|| self.binop_err(exp_loc))?;

                let loc = loc + self.loc();
                self.expect("]")?;
                node = Node::with_unop(UnOpKind::Deref, exp, loc);
            } else if self.consume(".") {
                node = self.struct_ref(node)?;
            } else {
                break;
            }
        }

        Ok(node)
    }

    /// ```text
    /// mul = unary ( "*" unary | "/" unary )*
    /// ```
    fn mul(&mut self) -> Result<Node> {
        let mut left = self.unary()?;

        let op_loc = self.loc();
        let node = loop {
            left = if self.consume("*") {
                let right = self.unary()?;
                Node::with_binop(BinOpKind::Mul, left, right)
            } else if self.consume("/") {
                let right = self.unary()?;
                Node::with_binop(BinOpKind::Div, left, right)
            } else {
                break left;
            }
            .ok_or_else(|| self.binop_err(op_loc))?;
        };

        Ok(node)
    }

    /// ```text
    /// add = mul ( "+" mul | "-" mul )*
    /// ```
    fn add(&mut self) -> Result<Node> {
        let mut left = self.mul()?;

        let op_loc = self.loc();
        let node = loop {
            left = if self.consume("+") {
                let right = self.mul()?;
                Node::with_binop(BinOpKind::Add, left, right)
            } else if self.consume("-") {
                let right = self.mul()?;
                Node::with_binop(BinOpKind::Sub, left, right)
            } else {
                break left;
            }
            .ok_or_else(|| self.binop_err(op_loc))?;
        };

        Ok(node)
    }

    /// ```text
    /// relational = add ( "<" add | "<=" add | ">" add | ">=" add)*
    /// ```
    fn relational(&mut self) -> Result<Node> {
        let mut left = self.add()?;

        let op_loc = self.loc();
        let node = loop {
            left = if self.consume("<") {
                let right = self.add()?;
                Node::with_binop(BinOpKind::Lt, left, right)
            } else if self.consume("<=") {
                let right = self.add()?;
                Node::with_binop(BinOpKind::Le, left, right)
            } else if self.consume(">") {
                let right = self.add()?;
                Node::with_binop(BinOpKind::Gt, left, right)
            } else if self.consume(">=") {
                let right = self.add()?;
                Node::with_binop(BinOpKind::Ge, left, right)
            } else {
                break left;
            }
            .ok_or_else(|| self.binop_err(op_loc))?;
        };

        Ok(node)
    }

    /// ```text
    /// equality = relational ( "==" relational | "!=" relational )*
    /// ```
    fn equality(&mut self) -> Result<Node> {
        let mut left = self.relational()?;

        let op_loc = self.loc();
        let node = loop {
            left = if self.consume("==") {
                let right = self.relational()?;
                Node::with_binop(BinOpKind::Eq, left, right)
            } else if self.consume("!=") {
                let right = self.relational()?;
                Node::with_binop(BinOpKind::Ne, left, right)
            } else {
                break left;
            }
            .ok_or_else(|| self.binop_err(op_loc))?;
        };

        Ok(node)
    }

    /// ```text
    /// assign = equality ( "=" assign )?
    /// ```
    fn assign(&mut self) -> Result<Node> {
        let left = self.equality()?;

        let op_loc = self.loc();
        let node = if self.consume("=") {
            let right = self.assign()?;
            Node::with_binop(BinOpKind::Assign, left, right)
        } else {
            Some(left)
        }
        .ok_or_else(|| self.binop_err(op_loc))?;

        Ok(node)
    }

    /// ```text
    /// expr = assign
    /// ```
    fn expr(&mut self) -> Result<Node> {
        self.assign()
    }

    fn expr_stmt(&mut self) -> Result<Node> {
        let node = self.expr()?;
        let loc = node.loc;
        Ok(Node::with_unop(UnOpKind::Expr, node, loc))
    }

    /// ```text
    /// declaration = basetype ident ( "[" num "]" )* ( "=" expr )? ";"
    /// ```
    fn decl(&mut self) -> Result<Node> {
        let loc = self.loc();

        let ty = self.basetype()?;

        let name_loc = self.loc();
        let Some(name) = self.consume_ident() else {
            return self.comp_err("variable name is required", self.loc());
        };
        let ty = self.read_type_suffix(ty)?;

        let var = self.new_lvar(name, ty);
        let var = Node::with_var(var, name_loc);

        let init = if self.consume("=") {
            Some(self.expr()?)
        } else {
            None
        };

        let loc = loc + self.loc();
        self.expect(";")?;

        Ok(Node::with_var_decl(var, init, loc))
    }

    /// ```text
    /// stmt-expr-tail = stmt-expr "}" ")"
    /// stmt-expr = stmt+
    /// ```
    fn stmt_expr(&mut self) -> Result<Node> {
        let prev_scope = self.enter_scope();

        let loc = self.loc();
        let stmt = match self.stmt() {
            Ok(stmt) => stmt,
            Err(_) => {
                return self.comp_err("stmt expr must have one or more stmt", loc);
            }
        };
        let mut stmts = vec![stmt];

        while !self.consume("}") {
            stmts.push(self.stmt()?);
        }

        let last_stmt = stmts.last().unwrap();
        if last_stmt.ty.kind == TypeKind::Void {
            self.comp_err("stmt expr returning void is not supported", last_stmt.loc)
        } else {
            let loc = loc + self.loc();
            self.expect(")")?;

            self.exit_scope(prev_scope);
            Ok(Node::with_stmt_expr(stmts, loc))
        }
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
        if self.is_typename() {
            return self.decl();
        }

        let loc = self.loc();

        // Recognizes a block.
        if self.consume("{") {
            let mut stmts = vec![];
            let brace_loc = loc;
            let mut loc = loc + self.loc();

            let prev_scope = self.enter_scope();
            while !self.consume("}") {
                if self.tok().data == TokenKind::Eof {
                    return self.comp_err("this brace is not closed", brace_loc);
                }
                stmts.push(self.stmt()?);
                loc += self.loc();
            }
            self.exit_scope(prev_scope);
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

            let init = self.expr_stmt().ok();
            self.expect(";")?;

            let cond = self.expr().ok();
            self.expect(";")?;

            let inc = self.expr_stmt().ok();
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
            self.expr_stmt()?
        };
        self.expect(";")?;

        Ok(node)
    }

    /// ```text
    /// function = basetype ident "(" params? ")" "{" stmt* "}"
    /// ```
    fn function(&mut self) -> Result<Node> {
        assert!(self.locals.is_empty());

        let loc = self.loc();

        let ty = self.basetype()?;
        let name = self.expect_ident()?;

        self.expect("(")?;
        let prev_scope = self.enter_scope();
        let params = self.params()?;
        self.expect(")")?;

        self.expect("{")?;
        let mut stmts = vec![];

        let loc = loop {
            let loc = loc + self.loc();
            if self.consume("}") {
                break loc;
            }
            stmts.push(self.stmt()?);
        };

        self.exit_scope(prev_scope);
        let locals = mem::replace(&mut self.locals, vec![]);

        Ok(Node::with_fn(name, ty, params, stmts, locals, loc))
    }

    /// ```text
    /// global-var = basetype ident ( "[" num "]" )* ";"
    /// ```
    fn global_var(&mut self) -> Result<Rc<RefCell<Var>>> {
        let ty = self.basetype()?;

        let loc = self.loc();
        let Some(name) = self.consume_ident() else {
            return self.comp_err("identifier is required", loc);
        };

        let ty = self.read_type_suffix(ty)?;
        self.expect(";")?;

        Ok(self.new_gvar(name, ty))
    }

    fn comp_err<T>(&self, msg: impl Into<String>, loc: Loc) -> Result<T> {
        Err(Error::CompileError {
            message: msg.into(),
            file_name: self.file_name,
            input: self.input,
            loc,
        })
    }

    /// [Node::with_binop()] can return `None` when the combination of `lhs` and `rhs` type is invald.
    /// This function converts `None` to [Error] with [Option::ok_or_else()].
    fn binop_err(&self, loc: Loc) -> Error {
        self.comp_err::<()>("invalid operand", loc).err().unwrap()
    }
}

/// Finds the member of struct type `ty` whose name is `name` and return it.
/// # Remarks
///
/// `ty.kind` must be [TypeKind::Struct].
fn find_member(ty: &Type, name: &'static str) -> Option<Rc<Member>> {
    match &ty.kind {
        TypeKind::Struct { members } => members
            .into_iter()
            .find(|mem| mem.name == name)
            .map(|mem| mem.clone()),
        _ => unreachable!("find_member() requires ty.kind is TypeKind::Struct"),
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
    /// &
    Addr,
    /// *
    Deref,
}

/// Represents a binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    /// num + num
    Add,
    /// ptr + num or num + ptr
    ///
    /// [bool] value is `true` when `lhs` is ptr.
    PtrAdd(bool),
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
    Member {
        instance: Box<Node>,
        member: Rc<Member>,
    },
    /// Variable declaration.
    VarDecl {
        var: Box<Node>,
        /// Initial value.
        init: Option<Box<Node>>,
    },
    /// Variable.
    Var(Rc<RefCell<Var>>),
    /// Block.
    Block { stmts: Vec<Node> },
    /// Statement expression
    StmtExpr { stmts: Vec<Node> },
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
        return_ty: Rc<Type>,
        params: Vec<Rc<RefCell<Var>>>,
        stmts: Vec<Node>,
        locals: Vec<Rc<RefCell<Var>>>,
    },
    /// Whole program.
    Program {
        funcs: Vec<Node>,
        globals: Vec<Rc<RefCell<Var>>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub data: NodeKind,
    pub ty: Rc<Type>,
    pub loc: Loc,
}

impl Node {
    pub fn with_num(num: usize, loc: Loc) -> Self {
        Self {
            data: NodeKind::Num(num),
            ty: Type::int_type(),
            loc,
        }
    }

    pub fn with_unop(op: UnOpKind, operand: Node, op_loc: Loc) -> Self {
        let loc = op_loc + operand.loc;

        let ty = match op {
            UnOpKind::Addr => Type::with_ptr(operand.ty.clone()),
            UnOpKind::Deref => match operand.ty.base() {
                Some(base) => base,
                None => Type::int_type(),
            },
            _ => Type::int_type(),
        };

        Self {
            data: NodeKind::UnOp {
                op,
                operand: Box::new(operand),
            },
            ty,
            loc: loc,
        }
    }

    pub fn with_binop(op: BinOpKind, lhs: Node, rhs: Node) -> Option<Self> {
        // binop loc must be between lhs and rhs.
        let loc = lhs.loc + rhs.loc;

        // Determins the operator and the type of the node.
        // If OpKind is `Add` or `Sub` and `lhs` or `rhs` has base type,
        // the type of node may be pointer-like.
        let (op, ty) = match op {
            BinOpKind::Add => {
                if lhs.ty.is_integer() && rhs.ty.is_integer() {
                    (op, Type::int_type())
                } else if lhs.ty.base().is_some() && rhs.ty.is_integer() {
                    (BinOpKind::PtrAdd(true), lhs.ty.clone())
                } else if lhs.ty.is_integer() && rhs.ty.base().is_some() {
                    (BinOpKind::PtrAdd(false), rhs.ty.clone())
                } else {
                    return None;
                }
            }
            BinOpKind::Sub => {
                if lhs.ty.is_integer() && rhs.ty.is_integer() {
                    (op, Type::int_type())
                } else if lhs.ty.base().is_some() && rhs.ty.is_integer() {
                    (BinOpKind::PtrSub, lhs.ty.clone())
                } else if lhs.ty.base().is_some() && rhs.ty.base().is_some() {
                    (BinOpKind::PtrDiff, lhs.ty.clone())
                } else {
                    return None;
                }
            }
            BinOpKind::Assign => (op, lhs.ty.clone()),
            _ => (op, Type::int_type()),
        };

        Some(Self {
            data: NodeKind::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            ty,
            loc,
        })
    }

    pub fn with_member(instance: Node, member: Rc<Member>, loc: Loc) -> Self {
        let ty = member.ty.clone();
        Self {
            data: NodeKind::Member {
                instance: Box::new(instance),
                member,
            },
            ty,
            loc,
        }
    }

    pub fn with_var_decl(var: Node, init: Option<Node>, loc: Loc) -> Self {
        Self {
            data: NodeKind::VarDecl {
                var: Box::new(var),
                init: init.map(|node| Box::new(node)),
            },
            ty: Type::void(),
            loc,
        }
    }

    pub fn with_var(var: Rc<RefCell<Var>>, loc: Loc) -> Self {
        let ty = var.borrow().ty.clone();
        Self {
            data: NodeKind::Var(var),
            ty,
            loc,
        }
    }

    pub fn with_block(stmts: Vec<Node>, loc: Loc) -> Self {
        Self {
            data: NodeKind::Block { stmts },
            ty: Type::void(),
            loc,
        }
    }

    /// Returns `None` if returning void.
    ///
    /// # Remarks
    ///
    /// `stmts` must not be empty and the type of last stmt of `stmts` must not be `void`.
    pub fn with_stmt_expr(stmts: Vec<Node>, loc: Loc) -> Self {
        let ty = stmts.last().unwrap().ty.clone();
        Self {
            data: NodeKind::StmtExpr { stmts },
            ty,
            loc,
        }
    }

    pub fn with_while(cond: Node, stmt: Node, loc: Loc) -> Self {
        Self {
            data: NodeKind::While {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
            },
            ty: Type::void(),
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
            ty: Type::void(),
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
            ty: Type::void(),
            loc,
        }
    }

    pub fn with_fn_call(name: &'static str, ty: Rc<Type>, args: Vec<Node>, loc: Loc) -> Self {
        Self {
            data: NodeKind::FnCall { name, args },
            ty,
            loc,
        }
    }

    pub fn with_fn(
        name: &'static str,
        return_ty: Rc<Type>,
        params: Vec<Rc<RefCell<Var>>>,
        stmts: Vec<Node>,
        locals: Vec<Rc<RefCell<Var>>>,
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
            ty: Type::void(),
            loc,
        }
    }

    pub fn with_prog(funcs: Vec<Node>, globals: Vec<Rc<RefCell<Var>>>) -> Self {
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
            data: NodeKind::Program { funcs, globals },
            ty: Type::void(),
            loc,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VarKind {
    /// String literal terminated with '\0'.
    Str(&'static str),
    /// Others.
    Others,
}

/// Represets variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    /// Variable name.
    pub name: &'static str,
    /// Variable kind if it is string.
    pub kind: VarKind,
    /// Type.
    pub ty: Rc<Type>,
    /// local of global
    pub is_local: bool,
    /// Variable offset from RBP.
    pub offset: usize,
}

impl Var {
    pub fn new(name: &'static str, ty: Rc<Type>, is_local: bool) -> Self {
        Self {
            name,
            kind: VarKind::Others,
            ty,
            is_local,
            offset: 0,
        }
    }
}
