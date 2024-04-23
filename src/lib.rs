use std::str::from_utf8;

type StdResult<T, E> = std::result::Result<T, E>;
type Result<T> = StdResult<T, Error>;

/// Executes the main logic.
///
/// * args - command-line arguments
///
/// # Return
///
/// Exit code.
pub fn main(args: Vec<String>) -> u8 {
    if args.len() != 2 {
        eprintln!("Only ONE argument is required.");
        return 1;
    }

    // prepare for assembler
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let input = args[1].as_str();

    // catch any error
    let node = match parse(input) {
        Err(e) => {
            e.show(input);
            return 1;
        }
        Ok(n) => n,
    };
    node.gen();

    // return code
    println!("  pop rax");
    println!("  ret");

    0
}

/// called by main to catch any error in one place
fn parse(input: &str) -> Result<Node> {
    let tokens = Tokenizer::new(input).tokenize()?;
    let mut parser = Parser::new(tokens);

    parser.expr()
}

/// Reads a token when it is the expected char,
/// otherwise returns an error.
fn expect(token: &Token, op: char) -> Result<()> {
    match token.value {
        TokenKind::Reserved(kw) if op == kw.chars().next().unwrap() => Ok(()),
        _ => Err(Error {
            value: ErrorKind::Error(format!("not '{}'", op).leak()),
            loc: token.loc,
        }),
    }
}

/// Reads a token when it is a number,
/// otherwise returns an error.
fn expect_number(token: &Token) -> Result<u64> {
    match token.value {
        TokenKind::Num(n) => Ok(n),
        _ => Err(Error {
            value: ErrorKind::Error("not a number"),
            loc: token.loc,
        }),
    }
}

/// Read a token when it is the expected char and returns true,
/// otherwise returns false.
fn consume(token: &Token, op: char) -> bool {
    match token.value {
        TokenKind::Reserved(kw) if kw.chars().next().unwrap() == op => true,
        _ => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents all errros.
enum ErrorKind {
    Error(&'static str),
}

type Error = Annot<ErrorKind>;

impl Error {
    /// Shows the error in an understandable way.
    fn show(&self, input: &str) {
        let msg = match self.value {
            ErrorKind::Error(msg) => msg,
        };
        eprintln!("{}", input);
        // inserts '^' at the error point
        eprintln!("{:spaces_num$}^ {}", "", msg, spaces_num = self.loc.start);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Represents a location as half interval [start, end).
struct Loc {
    start: usize,
    end: usize,
}

impl Loc {
    /// Merges two Locations.
    fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};

        Loc {
            start: min(self.start, other.start),
            end: max(self.end, other.end),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Wraps any type with information.
///
/// Value of type `T` with location info.
struct Annot<T> {
    value: T,
    loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents a token.
///
/// The value and the kind.
enum TokenKind {
    /// Signature.
    Reserved(&'static str),
    /// Digit token.
    Num(u64),
    /// HACK: I don't think Eof is needed in Rust program...
    Eof,
}

type Token = Annot<TokenKind>;

impl Token {
    fn with_reserved(kw: &'static str, pos: usize) -> Self {
        Self {
            value: TokenKind::Reserved(kw),
            loc: Loc {
                start: pos,
                end: pos + kw.len(),
            },
        }
    }

    fn with_num(num: u64, start: usize, end: usize) -> Self {
        Self {
            value: TokenKind::Num(num),
            loc: Loc { start, end },
        }
    }
}

#[derive(Debug)]
struct Tokenizer<'a> {
    /// The input as bytes string.
    input: &'a [u8],
    /// reading position of [Self::input].
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    /// Constructor.
    fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    /// Tokenizes the input and returns the vector of the tokens.
    /// When fails, return an error.
    fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];

        while self.pos < self.input.len() {
            let now = self.input[self.pos];
            // skip a whitespace
            if now.is_ascii_whitespace() {
                self.pos += 1;
                continue;
            }

            match now {
                b'+' | b'-' | b'*' | b'/' | b'(' | b')' => {
                    // This cast always succeeds because of checking.
                    let kw = (now as char).to_string().leak();
                    tokens.push(Token::with_reserved(kw, self.pos));
                    self.pos += 1;
                }
                b'0'..=b'9' => {
                    let start = self.pos;
                    tokens.push(Token::with_num(self.str_to_num()?, start, self.pos));
                }
                _ => {
                    return Err(Error {
                        value: ErrorKind::Error("cannot tokenize"),
                        loc: Loc {
                            start: self.pos,
                            end: self.pos + 1,
                        },
                    })
                }
            }
        }

        Ok(tokens)
    }

    /// Converts a str to a number , reads the number and returns it.
    /// When str starts with a number, returns an error.
    fn str_to_num(&mut self) -> Result<u64> {
        let start = self.pos;
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
            self.pos += 1;
        }

        if start == self.pos {
            return Err(Error {
                value: ErrorKind::Error("Should be a number"),
                loc: Loc {
                    start: self.pos,
                    end: self.pos + 1,
                },
            });
        } else {
            let n = from_utf8(&self.input[start..self.pos])
                // This always succeeds becase of construction
                .unwrap()
                .parse()
                // So does this!
                .unwrap();
            Ok(n)
        }
    }
}

/// Respresents binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinOpKind {
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
}

type BinOp = Annot<BinOpKind>;

impl BinOp {
    /// Constructor.
    fn new(op: BinOpKind, loc: Loc) -> Self {
        Self { value: op, loc }
    }
}

/// Represents a node of the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NodeKind {
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

type Node = Annot<NodeKind>;

impl Node {
    fn with_num(num: u64, loc: Loc) -> Self {
        Self {
            value: NodeKind::Num(num),
            loc,
        }
    }

    fn with_binop(op: BinOp, left: Node, right: Node) -> Self {
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

    /// Generates the assembly code from a top node.
    fn gen(&self) {
        match &self.value {
            // When the node is a number, it is a terminal.
            NodeKind::Num(val) => {
                println!("  push {}", val);
                return;
            }
            NodeKind::BinOp { op, left, right } => {
                // When the node is a binary operator, it has left and right side.
                left.gen();
                right.gen();

                println!("  pop rdi");
                println!("  pop rax");

                match op.value {
                    BinOpKind::Add => println!("  add rax, rdi"),
                    BinOpKind::Sub => println!("  sub rax, rdi"),
                    BinOpKind::Mul => println!("  imul rax, rdi"),
                    BinOpKind::Div => {
                        println!("  cqo");
                        println!("  idiv rdi");
                    }
                }

                println!("  push rax");
            }
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    /// Constructor.
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    /// The shared reference of the current token.
    fn tok(&self) -> &Token {
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

    /// primary = num | "(" expr ")"
    fn primary(&mut self) -> Result<Node> {
        let tok_loc = self.tok().loc;
        if consume(self.tok(), '(') {
            self.pos += 1;
            let ret = self.expr()?;
            expect(self.tok(), ')')?;
            self.pos += 1;
            Ok(ret)
        } else {
            let ret = Node::with_num(expect_number(self.tok())?, tok_loc);
            self.pos += 1;
            Ok(ret)
        }
    }

<<<<<<< HEAD
    /// mul = primary ( "*" primary | "/" primary )*
=======
    /// unary = ( "+" | "-" )? primary
    fn unary(&mut self) -> Result<Node> {
        // FIXME: Two operators, "+" and "-", are not binary operator.
        // So the loc of this function return value is NOT correct.
        if consume(&self.tok(), '+') {
            self.pos += 1;
            Ok(self.primary()?)
        } else if consume(self.tok(), '-') {
            self.pos += 1;
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
>>>>>>> 04375b7 (Add unary operatos + and -)
    fn mul(&mut self) -> Result<Node> {
        let mut ret = self.unary()?;

        loop {
            let tok_loc = self.tok().loc;
            if consume(self.tok(), '*') {
                self.pos += 1;
                ret = Node::with_binop(BinOp::new(BinOpKind::Mul, tok_loc), ret, self.unary()?);
            } else if consume(self.tok(), '/') {
                self.pos += 1;
                ret = Node::with_binop(BinOp::new(BinOpKind::Div, tok_loc), ret, self.unary()?);
            } else {
                return Ok(ret);
            }
        }
    }

    /// expr = mul ( "+" mul | "-" mul)*
    fn expr(&mut self) -> Result<Node> {
        let mut ret = self.mul()?;

        loop {
            let tok_loc = self.tok().loc;
            if consume(self.tok(), '+') {
                self.pos += 1;
                ret = Node::with_binop(BinOp::new(BinOpKind::Add, tok_loc), ret, self.mul()?);
            } else if consume(self.tok(), '-') {
                self.pos += 1;
                ret = Node::with_binop(BinOp::new(BinOpKind::Sub, tok_loc), ret, self.mul()?);
            } else {
                return Ok(ret);
            }
        }
    }
}
