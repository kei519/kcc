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
    match parse(input) {
        Err(e) => {
            e.show(input);
            return 1;
        }
        _ => (),
    }

    // return code
    println!("  ret");

    0
}

/// called by main to catch any error in one place
fn parse(input: &str) -> Result<()> {
    let tokens = Tokenizer::new(input).tokenize()?;

    let mut pos = 0;

    println!("  mov rax, {}", expect_number(&tokens[pos])?);
    pos += 1;

    while pos < tokens.len() {
        // HACK: I don't think Eof is needed in Rust program.
        if let TokenKind::Eof = tokens[pos].value {
            break;
        }

        if consume(&tokens[pos], '+') {
            pos += 1;
            // FIXME: This implemntation is not good
            println!("  add rax, {}", expect_number(&tokens[pos])?);
            pos += 1;
            continue;
        }

        expect(&tokens[pos], '-')?;
        pos += 1;
        println!("  sub rax, {}", expect_number(&tokens[pos])?);
        pos += 1;
    }

    Ok(())
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
        _ => {
            eprintln!();
            Err(Error {
                value: ErrorKind::Error("not a number"),
                loc: token.loc,
            })
        }
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
    /// EOF.
    // #HACK: I don't want to use Eof.
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

    fn with_eof(input: &[u8]) -> Self {
        Self {
            value: TokenKind::Eof,
            loc: Loc {
                start: input.len(),
                end: input.len() + 1,
            },
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
                b'+' | b'-' => {
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
        tokens.push(Token::with_eof(self.input));

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
