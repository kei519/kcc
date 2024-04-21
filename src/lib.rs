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
    let tokens = match Tokenizer::new(input).tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("{:?}", e);
            return 1;
        }
    };

    let mut pos = 0;

    println!("  mov rax, {}", expect_number(&tokens[pos]).unwrap());
    pos += 1;

    while pos < tokens.len() {
        if consume(&tokens[pos], '+').is_ok() {
            pos += 1;
            // FIXME: This implemntation is not good
            println!("  add rax, {}", expect_number(&tokens[pos]).unwrap());
            pos += 1;
            continue;
        }

        consume(&tokens[pos], '-').unwrap();
        pos += 1;
        println!("  sub rax, {}", expect_number(&tokens[pos]).unwrap());
        pos += 1;
    }

    // return code
    println!("  ret");

    0
}

fn expect_number(token: &Token) -> StdResult<u64, ()> {
    match token.value {
        TokenKind::Num(n) => Ok(n),
        _ => {
            eprintln!("not a number");
            Err(())
        }
    }
}

fn consume(token: &Token, op: char) -> StdResult<(), ()> {
    match token.value {
        TokenKind::Num(_) => {
            eprintln!("not {}", op);
            Err(())
        }
        TokenKind::Reserved(kw) => {
            if kw.chars().next().unwrap() as char == op {
                Ok(())
            } else {
                Err(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents all errros.
enum Error {
    ParseError,
    Eof,
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
                _ => return Err(Error::ParseError),
            }
        }

        Ok(tokens)
    }

    fn str_to_num(&mut self) -> Result<u64> {
        let start = self.pos;
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
            self.pos += 1;
        }

        if start == self.pos {
            Err(Error::ParseError)
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
