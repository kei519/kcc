use crate::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents a token.
///
/// The value and the kind.
pub enum TokenKind {
    /// Signature.
    Reserved(&'static [u8]),
    /// Digit token.
    Num(u64),
    /// HACK: I don't think Eof is needed in Rust program...
    Eof,
}

pub type Token = Annot<TokenKind>;

impl Token {
    pub fn with_reserved(kw: &'static [u8], pos: usize) -> Self {
        Self {
            value: TokenKind::Reserved(kw),
            loc: Loc {
                start: pos,
                end: pos + kw.len(),
            },
        }
    }

    pub fn with_num(num: u64, start: usize, end: usize) -> Self {
        Self {
            value: TokenKind::Num(num),
            loc: Loc { start, end },
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    /// The input as bytes string.
    input: &'a [u8],
    /// reading position of [Self::input].
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    /// Constructor.
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    /// Tokenizes the input and returns the vector of the tokens.
    /// When fails, return an error.
    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        const KW: [&'static [u8]; 13] = [
            b"==", b"!=", b"<=", b">=", b">", b"<", b"+", b"-", b"*", b"/", b"(", b")", b";",
        ];

        let mut tokens = vec![];

        'l: while self.pos < self.input.len() {
            // skip a whitespace
            if self.input[self.pos].is_ascii_whitespace() {
                self.pos += 1;
                continue;
            }

            let input_slice = &self.input[self.pos..];
            for kw in KW {
                if input_slice.starts_with(kw) {
                    tokens.push(Token::with_reserved(kw, self.pos));
                    self.pos += kw.len();
                    continue 'l;
                }
            }

            match input_slice[0] {
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
    pub fn str_to_num(&mut self) -> Result<u64> {
        use std::str::from_utf8;

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
