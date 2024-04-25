use crate::*;

/// Checks the character is one of the separators in C.
pub fn is_separator(c: u8) -> bool {
    match c {
        b'(' | b')' | b'[' | b']' | b'{' | b'}' | b';' | b',' | b'.' | b':' => true,
        _ => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents a token.
///
/// The value and the kind.
pub enum TokenKind {
    /// Signature.
    Reserved(&'static [u8]),
    /// Identifier
    Ident(&'static str),
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

    pub fn with_ident(name: &'static str, pos: usize) -> Self {
        Self {
            value: TokenKind::Ident(name),
            loc: Loc {
                start: pos,
                end: pos + name.len(),
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
pub struct Tokenizer {
    /// The input as bytes string.
    input: &'static [u8],
    /// reading position of [Self::input].
    pos: usize,
}

impl Tokenizer {
    /// Constructor.
    pub fn new(input: &'static str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    /// Tokenizes the input and returns the vector of the tokens.
    /// When fails, return an error.
    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        /// Keywords that must be followded by white spaces.
        const SP_KW: [&'static [u8]; 1] = [b"return"];

        /// Keywords that must be followed by separators or white spaces.
        const SEP_KW: [&'static [u8]; 4] = [b"while", b"else", b"for", b"if"];

        /// Keywords other than above.
        const KW: [&'static [u8]; 14] = [
            b"==", b"!=", b"<=", b">=", b">", b"<", b"+", b"-", b"*", b"/", b"(", b")", b";", b"=",
        ];

        let mut tokens = vec![];

        'l: while self.pos < self.input.len() {
            // skip a whitespace
            if self.input[self.pos].is_ascii_whitespace() {
                self.pos += 1;
                continue;
            }

            let input_slice = &self.input[self.pos..];
            for kw in SP_KW {
                if input_slice.starts_with(kw) {
                    match self.input.get(self.pos + kw.len()) {
                        Some(c) if !c.is_ascii_whitespace() => continue,
                        _ => {
                            tokens.push(Token::with_reserved(kw, self.pos));
                            self.pos += kw.len();
                            continue 'l;
                        }
                    }
                }
            }

            for kw in SEP_KW {
                if input_slice.starts_with(kw) {
                    match self.input.get(self.pos + kw.len()) {
                        Some(&c) if !c.is_ascii_whitespace() && !is_separator(c) => continue,
                        _ => {
                            tokens.push(Token::with_reserved(kw, self.pos));
                            self.pos += kw.len();
                            continue 'l;
                        }
                    }
                }
            }

            for kw in KW {
                if input_slice.starts_with(kw) {
                    tokens.push(Token::with_reserved(kw, self.pos));
                    self.pos += kw.len();
                    continue 'l;
                }
            }

            if input_slice[0].is_ascii_digit() {
                let start = self.pos;
                // This conversion never fails because of checking.
                tokens.push(Token::with_num(self.str_to_num().unwrap(), start, self.pos));
                continue;
            }

            // The token should be a identifer.
            let start = self.pos;
            while self.pos < self.input.len()
                && !self.input[self.pos].is_ascii_whitespace()
                && (self.input[self.pos] == b'_' || !self.input[self.pos].is_ascii_punctuation())
            {
                self.pos += 1;
            }
            let name = std::str::from_utf8(&self.input[start..self.pos])
                // This never failse because of construction.
                .unwrap();
            tokens.push(Token::with_ident(name, start));
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
