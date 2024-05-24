use crate::util::{Error, Loc, Result};

const KW: [&'static str; 23] = [
    "==", "!=", "<=", ">=", "->", "+", "-", "*", "/", "(", ")", "<", ">", ";", ",", ".", ":", "[",
    "]", "{", "}", "=", "&",
];

const SEP_KW: [&'static str; 12] = [
    "return", "int", "while", "for", "if", "else", "sizeof", "char", "struct", "typedef", "long",
    "short",
];

const SEP: [u8; 10] = [b';', b',', b'.', b':', b'(', b')', b'[', b']', b'{', b'}'];

pub struct Tokenizer {
    file_name: &'static str,
    input: &'static str,
    pos: usize,
}

impl Tokenizer {
    pub fn new(file_name: &'static str, input: &'static str) -> Self {
        Self {
            file_name,
            input,
            pos: 0,
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>> {
        let mut ret = vec![];

        'main: while !self.cur().is_empty() {
            // Skip white spaces.
            if self.head().is_ascii_whitespace() {
                self.next();
                continue;
            }

            // Skips line comments.
            if self.cur().starts_with(b"//") {
                self.pos += 2;
                while self.next() && self.head() != b'\n' {}
                continue;
            }

            // Skips block comments.
            if self.cur().starts_with(b"/*") {
                self.pos += 2;
                match self.cur().windows(2).position(|s| s == b"*/") {
                    Some(pos) => self.pos += pos + 2,
                    None => {
                        return self.comp_err(
                            "unclosed block comment",
                            Loc::range(self.pos - 2, self.input.len()),
                        )
                    }
                }
                continue;
            }

            if self.head() == b'"' {
                ret.push(self.read_string_literal()?);
                continue;
            }

            // Numeric literal.
            if self.head().is_ascii_digit() {
                let start = self.pos;
                let num = self.str_to_num();
                let end = self.pos;
                ret.push(Token::with_num(num, Loc::range(start, end)));
                continue;
            }

            // Reserved words that must be separated by a separator.
            for &kw in &SEP_KW {
                if self.cur().starts_with(kw.as_bytes())
                    && self.cur()[kw.len()..]
                        .first()
                        // The default value is `true`
                        // because the keyword is at the end of the input.
                        .map_or(true, |&c| c.is_ascii_whitespace() || SEP.contains(&c))
                {
                    let start = self.pos;
                    self.pos += kw.len();
                    let end = self.pos;
                    ret.push(Token::with_reserved(kw, Loc::range(start, end)));
                    continue 'main;
                }
            }

            // Reserved words.
            for &kw in &KW {
                if self.cur().starts_with(kw.as_bytes()) {
                    let start = self.pos;
                    self.pos += kw.len();
                    let end = self.pos;
                    ret.push(Token::with_reserved(kw, Loc::range(start, end)));
                    continue 'main;
                }
            }

            // Identifier.
            if is_ident_start(self.head()) {
                let start = self.pos;
                self.next();

                while !self.cur().is_empty() && is_ident_continue(self.head()) {
                    self.next();
                }
                let end = self.pos;

                ret.push(Token::with_ident(
                    &self.input[start..end],
                    Loc::range(start, end),
                ));
                continue;
            }

            // Unrecognized token.
            return self.comp_err("unrecognized token", Loc::at(self.pos));
        }

        ret.push(Token::with_eof(Loc::at(self.pos)));
        Ok(ret)
    }

    /// Returns the byte sequence starting from the current position.
    fn cur(&self) -> &'static [u8] {
        &self.input.as_bytes()[self.pos..]
    }

    /// Returns the byte at the current position.
    fn head(&self) -> u8 {
        self.cur()[0]
    }

    /// Advances the cursor if the cursor will not exceed the input.
    /// Returns `true` if the cursor does not reach eof.
    fn next(&mut self) -> bool {
        if self.pos >= self.input.len() {
            return false;
        }

        self.pos += 1;
        if self.pos < self.input.len() {
            true
        } else {
            false
        }
    }

    /// Converts the byte sequence starting from the current position to a number.
    ///
    /// # Remarks
    ///
    /// This function assumes that the byte sequence starting with a digit.
    fn str_to_num(&mut self) -> usize {
        let mut num = 0;
        while !self.cur().is_empty() && self.cur()[0].is_ascii_digit() {
            num = num * 10;
            num += (self.cur()[0] - b'0') as usize;
            self.next();
        }
        num
    }

    fn read_string_literal(&mut self) -> Result<Token> {
        let start = self.pos;
        let mut buf = String::new();

        let mut is_escaping = false;
        loop {
            if !self.next() {
                return self.comp_err("unclosed string literal", Loc::range(start, self.pos));
            }

            if is_escaping {
                buf.push(get_escape_char(self.head()) as char);
                is_escaping = false;
                continue;
            }

            if self.head() == b'"' {
                break;
            }

            if self.head() == b'\\' {
                is_escaping = true;
            } else {
                buf.push(self.head() as char);
            }
        }

        self.next();
        let end = self.pos;
        Ok(Token::with_str(buf, Loc::range(start, end)))
    }

    fn comp_err<T>(&self, msg: impl Into<String>, loc: Loc) -> Result<T> {
        Err(Error::CompileError {
            message: msg.into(),
            file_name: self.file_name,
            input: self.input,
            loc,
        })
    }
}

fn get_escape_char(c: u8) -> u8 {
    match c {
        b'a' => 7,
        b'b' => 8,
        b't' => b'\t',
        b'n' => b'\n',
        b'v' => 11,
        b'f' => 12,
        b'r' => b'\r',
        b'e' => 27,
        b'0' => b'\0',
        _ => c,
    }
}

/// Returns `true` if the given byte is the start of an identifier.
fn is_ident_start(c: u8) -> bool {
    c == b'_' || c.is_ascii_alphabetic()
}

/// Returns `true` if the given byte is a part of an identifier.
fn is_ident_continue(c: u8) -> bool {
    c == b'_' || c.is_ascii_alphanumeric()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// Numeric literal.
    Num(usize),
    /// String literal terminated with '\0'.
    Str(&'static str),
    // Keyword.
    Reserved(&'static str),
    /// Identifier.
    Ident(&'static str),
    /// Represents the end of file.
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub data: TokenKind,
    pub loc: Loc,
}

impl Token {
    pub fn with_num(num: usize, loc: Loc) -> Self {
        Self {
            data: TokenKind::Num(num),
            loc,
        }
    }

    pub fn with_str(mut s: String, loc: Loc) -> Self {
        s.push('\0');
        Self {
            data: TokenKind::Str(s.leak()),
            loc,
        }
    }

    pub fn with_reserved(keyword: &'static str, loc: Loc) -> Self {
        Self {
            data: TokenKind::Reserved(keyword),
            loc,
        }
    }

    pub fn with_ident(ident: &'static str, loc: Loc) -> Self {
        Self {
            data: TokenKind::Ident(ident),
            loc,
        }
    }

    pub fn with_eof(loc: Loc) -> Self {
        Self {
            data: TokenKind::Eof,
            loc,
        }
    }
}
