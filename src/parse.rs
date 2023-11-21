use std::process::exit;

/// 予約されている文字を保持。
const RESERVEDS: [&str; 14] = [
    "+", "-", "*", "/", "(", ")", "<=", ">=", "<", ">", "==", "!=", "=", ";",
];

/// トークン（とその値）保持用の列挙型。
#[derive(Debug)]
pub(crate) enum Token<'a> {
    Reserved(&'a str),
    Return,
    Identifier(usize), // この`usize`はオフセット
    Number(u32),
}

/// トークナイズのための構造体。
pub(crate) struct Tokenizer<'a> {
    /// 入力された文字列
    input: &'a str,
    /// 現在のスライスの位置
    pos: usize,
    /// peek した次のトークン
    peeked: Option<Option<Token<'a>>>,
    /// 現在何文字読んだか
    index: usize,
    /// ローカル変数のリストを保持
    locals: Vec<LVar<'a>>,
}

impl<'a> Tokenizer<'a> {
    /// トークナイザの初期化。
    ///
    /// * `input` - トークナイズする入力。
    pub(crate) fn new(input: &str) -> Tokenizer {
        return Tokenizer {
            input,
            pos: 0,
            peeked: None,
            index: 0,
            locals: Vec::new(),
        };
    }

    /// 処理の先頭から数値を取り出す。
    /// 見つかれば`Some(_)`、なければ`None`を返す。
    fn parse_number(&mut self) -> Option<u32> {
        let mut is_first = true;
        let mut n = 0;

        loop {
            match self.input[self.pos..].chars().next() {
                Some(c) if c.is_ascii_digit() => {
                    if is_first {
                        is_first = false;
                    }
                    n = n * 10 + (c as u8 - b'0') as u32;
                    self.pos += c.len_utf8();
                    self.index += 1;
                }
                _ => {
                    if is_first {
                        return None;
                    }
                    return Some(n);
                }
            }
        }
    }

    /// 空白文字以外が来るまで読み飛ばす。
    /// 読み飛ばさなかった場合は `false`、
    /// 読み飛ばした場合は `true` を返す。
    /// （今後スペースがあるか大事な局面があるため）
    fn skip_whitespace(&mut self) -> bool {
        let mut is_first = true;

        while let Some(now) = self.input[self.pos..].chars().next() {
            if now.is_ascii_whitespace() {
                if is_first {
                    is_first = false;
                }
                self.pos += now.len_utf8();
                self.index += 1;
            } else {
                break;
            }
        }
        !is_first
    }

    /// 先頭が指定された文字列と一致しているかを返す。
    /// また、もし一致していた場合はその分だけ読み飛ばす。
    fn cmp(&mut self, s: &str) -> bool {
        let result = self.input[self.pos..].starts_with(s);

        if result {
            for c in s.chars() {
                self.pos += c.len_utf8();
            }
            self.index += s.len();
        }

        result
    }

    /// 渡された文字列で始まり、その直後にASCII空白文字があるかどうかを返す。
    /// あった場合には、渡された文字列分読み飛ばす。
    fn starts_with_before_whitespace(&mut self, s: &str) -> bool {
        let result = self.input[self.pos..].starts_with(s);

        if !result {
            return false;
        }

        match self.input[self.pos + s.len()..].chars().next() {
            Some(c) if !c.is_ascii_whitespace() => return false,
            _ => (),
        }

        for c in s.chars() {
            self.pos += c.len_utf8();
            self.index += 1;
        }
        return true;
    }

    /// 次のトークンを消費せずに見る。
    pub(crate) fn peek(&mut self) -> Option<&Token> {
        let next = self.next();
        self.peeked.get_or_insert_with(|| next).as_ref()
    }

    /// 変数を検索して、見つかればそれを返す。
    /// 見つからなければ`None`。
    pub(crate) fn find_lvar(&self, name: &str) -> Option<&LVar> {
        self.locals.iter().find(|e| e.name == name)
    }

    /// 変数名（予約文字でないもの）をパースする。
    /// その変数のオフセットを返す。
    pub(crate) fn parse_lvar(&mut self) -> Option<usize> {
        let first_pos = self.pos;

        loop {
            // 予約文字が先頭になったら終了
            if RESERVEDS
                .iter()
                .any(|e| self.input[self.pos..].starts_with(e))
            {
                break;
            }

            // 先頭が予約文字で始まっていなければ、１文字ずつ読んでいく
            match self.input[self.pos..].chars().next() {
                None => break,
                Some(c) => {
                    if c.is_ascii_whitespace() {
                        break;
                    }

                    self.pos += c.len_utf8();
                    self.index += 1;
                }
            }
        }

        if first_pos == self.pos {
            return None;
        }

        if let Some(lvar) = self.find_lvar(&self.input[first_pos..self.pos]) {
            return Some(lvar.offset);
        } else {
            let lvar = LVar::new(&self.input[first_pos..self.pos], (self.num_lvar() + 1) * 8);
            let offset = lvar.offset;
            self.locals.push(lvar);
            return Some(offset);
        }
    }

    /// 現在の変数の数を返す。
    pub(crate) fn num_lvar(&self) -> usize {
        self.locals.len()
    }

    /// エラーメッセージを出す。
    ///
    /// * `message` - 出力するメッセージ
    pub(crate) fn error(&self, message: &str) -> ! {
        eprintln!("{}", self.input);

        for _ in 0..self.index {
            eprint!(" ");
        }

        eprint!("^ ");
        eprintln!("{}", message);

        exit(1)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    /// イテレータの `next()` で返す値は `Token` 型。
    type Item = Token<'a>;

    /// 次のトークンを読み返す。
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(content) => return content,
            _ => (),
        };

        self.skip_whitespace();

        if self.starts_with_before_whitespace("return") {
            return Some(Token::Return);
        }

        // 最後まで読んでいたら `None` を返す
        if self.index == self.input.len() {
            return None;
        }

        // 数値かどうかをチェック
        if let Some(num) = self.parse_number() {
            return Some(Token::Number(num));
        }

        if let Some(reserved) = RESERVEDS.iter().find(|e| self.cmp(&e)) {
            Some(Token::Reserved(reserved))
        } else if let Some(offset) = self.parse_lvar() {
            Some(Token::Identifier(offset))
        } else {
            self.error("トークナイズできません。")
        }
    }
}

/// 変数を保持するための構造体。
pub(crate) struct LVar<'a> {
    /// 変数名
    name: &'a str,
    /// rbp からのオフセット（マイナス方向）
    offset: usize,
}

impl<'a> LVar<'a> {
    pub(crate) fn new(name: &str, offset: usize) -> LVar {
        LVar { name, offset }
    }
}
