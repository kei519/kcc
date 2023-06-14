use std::process::exit;

/// トークン（とその値）保持用の列挙型。
#[derive(Debug)]
pub(crate) enum Token<'a> {
    Reserved(&'a str),
    Number(u32),
}

/// トークナイズのための構造体。
pub(crate) struct Tokenizer<'a, 'b> {
    input: &'a str,
    pos: usize,
    peeked: Option<Option<Token<'b>>>,
    index: usize,
}

impl<'a, 'b> Tokenizer<'a, 'b> {
    /// トークナイザの初期化。
    /// 
    /// * `input` - トークナイズする入力。
    pub(crate) fn new(input: &str) -> Tokenizer {
        return Tokenizer {
            /// 入力された文字列
            input,
            /// 現在のスライスの位置
            pos: 0,
            /// peek した次のトークン
            peeked: None,
            /// 現在何文字読んだか
            index: 0,
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
                    if is_first { is_first = false; }
                    n = n * 10 + (c as u8 - b'0') as u32;
                    self.pos += c.len_utf8();
                    self.index += 1;
                }
                _ => {
                    if is_first { return None; }
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

    /// 次のトークンを消費せずに見る。
    pub(crate) fn peek(&mut self) -> Option<&Token> {
        let next = self.next();
        self.peeked.get_or_insert_with(|| next).as_ref()
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

impl<'a, 'b> Iterator for Tokenizer<'a, 'b> {
    /// イテレータの `next()` で返す値は `Token` 型。
    type Item = Token<'b>;

    /// 次のトークンを読み返す。
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(content) => return content,
            _ => (),
        };

        self.skip_whitespace();

        // 最後まで読んでいたら `None` を返す
        if self.index == self.input.len() {
            return None;
        }

        // 数値かどうかをチェック
        match self.parse_number() {
            Some(num) => return Some(Token::Number(num)),
            _ => (),
        };

        if self.cmp("+") {
            Some(Token::Reserved("+"))
        } else if self.cmp("-") {
            Some(Token::Reserved("-"))
        } else if self.cmp("*") {
            Some(Token::Reserved("*"))
        } else if self.cmp("/") {
            Some(Token::Reserved("/"))
        } else if self.cmp("(") {
            Some(Token::Reserved("("))
        } else if self.cmp(")") {
            Some(Token::Reserved(")"))
        } else if self.cmp("<=") {
            Some(Token::Reserved("<="))
        } else if self.cmp(">=") {
            Some(Token::Reserved(">="))
        } else if self.cmp("<") {
            Some(Token::Reserved("<"))
        } else if self.cmp(">") {
            Some(Token::Reserved(">"))
        } else if self.cmp("==") {
            Some(Token::Reserved("=="))
        } else if self.cmp("!=") {
            Some(Token::Reserved("!="))
        } else {
            self.error("トークナイズできません。")
        }
    }
}