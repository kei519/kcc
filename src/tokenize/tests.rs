use crate::util::Loc;

use super::{TokenKind, Tokenizer};

#[test]
fn test_empty_input() {
    let input = "";
    let tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize();
    assert!(tokens.is_ok());
    let tokens = tokens.unwrap();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].data, TokenKind::Eof);
}

#[test]
fn test_some_input() {
    let input = "abc";
    let tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize();
    assert!(tokens.is_err());
}

#[test]
fn test_number() {
    let input = "123";
    let tokens = Tokenizer::new(input).tokenize().unwrap();
    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0].data, TokenKind::Num(123));
    assert_eq!(tokens[0].loc, Loc::range(0, 3));

    assert_eq!(tokens[1].data, TokenKind::Eof);
    assert_eq!(tokens[1].loc, Loc::at(3));
}
