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
