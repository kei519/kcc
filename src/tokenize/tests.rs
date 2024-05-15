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

#[test]
fn skip_white_space() {
    let input = "  123";
    let tokens = Tokenizer::new(input).tokenize().unwrap();
    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0].data, TokenKind::Num(123));
    assert_eq!(tokens[0].loc, Loc::range(2, 5));

    assert_eq!(tokens[1].data, TokenKind::Eof);
    assert_eq!(tokens[1].loc, Loc::at(5));
}

#[test]
fn test_plus() {
    let input = "+";
    let tokens = Tokenizer::new(input).tokenize().unwrap();
    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0].data, TokenKind::Reserved("+"));
    assert_eq!(tokens[0].loc, Loc::range(0, 1));

    assert_eq!(tokens[1].data, TokenKind::Eof);
    assert_eq!(tokens[1].loc, Loc::at(1));
}

#[test]
fn test_minus() {
    let input = "-";
    let tokens = Tokenizer::new(input).tokenize().unwrap();
    assert_eq!(tokens.len(), 2);

    assert_eq!(tokens[0].data, TokenKind::Reserved("-"));
    assert_eq!(tokens[0].loc, Loc::range(0, 1));

    assert_eq!(tokens[1].data, TokenKind::Eof);
    assert_eq!(tokens[1].loc, Loc::at(1));
}

#[test]
fn test_expr_with_plus_minus() {
    let input = "1 +\t2\n  -\r99  ";
    let tokens = Tokenizer::new(input).tokenize().unwrap();
    assert_eq!(tokens.len(), 6);

    assert_eq!(tokens[0].data, TokenKind::Num(1));
    assert_eq!(tokens[0].loc, Loc::range(0, 1));

    assert_eq!(tokens[1].data, TokenKind::Reserved("+"));
    assert_eq!(tokens[1].loc, Loc::range(2, 3));

    assert_eq!(tokens[2].data, TokenKind::Num(2));
    assert_eq!(tokens[2].loc, Loc::range(4, 5));

    assert_eq!(tokens[3].data, TokenKind::Reserved("-"));
    assert_eq!(tokens[3].loc, Loc::range(8, 9));

    assert_eq!(tokens[4].data, TokenKind::Num(99));
    assert_eq!(tokens[4].loc, Loc::range(10, 12));

    assert_eq!(tokens[5].data, TokenKind::Eof);
    assert_eq!(tokens[5].loc, Loc::at(14));
}
