use super::{NodeKind, Parser};
use crate::{tokenize::Token, util::Loc};

#[test]
fn test_empty_tokens() {
    let tokens = vec![Token::with_eof(Loc::at(0))];
    let nodes = Parser::new("", tokens).parse();
    assert!(nodes.is_err());
}

#[test]
fn test_one_num() {
    let tokens = vec![
        Token::with_num(10, Loc::range(0, 2)),
        Token::with_eof(Loc::at(2)),
    ];
    let nodes = Parser::new("10", tokens).parse().unwrap();
    assert_eq!(nodes.len(), 1);
    assert_eq!(nodes[0].data, NodeKind::Num(10));
    assert_eq!(nodes[0].loc, Loc::range(0, 2));
}

#[test]
fn test_two_num() {
    let tokens = vec![
        Token::with_num(1000, Loc::range(0, 4)),
        Token::with_num(856, Loc::range(5, 8)),
        Token::with_eof(Loc::at(7)),
    ];
    let nodes = Parser::new("1000 856", tokens).parse();
    assert!(nodes.is_err());
}
