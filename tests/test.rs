#[test]
fn test_empty_string() {
    assert!(kcc::main([""]).is_ok());
}

#[test]
fn test_some_string() {
    assert!(kcc::main(["hoge"]).is_err());
}
