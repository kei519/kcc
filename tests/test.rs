use std::process::Command;

/// Run a test with the given input and asserts the exit code and the expected.
macro_rules! test {
    ($expected:expr, $input:expr) => {
        assert_eq!($expected as i32, run_test($input));
    };
}

/// Run a test with the given input and returns the exit code.
fn run_test(input: impl Into<String>) -> i32 {
    let test_file = kcc::mktemp().unwrap();
    kcc::main(["-o", test_file.to_str().unwrap(), input.into().as_str()]).unwrap();
    let result = Command::new(test_file).output().unwrap();
    result.status.code().unwrap()
}

#[test]
fn test1() {
    test!(0, "0");
}

#[test]
fn test2() {
    test!(42, "42");
}

#[test]
fn test3() {
    test!(21, "5+20-4");
}

#[test]
fn test4() {
    test!(41, " 12 + 34 - 5 ");
}

#[test]
fn test5() {
    test!(47, "5+6*7");
}

#[test]
fn test6() {
    test!(15, "5*(9-6)");
}

#[test]
fn test7() {
    test!(4, "(3+5)/2");
}

#[test]
fn test8() {
    test!(10, "-10+20");
}

#[test]
fn test9() {
    test!(10, "- -10");
}

#[test]
fn test10() {
    test!(10, "- - +10");
}
