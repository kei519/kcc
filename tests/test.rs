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
    if let Some(exit) = result.status.code() {
        exit
    } else {
        panic!("{}", result.status);
    }
}

#[test]
fn test1() {
    test!(0, "0;");
}

#[test]
fn test2() {
    test!(42, "42;");
}

#[test]
fn test3() {
    test!(21, "5+20-4;");
}

#[test]
fn test4() {
    test!(41, " 12 + 34 - 5 ;");
}

#[test]
fn test5() {
    test!(47, "5+6*7;");
}

#[test]
fn test6() {
    test!(15, "5*(9-6);");
}

#[test]
fn test7() {
    test!(4, "(3+5)/2;");
}

#[test]
fn test8() {
    test!(10, "-10+20;");
}

#[test]
fn test9() {
    test!(10, "- -10;");
}

#[test]
fn test10() {
    test!(10, "- - +10;");
}

#[test]
fn test11() {
    test!(0, "0 == 1;");
}

#[test]
fn test12() {
    test!(1, "42 == 42;");
}

#[test]
fn test13() {
    test!(1, "0 != 1;");
}

#[test]
fn test14() {
    test!(0, "42 != 42;");
}

#[test]
fn test15() {
    test!(1, "0 < 1;");
}

#[test]
fn test16() {
    test!(0, "1 < 1;");
}

#[test]
fn test17() {
    test!(0, "2 < 1;");
}

#[test]
fn test18() {
    test!(1, "0 <= 1;");
}

#[test]
fn test19() {
    test!(1, "1 <= 1;");
}

#[test]
fn test20() {
    test!(0, "2 <= 1;");
}

#[test]
fn test21() {
    test!(1, "1 > 0;");
}

#[test]
fn test22() {
    test!(0, "1 > 1;");
}

#[test]
fn test23() {
    test!(0, "1 > 2;");
}

#[test]
fn test24() {
    test!(1, "1 >= 0;");
}

#[test]
fn test25() {
    test!(1, "1 >= 1;");
}

#[test]
fn test26() {
    test!(0, "1 >= 2;");
}

#[test]
fn test27() {
    test!(3, "1; 2; 3;");
}
