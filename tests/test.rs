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
    test!(0, "return 0;");
}

#[test]
fn test2() {
    test!(42, "return 42;");
}

#[test]
fn test3() {
    test!(21, "return 5+20-4;");
}

#[test]
fn test4() {
    test!(41, "return  12 + 34 - 5 ;");
}

#[test]
fn test5() {
    test!(47, "return 5+6*7;");
}

#[test]
fn test6() {
    test!(15, "return 5*(9-6);");
}

#[test]
fn test7() {
    test!(4, "return (3+5)/2;");
}

#[test]
fn test8() {
    test!(10, "return -10+20;");
}

#[test]
fn test9() {
    test!(10, "return - -10;");
}

#[test]
fn test10() {
    test!(10, "return - - +10;");
}

#[test]
fn test11() {
    test!(0, "return 0 == 1;");
}

#[test]
fn test12() {
    test!(1, "return 42 == 42;");
}

#[test]
fn test13() {
    test!(1, "return 0 != 1;");
}

#[test]
fn test14() {
    test!(0, "return 42 != 42;");
}

#[test]
fn test15() {
    test!(1, "return 0 < 1;");
}

#[test]
fn test16() {
    test!(0, "return 1 < 1;");
}

#[test]
fn test17() {
    test!(0, "return 2 < 1;");
}

#[test]
fn test18() {
    test!(1, "return 0 <= 1;");
}

#[test]
fn test19() {
    test!(1, "return 1 <= 1;");
}

#[test]
fn test20() {
    test!(0, "return 2 <= 1;");
}

#[test]
fn test21() {
    test!(1, "return 1 > 0;");
}

#[test]
fn test22() {
    test!(0, "return 1 > 1;");
}

#[test]
fn test23() {
    test!(0, "return 1 > 2;");
}

#[test]
fn test24() {
    test!(1, "return 1 >= 0;");
}

#[test]
fn test25() {
    test!(1, "return 1 >= 1;");
}

#[test]
fn test26() {
    test!(0, "return 1 >= 2;");
}

#[test]
fn test27() {
    test!(1, "return 1; 2; 3;");
}

#[test]
fn test28() {
    test!(2, "1; return 2; 3;");
}

#[test]
fn test29() {
    test!(3, "1; 2; return 3;");
}

#[test]
fn test30() {
    test!(3, "int a=3; return a;");
}

#[test]
fn test31() {
    test!(8, "int a=3; int z=5; return a+z;");
}

#[test]
fn test32() {
    test!(12, "int b; int a = b = 6; return a + b;");
}

#[test]
fn test33() {
    test!(3, "int foo=3; return foo;");
}

#[test]
fn test34() {
    test!(8, "int foo123=3; int bar=5; return foo123+bar;");
}

#[test]
fn test35() {
    test!(
        123,
        "int bar; int foo = bar = 50; int baz = 23; return foo + bar + baz;"
    );
}

#[test]
fn test36() {
    test!(1, "int a; a = 1; return a;");
}

#[test]
fn test37() {
    test!(30, "int a = 30; { 50; } return a;");
}

#[test]
fn test38() {
    test!(50, "int a = 30; { return 50; } return a;");
}

#[test]
fn test39() {
    test!(10, "int i = 0; while(i < 10) i = i + 1; return i;");
}

#[test]
fn test40() {
    test!(
        100,
        "int i = 0; while(i < 100) { int b = 10; i = i+b; } return i;"
    );
}

#[test]
fn test41() {
    test!(
        55,
        "int i = 0; int j = 0; for (i = 0; i <= 10; i=i+1) j=i+j; return j;"
    );
}

#[test]
fn test42() {
    test!(
        5,
        "int i = 200; int count = 0; for(i=0;i<10;) { i=i+2; count=count+1; } return count;"
    );
}

#[test]
fn test43() {
    test!(3, "if (0) return 2; return 3;");
}

#[test]
fn test44() {
    test!(3, "if (1-1) return 2; return 3;");
}

#[test]
fn test45() {
    test!(2, "if (1) return 2; return 3;");
}

#[test]
fn test46() {
    test!(2, "if (2-1) return 2; return 3;");
}

#[test]
fn test47() {
    test!(
        1,
        "if (1) { return 1; } else if (0) { return 2; } else { return 3; }"
    );
}

#[test]
fn test48() {
    test!(
        2,
        "if (0) { return 1; } else if (1) { return 2; } else { return 3; }"
    );
}

#[test]
fn test49() {
    test!(
        3,
        "if (0) { return 1; } else if (0) { return 2; } else { return 3; }"
    );
}

#[test]
fn test50() {
    test!(
        4,
        "if (0) { return 1; } else if (0) { return 2; }
            else if (1) { return 4; } else { return 3; }"
    );
}
