use std::process::Command;

/// Run a test with the given input and asserts the exit code and the expected.
macro_rules! test {
    ($input:expr, $expected:expr) => {
        assert_eq!(run_test($input), $expected as i32);
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
fn test_num1() {
    test!("0", 0);
}

#[test]
fn test_num2() {
    test!("47", 47);
}

#[test]
fn test_rand_num() {
    let rand = kcc::rand() as u8;
    test!(rand.to_string(), rand);
}

#[test]
fn test_add() {
    test!("1+2", 3);
}

#[test]
fn test_sub() {
    test!("58-9", 49);
}

#[test]
fn test_add_sub() {
    test!("1\t+  2\t\t-\r3\n", 0);
}
