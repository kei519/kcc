use std::{
    hint,
    io::Write as _,
    process::{Command, Stdio},
    sync::atomic::{AtomicBool, Ordering},
};

/// Run a test with the given input and asserts the exit code and the expected.
macro_rules! test {
    ($expected:expr, $input:expr) => {
        assert_eq!($expected as i32, run_test($input));
    };
}

static OBJ_IS_BUILD: AtomicBool = AtomicBool::new(false);

static OBJ_IS_BUILDING: AtomicBool = AtomicBool::new(false);

const OBJ_PATH: &'static str = "tmp/tmp.o";

/// Run a test with the given input and returns the exit code.
fn run_test(input: impl Into<String>) -> i32 {
    while !OBJ_IS_BUILD.load(Ordering::Acquire) {
        if OBJ_IS_BUILDING
            .compare_exchange_weak(false, true, Ordering::Relaxed, Ordering::Relaxed)
            .is_err()
        {
            hint::spin_loop();
            continue;
        }
        build_obj();
        OBJ_IS_BUILD.store(true, Ordering::Release);
    }

    let test_file = kcc::mktemp().unwrap();
    kcc::main([
        "-o",
        test_file.to_str().unwrap(),
        "--obj",
        OBJ_PATH,
        input.into().as_str(),
    ])
    .unwrap();
    let result = Command::new(test_file).output().unwrap();
    if let Some(exit) = result.status.code() {
        exit
    } else {
        panic!("{}", result.status);
    }
}

fn build_obj() {
    let mut proc = Command::new("gcc")
        .arg("-c")
        .arg("-xc")
        .arg(format!("-o{}", OBJ_PATH))
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();

    let code = r"
int ret3() { return 3; }
int ret5() { return 5; }
int add(int x, int y) { return x+y; }
int sub(int x, int y) { return x-y; }

int add6(int a, int b, int c, int d, int e, int f) {
    return a+b+c+d+e+f;
}";
    writeln!(proc.stdin.as_ref().unwrap(), "{}", code).unwrap();

    if proc.wait().is_err() {
        panic!("cannot build object file");
    }
}

#[test]
fn test1() {
    test!(0, "int main() { return 0; }");
}

#[test]
fn test2() {
    test!(42, "int main() { return 42; }");
}

#[test]
fn test3() {
    test!(21, "int main() { return 5+20-4; }");
}

#[test]
fn test4() {
    test!(41, "int main() { return  12 + 34 - 5 ; }");
}

#[test]
fn test5() {
    test!(47, "int main() { return 5+6*7; }");
}

#[test]
fn test6() {
    test!(15, "int main() { return 5*(9-6); }");
}

#[test]
fn test7() {
    test!(4, "int main() { return (3+5)/2; }");
}

#[test]
fn test8() {
    test!(10, "int main() { return -10+20; }");
}

#[test]
fn test9() {
    test!(10, "int main() { return - -10; }");
}

#[test]
fn test10() {
    test!(10, "int main() { return - - +10; }");
}

#[test]
fn test11() {
    test!(0, "int main() { return 0 == 1; }");
}

#[test]
fn test12() {
    test!(1, "int main() { return 42 == 42; }");
}

#[test]
fn test13() {
    test!(1, "int main() { return 0 != 1; }");
}

#[test]
fn test14() {
    test!(0, "int main() { return 42 != 42; }");
}

#[test]
fn test15() {
    test!(1, "int main() { return 0 < 1; }");
}

#[test]
fn test16() {
    test!(0, "int main() { return 1 < 1; }");
}

#[test]
fn test17() {
    test!(0, "int main() { return 2 < 1; }");
}

#[test]
fn test18() {
    test!(1, "int main() { return 0 <= 1; }");
}

#[test]
fn test19() {
    test!(1, "int main() { return 1 <= 1; }");
}

#[test]
fn test20() {
    test!(0, "int main() { return 2 <= 1; }");
}

#[test]
fn test21() {
    test!(1, "int main() { return 1 > 0; }");
}

#[test]
fn test22() {
    test!(0, "int main() { return 1 > 1; }");
}

#[test]
fn test23() {
    test!(0, "int main() { return 1 > 2; }");
}

#[test]
fn test24() {
    test!(1, "int main() { return 1 >= 0; }");
}

#[test]
fn test25() {
    test!(1, "int main() { return 1 >= 1; }");
}

#[test]
fn test26() {
    test!(0, "int main() { return 1 >= 2; }");
}

#[test]
fn test27() {
    test!(1, "int main() { return 1; 2; 3; }");
}

#[test]
fn test28() {
    test!(2, "int main() { 1; return 2; 3; }");
}

#[test]
fn test29() {
    test!(3, "int main() { 1; 2; return 3; }");
}

#[test]
fn test30() {
    test!(3, "int main() { int a=3; return a; }");
}

#[test]
fn test31() {
    test!(8, "int main() { int a=3; int z=5; return a+z; }");
}

#[test]
fn test32() {
    test!(12, "int main() { int b; int a = b = 6; return a + b; }");
}

#[test]
fn test33() {
    test!(3, "int main() { int foo=3; return foo; }");
}

#[test]
fn test34() {
    test!(
        8,
        "int main() { int foo123=3; int bar=5; return foo123+bar; }"
    );
}

#[test]
fn test35() {
    test!(
        123,
        "int main() { int bar; int foo = bar = 50; int baz = 23; return foo + bar + baz; }"
    );
}

#[test]
fn test36() {
    test!(1, "int main() { int a; a = 1; return a; }");
}

#[test]
fn test37() {
    test!(30, "int main() { int a = 30; { 50; } return a; }");
}

#[test]
fn test38() {
    test!(50, "int main() { int a = 30; { return 50; } return a; }");
}

#[test]
fn test39() {
    test!(
        10,
        "int main() { int i = 0; while(i < 10) i = i + 1; return i; }"
    );
}

#[test]
fn test40() {
    test!(
        100,
        "int main() { int i = 0; while(i < 100) { int b = 10; i = i+b; } return i; }"
    );
}

#[test]
fn test41() {
    test!(
        55,
        "int main() { int i = 0; int j = 0; for (i = 0; i <= 10; i=i+1) j=i+j; return j; }"
    );
}

#[test]
fn test42() {
    test!(
        5,
        "int main() { int i = 200; int count = 0; for(i=0;i<10;) { i=i+2; count=count+1; } return count; }"
    );
}

#[test]
fn test43() {
    test!(3, "int main() { if (0) return 2; return 3; }");
}

#[test]
fn test44() {
    test!(3, "int main() { if (1-1) return 2; return 3; }");
}

#[test]
fn test45() {
    test!(2, "int main() { if (1) return 2; return 3; }");
}

#[test]
fn test46() {
    test!(2, "int main() { if (2-1) return 2; return 3; }");
}

#[test]
fn test47() {
    test!(
        1,
        "int main() { if (1) { return 1; } else if (0) { return 2; } else { return 3; } }"
    );
}

#[test]
fn test48() {
    test!(
        2,
        "int main() { if (0) { return 1; } else if (1) { return 2; } else { return 3; } }"
    );
}

#[test]
fn test49() {
    test!(
        3,
        "int main() { if (0) { return 1; } else if (0) { return 2; } else { return 3; } }"
    );
}

#[test]
fn test50() {
    test!(
        4,
        "int main() {if (0) { return 1; } else if (0) { return 2; }
            else if (1) { return 4; } else { return 3; }}"
    );
}

#[test]
fn test51() {
    test!(3, "int main() { return ret3(); }");
}

#[test]
fn test52() {
    test!(5, "int main() { return ret5(); }");
}

#[test]
fn test53() {
    test!(8, "int main() { return add(3, 5); }");
}

#[test]
fn test54() {
    test!(2, "int main() { return sub(5, 3); }");
}

#[test]
fn test55() {
    test!(21, "int main() { return add6(1, 2, 3, 4, 5, 6); }");
}

#[test]
fn test56() {
    test!(
        32,
        "int main() { return ret32(); } int ret32() { return 32; }"
    );
}

#[test]
fn test57() {
    test!(
        7,
        "int main() { return add2(3, 4); } int add2(int x, int y) { return x+y; }"
    );
}

#[test]
fn test58() {
    test!(
        1,
        "int main() { return sub2(4, 3); } int sub2(int x, int y) { return x-y; }"
    );
}

#[test]
fn test59() {
    test!(
        55,
        "int main() { return fib(9); }
            int fib(int x) { if (x <= 1) return 1; return fib(x-1) + fib(x-2); }"
    );
}

#[test]
fn test60() {
    test!(3, "int main() { int x = 3; return *&x; }");
}

#[test]
fn test61() {
    test!(
        3,
        "int main() { int x = 3; int *y = &x; int **z = &y; return **z; }"
    );
}

#[test]
fn test62() {
    test!(5, "int main() { int x = 3; int y = 5; return *(&x+1); }");
}

#[test]
fn test63() {
    test!(3, "int main() { int x = 3; int y = 5; return *(&y-1); }");
}

#[test]
fn test64() {
    test!(5, "int main() { int x=3; int *y=&x; *y=5; return x; }");
}

#[test]
fn test65() {
    test!(7, "int main() { int x=3; int y=5; *(&x+1)=7; return y; }");
}

#[test]
fn test66() {
    test!(7, "int main() { int x=3; int y=5; *(&y-1)=7; return x; }");
}

#[test]
fn test67() {
    test!(2, "int main() { int x=3; return (&x+2)-&x; }");
}

#[test]
fn test68() {
    test!(3, "int main() { int x[2]; int *y=&x; *y=3; return *x; }");
}

#[test]
fn test69() {
    test!(
        3,
        "int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *x; }"
    );
}

#[test]
fn test70() {
    test!(
        4,
        "int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *(x+1); }"
    );
}

#[test]
fn test71() {
    test!(
        5,
        "int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *(x+2); }"
    );
}

#[test]
fn test72() {
    test!(0, "int main() { int x[2][3]; int *y=x; *y=0; return **x; }");
}

#[test]
fn test73() {
    test!(
        1,
        "int main() { int x[2][3]; int *y=x; *(y+1)=1; return *(*x+1); }"
    );
}

#[test]
fn test74() {
    test!(
        2,
        "int main() { int x[2][3]; int *y=x; *(y+2)=2; return *(*x+2); }"
    );
}

#[test]
fn test75() {
    test!(
        3,
        "int main() { int x[2][3]; int *y=x; *(y+3)=3; return **(x+1); }"
    );
}

#[test]
fn test76() {
    test!(
        4,
        "int main() { int x[2][3]; int *y=x; *(y+4)=4; return *(*(x+1)+1); }"
    );
}

#[test]
fn test77() {
    test!(
        5,
        "int main() { int x[2][3]; int *y=x; *(y+5)=5; return *(*(x+1)+2); }"
    );
}

#[test]
fn test78() {
    test!(
        6,
        "int main() { int x[2][3]; int *y=x; *(y+6)=6; return **(x+2); }"
    );
}
