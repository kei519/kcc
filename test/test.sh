#!/bin/bash

DIR=$(dirname $0)

cat <<EOF | gcc -xc -c -o $DIR/tmp2.o -
int ret3() { return 3; }
int ret5() { return 5; }
int add(int x, int y) { return x + y; }
int sub(int x, int y) { return x - y; }

int add6(int a, int b, int c, int d, int e, int f) {
	return a + b + c + d + e + f;
}

int add7(int a, int b, int c, int d, int e, int f, int g) {
	return a + b + c + d + e + f + g;
}
EOF

assert() {
	expected="$1"
	input="$2"

	"$DIR/../target/release/kcc" "$input" > "$DIR/tmp.s"
	cc -static -o "$DIR/tmp" "$DIR/tmp.s" "$DIR/tmp2.o"
	"$DIR/tmp"
	actual="$?"

	if [ "$actual" == "$expected" ]; then
		echo "$input => $actual"
	else
		echo "$input => $expected expected, but got $actual"
		exit 1
	fi
}

# one number test
assert 0 "return 0;"
assert 42 "return 42;"

# add / sub test
assert 21 "return 5+20-4;"
assert 0 "return 100-32+10-78;"

# power of tokenizer (skip whitespaces) test
assert 41 "return  12 + 34 - 5 ;"
assert 43 "return    43		-
93+93;"

# mul / div / parentheses test
assert 8 "return 12 * 4 - 40;"
assert 53 "return 50 + 30 / 10;"
assert 91 "return 9 * 9 + 100 / 10;"
assert 15 "return (3 + 2) * ( 4 - 1);"

# unary operators test
assert 33 "return -12 + 45;"
assert 18 "return -2 * -9;"
assert 24 "return (-4 + +1) * -(+5 - -3);"

# relational operators test
assert 1 "return 0 == 0;"
assert 0 "return 0 == 1;"
assert 1 "return 0 != 1;"
assert 0 "return 0 != 0;"
assert 1 "return 0 < 1;"
assert 0 "return 0 > 1;"
assert 1 "return 0 <= 0;"
assert 0 "return 0 > 0;"
assert 1 "return 0 <= 1;"
assert 0 "return 0 > 1;"
assert 1 "return 1 >= 0;"
assert 0 "return 1 < 0;"
assert 1 "return 0 >= 0;"
assert 0 "return 0 < 0;"
assert 1 "return 91 - 10 == 9 * 9;"
assert 101 "return (32 < 54) + 100;"

# multiple statements test
assert 1 "0; return 1;"
assert 84 "12 + 43; 100; 3*4/(43 + 2); return 85 -1;"

# return test
assert 140 "return 140;"
assert 10 "96 - -9; return (11 - 9) * 5;"
assert 1 "74; return 74 - 73; return 12;"

# variable test
assert 49 "a = 49; return a; "
assert 87 "hoge = 234; fuga = hoge - 87; return hoge - fuga;"
assert 16 "i = j = 16; return i;"
assert 16 "i = j = 16; return j;"
assert 34 "return (i = (j = 34));"

# while test
assert 10 "a = 0; while (a < 10) a = a + 1; return a;"
assert 16 "while (1) return 16; return 94;"
assert 2 "while (0) return 248; return 2;"
assert 48 "a = 0; while (a < 1) while (a < 48) a = a + 1; return a;"

# if test
assert 100 "if (1) return 100; return 50;"
assert 50 "if (0) return 100; return 50;"
assert 100 "if (1) return 100; else return 200; return 50;"
assert 200 "if (0) return 100; else return 200; return 50;"
assert 100 "a = 0; while (1) if (a < 100) a = a + 1; else return a;"

# for test
assert 10 "for(i = 0; i < 10; i = i + 1) 1; return i;"
assert 92 "i = 0; for (;;) if (i >= 92) return i; else i = i + 1;"
assert 46 "for (i = 100; ; i = i - 1) if (i == 46) return i;"

# block test
assert 123 "a = 3; { a = a + 100; a = a + 20; } return a;"
assert 8 "a = b = 0; while ( a + b < 10) { a = a + 1; b = b + 2; } return b;"

# calling function test
assert 3 "return ret3();"
assert 5 "return ret5();"
assert 11 "return add(3, 8);"
assert 5 "return sub(8, 3);"
assert 21 "return add6(1, 2, 3, 4, 5, 6);"
assert 28 "return add7(1, 2, 3, 4, 5, 6, 7);"

echo
echo OK
