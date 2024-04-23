#!/bin/bash

DIR=$(dirname $0)

assert() {
	expected="$1"
	input="$2"

	"$DIR/../target/release/kcc" "$input" > "$DIR/tmp.s"
	cc -o "$DIR/tmp" "$DIR/tmp.s"
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
assert 0 0
assert 42 42

# add / sub test
assert 21 "5+20-4"
assert 0 "100-32+10-78"

# power of tokenizer (skip whitespaces) test
assert 41 " 12 + 34 - 5 "
assert 43 "   43		-
93+93"

# mul / div / parentheses test
assert 8 "12 * 4 - 40"
assert 53 "50 + 30 / 10"
assert 91 "9 * 9 + 100 / 10"
assert 15 "(3 + 2) * ( 4 - 1)"

# unary operators test
assert 33 "-12 + 45"
assert 18 "-2 * -9"
assert 24 "(-4 + +1) * -(+5 - -3)"

# relational operators test
assert 1 "0 == 0"
assert 0 "0 == 1"
assert 1 "0 != 1"
assert 0 "0 != 0"
assert 1 "0 < 1"
assert 0 "0 > 1"
assert 1 "0 <= 0"
assert 0 "0 > 0"
assert 1 "0 <= 1"
assert 0 "0 > 1"
assert 1 "1 >= 0"
assert 0 "1 < 0"
assert 1 "0 >= 0"
assert 0 "0 < 0"
assert 1 "91 - 10 == 9 * 9"
assert 101 "(32 < 54) + 100"

echo OK
