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

echo OK
