#!/bin/bash

file="result"

assert() {
	expected="$1"
	input="$2"

	if ! [ -d $file ] ; then
		mkdir $file
	fi

	target/debug/kcc "$input" > "$file/tmp.s"
	cc -o "$file/tmp" "$file/tmp.s"
	$file/tmp
	actual="$?"

	if [ "$actual" = "$expected" ]; then
		echo "$input => $actual"
	else
		echo "$input => $expected expected, but got $actual"
		exit 1
	fi
}

# 加減算
assert 21 "5+20-4"
assert 41 " 12 + 34 - 5 "

# 四則演算
assert 47 '5+6*7'
assert 15 '5*(9-6)'
assert 4 '(3+5)/2'

# 単項±
assert 10 '-10+20'
assert 22 '5--23+(2*-3)'

# 比較演算子
assert 1 '1 == 4 > 3'
assert 0 '2 * 3 == 5'
assert 1 '1 != 5 < 4'
assert 0 '9 != 81 / 9'
assert 1 '8 <= 8'
assert 1 '35 >= 10'

echo OK