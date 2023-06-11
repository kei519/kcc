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

assert 21 "5+20-4"
assert 41 " 12 + 34 - 5 "

assert 47 '5+6*7'
assert 15 '5*(9-6)'
assert 4 '(3+5)/2'

echo OK