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

echo OK