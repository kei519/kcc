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

assert 0 0
assert 42 42

echo OK