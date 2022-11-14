.DEFAULT_GOAL := all

all: time-c time-node time-lisp time-rust
time-c: build
	time ./a.out

build: 
	clang ./day-01-not-quite-lisp.c

run-c: build
	./a.out

run-node:
	node ./day-01-not-quite-lisp.js

time-node:
	time node ./day-01-not-quite-lisp.js

time-lisp:
	time sbcl --script ./day-01-not-quite-lisp.lisp

time-rust: day-01-not-quite-lisp
	time ./day-01-not-quite-lisp

run-rust: day-01-not-quite-lisp
	./day-01-not-quite-lisp

build-rust:
	rustc ./day-01-not-quite-lisp.rs

clean:
	rm -f ./*.out
	rm -f ./*-test.txt
	rm -f ./test.txt
	rm -f ./day-01-not-quite-lisp
