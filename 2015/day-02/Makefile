.DEFAULT_GOAL := all

all: time-c time-node time-lisp time-rust

rust.out:
	rustc ./day-02-i-was-told-there-would-be-no-math.rs -o ./rust.out

time-rust: ./rust.out
	time ./rust.out

run-rust: ./rust.out
	./rust.out

time-c: ./a.out
	time ./a.out

a.out:
	clang ./day-02-i-was-told-there-would-be-no-math.c

run-c: ./a.out
	./a.out

run-node:
	node ./day-02-i-was-told-there-would-be-no-math.js

time-node:
	time node ./day-02-i-was-told-there-would-be-no-math.js

run-lisp:
	sbcl --script ./day-02-i-was-told-there-would-be-no-math.lisp

time-lisp:
	time sbcl --script ./day-02-i-was-told-there-would-be-no-math.lisp

clean:
	rm -f ./*.out
	rm -f ./test.txt
