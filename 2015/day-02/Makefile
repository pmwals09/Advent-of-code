.DEFAULT_GOAL := all

all: time-c time-node time-lisp
time-c: build
	time ./a.out

build: 
	clang ./day-02-i-was-told-there-would-be-no-math.c

run-c: build
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

