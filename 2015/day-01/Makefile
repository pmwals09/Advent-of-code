.DEFAULT_GOAL := all

all: time-c time-node time-lisp
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

clean:
	rm -f ./*.out
	rm -f ./*-test.txt
	rm -f ./test.txt
