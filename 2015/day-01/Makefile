.DEFAULT_GOAL := all

all: time-c time-node time-lisp time-rust time-go
time-c: build
	time -l ./a.out

build: 
	clang ./day-01-not-quite-lisp.c

run-c: build
	./a.out

run-node:
	node ./day-01-not-quite-lisp.js

time-node:
	time -l node ./day-01-not-quite-lisp.js

time-lisp:
	time -l sbcl --script ./day-01-not-quite-lisp.lisp

time-rust: day-01-not-quite-lisp
	time -l ./day-01-not-quite-lisp

run-rust: day-01-not-quite-lisp
	./day-01-not-quite-lisp

build-rust:
	rustc ./day-01-not-quite-lisp.rs

run-go: day-01-not-quite-lisp-go
	./day-01-not-quite-lisp-go

build-go:
	go build -o ./day-01-not-quite-lisp-go ./day-01-not-quite-lisp.go

time-go: day-01-not-quite-lisp-go
	time -l ./day-01-not-quite-lisp-go


clean:
	rm -f ./*.out
	rm -f ./*-test.txt
	rm -f ./test.txt
	rm -f ./day-01-not-quite-lisp
	rm -f ./go.mod
	rm -f ./day-01-not-quite-lisp-go
