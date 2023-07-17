.DEFAULT_GOAL := all
NAME = day-01-not-quite-lisp

all: time-c time-node time-lisp time-rust time-go
time-c: a.out
	time -l ./a.out

a.out:
	clang ./$(NAME).c

run-c: a.out
	./a.out

run-node:
	node ./$(NAME).js

time-node:
	time -l node ./$(NAME).js

time-lisp:
	time -l sbcl --script ./$(NAME).lisp

time-rust: rust.out
	time -l ./rust.out

run-rust: rust.out
	./rust.out

rust.out:
	rustc -o rust.out ./$(NAME).rs

run-go:
	go run ./$(NAME).go

go.out:
	go build -o ./go.out ./$(NAME).go

time-go: go.out
	time -l ./go.out


clean:
	rm -f ./*.out
	rm -f ./*-test.txt
	rm -f ./test.txt
	rm -f ./go.mod
