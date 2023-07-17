.DEFAULT_GOAL := all
NAME = day-02-i-was-told-there-would-be-no-math

all: time-c time-node time-lisp time-rust time-go

rust.out:
	rustc ./$(NAME).rs -o ./rust.out

time-rust: ./rust.out
	time ./rust.out

run-rust: ./rust.out
	./rust.out

time-c: ./a.out
	time ./a.out

a.out:
	clang ./$(NAME).c

run-c: ./a.out
	./a.out

run-node:
	node ./$(NAME).js

time-node:
	time node ./$(NAME).js

run-lisp:
	sbcl --script ./$(NAME).lisp

time-lisp:
	time sbcl --script ./$(NAME).lisp

go.out:
	go build -o go.out ./$(NAME).go

time-go: go.out
	time ./go.out

run-go:
	go run ./$(NAME).go

clean:
	rm -f ./*.out
	rm -f ./test.txt
	rm -f go.mod
