.DEFAULT_GOAL := all
NAME = day-05-doesnt-he-have-intern-elves-for-this

all: time-c time-node time-lisp time-rust time-go
time-c: a.out
	echo "\n\nTiming C..."
	time -l ./a.out

a.out: 
	clang ./$(NAME).c

run-c: a.out
	./a.out

time-rust: rust.out
	echo "\n\nTiming Rust..."
	time -l ./$(NAME)

rust.out:
	rustc ./$(NAME).rs

run-rust: rust.out
	./$(NAME)

run-node:
	node ./$(NAME).js

time-node:
	echo "\n\nTiming Node..."
	time -l node ./$(NAME).js

run-lisp:
	sbcl --script ./$(NAME).lisp

time-lisp:
	echo "\n\nTiming Lisp..."
	time -l sbcl --script ./$(NAME).lisp

run-go:
	go run ./$(NAME).go

go.out:
	go build -o go.out ./$(NAME).go

time-go: go.out
	echo "\n\nTiming Go..."
	time -l ./go.out

clean:
	rm -f ./*.out; \
	rm -f ./*test.txt; \
	rm -f ./$(NAME);
