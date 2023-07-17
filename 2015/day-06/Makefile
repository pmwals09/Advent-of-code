.DEFAULT_GOAL := all
NAME = day-06-probably-a-fire-hazard

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
	time -l ./rust.out

rust.out:
	rustc -o rust.out ./$(NAME).rs

run-rust: rust.out
	./rust.out

run-node:
	node ./$(NAME).js

time-node:
	echo "\n\nTiming Node..."
	time -l node ./$(NAME).js

run-lisp:
	sbcl --script ./$(NAME).lisp

time-lisp:
	time sbcl --script ./$(NAME).lisp

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
	rm -f ./*.fasl; \
	rm -f ./$(NAME)

leaks: a.out
	leaks --atExit -- ./a.out

