.DEFAULT_GOAL := all
NAME = day-07-some-assembly-required

all: time-node time-lisp time-go
time-c: a.out
	echo "\n\nTiming C..."
	time ./a.out

a.out:
	clang ./$(NAME).c

run-c: a.out
	./a.out

run-node:
	node ./$(NAME).js

time-node:
	echo "\n\nTiming Node..."
	time -l node ./$(NAME).js

run-lisp:
	sbcl --script ./$(NAME).lisp

time-lisp:
	time -l sbcl --script ./$(NAME).lisp

go.out:
	go build -o go.out ./$(NAME).go

time-go: go.out
	echo "\n\nTiming Go..."
	time -l ./go.out

run-go:
	go run ./$(NAME).go

clean:
	rm -f ./*.out
	rm -f ./*test.txt
	rm -f ./*.fasl

leaks: build
	leaks --atExit -- ./a.out

