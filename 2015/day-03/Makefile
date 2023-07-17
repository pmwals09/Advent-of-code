.DEFAULT_GOAL := all
NAME = day-03-perfectly-spherical-houses-in-a-vacuum

all: time-c time-node time-lisp time-go
time-c: build
	echo "\n\nTiming C..."
	time -l ./a.out

build: 
	clang ./$(NAME).c

run-c: build
	./a.out

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

time-go: go.out
	echo "\n\nTiming Golang..."
	time -l ./go.out

run-go:
	go run ./$(NAME)

go.out:
	go build -o go.out ./$(NAME).go

clean:
	rm -f ./*.out
	rm -f ./*test.txt
	rm -f ./go.mod
