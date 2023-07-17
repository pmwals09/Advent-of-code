.DEFAULT_GOAL := all
NAME = day-04-the-ideal-stocking-stuffer

all: time-c time-node time-lisp time-go
time-c: build
	time ./a.out

build: 
	clang ./$(NAME).c

run-c: build
	./a.out

time-rust: rust.out
	cd ./rust/ && \
	time ./rust/target/release/$(NAME); \
	cd ..

rust.out:
	cd ./rust/ && \
	cargo build -0 rust.out --release; \
	cd ..

run-rust: rust.out
	cd ./rust/ && \
	cargo run; \
	cd ..

run-node:
	node ./$(NAME).js

time-node:
	echo "\n\nTiming Node..."
	time -l node ./$(NAME).js

run-lisp:
	sbcl --script ./$(NAME).lisp

time-lisp:
	echo "\n\nTiming Lisp..."
	time sbcl --script ./$(NAME).lisp

go.out:
	go build -o go.out ./$(NAME).go

time-go: go.out
	echo "\n\nTiming Go..."
	time -l ./go.out

clean:
	rm -f ./*.out
	rm -f ./*test.txt
