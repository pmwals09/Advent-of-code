.DEFAULT_GOAL := all
NAME = day-04-the-ideal-stocking-stuffer

all: time-c time-node time-lisp
time-c: build
	time ./a.out

build: 
	clang ./$(NAME).c

run-c: build
	./a.out

time-rust: build-rust
	cd ./rust/ && \
	time ./rust/target/release/$(NAME); \
	cd ..

build-rust:
	cd ./rust/ && \
	cargo build --release; \
	cd ..

run-rust: build-rust
	cd ./rust/ && \
	cargo run; \
	cd ..

run-node:
	node ./$(NAME).js

time-node:
	time node ./$(NAME).js

run-lisp:
	sbcl --script ./$(NAME).lisp

time-lisp:
	time sbcl --script ./$(NAME).lisp

clean:
	rm -f ./*.out
	rm -f ./*test.txt
