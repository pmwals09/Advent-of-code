.DEFAULT_GOAL := all
NAME = day-05-doesnt-he-have-intern-elves-for-this

all: time-c time-node time-lisp time-rust
time-c: a.out
	time ./a.out

a.out: 
	clang ./$(NAME).c

run-c: a.out
	./a.out

time-rust: $(NAME)
	time ./$(NAME)

$(NAME):
	rustc ./$(NAME).rs

run-rust: $(NAME)
	./$(NAME)

run-node:
	node ./$(NAME).js

time-node:
	time node ./$(NAME).js

run-lisp:
	sbcl --script ./$(NAME).lisp

time-lisp:
	time sbcl --script ./$(NAME).lisp

clean:
	rm -f ./*.out; \
	rm -f ./*test.txt; \
	rm -f ./$(NAME);
