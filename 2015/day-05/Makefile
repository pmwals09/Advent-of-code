.DEFAULT_GOAL := all

all: time-c time-node
time-c: build
	time ./a.out

build: 
	clang ./day-05-doesnt-he-have-intern-elves-for-this.c

run-c: build
	./a.out

run-node:
	node ./day-05-doesnt-he-have-intern-elves-for-this.js

time-node:
	time node ./day-05-doesnt-he-have-intern-elves-for-this.js

clean:
	rm ./*.out
	rm ./*-test.txt
