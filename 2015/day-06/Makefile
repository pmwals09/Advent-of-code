.DEFAULT_TARGET := all
.PHONY = node build
all: time-c time-node
time-c: build
	time ./a.out
time-node:
	time node ./day-06-probably-a-fire-hazard.js
node:
	node ./day-06-probably-a-fire-hazard.js
run-c: build
	./a.out
a.out:
	clang ./day-06-probably-a-fire-hazard.c
build:
	clang ./day-06-probably-a-fire-hazard.c
clean:
	if [ -f ./*.out ] ; then rm ./*.out; fi
	if [ -f ./*-test.txt ] ; then rm ./*-test.txt; fi
leaks: build
	leaks --atExit -- ./a.out

