.DEFAULT_GOAL := all
NAME = day-08-matchsticks

all: time-node time-go

run-node:
	node ./$(NAME).js

time-node:
	echo "\n\nTiming Node..."
	time -l node ./$(NAME).js

run-go:
	go run ./$(NAME).go

go.out:
	go build -o go.out ./$(NAME).go

time-go: go.out
	echo "\n\nTiming Go..."
	time -l ./go.out

clean:
	rm -f ./*.out
	rm -f ./*test.txt
