.DEFAULT_GOAL := all
NAME = day-10-elves-look-elves-say

all: time-node time-go

run-node:
	node ./$(NAME).js

time-node:
	@echo "Timing Node..."
	time -l node ./$(NAME).js
	@echo ""

run-go:
	go run ./$(NAME).go

go.out:
	go build -o go.out ./$(NAME).go

time-go: go.out
	@echo "Timing Go..."
	time -l ./go.out
	@echo ""

clean:
	rm -f ./*.out
	rm -f ./go.mod

