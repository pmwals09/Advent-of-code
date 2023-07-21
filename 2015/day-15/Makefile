.DEFAULT_GOAL := all
NAME = day-15-science-for-hungry-people

all: time-node time-go

run-node:
	node ./$(NAME).js

time-node:
	@echo "Timing Node...\n"
	time -l node ./$(NAME).js
	@echo ""

run-go:
	go run ./$(NAME).go

go.out:
	go build -o go.out ./$(NAME).go

time-go: go.out
	@echo "Timing Go...\n"
	time -l ./go.out
	@echo ""

clean:
	rm -f ./*.out
	rm -f ./go.mod
