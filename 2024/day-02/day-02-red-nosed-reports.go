package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

const (
	_ int = iota
	INCREASING
	DECREASING
)

func main() {
	ba, err := os.ReadFile("./day-02-input.txt")
	if err != nil {
		panic(fmt.Sprintf("Error opening file: %s", err.Error()))
	}

	fmt.Println("Part One:", PartOne(string(ba)))
	fmt.Println("Part Two:", PartTwo(string(ba)))
}

func PartOne(input string) int {
	lines := strings.Split(strings.Trim(input, "\n"), "\n")
	var count int
	for _, line := range lines {
		l := strings.Split(line, " ")
		if isSafe(l, -1) {
			count++
		}
	}
	return count
}

func PartTwo(input string) int {
	lines := strings.Split(strings.Trim(input, "\n"), "\n")
	var count int
	for _, line := range lines {
		l := strings.Split(line, " ")
		for i := -1; i < len(l); i++ {
			if isSafe(l, i) {
				count++
				break
			}
		}
	}
	return count
}

func isSafe(line []string, idxToRemove int) bool {
    l := make([]string, len(line))
    copy(l, line)
	deltas := getLevelDeltas(l, idxToRemove)
	direction := getDirection(deltas)

	for _, d := range deltas {
		if !testDelta(d, direction) {
			return false
		}
	}

	return true
}

func testDelta(el int, direction int) bool {
	var passDir, stepSize bool
	if direction == INCREASING && el > 0 {
		passDir = true
	} else if direction == DECREASING && el < 0 {
		passDir = true
	}

	if abs(el) > 0 && abs(el) < 4 {
		stepSize = true
	}

	return passDir && stepSize
}

func getLevelDeltas(levels []string, idxToRemove int) []int {
	if idxToRemove >= 0 {
		levels = append(levels[:idxToRemove], levels[idxToRemove+1:]...)
	}
	deltas := make([]int, 0, len(levels)-1)
	curr := getInt(levels[0])
	for i := 1; i < len(levels); i++ {
		next := getInt(levels[i])
		deltas = append(deltas, next-curr)
		curr = next
	}
	return deltas
}

func getDirection(deltas []int) int {
	switch {
	case deltas[0] > 0:
		return INCREASING
	case deltas[0] < 0:
		return DECREASING
	default:
		return 0
	}
}

func getInt(n string) int {
	val, err := strconv.Atoi(n)
	if err != nil {
		panic(fmt.Sprintf("Not a number: %s", n))
	}
	return val
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}
