package main

import (
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	ba, err := os.ReadFile("./day-01-input.txt")
	if err != nil {
		panic(fmt.Errorf("Problem reading input: %s", err.Error()))
	}

	fmt.Println("Part One:", PartOne(string(ba)))
	fmt.Println("Part Two:", PartTwo(string(ba)))
}

func PartOne(input string) int {
	leftValues := make([]int, 0)
	rightValues := make([]int, 0)

	for _, line := range strings.Split(input, "\n") {
		if len(strings.Trim(line, " ")) > 0 {
			left, right, _ := strings.Cut(line, "   ")
			leftValues = append(leftValues, unsafeToInt(left))
			rightValues = append(rightValues, unsafeToInt(right))
		}
	}

	slices.Sort(leftValues)
	slices.Sort(rightValues)

	var sum int

	for i := range leftValues {
		sum += int(math.Abs(float64(leftValues[i] - rightValues[i])))
	}

	return sum
}

func unsafeToInt(val string) int {
	out, err := strconv.Atoi(val)
	if err != nil {
		panic(fmt.Errorf("Error converting string to int: %s", err.Error()))
	}
	return out
}

func PartTwo(input string) int {
	leftValues := make([]int, 0)
	rightCounts := make(map[int]int)

	for _, line := range strings.Split(input, "\n") {
		if len(strings.Trim(line, " ")) > 0 {
			left, right, _ := strings.Cut(line, "   ")
			leftValues = append(leftValues, unsafeToInt(left))
			rightCounts[unsafeToInt(right)]++
		}
	}

    var sum int

    for _, val := range leftValues {
        sum += val * rightCounts[val]
    }

	return sum
}
