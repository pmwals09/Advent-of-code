package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	ba, err := os.ReadFile("./day-03-input.txt")
	if err != nil {
		panic(fmt.Sprintf("Cannot open file: %s", err.Error()))
	}

	fmt.Println("Part One: ", PartOne(string(ba)))
	fmt.Println("Part Two: ", PartTwo(string(ba)))
}

func PartOne(input string) int {
	re := regexp.MustCompile(`mul\((?P<first>\d+),(?P<second>\d+)\)`)
	matches := re.FindAllStringSubmatch(input, -1)
	var sum int
	for _, m := range matches {
		sum += parseMatch(m)
	}
	return sum
}

func PartTwo(input string) int {
	re := regexp.MustCompile(`mul\((?P<first>\d+),(?P<second>\d+)\)|do\(\)|don't\(\)`)
	matches := re.FindAllStringSubmatch(input, -1)
	enabled := make([][]string, 0)
	on := true
	for _, m := range matches {
		switch {
		case strings.HasPrefix(m[0], "mul") && on:
			enabled = append(enabled, m)
		case m[0] == "don't()":
			on = false
		case m[0] == "do()":
			on = true
		}
	}
    var sum int
	for _, e := range enabled {
        sum += parseMatch(e)
	}

	return sum
}

func parseMatch(match []string) int {
	return unsafeInt(match[1]) * unsafeInt(match[2])
}

func unsafeInt(val string) int {
	v, err := strconv.Atoi(val)
	if err != nil {
		panic(fmt.Sprintf("Error parsing value: %q", val))
	}
	return v
}
