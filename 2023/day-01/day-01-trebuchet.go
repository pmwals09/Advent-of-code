package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

func main() {
	input := getInput("./day-01-input.txt")
	lines := strings.Split(string(input), "\n")
	fmt.Println("Part one:", partOne(lines))
	fmt.Println("Part two:", partTwo(lines))
}

func getInput(filename string) []byte {
	ba, err := os.ReadFile(filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}
	return ba
}

func partOne(lines []string) int {
	sum := 0
	for _, line := range lines {
		var firstNum string
		var secondNum string
		for _, c := range line {
			if unicode.IsDigit(c) {
				firstNum = string(c)
				break
			}
		}

		for i := len(line) - 1; i >= 0; i-- {
			c := line[i]
			if unicode.IsDigit(rune(c)) {
				secondNum = string(c)
				break
			}
		}

		numString := firstNum + secondNum
		n, err := strconv.Atoi(numString)
		if err != nil {
			panic(fmt.Sprintf("Error converting string to int: %v", err))
		}
		sum += n
	}
	return sum
}

func partTwo(lines []string) int {
	sum := 0
	for _, line := range lines {
		var firstNum string
		var secondNum string
		for i := 0; i < len(line); i++ {
			if ok, val := isNumber(line, i); ok {
				firstNum = val
				break
			}
		}

		for i := len(line) - 1; i >= 0; i-- {
			if ok, val := isNumber(line, i); ok {
				secondNum = val
				break
			}
		}

		numString := normalizeNums(firstNum) + normalizeNums(secondNum)
		n, err := strconv.Atoi(numString)
		if err != nil {
			panic(fmt.Sprintf("Error converting string to int: %v", err))
		}
		sum += n
	}
	return sum
}

func isNumber(line string, i int) (bool, string) {
	if unicode.IsDigit(rune(line[i])) {
		return true, string(line[i])
	}
	return isStringNumber(line, i)
}

func isStringNumber(line string, startIndex int) (bool, string) {
	numStrings := []string{
		"one",
		"two",
		"three",
		"four",
		"five",
		"six",
		"seven",
		"eight",
		"nine",
	}

	for _, numString := range numStrings {
		endIndex := startIndex + len(numString)
		if endIndex > len(line) {
			continue
		}
		if line[startIndex:endIndex] == numString {
			return true, numString
		}
	}

	return false, ""
}

func normalizeNums(num string) string {
	if len(num) == 1 {
		return num
	}
	numStringMap := map[string]string{
		"one":   "1",
		"two":   "2",
		"three": "3",
		"four":  "4",
		"five":  "5",
		"six":   "6",
		"seven": "7",
		"eight": "8",
		"nine":  "9",
	}
	return numStringMap[num]
}
