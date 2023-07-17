package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	f, _ := os.Open("./day-05-data.txt")
	defer f.Close()

	scanner := bufio.NewScanner(f)
	var countPartOne int
	var countPartTwo int
	for scanner.Scan() {
		if isNicePartOne(scanner.Text()) {
			countPartOne++
		}

		if isNicePartTwo(scanner.Text()) {
			countPartTwo++
		}
	}

	fmt.Println("Part one:", countPartOne)
	fmt.Println("Part two:", countPartTwo)
}

func isNicePartOne(s string) bool {
	return !hasNaughtyPair(s) && hasDoubled(s) && hasThreeVowels(s)
}

func hasNaughtyPair(s string) bool {
	naughtyPairs := []string{"ab", "cd", "pq", "xy"}
	for _, p := range naughtyPairs {
		if strings.Index(s, p) > -1 {
			return true
		}
	}

	return false
}

func hasDoubled(s string) bool {
	for i := 1; i < len(s); i++ {
		if s[i] == s[i-1] {
			return true
		}
	}
	return false
}

func hasThreeVowels(s string) bool {
	vowels := []string{"a", "e", "i", "o", "u"}
	var count int
	for i := 0; i < len(s) && count < 3; i++ {
		for j := 0; j < len(vowels); j++ {
			if string(s[i]) == vowels[j] {
				count++
			}
		}
	}

	return count >= 3
}

func isNicePartTwo(s string) bool {
	return hasDoublePair(s) && hasAbaPattern(s)
}

func hasDoublePair(s string) bool {
	for i := 0; i < len(s)-3; i++ {
		for j := i + 2; j < len(s)-1; j++ {
			if s[i:i+2] == s[j:j+2] {
				return true
			}
		}
	}
	return false
}

func hasAbaPattern(s string) bool {
	for i := 0; i < len(s)-2; i++ {
		if s[i] == s[i+2] {
			return true
		}
	}

	return false
}
