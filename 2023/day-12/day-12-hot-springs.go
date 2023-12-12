// Credit to https://github.com/pemoreau/advent-of-code/blob/main/go/2023/12/day12.go
package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type S struct {
	Pattern string
	Groups  string
}

func NewS(pattern string, groups []uint8) S {
	return S{
		Pattern: pattern,
		Groups:  string(groups),
	}
}

type Cache struct {
	C map[S]int
}

func (c *Cache) Set(pattern string, groups []uint8, value int) int {
	c.C[NewS(pattern, groups)] = value
	return value
}

func (c *Cache) Get(pattern string, groups []uint8) (int, bool) {
	val, ok := c.C[NewS(pattern, groups)]
	return val, ok
}

func (c *Cache) Clear() {
	for k := range c.C {
		delete(c.C, k)
	}
}

var c = Cache{C: map[S]int{}}

func main() {
	input := getInput("./day-12-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input))
}

func getInput(filename string) string {
	ba, err := os.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	return string(ba)
}

func PartOne(input string) int {
	lines := strings.Split(input, "\n")
	var total int
	for _, line := range lines {
		pattern, counts := GetPatternAndCounts(line)
		total += GetLineCombinations(pattern, counts)
	}
	c.Clear()
	return total
}

func PartTwo(input string) int {
	lines := strings.Split(input, "\n")
	var total int
	for _, line := range lines {
		pattern, counts := GetPatternAndCounts(line)
		expandedPattern := pattern
		expandedCounts := make([]uint8, len(counts))
		copy(expandedCounts, counts)
		for i := 0; i < 4; i++ {
			expandedPattern += ("?" + pattern)
			expandedCounts = append(expandedCounts, counts...)
		}
		total += GetLineCombinations(expandedPattern, expandedCounts)
	}
	c.Clear()
	return total
}

func GetPatternAndCounts(line string) (string, []uint8) {
	pattern, counts, _ := strings.Cut(line, " ")
	nStrs := strings.Split(counts, ",")
	nums := []uint8{}
	for _, n := range nStrs {
		i, _ := strconv.Atoi(n)
		nums = append(nums, uint8(i))
	}
	return pattern, nums
}

func GetLineCombinations(pattern string, groups []uint8) int {
	if len(pattern) == 0 && len(groups) == 0 {
		return 1
	}

	if len(pattern) == 0 {
		return 0
	}

	if val, ok := c.Get(pattern, groups); ok {
		return val
	}

	minPatternLen := 0
	for _, g := range groups {
		minPatternLen += int(g)
	}
	minPatternLen += (len(groups) - 1) // for the dots in between each group
	if len(pattern) < minPatternLen {
		return c.Set(pattern, groups, 0)
	}

	switch pattern[0] {
  case '.':
		return c.Set(pattern, groups, GetLineCombinations(pattern[1:], groups))
	case '?':
		return c.Set(pattern, groups, GetLineCombinations(pattern[1:], groups)+GetLineCombinations("#"+pattern[1:], groups))
	case '#':
		{
			if len(groups) == 0 {
				return c.Set(pattern, groups, 0)
			}

			nextGroupSize := int(groups[0])
			nextDotIdx := strings.Index(pattern, ".")
			if nextDotIdx == -1 {
				nextDotIdx = len(pattern)
			}
			if nextDotIdx < nextGroupSize {
				return c.Set(pattern, groups, 0)
			}

			afterGroup := pattern[nextGroupSize:]
			if len(afterGroup) == 0 {
				return c.Set(pattern, groups, GetLineCombinations(afterGroup, groups[1:]))
			}
			if afterGroup[0] == '#' {
				return c.Set(pattern, groups, 0)
			}
			return c.Set(pattern, groups, GetLineCombinations(afterGroup[1:], groups[1:]))
		}
	}
	panic("Unknown pattern character")
}
