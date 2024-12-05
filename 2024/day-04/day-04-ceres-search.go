package main

import (
	"fmt"
	"os"
	"strings"
)

type path struct {
	currIdx     int
	currRuneIdx int // the current index in `letters`
	direction   []int
	startIdx    int
}

// assuming we're working on a finished path, so p.currRuneIdx == last rune idx
func (p path) getIdxFromLetterIdx(letterIdx, lineLen int) int {
	numIter := abs(p.currRuneIdx - letterIdx)
	curr := p.currIdx
	for i := numIter; i > 0; i-- {
		curr = moveDirection(curr, lineLen, []int{-1 * p.direction[0], -1 * p.direction[1]})
	}
	return curr
}

func abs(val int) int {
	if val < 0 {
		return -1 * val
	}
	return val
}

type neighbor struct {
	idx       int
	direction []int
}

func main() {
	ba, err := os.ReadFile("./day-04-input.txt")
	if err != nil {
		panic(fmt.Sprintf("error opening file: %s", err.Error()))
	}

	fmt.Println("Part One: ", PartOne(string(ba)))
	fmt.Println("Part Two: ", PartTwo(string(ba)))
}

func PartOne(input string) int {
	letters := []rune{'X', 'M', 'A', 'S'}
	matches := getMatches(input, letters)
	return len(matches)
}

func PartTwo(input string) int {
	letters := []rune{'M', 'A', 'S'}
	matches := getMatches(input, letters)
	lineLen := strings.Index(input, "\n")

	aMap := make(map[int][]path)
	for _, m := range matches {
		aIdx := m.getIdxFromLetterIdx(1, lineLen)
		if _, ok := aMap[aIdx]; !ok {
			aMap[aIdx] = []path{m}
		} else {
			aMap[aIdx] = append(aMap[aIdx], m)
		}
	}

	var count int
	for _, v := range aMap {
		if len(v) >= 2 && hasXMatch(v, lineLen) {
			count++
		}
	}
	return count
}

func hasXMatch(candidates []path, lineLen int) bool {
	// needs 2 items in candidates where the startIdx differs by 2 rows or 2 cols
	for i, c := range candidates {
		for j, d := range candidates {
			if i == j {
				continue
			}

			if c.startIdx+2 == d.startIdx {
				return true
			}

			if c.startIdx-2 == d.startIdx {
				return true
			}

			if c.startIdx+2*lineLen+2 == d.startIdx {
				return true
			}

			if c.startIdx-(2*lineLen+2) == d.startIdx {
				return true
			}
		}
	}
	return false
}

func getMatches(input string, letters []rune) []path {
	candidates := make([]path, 0)
	matches := make([]path, 0)
	lineLen := strings.Index(input, "\n")
	for i, c := range input {
		if c == letters[0] {
			candidates = append(candidates, path{i, 0, nil, i})
		}
	}

	for len(candidates) > 0 {
		c := candidates[0]
		candidates = candidates[1:]

		if c.currRuneIdx == len(letters)-1 {
			matches = append(matches, c)
		} else {
			neighbors := getNeighbors(
				c.currIdx,
				len(input)-1,
				lineLen,
				c.direction)
			for _, n := range neighbors {
				if rune(input[n.idx]) == letters[c.currRuneIdx+1] {
					candidates = append(
						candidates,
						path{n.idx, c.currRuneIdx + 1, n.direction, c.startIdx})
				}
			}
		}
	}

	return matches
}

func moveDirection(currIdx, lineLen int, direction []int) int {
	newIdx := currIdx + (direction[0] * (lineLen + 1)) + direction[1]
	return newIdx
}

func getNeighbors(idx int, maxIdx int, lineLen int, direction []int) []neighbor {
	out := make([]neighbor, 0, 8)

	if len(direction) > 0 {
		newIdx := moveDirection(idx, lineLen, direction)
		if newIdx >= 0 && newIdx <= maxIdx {
			out = append(out, neighbor{newIdx, direction})
		}
		return out
	}

	offsets := []int{lineLen + 2, lineLen + 1, lineLen, 1}
	lowDirection := [][]int{{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}}
	highDirection := [][]int{{1, 1}, {1, 0}, {1, -1}, {0, 1}}
	for i, o := range offsets {
		if idx-o >= 0 {
			out = append(out, neighbor{idx - o, lowDirection[i]})
		}
		if idx+o <= maxIdx {
			out = append(out, neighbor{idx + o, highDirection[i]})
		}
	}

	return out
}
