package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"unicode"
)

type Schematic struct {
	RawInput   string
	Numbers    []Number
	Gears      []Gear
	LineLength int
}

func (s Schematic) GetNeighborIndices(ri RelevantItem) []int {
	indices := []int{}
	// top neighbors
	rangeStart := ri.GetOffset() - s.LineLength - 2
	rangeEnd := rangeStart + ri.GetLength() + 1
	cappedRange, err := s.CappedRange(rangeStart, rangeEnd)
	if err == nil {
		for i := cappedRange.Start; i <= cappedRange.End; i++ {
			indices = append(indices, i)
		}
	}

	// right neighbor
	rightIdx := ri.GetOffset() + ri.GetLength()
	if s.IdxInBounds(rightIdx) {
		indices = append(indices, rightIdx)
	}

	// bottom neighbors
	rangeStart = ri.GetOffset() + s.LineLength
	rangeEnd = rangeStart + ri.GetLength() + 1
	cappedRange, err = s.CappedRange(rangeStart, rangeEnd)
	if err == nil {
		for i := cappedRange.Start; i <= cappedRange.End; i++ {
			indices = append(indices, i)
		}
	}

	// left neighbor
	leftIdx := ri.GetOffset() - 1
	if s.IdxInBounds(leftIdx) {
		indices = append(indices, leftIdx)
	}
	return indices
}

type Range struct {
	Start int
	End   int
}

func (s Schematic) CappedRange(start int, end int) (Range, error) {
	if (start < 0 && end < 0) || (start >= len(s.RawInput) && end >= len(s.RawInput)) {
		return Range{}, fmt.Errorf("Entire range is out of bounds")
	}

	rangeStart := start
	rangeEnd := end
	if start < 0 {
		rangeStart = 0
	} else if s.RawInput[start] == '\n' {
		rangeStart = start + 1
	}

	if end >= len(s.RawInput) {
		rangeEnd = len(s.RawInput) - 1
	} else if s.RawInput[end] == '\n' {
		rangeEnd = end - 1
	}

	return Range{Start: rangeStart, End: rangeEnd}, nil
}

func (s Schematic) IdxInBounds(idx int) bool {
	return idx >= 0 && idx < len(s.RawInput) && rune(s.RawInput[idx]) != '\n'
}

type RelevantItem interface {
	GetOffset() int
	GetLength() int
	Intersects(other RelevantItem, s Schematic) bool
}

type Number struct {
	Raw    string
	Offset int
	Length int
	Value  int
}

func (n Number) GetOffset() int {
	return n.Offset
}

func (n Number) GetLength() int {
	return n.Length
}

func (n Number) Intersects(other RelevantItem, s Schematic) bool {
	thisNeighbors := s.GetNeighborIndices(n)
	for _, n := range thisNeighbors {
		for i := other.GetOffset(); i < other.GetOffset()+other.GetLength(); i++ {
			if n == i {
				return true
			}
		}
	}
	return false
}

type Gear struct {
	Offset int
}

func (g Gear) GetOffset() int {
	return g.Offset
}

func (g Gear) GetLength() int {
	return 1
}

func (g Gear) Intersects(other RelevantItem, s Schematic) bool {
	thisNeighbors := s.GetNeighborIndices(g)
	for _, n := range thisNeighbors {
		for i := other.GetOffset(); i < other.GetOffset()+other.GetLength(); i++ {
			if n == i {
				return true
			}
		}
	}
	return false
}

func NewSchematic(input string) Schematic {
	s := Schematic{RawInput: input}
	for i := 0; i < len(input); i++ {
		c := rune(input[i])
		if c == '\n' && s.LineLength == 0 {
			s.LineLength = i
		}
		if unicode.IsDigit(c) {
			n := Number{}
			n.Offset = i
			n.Length = 1

			nextIdx := i + 1
			for nextIdx < len(input) && unicode.IsDigit(rune(input[nextIdx])) {
				n.Length++
				nextIdx++
			}

			n.Raw = input[i : i+n.Length]
			n.Value, _ = strconv.Atoi(n.Raw)
			i += n.Length - 1
			s.Numbers = append(s.Numbers, n)
		}
		if c == '*' {
			g := Gear{}
			g.Offset = i

			s.Gears = append(s.Gears, g)
		}
	}

	return s
}

func main() {
	input := getInput("./day-03-input.txt")
	s := NewSchematic(input)
	fmt.Println("Part One: ", PartOne(s))
	fmt.Println("Part Two: ", PartTwo(s))
}

func getInput(filepath string) string {
	ba, err := os.ReadFile(filepath)
	if err != nil {
		panic(fmt.Sprintf("error reading file: %s", err))
	}
	return string(ba)
}

func PartOne(schematic Schematic) int {
	var total int
	for _, num := range schematic.Numbers {
		neighborsIdx := schematic.GetNeighborIndices(num)
		if slices.ContainsFunc(neighborsIdx, func(idx int) bool {
			r := rune(schematic.RawInput[idx])
			return !unicode.IsDigit(r) && r != '.'
		}) {
			total += num.Value
		}
	}
	return total
}

func PartTwo(schematic Schematic) int {
	var total int
	for _, g := range schematic.Gears {
		neighboringNumbers := []Number{}
		for _, n := range schematic.Numbers {
			if g.Intersects(n, schematic) {
				neighboringNumbers = append(neighboringNumbers, n)
			}
		}
		if len(neighboringNumbers) == 2 {
			ratio := neighboringNumbers[0].Value * neighboringNumbers[1].Value
			total += ratio
		}
	}
	return total
}
