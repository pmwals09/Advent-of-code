package main

import (
	"fmt"
	"math"
	"os"
	"strings"
)

type Galaxy struct {
	Row int
	Col int
}

func main() {
	input := getInput("./day-11-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input, 1000000 - 1))
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
	emptyRows := GetEmptyRows(lines)
	emptyCols := GetEmptyCols(lines)
	galaxies := GetUnadjustedGalaxies(lines)

	total := 0
	for first := 0; first < len(galaxies)-1; first++ {
		for second := first + 1; second < len(galaxies); second++ {
			adjustedFirst := AdjustGalaxyCoordinates(galaxies[first], emptyRows, emptyCols, 1)
			adjustedSecond := AdjustGalaxyCoordinates(galaxies[second], emptyRows, emptyCols, 1)

			total += int(math.Abs(float64(adjustedFirst.Row-adjustedSecond.Row))) + int(math.Abs(float64(adjustedFirst.Col-adjustedSecond.Col)))
		}
	}
	return total
}

func PartTwo(input string, scale int) int {
	lines := strings.Split(input, "\n")
	emptyRows := GetEmptyRows(lines)
	emptyCols := GetEmptyCols(lines)
	galaxies := GetUnadjustedGalaxies(lines)

	total := 0
	for first := 0; first < len(galaxies)-1; first++ {
		for second := first + 1; second < len(galaxies); second++ {
			adjustedFirst := AdjustGalaxyCoordinates(galaxies[first], emptyRows, emptyCols, scale)
			adjustedSecond := AdjustGalaxyCoordinates(galaxies[second], emptyRows, emptyCols, scale)

			total += int(math.Abs(float64(adjustedFirst.Row-adjustedSecond.Row))) + int(math.Abs(float64(adjustedFirst.Col-adjustedSecond.Col)))
		}
	}
	return total
}

func GetUnadjustedGalaxies(lines []string) []Galaxy {
	galaxies := []Galaxy{}
	for i, l := range lines {
		for j, c := range l {
			if c == '#' {
				galaxies = append(galaxies, Galaxy{Row: i, Col: j})
			}
		}
	}
	return galaxies
}

func GetEmptyRows(lines []string) []int {
	emptyRows := []int{}
	for i, l := range lines {
		hasFound := false
		for _, c := range l {
			if c == '#' {
				hasFound = true
			}
		}
		if !hasFound {
			emptyRows = append(emptyRows, i)
		}
	}
	return emptyRows
}

func GetEmptyCols(lines []string) []int {
	emptyCols := []int{}
	for colIdx := 0; colIdx < len(lines); colIdx++ {
		hasFound := false
		for rowIdx := 0; rowIdx < len(lines[0]); rowIdx++ {
			c := lines[rowIdx][colIdx]
			if c == '#' {
				hasFound = true
			}
		}
		if !hasFound {
			emptyCols = append(emptyCols, colIdx)
		}
	}
	return emptyCols
}

func AdjustGalaxyCoordinates(g Galaxy, emptyRows []int, emptyCols []int, scale int) Galaxy {
	rowAdjust := 0
	colAdjust := 0
	for _, rowIdx := range emptyRows {
		if rowIdx < g.Row {
			rowAdjust++
		}
	}
	g.Row += (rowAdjust * scale)
	for _, colIdx := range emptyCols {
		if colIdx < g.Col {
			colAdjust++
		}
	}
	g.Col += (colAdjust * scale)
	return g
}
