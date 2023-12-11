package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	input := getInput("./day-09-input.txt")
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
	results := []int{}
	for _, l := range lines {
		rows := [][]int{}
		buildReductionRows(l, &rows)

		for i := len(rows) - 2; i >= 0; i-- {
			rows[i] = append(rows[i], rows[i][len(rows[i])-1]+rows[i+1][len(rows[i+1])-1])
		}

		results = append(results, rows[0][len(rows[0])-1])
	}
	var total int
	for _, r := range results {
		total += r
	}

	return total
}

func PartTwo(input string) int {
	lines := strings.Split(input, "\n")
	results := []int{}
	for _, l := range lines {
		rows := [][]int{}
    buildReductionRows(l, &rows)

    for i := len(rows) - 2; i >= 0; i-- {
      rows[i][0] = rows[i][0] - rows[i + 1][0]
    }

    results = append(results, rows[0][0])
	}

	var total int
	for _, r := range results {
		total += r
	}

	return total
}

func firstRow(line string) []int {
	nums := strings.Fields(line)
	firstRow := []int{}
	for _, num := range nums {
		n, _ := strconv.Atoi(num)
		firstRow = append(firstRow, n)
	}
	return firstRow
}

func notAllZeroes(row []int) bool {
	return slices.ContainsFunc(
		row,
		func(i int) bool {
			return i != 0
		},
	)
}

func buildReductionRows(line string, rows *[][]int) {
	*rows = append(*rows, firstRow(line))
	for notAllZeroes((*rows)[len(*rows)-1]) {
		prevRow := (*rows)[len(*rows)-1]
		newRow := []int{}
		for i := 1; i < len(prevRow); i++ {
			a := prevRow[i-1]
			b := prevRow[i]
			newRow = append(newRow, b-a)
		}
		*rows = append(*rows, newRow)
	}
}
