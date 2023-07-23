package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	f, _ := os.Open("./day-18-data.txt")
	s := bufio.NewScanner(f)
	originalGrid := [][]byte{}
	for s.Scan() {
		row := []byte{}
		for _, c := range s.Text() {
			if c == '.' {
				row = append(row, 0)
			} else if c == '#' {
				row = append(row, 1)
			}
		}
		originalGrid = append(originalGrid, row)
	}

	gridA := copyFromOriginal(originalGrid)
	gridB := makeBlank(gridA)

	for i := 0; i < 100; i++ {
		if i%2 == 0 {
			step(&gridA, &gridB, getNeighborSum)
		} else {
			step(&gridB, &gridA, getNeighborSum)
		}
	}

	fmt.Println("Part one:", sumGrid(gridA))

	gridA = copyFromOriginal(originalGrid)
	gridB = makeBlank(gridA)
	for i := 0; i < 100; i++ {
		if i%2 == 0 {
			step(&gridA, &gridB, getNeighborSumWithCorners)
		} else {
			step(&gridB, &gridA, getNeighborSumWithCorners)
		}
	}

	fmt.Println("Part two:", sumGridWithCorners(gridA))
}

func copyFromOriginal(arr [][]byte) [][]byte {
	out := [][]byte{}
	for rowIdx, row := range arr {
		out = append(out, make([]byte, len(row)))
		for colIdx, val := range row {
			out[rowIdx][colIdx] = val
		}
	}
	return out
}

func makeBlank(arr [][]byte) [][]byte {
	out := [][]byte{}
	for range arr {
		out = append(out, make([]byte, len(arr[0])))
	}
	return out
}

func step(from *[][]byte, to *[][]byte, sumFn func(*[][]byte, int, int) int) {
	for rowIdx := 0; rowIdx < len(*from); rowIdx++ {
		row := (*from)[rowIdx]
		for colIdx := 0; colIdx < len(row); colIdx++ {
			val := row[colIdx]
			neighborSum := sumFn(from, rowIdx, colIdx)
			if val == 1 && !(neighborSum == 2 || neighborSum == 3) {
				(*to)[rowIdx][colIdx] = 0
			} else if val == 0 && neighborSum == 3 {
				(*to)[rowIdx][colIdx] = 1
			} else {
				(*to)[rowIdx][colIdx] = (*from)[rowIdx][colIdx]
			}
		}
	}

}

func sumGrid(g [][]byte) int {
	var sum int
	for _, row := range g {
		for _, ea := range row {
			sum += int(ea)
		}
	}
	return sum
}

func sumGridWithCorners(g [][]byte) int {
	var sum int
	for rowIdx, row := range g {
		for colIdx, ea := range row {
			if isCorner(rowIdx, colIdx, len(g)-1) {
				sum += 1
			} else {
				sum += int(ea)
			}
		}
	}
	return sum
}

func getNeighborSum(arr *[][]byte, rowIdx int, colIdx int) int {
	var sum int
	for i := rowIdx - 1; i <= rowIdx+1; i++ {
		for j := colIdx - 1; j <= colIdx+1; j++ {
			if isOutOfBounds(i, j, len(*arr)-1) || (i == rowIdx && j == colIdx) {
				continue
			} else {
				sum += int((*arr)[i][j])
			}
		}
	}
	return sum
}

func getNeighborSumWithCorners(arr *[][]byte, rowIdx int, colIdx int) int {
	var sum int
	for i := rowIdx - 1; i <= rowIdx+1; i++ {
		for j := colIdx - 1; j <= colIdx+1; j++ {
			if isOutOfBounds(i, j, len(*arr)-1) || (i == rowIdx && j == colIdx) {
				continue
			} else if isCorner(i, j, len(*arr)-1) {
				sum += 1
			} else {
				sum += int((*arr)[i][j])
			}
		}
	}
	return sum
}

func isOutOfBounds(rowIdx int, colIdx int, max int) bool {
	return rowIdx < 0 || colIdx < 0 || rowIdx > max || colIdx > max
}

func isCorner(rowIdx int, colIdx int, max int) bool {
	isUpperLeft := rowIdx == 0 && colIdx == 0
	isUpperRight := rowIdx == 0 && colIdx == max
	isLowerLeft := rowIdx == max && colIdx == 0
	isLowerRight := rowIdx == max && colIdx == max
	return isUpperLeft || isUpperRight || isLowerLeft || isLowerRight
}
