package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
)

const TARGET = 150

func main() {
	f, _ := os.Open("./day-17-data.txt")
	defer f.Close()
	s := bufio.NewScanner(f)
	containers := []int{}
	for s.Scan() {
		containerSize, _ := strconv.Atoi(s.Text())
		containers = append(containers, containerSize)
	}

	valid := make([][]int, 0)
	makePaths(containers, []int{}, &valid)
	fmt.Println("Part one:", len(valid))
	minPathLen := math.MaxInt
	for _, p := range valid {
		pathLen := len(p)
		if pathLen < minPathLen {
			minPathLen = pathLen
		}
	}

	var minCount int
	for _, p := range valid {
		if len(p) == minPathLen {
			minCount++
		}
	}

	fmt.Println("Part two:", minCount)
}

func makePaths(remaining []int, path []int, valid *[][]int) {
	s := pathSum(path)

	if s == TARGET {
		*valid = append(*valid, path)
	} else if s > TARGET {
		return
	} else {
		for i, c := range remaining {
			makePaths(remaining[i+1:], append(path, c), valid)
		}
	}

}

func pathSum(path []int) int {
	var sum int

	for _, ea := range path {
		sum += ea
	}
	return sum
}
