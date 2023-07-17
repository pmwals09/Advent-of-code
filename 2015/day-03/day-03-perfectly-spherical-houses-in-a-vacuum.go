package main

import (
	"fmt"
	"os"
)

type Point struct {
	row int
	col int
}

func (p *Point) move(c byte) {
	switch string(c) {
	case "^":
		p.row += 1
	case ">":
		p.col += 1
	case "v":
		p.row -= 1
	case "<":
		p.col -= 1
	}
}

func main() {
	f, _ := os.ReadFile("./day-03-data.txt")

	partOnePos := Point{row: 0, col: 0}
	partTwoSantaPos := Point{row: 0, col: 0}
	partTwoRoboPos := Point{row: 0, col: 0}

	partOneGifts := make(map[Point]int)
	partTwoGifts := make(map[Point]int)

	partOneGifts[partOnePos] += 1
	partTwoGifts[partTwoSantaPos] += 1

	partOneCount := 1
	partTwoCount := 1

	for i, c := range f {
		partOnePos.move(c)
		if partOneGifts[partOnePos] == 0 {
			partOneCount += 1
		}
		partOneGifts[partOnePos] += 1

		var mover *Point

		if i%2 == 0 {
			mover = &partTwoSantaPos
		} else {
			mover = &partTwoRoboPos
		}

		mover.move(c)
		if partTwoGifts[*mover] == 0 {
			partTwoCount += 1
		}
		partTwoGifts[*mover] += 1
	}

	fmt.Println("Part one:", partOneCount)
	fmt.Println("Part two:", partTwoCount)
}
