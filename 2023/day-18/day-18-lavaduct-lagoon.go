package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	input := getInput("./day-18-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input))
}

func getInput(path string) string {
	ba, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return string(ba)
}

type Point struct {
	Row int
	Col int
}

func (p Point) Copy() Point {
	return Point{
		Row: p.Row,
		Col: p.Col,
	}
}

func PartOne(input string) int {
	var perimeter int
	vertices := []Point{{0, 0}}
	lines := strings.Split(input, "\n")
	for _, line := range lines {
		dir, distance := ParseLinePartOne(line)
		GetVertex(&vertices, &perimeter, dir, distance)
	}

	return GetArea(vertices, perimeter)
}

func ParseLinePartOne(line string) (string, int) {
	parts := strings.Fields(line)
	dir, amt := parts[0], parts[1]
	distance, _ := strconv.Atoi(amt)
	return dir, distance
}

func PartTwo(input string) int {
	var perimeter int
	vertices := []Point{{0, 0}}
	lines := strings.Split(input, "\n")
	for _, line := range lines {
		dir, distance := ParseLinePartTwo(line)
		GetVertex(&vertices, &perimeter, dir, distance)
	}

	return GetArea(vertices, perimeter)
}

func ParseLinePartTwo(line string) (string, int) {
	hexStr := strings.Trim(strings.Fields(line)[2], "(#)")
  hexDist, hexDir := hexStr[0:len(hexStr)-1], hexStr[len(hexStr)-1]
  i, _ := strconv.ParseInt(hexDist, 16, 0)
  directions := []string{"R", "D", "L", "U"}
  return directions[int(hexDir)-'0'], int(i)
}

func GetVertex(vertices *[]Point, perimeter *int, dir string, distance int) {
	last := (*vertices)[len(*vertices)-1]
	newVertex := last.Copy()
	switch dir {
	case "U":
		newVertex.Row -= distance
	case "R":
		newVertex.Col += distance
	case "D":
		newVertex.Row += distance
	case "L":
		newVertex.Col -= distance
	}
	*vertices = append(*vertices, newVertex)
	*perimeter += distance
}

// https://stackoverflow.com/questions/451426/how-do-i-calculate-the-area-of-a-2d-polygon/717367#717367
func GetArea(vertices []Point, perimeter int) int {
	var area int
	for i := 0; i < len(vertices)-1; i += 2 {
		area += vertices[i+1].Col*(vertices[i+2].Row-vertices[i].Row) + vertices[i+1].Row*(vertices[i].Col-vertices[i+2].Col)
	}

	return area/2 + perimeter/2 + 1
}
