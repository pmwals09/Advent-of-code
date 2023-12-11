package main

import (
	"fmt"
	"os"
	"strings"
)

type Node struct {
	Char    rune
	Row     int
	Col     int
	Visited bool
}

func NewNode(c rune, rIdx int, cIdx int) Node {
	return Node{
		Char:    c,
		Row:     rIdx,
		Col:     cIdx,
		Visited: false,
	}
}

func EmptyNode() Node {
	return Node{
		Char:    '0',
		Row:     0,
		Col:     0,
		Visited: false,
	}
}

func (n *Node) Equals(other *Node) bool {
	return n.Row == other.Row && n.Col == other.Col
}

func (c *Node) ValidTopNeighbor(pipeMap PipeMap) (bool, *Node) {
	if c.Row == 0 {
		return false, nil
	}
	neighborNode := pipeMap.M[c.Row-1][c.Col]
	char := neighborNode.Char
	if char == '|' || char == 'F' || char == '7' || char == 'S' {
		return true, neighborNode
	}
	return false, nil
}

func (c *Node) ValidRightNeighbor(pipeMap PipeMap) (bool, *Node) {
	if c.Col == len(pipeMap.M[0])-1 {
		return false, nil
	}
	neighborNode := pipeMap.M[c.Row][c.Col+1]
	char := neighborNode.Char
	if char == '-' || char == 'J' || char == '7' || char == 'S' {
		return true, neighborNode
	}

	return false, nil
}

func (c *Node) ValidBottomNeighbor(pipeMap PipeMap) (bool, *Node) {
	if c.Row < len(pipeMap.M)-1 {
		neighborNode := pipeMap.M[c.Row+1][c.Col]
		char := neighborNode.Char
		if char == '|' || char == 'L' || char == 'J' || char == 'S' {
			return true, neighborNode
		}
		return false, nil
	}
	return false, nil
}

func (c *Node) ValidLeftNeighbor(pipeMap PipeMap) (bool, *Node) {
	if c.Col > 0 {
		neighborNode := pipeMap.M[c.Row][c.Col-1]
		char := neighborNode.Char
		if char == '-' || char == 'L' || char == 'F' || char == 'S' {
			return true, neighborNode
		}
		return false, nil
	}
	return false, nil
}

func (c *Node) GetValidNeighbors(pipeMap PipeMap) []*Node {
	neighbors := []*Node{}
	switch c.Char {
	case 'S':
		if ok, n := c.ValidTopNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidRightNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidBottomNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidLeftNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
	case 'L':
		if ok, n := c.ValidTopNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidRightNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
	case 'F':
		if ok, n := c.ValidRightNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidBottomNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
	case 'J':
		if ok, n := c.ValidTopNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidLeftNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
	case '7':
		if ok, n := c.ValidBottomNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidLeftNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
	case '-':
		if ok, n := c.ValidRightNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidLeftNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
	case '|':
		if ok, n := c.ValidTopNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
		if ok, n := c.ValidBottomNeighbor(pipeMap); ok {
			neighbors = append(neighbors, n)
		}
	}
	return neighbors
}

func (n *Node) GetUnvisitedNeighbors(pipeMap *PipeMap) []*Node {
	out := []*Node{}
	if n.Row > 0 {
		neighborNode := pipeMap.M[n.Row-1][n.Col]
		if !neighborNode.Visited {
			out = append(out, neighborNode)
		}
	}
	if n.Col > 0 {
		neighborNode := pipeMap.M[n.Row][n.Col-1]
		if !neighborNode.Visited {
			out = append(out, neighborNode)
		}
	}
	if n.Row < len(pipeMap.M)-1 {
		neighborNode := pipeMap.M[n.Row+1][n.Col]
		if !neighborNode.Visited {
			out = append(out, neighborNode)
		}
	}
	if n.Col < len(pipeMap.M[0])-1 {
		neighborNode := pipeMap.M[n.Row][n.Col+1]
		if !neighborNode.Visited {
			out = append(out, neighborNode)
		}
	}
	return out
}

type Path struct {
	Course []*Node
}

type PipeMap struct {
	M [][]*Node
}

func NewPipeMap(input string) PipeMap {
	pm := PipeMap{}
	pm.M = [][]*Node{}
	rows := strings.Split(input, "\n")
	for i, r := range rows {
		nodeRow := []*Node{}
		for j, c := range r {
			n := NewNode(c, i, j)
			nodeRow = append(nodeRow, &n)
		}
		pm.M = append(pm.M, nodeRow)
	}
	return pm
}

func main() {
	input := getInput("./day-10-input.txt")
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
	pipeMap := NewPipeMap(input)

	paths := GetPaths(pipeMap)

	return (len(paths[0].Course) - 1) / 2
}

func PartTwo(input string) int {
	pipeMap := NewPipeMap(input)
	path := GetPaths(pipeMap)[0]
	for _, n := range path.Course {
		n.Visited = true
	}

	total := 0
	inside := false
	var prevPath *Node = nil
	for _, row := range pipeMap.M {
		for _, c := range row {
			if c.Visited {
				switch c.Char {
				case '|':
					inside = !inside
					prevPath = nil
				case 'L', 'F':
					prevPath = c
				case 'J':
					if prevPath.Char == 'F' {
						inside = !inside
					}
					prevPath = nil
				case '7':
					if prevPath.Char == 'L' {
						inside = !inside
					}
          prevPath = nil
				}
			} else {
				prevPath = nil
				if inside {
					total++
				}
			}
		}
	}
	return total
}

func GetPaths(pm PipeMap) []Path {
	var start *Node
	for _, row := range pm.M {
		for _, c := range row {
			if c.Char == 'S' {
				start = c
			}
		}
	}
	start.Visited = true
	paths := []Path{}
	neighbors := start.GetValidNeighbors(pm)
	for _, n := range neighbors {
		paths = append(paths, Path{[]*Node{start, n}})
	}

	done := []Path{}
	for len(paths) > 0 {
		p := paths[0]
		paths = paths[1:]

		lastNode := p.Course[len(p.Course)-1]
		if lastNode.Row == start.Row && lastNode.Col == start.Col {
			done = append(done, p)
		} else {
			neighbors := lastNode.GetValidNeighbors(pm)
			for _, n := range neighbors {
				if len(p.Course) > 1 {
					if !n.Equals(p.Course[len(p.Course)-2]) {
						newCourse := make([]*Node, len(p.Course))
						copy(newCourse, p.Course)
						newCourse = append(newCourse, n)
						newPath := Path{newCourse}
						paths = append(paths, newPath)
					}
				} else {
					newCourse := make([]*Node, len(p.Course))
					copy(newCourse, p.Course)
					newCourse = append(newCourse, n)
					newPath := Path{newCourse}
					paths = append(paths, newPath)
				}
			}
		}
	}
	return done
}
