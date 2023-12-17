package main

import (
	"fmt"
	"os"
	"slices"
	"strings"
)

type Cell struct {
	Visited           bool
	Char              rune
	Row               int
	Col               int
	DirectionsCrossed []Direction
}

type Direction int

const (
	Up Direction = iota
	Right
	Down
	Left
)

type Cursor struct {
	Row                int
	Col                int
	LastKnownDirection Direction
}

func (c *Cursor) MoveUp() {
	c.Row--
	c.LastKnownDirection = Up
}

func (c *Cursor) MoveRight() {
	c.Col++
	c.LastKnownDirection = Right
}

func (c *Cursor) MoveDown() {
	c.Row++
	c.LastKnownDirection = Down
}

func (c *Cursor) MoveLeft() {
	c.Col--
	c.LastKnownDirection = Left
}

func (c *Cursor) Copy() Cursor {
	return Cursor{Row: c.Row, Col: c.Col, LastKnownDirection: c.LastKnownDirection}
}

func (c *Cursor) IsInBounds(grid [][]*Cell) bool {
	return c.Row >= 0 && c.Row < len(grid) && c.Col >= 0 && c.Col < len(grid[c.Row])
}

func main() {
	input := getInput("./day-16-input.txt")
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
	grid := MakeGrid(input)
	cursors := []Cursor{{Row: 0, Col: 0, LastKnownDirection: Right}}
	for len(cursors) > 0 {
		cursor := cursors[0]
		cursors = cursors[1:]
		if !cursor.IsInBounds(grid) {
			continue
		}
		cursorCell := grid[cursor.Row][cursor.Col]
		if slices.Contains(cursorCell.DirectionsCrossed, cursor.LastKnownDirection) {
			continue
		}
		VisitCell(cursorCell, cursor, &cursors)
	}

	var total int
	for _, row := range grid {
		for _, cell := range row {
			if cell.Visited {
				total++
			}
		}
	}
	return total
}

func PartTwo(input string) int {
	// get all the edge cells
	grid := MakeGrid(input)
	startCursors := []Cursor{}
	for rowIdx, row := range grid {
		for colIdx := range row {
			if rowIdx == 0 {
				startCursors = append(startCursors, Cursor{Row: rowIdx, Col: colIdx, LastKnownDirection: Down})
			}
			if rowIdx == len(grid)-1 {
				startCursors = append(startCursors, Cursor{Row: rowIdx, Col: colIdx, LastKnownDirection: Up})
			}
			if colIdx == 0 {
				startCursors = append(startCursors, Cursor{Row: rowIdx, Col: colIdx, LastKnownDirection: Right})
			}
			if colIdx == len(grid[rowIdx])-1 {
				startCursors = append(startCursors, Cursor{Row: rowIdx, Col: colIdx, LastKnownDirection: Left})
			}
		}
	}
	var maxPower int
	for _, startCursor := range startCursors {
    grid = MakeGrid(input)
		cursors := []Cursor{startCursor}
		for len(cursors) > 0 {
			cursor := cursors[0]
			cursors = cursors[1:]
			if !cursor.IsInBounds(grid) {
				continue
			}
			cursorCell := grid[cursor.Row][cursor.Col]
			if slices.Contains(cursorCell.DirectionsCrossed, cursor.LastKnownDirection) {
				continue
			}
			VisitCell(cursorCell, cursor, &cursors)
		}

		var total int
		for _, row := range grid {
			for _, cell := range row {
				if cell.Visited {
					total++
				}
			}
		}
		if total > maxPower {
			maxPower = total
		}
	}
	return maxPower
}

func VisitCell(cursorCell *Cell, cursor Cursor, cursors *[]Cursor) {
	cursorCell.Visited = true
	cursorCell.DirectionsCrossed = append(cursorCell.DirectionsCrossed, cursor.LastKnownDirection)
	c := cursorCell.Char
	var updatedCursors []Cursor
	switch c {
	case '.':
		updatedCursors = HandleDot(cursor)
	case '-':
		updatedCursors = HandleDash(cursor)
	case '|':
		updatedCursors = HandlePipe(cursor)
	case '/':
		updatedCursors = HandleSlash(cursor)
	case '\\':
		updatedCursors = HandleBackslash(cursor)
	default:
		panic(fmt.Sprintf("Unknown char: %c", c))
	}
	*cursors = append(*cursors, updatedCursors...)
}

func HandleDot(cursor Cursor) []Cursor {
	if cursor.LastKnownDirection == Up {
		cursor.MoveUp()
	} else if cursor.LastKnownDirection == Right {
		cursor.MoveRight()
	} else if cursor.LastKnownDirection == Down {
		cursor.MoveDown()
	} else if cursor.LastKnownDirection == Left {
		cursor.MoveLeft()
	} else {
		panic(fmt.Sprintf("Unknown direction: %d", cursor.LastKnownDirection))
	}
	return []Cursor{cursor}
}

func HandleDash(cursor Cursor) []Cursor {
	if cursor.LastKnownDirection == Up || cursor.LastKnownDirection == Down {
		newCursor := cursor.Copy()
		newCursor.MoveRight()
		cursor.MoveLeft()
		return []Cursor{cursor, newCursor}
	} else if cursor.LastKnownDirection == Right {
		cursor.MoveRight()
		return []Cursor{cursor}
	} else if cursor.LastKnownDirection == Left {
		cursor.MoveLeft()
		return []Cursor{cursor}
	} else {
		panic(fmt.Sprintf("Unknown direction: %d", cursor.LastKnownDirection))
	}
}

func HandlePipe(cursor Cursor) []Cursor {
	if cursor.LastKnownDirection == Up {
		cursor.MoveUp()
		return []Cursor{cursor}
	} else if cursor.LastKnownDirection == Right || cursor.LastKnownDirection == Left {
		newCursor := cursor.Copy()
		newCursor.MoveUp()
		cursor.MoveDown()
		return []Cursor{cursor, newCursor}
	} else if cursor.LastKnownDirection == Down {
		cursor.MoveDown()
		return []Cursor{cursor}
	} else {
		panic(fmt.Sprintf("Unknown direction: %d", cursor.LastKnownDirection))
	}
}

func HandleSlash(cursor Cursor) []Cursor {
	if cursor.LastKnownDirection == Up {
		cursor.MoveRight()
	} else if cursor.LastKnownDirection == Right {
		cursor.MoveUp()
	} else if cursor.LastKnownDirection == Down {
		cursor.MoveLeft()
	} else if cursor.LastKnownDirection == Left {
		cursor.MoveDown()
	} else {
		panic(fmt.Sprintf("Unknown direction: %d", cursor.LastKnownDirection))
	}
	return []Cursor{cursor}
}
func HandleBackslash(cursor Cursor) []Cursor {
	if cursor.LastKnownDirection == Up {
		cursor.MoveLeft()
	} else if cursor.LastKnownDirection == Right {
		cursor.MoveDown()
	} else if cursor.LastKnownDirection == Down {
		cursor.MoveRight()
	} else if cursor.LastKnownDirection == Left {
		cursor.MoveUp()
	} else {
		panic(fmt.Sprintf("Unknown direction: %d", cursor.LastKnownDirection))
	}
	return []Cursor{cursor}
}

func MakeGrid(input string) [][]*Cell {
	cells := [][]*Cell{}
	lines := strings.Split(input, "\n")
	for i, line := range lines {
		row := []*Cell{}
		for j, c := range line {
			row = append(row, &Cell{
				Visited: false,
				Char:    c,
				Row:     i,
				Col:     j,
			})
		}
		cells = append(cells, row)
	}
	return cells
}
