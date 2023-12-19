// Credit to https://github.com/hyper-neutrino/advent-of-code
package main

import (
	"container/heap"
	"fmt"
	"os"
	"strings"
)

func main() {
	input := getInput("./day-17-input.txt")
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

type Grid [][]rune

func (g Grid) InBounds(s PositionState) bool {
	return s.Row >= 0 && s.Row < len(g) && s.Col >= 0 && s.Col < len(g[0])
}

func (g Grid) IsAtEnd(s PositionState) bool {
	return s.Row == len(g)-1 && s.Col == len(g[0])-1
}

type DirectionPair struct {
	DirRow int
	DirCol int
}
type PositionState struct {
	Row int
	Col int
	DirectionPair
	DirDistance int
}

func (p PositionState) Continue() PositionState {
	return PositionState{
		Row:           p.Row + p.DirRow,
		Col:           p.Col + p.DirCol,
		DirDistance:   p.DirDistance + 1,
		DirectionPair: p.DirectionPair,
	}
}

func (p PositionState) Turn(d DirectionPair) PositionState {
	return PositionState{
		Row:           p.Row + d.DirRow,
		Col:           p.Col + d.DirCol,
		DirDistance:   1,
		DirectionPair: d,
	}
}

type State struct {
	PositionState
	HeatLoss int
}

type StatePQ []State

func (s *StatePQ) Push(x any) {
	*s = append(*s, x.(State))
}
func (s *StatePQ) Pop() any {
	item := (*s)[len(*s)-1]
	*s = (*s)[:len(*s)-1]
	return item
}
func (s StatePQ) Len() int {
	return len(s)
}
func (s StatePQ) Less(i, j int) bool {
	return s[i].HeatLoss < s[j].HeatLoss
}
func (s StatePQ) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func PartOne(input string) int {
	grid := MakeGrid(input)
	visited := map[PositionState]bool{}

	pq := StatePQ{}
	heap.Init(&pq)
	heap.Push(&pq, State{
		HeatLoss: 0,
		PositionState: PositionState{
			Row: 0,
			Col: 0,
			DirectionPair: DirectionPair{
				DirRow: 0,
				DirCol: 0,
			},
			DirDistance: 0,
		},
	})

	for len(pq) > 0 {
		curr := heap.Pop(&pq).(State)

		if grid.IsAtEnd(curr.PositionState) {
			return curr.HeatLoss
		}

		if _, ok := visited[curr.PositionState]; ok {
			continue
		}
		visited[curr.PositionState] = true

		if curr.DirDistance < 3 && curr.DirectionPair != (DirectionPair{0, 0}) {
			next := curr.PositionState.Continue()
			if grid.InBounds(next) {
				heap.Push(&pq, State{
					PositionState: next,
					HeatLoss:      curr.HeatLoss + int(grid[next.Row][next.Col]-'0'),
				})
			}
		}

		for _, ea := range []DirectionPair{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
			if ea != curr.DirectionPair && !(ea.DirRow == -curr.DirRow && ea.DirCol == -curr.DirCol) {
				next := curr.PositionState.Turn(ea)
				if grid.InBounds(next) {
					heap.Push(&pq, State{
						PositionState: next,
						HeatLoss:      curr.HeatLoss + int(grid[next.Row][next.Col]-'0'),
					})
				}
			}
		}
	}
	return 0
}

func PartTwo(input string) int {
	grid := MakeGrid(input)
	visited := map[PositionState]bool{}

	pq := StatePQ{}
	heap.Init(&pq)
	heap.Push(&pq, State{
		HeatLoss: 0,
		PositionState: PositionState{
			Row: 0,
			Col: 0,
			DirectionPair: DirectionPair{
				DirRow: 0,
				DirCol: 0,
			},
			DirDistance: 0,
		},
	})

	for len(pq) > 0 {
		curr := heap.Pop(&pq).(State)

		if grid.IsAtEnd(curr.PositionState) && curr.DirDistance >= 4 {
			return curr.HeatLoss
		}

		if _, ok := visited[curr.PositionState]; ok {
			continue
		}
		visited[curr.PositionState] = true

		if curr.DirDistance < 10 && curr.DirectionPair != (DirectionPair{0, 0}) {
			next := curr.PositionState.Continue()
			if grid.InBounds(next) {
				heap.Push(&pq, State{
					PositionState: next,
					HeatLoss:      curr.HeatLoss + int(grid[next.Row][next.Col]-'0'),
				})
			}
		}

    if curr.DirDistance >= 4 || curr.DirectionPair == (DirectionPair{0, 0}) {
      for _, ea := range []DirectionPair{{0, 1}, {1, 0}, {0, -1}, {-1, 0}} {
        if ea != curr.DirectionPair && !(ea.DirRow == -curr.DirRow && ea.DirCol == -curr.DirCol) {
          next := curr.PositionState.Turn(ea)
          if grid.InBounds(next) {
            heap.Push(&pq, State{
              PositionState: next,
              HeatLoss:      curr.HeatLoss + int(grid[next.Row][next.Col]-'0'),
            })
          }
        }
      }
    }
	}
	return 0
}

func MakeGrid(input string) Grid {
	grid := Grid{}
	lines := strings.Split(input, "\n")
	for _, l := range lines {
		row := []rune{}
		for _, c := range l {
			row = append(row, c)
		}
		grid = append(grid, row)
	}
	return grid
}
