package main

import (
	"fmt"
	"os"
  "slices"
	"strings"
)

func main() {
	input := getInput("./day-08-input.txt")
	fmt.Println("Part One: ", PartOne(input))
	fmt.Println("Part Two: ", PartTwo(input))
}

func getInput(fileName string) string {
	if ba, err := os.ReadFile(fileName); err != nil {
		panic("Cannot read file")
	} else {
		return string(ba)
	}
}

func PartOne(input string) int {
	directions, leftRightMap := parseInput(input)
  current := "AAA"
  steps := 0
  directionIdx := 0
  for current != "ZZZ" {
    n := leftRightMap[current]
    d := directions[directionIdx]
    if d == 'R' {
      current = n.Right
    } else {
      current = n.Left
    }
    steps += 1

    if directionIdx == len(directions) - 1 {
      directionIdx = 0
    } else {
      directionIdx += 1
    }
  }
  return steps
}

func PartTwo(input string) int {
  directions, lrMap := parseInput(input)
  // get all of the starting points
  paths := []string{}
  for k := range lrMap {
    if k[len(k)-1] == 'A' {
      paths = append(paths, k)
    }
  }

  pathLoopLength := []int{}
  for i, p := range paths {
    directionIdx := 0
    currentLoopLen := 0
    current := p
    pathLoopLength = append(pathLoopLength, 0)
    for {
      d := directions[directionIdx]
      if d == 'R' {
        current = lrMap[current].Right
      } else {
        current = lrMap[current].Left
      }

      if current[2] == 'Z' {
        pathLoopLength[i] = currentLoopLen + 1
        break
      }

      currentLoopLen += 1

      if directionIdx == len(directions) - 1 {
        directionIdx = 0
      } else {
        directionIdx += 1
      }
    }
  }


  maxPath := slices.Max(pathLoopLength)
  lcm := maxPath
  for slices.ContainsFunc(pathLoopLength, func(p int) bool {
    return lcm % p != 0
  }) {
    lcm += maxPath
  }

  return lcm

}

type Node struct {
  Left string
  Right string
}
func parseInput(input string) (string, map[string]Node) {
	directions, lrMapStr, _ := strings.Cut(input, "\n\n")
  lrMap := map[string]Node{}
  lines := strings.Split(lrMapStr, "\n")
  for _, line := range lines {
    l, r, _ := strings.Cut(line, " = ")
    left, right, _ := strings.Cut(r[1:len(r)-1], ", ")
    lrMap[l] = Node{Left: left, Right: right}
  }
  return directions, lrMap
}
