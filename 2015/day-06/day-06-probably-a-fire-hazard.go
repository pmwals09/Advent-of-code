package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Point struct {
  row int;
  col int;
}

func main() {
  partOneLights := [1000][1000]int{}
  partTwoLights := [1000][1000]int{}
  f, _ := os.Open("./day-06-data.txt")
  defer f.Close()
  s := bufio.NewScanner(f)

  for s.Scan() {
    instruction := s.Text()
    startPoint, endPoint, partOneCallback, partTwoCallback := parseInstruction(instruction)
    for row := startPoint.row; row < endPoint.row + 1; row++ {
      for col := startPoint.col; col < endPoint.col + 1; col++ {
        partOneLights[row][col] = partOneCallback(partOneLights[row][col])
        partTwoLights[row][col] = partTwoCallback(partTwoLights[row][col])
      }
    }
  }

  fmt.Println("Part one:", tally(partOneLights))
  fmt.Println("Part two:", tally(partTwoLights))
}

func tally(lights [1000][1000]int) int {
  var count int
  for row := 0; row < 1000; row++ {
    for col := 0; col < 1000; col++ {
      count += lights[row][col]
    }
  }
  return count
}

func parseInstruction(instruction string) (Point, Point, func(int) int, func(int) int) {
  before, after, _ := strings.Cut(instruction, " through ")
  words := strings.Fields(before)
  var partOneCb func(int) int
  var partTwoCb func(int) int
  if words[0] == "toggle" {
    partOneCb = func(x int) int {
      if x == 1 {
        return 0
      }
      return 1
    }
    partTwoCb = func(x int) int {
      return x + 2
    }
  } else if words[1] == "on" {
    partOneCb = func(x int) int {
      return 1
    }
    partTwoCb = func(x int) int {
      return x + 1
    }
  } else {
    partOneCb = func(x int) int {
      return 0
    }
    partTwoCb = func(x int) int {
      return int(math.Max(0, float64(x - 1)))
    }
  }
  beforeCol, beforeRow, _ := strings.Cut(words[len(words) - 1], ",")
  beforeRowP, _ := strconv.Atoi(beforeRow)
  beforeColP, _ := strconv.Atoi(beforeCol)

  beforeP := Point { row: beforeRowP, col: beforeColP }

  afterCol, afterRow, _ := strings.Cut(after, ",")
  afterRowP, _ := strconv.Atoi(afterRow)
  afterColP, _ := strconv.Atoi(afterCol)

  afterP := Point { row: afterRowP, col: afterColP }

  return beforeP, afterP, partOneCb, partTwoCb
}
