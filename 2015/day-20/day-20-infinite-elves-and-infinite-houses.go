package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
)

func main() {
  f, _ := os.ReadFile("./day-20-data.txt")
  num, _ := strconv.Atoi(string(f))
  foundPartOne := false
  for i := 1; i < math.MaxInt; i++ {
    var partOneGifts int
    var partTwoGifts int
    for _, n := range getFactors(i) {
      if !foundPartOne {
        partOneGifts += n * 10
      }
      if i / n <= 50 {
        partTwoGifts += n * 11
      }
    }
    if partOneGifts >= num && !foundPartOne {
      fmt.Println("Part one:", i)
      foundPartOne = true
    }
    if partTwoGifts >= num {
      fmt.Println("Part two:", i)
      break
    }
  }
}

func getFactors(n int) []int {
  set := make(map[int]struct{})
  sqrt := int(math.Sqrt(float64(n)))
  for i := 1; i <= sqrt; i++ {
    if n % i == 0 {
      set[i] = struct{}{}
      set[n/i] = struct{}{}
    }
  }
  out := []int{}
  for k := range set {
    out = append(out, k)
  }
  return out
}
