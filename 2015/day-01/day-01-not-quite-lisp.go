package main

import (
	"fmt"
	"os"
)
func main() {
  // read the file
  var floor int
  var neg_floor int
  f, _ := os.ReadFile("./day-01-data.json")
  for i := 0; i < len(f); i++ {
    if f[i] == ')' {
      floor--
    } else if f[i] == '(' {
      floor++
    }
    if floor < 0 && neg_floor == 0 {
      neg_floor = i
    }
  }
  fmt.Println("Part one:", floor)
  fmt.Println("Part two:", neg_floor)
}
