package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
  f, _ := os.ReadFile("./day-10-data.txt")
  str := string(f)
  for i := 0; i < 40; i++ {
    str = parseNum(str)
  }
  fmt.Println("Part one:", len(str))
  for i := 0; i < 10; i++ {
    str = parseNum(str)
  }
  fmt.Println("Part two:", len(str))
}

func parseNum(n string) string {
  var out strings.Builder
  latestLetter := ""
  latestLetterCount := 0
  for _, c := range n {
    if string(c) == latestLetter {
      latestLetterCount += 1
    } else {
      if latestLetterCount > 0 {
        // drain the latestLetter
        out.WriteString(strconv.Itoa(latestLetterCount))
        out.WriteString(latestLetter)
      }
      // init new latestLetter
      latestLetter = string(c)
      latestLetterCount = 1
    }
  }
  out.WriteString(strconv.Itoa(latestLetterCount))
  out.WriteString(latestLetter)

  return out.String()
}
