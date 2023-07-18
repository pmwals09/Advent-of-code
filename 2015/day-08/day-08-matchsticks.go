package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
)

func main() {
  f, _ := os.Open("./day-08-data.txt")
  defer f.Close()
  s := bufio.NewScanner(f)
  var partOneSum int
  var partTwoSum int
  for s.Scan() {
    str := s.Text()
    partOneSum += (len(str) - len(decodeStr(str)))
    partTwoSum += (len(encodeStr(str)) - len(str))
  }

  fmt.Println("Part one:", partOneSum)
  fmt.Println("Part two:", partTwoSum)
}

func decodeStr(s string) string {
  var out string;
  for i:= 0; i < len(s) - 1; {
    if i == 0 || i == len(s) - 1 {
      i++
      continue
    }
    c := string(s[i])
    next := string(s[i + 1])
    if c == "\\" {
      if next == "\"" || next == "\\" {
        i += 2
        out += "-"
        continue
      } else if next == "x" && i < len(s) - 3 { 
        lastTwoS := string(s[i + 2:i + 4])
        lastTwo, _ := regexp.MatchString("[0-9a-z]{2}", lastTwoS)
        if (lastTwo) {
          i += 4
          out += "1"
          continue
        }
      }
    }
    out += c
    i++
  }
  return out
}

func encodeStr(s string) string {
  out := `"`
  for i := 0; i < len(s); i++ {
    c := string(s[i])
    if c == `"` {
      out += `\"`
    } else if c == `\` {
      out += `\\`
    } else {
      out += c
    }
  }
  out += `"`
  return out
}

