package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
  f, _ := os.Open("./day-07-data.txt")
  defer f.Close()
  s := bufio.NewScanner(f)
  gates := make(map[string]string)
  computedGates := make(map[string]uint16)
  for s.Scan() {
    before, id, _ := strings.Cut(s.Text(), " -> ")
    gates[id] = before
  }
  getVal("a", gates, computedGates)
  fmt.Println("Part one:", computedGates["a"])

  gates["b"] = strconv.Itoa(int(computedGates["a"]))
  computedGates = make(map[string]uint16)
  getVal("a", gates, computedGates)

  fmt.Println("Part two:", computedGates["a"])
}

func getVal(target string, gates map[string]string, computedGates map[string]uint16) uint16 {
  targetInt, targetE := strconv.Atoi(target)
  if targetE == nil {
    res := uint16(targetInt)
    return res
  }

  wireVal := gates[target]
  wireValInt, wireValE := strconv.Atoi(wireVal)

  var res uint16

  if wireValE == nil {
    res = uint16(wireValInt)
  }

  tok := strings.Fields(wireVal)
  if len(tok) == 1 {
    // wire set to another wire, no gate
    res = getVal(tok[0], gates, computedGates)
  }

  if len(tok) == 2 {
    // NOT gate
    _, valRaw := tok[0], tok[1]
    val, valOk := computedGates[valRaw]
    if valOk {
      res = ^val
    } else {
      res = ^(getVal(valRaw, gates, computedGates))
    }
  }

  if len(tok) == 3 {
    // AND, LSHIFT, OR, RSHIFT
    lhRaw, gateName, rhRaw := tok[0], tok[1], tok[2]

    lhVal, lhOk := computedGates[lhRaw]
    rhVal, rhOk := computedGates[rhRaw]
    if !lhOk {
      lhVal = getVal(lhRaw, gates, computedGates)
    }

    if !rhOk {
      rhVal = getVal(rhRaw, gates, computedGates)
    }

    if gateName == "AND" {
      res = lhVal & rhVal
    }
    if gateName == "LSHIFT" {
      res = lhVal << rhVal
    }
    if gateName == "OR" {
      res = lhVal | rhVal
    }
    if gateName == "RSHIFT" {
      res = lhVal >> rhVal
    }
  }
  computedGates[target] = res
  return res
}
