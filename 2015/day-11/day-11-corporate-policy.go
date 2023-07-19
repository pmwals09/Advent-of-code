package main

import (
	"fmt"
	"os"
)

const Z_VAL = 122
const A_VAL = 97
const I_VAL = 105
const L_VAL = 108
const O_VAL = 111

func main() {
  currentPassword, _ := os.ReadFile("./day-11-data.txt")
  for !isOkPassword(currentPassword) {
    currentPassword = increment(currentPassword)
  }
  fmt.Println("Part one:", string(currentPassword))
  currentPassword = increment(currentPassword)
  for !isOkPassword(currentPassword) {
    currentPassword = increment(currentPassword)
  }
  fmt.Println("Part two:", string(currentPassword))
}

func increment(ba []byte) []byte {
  // increment the last byte
  idx := len(ba) - 1
  ba[idx] += 1
  // handle the carrying
  if ba[idx] > Z_VAL {
    carry := ba[idx] - Z_VAL
    ba[idx] = A_VAL
    for carry > 0 && idx > 0 {
      idx--
      ba[idx] += carry
      if ba[idx] > Z_VAL {
        carry = ba[idx] - Z_VAL
        ba[idx] = A_VAL
      } else {
        carry = 0
      }
    }

    if carry > 0 {
      ba = append([]byte{A_VAL + carry - 1}, ba...)
    }
  }

  return ba
}

func isOkPassword(ba []byte) bool {
  return !hasForbidden(ba) && hasRun(ba) && hasDoubles(ba)
}

func hasForbidden(ba []byte) bool {
  for _, b := range ba {
    if b == I_VAL || b == O_VAL || b == L_VAL {
      return true
    }
  }
  return false
}

func hasRun(ba []byte) bool {
  for i := 0; i < len(ba) - 2; i++ {
    if ba[i + 2] == ba[i + 1] + 1 && ba[i + 1] == ba[i] + 1 {
      return true
    }
  }
  return false
}

func hasDoubles(ba []byte) bool {
  doubles := make([]([]byte), 0)
  for i := 0; i < len(ba) - 1; {
    if ba[i] == ba[i + 1] {
      doubles = append(doubles, ba[i:i+2])
      i += 2
    } else {
      i++
    }
  }

  return len(doubles) > 1
}
