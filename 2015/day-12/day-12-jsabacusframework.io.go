package main

import (
	"bytes"
	"fmt"
	"os"
	"strconv"
)

const ZERO = 48
const NINE = 57
const MINUS = 45
const LSQUIRLY = 123
const RSQUIRLY = 125

func main() {
	f, _ := os.ReadFile("./day-12-data.txt")

	fmt.Println("Part one:", sumNumbers(f))

	objectOpens := make([]int, 0)
	var openIdx int
	for i := 0; i < len(f); i++ {
		b := f[i]
		if b == LSQUIRLY {
			objectOpens = append(objectOpens, i)
		} else if b == RSQUIRLY {
			openIdx, objectOpens = pop(objectOpens)
			objStr := f[openIdx : i+1]

			var objSum int
			if bytes.Contains(objStr, []byte(`:"red"`)) {
				objSum = 0
			} else {
				objSum = sumNumbers(objStr)
			}
			objSumBa := []byte(strconv.Itoa(objSum))

			f = spliceInSum(f, objSumBa, openIdx, i)

			i = openIdx - 1
		}
	}
	fmt.Println("Part two:", string(f))
}

func sumNumbers(ba []byte) int {
	var start int
	var end int
	var sum int
	for i, b := range ba {
		if b <= NINE && b >= ZERO {
			end = i
		} else {
			if end > start {
				if ba[start] != MINUS {
					start += 1
				}
				n, _ := strconv.Atoi(string(ba[start : end+1]))
				sum += n
			}
			start = i
		}
	}

	return sum
}

func pop(ia []int) (int, []int) {
	return ia[len(ia)-1], ia[:len(ia)-1]
}

func spliceInSum(ba []byte, val []byte, spliceStartIdx int, spliceEndIdx int) []byte {
	newBa := make([]byte, len(ba[:spliceStartIdx]))
	copy(newBa, ba[:spliceStartIdx])
	newBa = append(newBa, val...)
	newBa = append(newBa, ba[spliceEndIdx+1:]...)
	return newBa
}
