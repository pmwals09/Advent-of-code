package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	f, _ := os.Open("./day-16-data.txt")
	s := bufio.NewScanner(f)

	theSue := map[string]int{
		"children":    3,
		"cats":        7,
		"samoyeds":    2,
		"pomeranians": 3,
		"akitas":      0,
		"vizslas":     0,
		"goldfish":    5,
		"trees":       3,
		"cars":        2,
		"perfumes":    1,
	}
	for s.Scan() {
		sue, sueNumStr := makeSueMap(s.Text())

		isTheSue := true
		for k, v := range sue {
			if theSue[k] != v {
				isTheSue = false
			}
		}
		if isTheSue {
			fmt.Println("Part one:", sueNumStr)
		}
		isTheSue = true
		for k, v := range sue {
			switch k {
			case "cats", "trees":
				if theSue[k] >= v {
					isTheSue = false
				}
			case "pomeranians", "goldfish":
				if theSue[k] <= v {
					isTheSue = false
				}
			default:
				if theSue[k] != v {
					isTheSue = false
				}
			}
		}
		if isTheSue {
			fmt.Println("Part two:", sueNumStr)
		}
	}
}

func makeSueMap(s string) (map[string]int, string) {
	sue := make(map[string]int)
	before, after, _ := strings.Cut(s, ": ")
	items := strings.Split(after, ", ")
	for _, i := range items {
		kv := strings.Split(i, ": ")
		key, valueString := kv[0], kv[1]
		value, _ := strconv.Atoi(valueString)
		sue[key] = value
	}
	return sue, before
}
