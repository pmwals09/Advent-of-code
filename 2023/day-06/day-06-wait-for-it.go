package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Race struct {
	Time     int
	Distance int
}

func (r Race) RunRace(holdTime int) int {
	racingTime := r.Time - holdTime
	return holdTime * racingTime
}

func main() {
	input := getInput("./day-06-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input))
}

func getInput(path string) string {
	ba, err := os.ReadFile(path)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}
	return string(ba)
}

func PartOne(input string) int {
	races := InputToRaces(input)
	return RunAllRaces(races)
}

func PartTwo(input string) int {
	races := InputToRace(input)
	return RunAllRaces(races)
}

func RunAllRaces(races []Race) int {
	total := 1
	for _, r := range races {
		var lowEnd int
		var highEnd int
		for i := 0; i <= r.Time; i += 1 {
			if r.RunRace(i) > r.Distance {
				lowEnd = i
				break
			}
		}
		for i := r.Time; i >= 0; i -= 1 {
			if r.RunRace(i) > r.Distance {
				highEnd = i
				break
			}
		}

		total *= (highEnd - lowEnd + 1)
	}

	return total
}

func InputToRaces(input string) []Race {
	races := []Race{}
	lines := strings.Split(input, "\n")
	timeLine, distanceLine := lines[0], lines[1]
	timeStrs := strings.Fields(timeLine)[1:]
	distanceStrs := strings.Fields(distanceLine)[1:]

	for _, t := range timeStrs {
		r := Race{}
		r.Time, _ = strconv.Atoi(t)
		races = append(races, r)
	}

	for i, d := range distanceStrs {
		races[i].Distance, _ = strconv.Atoi(d)
	}
	return races
}

func InputToRace(input string) []Race {
	lines := strings.Split(input, "\n")
	timeLine, distanceLine := lines[0], lines[1]
	timeStrs := strings.Fields(timeLine)[1:]
	distanceStrs := strings.Fields(distanceLine)[1:]

  r := Race{}
  r.Time, _ = strconv.Atoi(strings.Join(timeStrs, ""))
  r.Distance, _ = strconv.Atoi(strings.Join(distanceStrs, ""))

	return []Race{ r }
}
