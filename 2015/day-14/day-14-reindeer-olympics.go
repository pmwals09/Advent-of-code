package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Reindeer struct {
	name         string
	speed        int
	flyDuration  int
	restDuration int
	points       int
}

func (r Reindeer) GetPosition(time int) int {
	cycleDuration := r.flyDuration + r.restDuration
	numCycles := time / cycleDuration
	cycleMod := time % cycleDuration
	if cycleMod > r.flyDuration {
		return r.speed * r.flyDuration * (numCycles + 1)
	} else {
		return r.speed*r.flyDuration*numCycles + cycleMod*r.speed
	}
}

const DURATION = 2503

func ReindeerFromString(s string) *Reindeer {
	tok := strings.Fields(s)
	name, speedStr, flyDurationStr, restDurationStr := tok[0], tok[3], tok[6], tok[13]
	speed, _ := strconv.Atoi(speedStr)
	flyDuration, _ := strconv.Atoi(flyDurationStr)
	restDuration, _ := strconv.Atoi(restDurationStr)

	return &Reindeer{name: name, speed: speed, flyDuration: flyDuration, restDuration: restDuration}
}

func main() {
	f, _ := os.Open("./day-14-data.txt")
	s := bufio.NewScanner(f)
	var reindeers []*Reindeer
	for s.Scan() {
		reindeers = append(reindeers, ReindeerFromString(s.Text()))
	}

	var maxDistance int
	for _, r := range reindeers {
		d := r.GetPosition(DURATION)
		if d > maxDistance {
			maxDistance = d
		}
	}
	fmt.Println("Part one:", maxDistance)

	for i := 1; i <= DURATION; i++ {
		var maxDistanceNow int
		for _, r := range reindeers {
			d := r.GetPosition(i)
			if d > maxDistanceNow {
				maxDistanceNow = d
			}
		}
		for _, r := range reindeers {
			d := r.GetPosition(i)
			if d == maxDistanceNow {
				r.points++
			}
		}
	}

	var maxPoints int
	var winner *Reindeer
	for _, r := range reindeers {
		if r.points > maxPoints {
			maxPoints = r.points
			winner = r
		}
	}

	fmt.Println("Part two:", winner.points)
}
