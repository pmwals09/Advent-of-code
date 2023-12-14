package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	input := getInput("./day-14-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input))
}

func getInput(path string) string {
	ba, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return string(ba)
}

func PartOne(input string) int {
	rockMap := GetRockMap(input)
	rockTracker := make([]int, len(rockMap[0]))
	TiltMapNorth(&rockMap, &rockTracker)
	return ScoreMap(rockMap)
}

// TODO:
// - Cache the results of a cycle
// - Ask the cache for this state and deduct that many i's from the loop
func PartTwo(input string) int {
	rockMap := GetRockMap(input)
	rockTracker := make([]int, len(rockMap[0]))
	cache := map[string]int{}
	loops := 1_000_000_000
	for i := 0; i < loops; i++ {
		TiltMapNorth(&rockMap, &rockTracker)
		TiltMapWest(&rockMap, &rockTracker)
		TiltMapSouth(&rockMap, &rockTracker)
		TiltMapEast(&rockMap, &rockTracker)
		k := RockMapToString(rockMap)
		if v, ok := cache[k]; ok {
			i = loops - (loops-i)%(i-v)
		} else {
			cache[k] = i
		}
	}
	return ScoreMap(rockMap)
}

func RockMapToString(rockMap [][]rune) string {
	var s strings.Builder
	for _, row := range rockMap {
		for _, c := range row {
			s.WriteRune(c)
		}
	}
	return s.String()
}

func GetRockMap(input string) [][]rune {
	lines := strings.Split(input, "\n")
	rockMap := [][]rune{}
	for _, l := range lines {
		newRow := []rune{}
		for _, c := range l {
			newRow = append(newRow, c)
		}
		rockMap = append(rockMap, newRow)
	}
	return rockMap
}

func TiltMapEast(rockMap *[][]rune, rockTracker *[]int) {
	RotateClockwise(rockMap)
	TiltMapNorth(rockMap, rockTracker)
	RotateClockwise(rockMap) // NOTE: Turn it back to north-facing so we can use the north tilt fn for part one
}

func TiltMapSouth(rockMap *[][]rune, rockTracker *[]int) {
	RotateClockwise(rockMap)
	TiltMapNorth(rockMap, rockTracker)
}

func TiltMapWest(rockMap *[][]rune, rockTracker *[]int) {
	RotateClockwise(rockMap)
	TiltMapNorth(rockMap, rockTracker)
}

func TiltMapNorth(rockMap *[][]rune, rockTracker *[]int) {
	for rowIdx := len(*rockMap) - 1; rowIdx >= 0; rowIdx-- {
		// first empty out the tracker if you get to a square rock
		for cellIdx, cell := range (*rockMap)[rowIdx] {
			if cell == '#' {
				// walk back and drop the stones you've collected
				if rowIdx < len(*rockMap)-1 {
					for i := rowIdx + 1; i < len(*rockMap); i++ {
						if (*rockTracker)[cellIdx] > 0 {
							(*rockMap)[i][cellIdx] = 'O'
							(*rockTracker)[cellIdx]--
						}
					}
				}
			}
		}
		// collect all the circle rocks in the tracker
		for cellIdx, cell := range (*rockMap)[rowIdx] {
			if cell == 'O' {
				(*rockTracker)[cellIdx]++
				(*rockMap)[rowIdx][cellIdx] = '.'
			}
		}
	}
	// drop any remaining rocks from the tracker
	for rowIdx := range *rockMap {
		for rockTrackerIdx, rockTrackerCell := range *rockTracker {
			if rockTrackerCell > 0 {
				(*rockMap)[rowIdx][rockTrackerIdx] = 'O'
				(*rockTracker)[rockTrackerIdx]--
			}
		}
	}
}

func ScoreMap(rockMap [][]rune) int {
	var score int
	for i := len(rockMap) - 1; i >= 0; i-- {
		for _, c := range rockMap[i] {
			if c == 'O' {
				score += (len(rockMap) - i)
			}
		}
	}
	return score
}

func RotateClockwise(arr *[][]rune) {
	rotatedArr := [][]rune{}
	for colIdx := 0; colIdx < len((*arr)[0]); colIdx++ {
		newRow := []rune{}
		for rowIdx := len(*arr) - 1; rowIdx >= 0; rowIdx-- {
			newRow = append(newRow, (*arr)[rowIdx][colIdx])
		}
		rotatedArr = append(rotatedArr, newRow)
	}
	for rowIdx, row := range rotatedArr {
		for colIdx, cell := range row {
			(*arr)[rowIdx][colIdx] = cell
		}
	}
}
