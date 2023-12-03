package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Game struct {
	Id    int
	Hands []Hand
}

type Hand struct {
	Blue  int
	Red   int
	Green int
}

func UnmarshalGame(line string) Game {
	var g Game
	gameIdStr, handsStr, _ := strings.Cut(line, ": ")
	gameId, _ := strconv.Atoi(strings.Split(gameIdStr, " ")[1])
	g.Id = gameId

	gameHands := []Hand{}
	hands := strings.Split(handsStr, "; ")
	for _, hand := range hands {
		h := Hand{}
		cubeGroups := strings.Split(hand, ", ")
		for _, cubeGroup := range cubeGroups {
			nStr, color, _ := strings.Cut(cubeGroup, " ")
			switch color {
			case "blue":
				h.Blue, _ = strconv.Atoi(nStr)
			case "red":
				h.Red, _ = strconv.Atoi(nStr)
			case "green":
				h.Green, _ = strconv.Atoi(nStr)
			}
		}
		gameHands = append(gameHands, h)
	}
	g.Hands = gameHands

	return g
}

func (g Game) IsValid(maxCubes Hand) bool {
	for _, hand := range g.Hands {
		if hand.Red > maxCubes.Red || hand.Green > maxCubes.Green || hand.Blue > maxCubes.Blue {
			return false
		}
	}
	return true
}

func (g Game) GetPower() int {
	minCubes := Hand{}
	for _, hand := range g.Hands {
		if hand.Blue > minCubes.Blue {
			minCubes.Blue = hand.Blue
		}
		if hand.Red > minCubes.Red {
			minCubes.Red = hand.Red
		}
		if hand.Green > minCubes.Green {
			minCubes.Green = hand.Green
		}
	}
	return minCubes.Blue * minCubes.Red * minCubes.Green
}

func main() {
	games := inputToGames(getInput())
	partOne := PartOne(games)
	fmt.Println("Part One:", partOne)
	partTwo := PartTwo(games)
	fmt.Println("Part Two:", partTwo)
}

func getInput() string {
	ba, _ := os.ReadFile("./day-02-input.txt")
	return string(ba)
}

func inputToGames(input string) []Game {
	games := []Game{}
	lines := strings.Split(input, "\n")
	for _, line := range lines {
		games = append(games, UnmarshalGame(line))
	}
	return games
}

func PartOne(games []Game) int {
	maxCubes := Hand{
		Red:   12,
		Green: 13,
		Blue:  14,
	}
	var total int
	for _, game := range games {
		if game.IsValid(maxCubes) {
			total += game.Id
		}
	}

	return total
}

func PartTwo(games []Game) int {
	var total int
	for _, game := range games {
		total += game.GetPower()
	}
	return total
}
