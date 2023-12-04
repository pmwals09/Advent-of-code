package main

import (
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
	"unicode"
)

type Card struct {
	Id               int
	WinningNumbers   map[int]bool
	CandidateNumbers map[int]bool
	Matches          int
	Count            int
}

func getCurrentRune(text string, idx int) rune {
	return rune(text[idx])
}

func CardFromString(cardText string) Card {
	tokens := []string{}
	tok := strings.Builder{}
	for i := 0; i < len(cardText); i++ {
		tok.Reset()
		for i < len(cardText) && !unicode.IsSpace(getCurrentRune(cardText, i)) {
			tok.WriteRune(getCurrentRune(cardText, i))
			i++
		}
		if len(tok.String()) > 0 {
			tokens = append(tokens, tok.String())
		}
	}

	c := Card{}
	c.Count = 1
	c.WinningNumbers = map[int]bool{}
	c.CandidateNumbers = map[int]bool{}
	c.Id, _ = strconv.Atoi(tokens[1][:len(tokens[1])-1])

	numBreakIdx := slices.Index(tokens, "|")

	for _, v := range tokens[2:numBreakIdx] {
		num, _ := strconv.Atoi(v)
		c.WinningNumbers[num] = true
	}
	for _, v := range tokens[numBreakIdx+1:] {
		num, _ := strconv.Atoi(v)
		c.CandidateNumbers[num] = true
		if c.WinningNumbers[num] {
			c.Matches += 1
		}
	}
	return c
}

func (c Card) GetScore() int {
	if c.Matches == 0 {
		return 0
	}
	return int(math.Exp2(float64(c.Matches - 1)))
}

func main() {
	input := getInput()
	fmt.Println("Part One:", PartOne(string(input)))
	fmt.Println("Part Two:", PartTwo(string(input)))
}

func getInput() []byte {
	ba, err := os.ReadFile("./day-04-input.txt")
	if err != nil {
		panic(fmt.Sprintf("Error opening file: %v", err))
	}
	return ba
}

func PartOne(input string) int {
	var total int
	for _, rawCard := range strings.Split(input, "\n") {
		c := CardFromString(rawCard)
		total += c.GetScore()
	}
	return total
}

func PartTwo(input string) int {
	cards := []Card{}
	for _, rawCard := range strings.Split(input, "\n") {
		cards = append(cards, CardFromString(rawCard))
	}
	for i, _ := range cards {
		if cards[i].Matches > 0 {
			for j := 0; j < cards[i].Count; j++ {
        for k := i + 1; k <= i + cards[i].Matches; k++ {
          cards[k].Count += 1
				}
			}
		}
	}
	var total int
	for _, c := range cards {
		total += c.Count
	}
	return total
}
