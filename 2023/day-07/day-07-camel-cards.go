package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

type Hand struct {
	Cards     string
	Bet       int
	CardFreq  map[rune]int
	HandOrder int
}

func NewHand(s string) Hand {
	hand, bet, _ := strings.Cut(s, " ")
	h := Hand{}
	h.Cards = hand
	h.Bet, _ = strconv.Atoi(bet)
	h.CardFreq = map[rune]int{}
	for _, c := range hand {
		h.CardFreq[c] += 1
	}
	return h
}

func main() {
	input := getInput("./day-07-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input))
}

func getInput(path string) string {
	ba, err := os.ReadFile(path)
	if err != nil {
		panic(fmt.Sprintf("Cannot read input file: %v", err.Error()))
	}
	return string(ba)
}

func PartOne(input string) int {
	hands := HandsFromInput(input)
	slices.SortFunc(hands, func(a Hand, b Hand) int {
		return Compare(a, b)
	})
	total := 0
	for i, h := range hands {
		total += (i + 1) * h.Bet
	}
	return total
}

func Compare(h Hand, other Hand) int {
	countFreq := map[int]int{}
	for _, v := range h.CardFreq {
		countFreq[v] += 1
	}
	hOrder := HandOrderFromCountFreq(countFreq)

	for k := range countFreq {
		delete(countFreq, k)
	}
	for _, v := range other.CardFreq {
		countFreq[v] += 1
	}
	otherOrder := HandOrderFromCountFreq(countFreq)

	if hOrder == otherOrder {
		cards := []byte{'2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'}
		return CompareCards(h, other, cards)
	}

	return hOrder - otherOrder
}

func HandOrderFromCountFreq(cf map[int]int) int {
  if val, ok := FiveOfAKind(cf); ok {
    return val
  }
  if val, ok := FourOfAKind(cf, 0); ok {
    return val
  }
  if val, ok := ThreeOfAKind(cf, 0); ok {
    return val
  }
  if val, ok := TwoOfAKind(cf, 0); ok {
    return val
  }
  val, _ := HighCard(cf, 0)
  return val
}

func CompareCards(h Hand, other Hand, cards []byte) int {
	for i := 0; i < len(h.Cards); i++ {
		hCard := h.Cards[i]
		otherCard := other.Cards[i]
		if hCard != otherCard {
			hCardScore := slices.Index(cards, hCard)
			otherCardScore := slices.Index(cards, otherCard)
			return hCardScore - otherCardScore
		}
	}
	return 0
}

func PartTwo(input string) int {
	hands := HandsFromInput(input)
	slices.SortFunc(hands, func(a Hand, b Hand) int {
		return NewCompare(a, b)
	})
	total := 0
	for i, h := range hands {
		total += (i + 1) * h.Bet
	}
	return total
}

func NewCompare(h Hand, other Hand) int {
	hOrder := HandOrderFromCountFreqJokers(h.CardFreq)
	otherOrder := HandOrderFromCountFreqJokers(other.CardFreq)

	if hOrder == otherOrder {
		cards := []byte{'J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'}
		return CompareCards(h, other, cards)
	}

	return hOrder - otherOrder
}

func HandOrderFromCountFreqJokers(cf map[rune]int) int {
	jValue := cf['J']
	counts := map[int]int{}
	for k, v := range cf {
		if k != 'J' {
			counts[v] += 1
		}
	}
	if val, ok := FiveOfAKind(counts); ok {
		return val
	}
	if val, ok := FourOfAKind(counts, jValue); ok {
		return val
	}
	if val, ok := ThreeOfAKind(counts, jValue); ok {
		return val
	}
  if val, ok := TwoOfAKind(counts, jValue); ok {
    return val
  }
  val, _ := HighCard(counts, jValue)
  return val
}

func HandsFromInput(input string) []Hand {
	h := []Hand{}
	lines := strings.Split(input, "\n")
	for _, l := range lines {
		h = append(h, NewHand(l))
	}
	return h
}

func FiveOfAKind(counts map[int]int) (int, bool) {
	_, ok := counts[5]
	return 6, ok
}

func FourOfAKind(counts map[int]int, nWildCards int) (int, bool) {
	if _, ok := counts[4]; ok {
		if nWildCards >= 1 {
			return 6, true
		}
		return 5, true
	} else {
		return 0, false
	}
}

func ThreeOfAKind(counts map[int]int, nWildCards int) (int, bool) {
	if _, ok := counts[3]; ok {
		if nWildCards >= 2 {
			return 6, true
		}
		if nWildCards >= 1 {
			return 5, true
		}
		if _, ok := counts[2]; ok {
			return 4, true
		}
		return 3, true
	}
  return 0, false
}

func TwoOfAKind(counts map[int]int, nWildCards int) (int, bool) {
	if nPairs, ok := counts[2]; ok {
		if nWildCards >= 3 {
			return 6, true
		}
		if nWildCards >= 2 {
			return 5, true
		}
		if nPairs > 1 {
			if nWildCards >= 1 {
				return 4, true
			}
			return 2, true
		}
		if nWildCards >= 1 {
			return 3, true
		}
		return 1, true
	}
  return 0, false
}

func HighCard(counts map[int]int, nWildCards int) (int, bool) {
	if nWildCards >= 4 {
		return 6, true
	}
	if nWildCards >= 3 {
		return 5, true
	}
	if nWildCards >= 2 {
		return 3, true
	}
	if nWildCards >= 1 {
		return 1, true
	}
	return 0, true
}
