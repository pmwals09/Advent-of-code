package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const TOTAL_TSP = 100

type Ingredient struct {
	name       string
	capacity   int
	durability int
	flavor     int
	texture    int
	calories   int
}

func NewIngredientFromStr(s string) *Ingredient {
	tok := strings.Fields(s)
	name, capacityStr, durabilityStr, flavorStr, textureStr, caloriesStr := tok[0], tok[2], tok[4], tok[6], tok[8], tok[10]
	capacity, _ := strconv.Atoi(strings.Trim(capacityStr, ","))
	durability, _ := strconv.Atoi(strings.Trim(durabilityStr, ","))
	flavor, _ := strconv.Atoi(strings.Trim(flavorStr, ","))
	texture, _ := strconv.Atoi(strings.Trim(textureStr, ","))
	calories, _ := strconv.Atoi(strings.Trim(caloriesStr, ","))
	return &Ingredient{name: name, capacity: capacity, durability: durability, flavor: flavor, texture: texture, calories: calories}
}

func (i Ingredient) Score(weight int) Score {
	return Score{
		capacity:   weight * i.capacity,
		durability: weight * i.durability,
		flavor:     weight * i.flavor,
		texture:    weight * i.texture,
		calories:   weight * i.calories,
	}
}

type Score struct {
	capacity   int
	durability int
	flavor     int
	texture    int
	calories   int
}

func main() {
	f, _ := os.Open("./day-15-data.txt")
	s := bufio.NewScanner(f)
	var ingredients []*Ingredient
	for s.Scan() {
		ingredients = append(ingredients, NewIngredientFromStr(s.Text()))
	}

	var maxScore int
	var calMaxScore int
	for i := 0; i < TOTAL_TSP; i++ {
		for j := 0; j < TOTAL_TSP; j++ {
			for k := 0; k < TOTAL_TSP; k++ {
				for l := 0; l < TOTAL_TSP; l++ {
					if i+j+k+l == TOTAL_TSP {
						recipeScore, calories := scoreRecipe([]int{i, j, k, l}, ingredients)
						if recipeScore > maxScore {
							maxScore = recipeScore
						}
						if calories == 500 && recipeScore > calMaxScore {
							calMaxScore = recipeScore
						}
					}
				}
			}
		}
	}
	fmt.Println("Part one:", maxScore)
	fmt.Println("Part two:", calMaxScore)
}

func scoreRecipe(weights []int, ingredients []*Ingredient) (int, int) {
	var scores []Score
	for idx, i := range ingredients {
		scores = append(scores, i.Score(weights[idx]))
	}
	totalScores := []int{0, 0, 0, 0, 0}

	for _, s := range scores {
		totalScores[0] += s.capacity
		totalScores[1] += s.durability
		totalScores[2] += s.flavor
		totalScores[3] += s.texture
		totalScores[4] += s.calories
	}

	for i := 0; i < len(totalScores)-1; i++ {
		if totalScores[i] < 0 {
			totalScores[i] = 0
		}
	}

	return totalScores[0] * totalScores[1] * totalScores[2] * totalScores[3], totalScores[4]
}
