package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Item struct {
	name   string
	cost   int
	armor  int
	damage int
}

type Player struct {
	hitPoints int
	damage    int
	armor     int
}

func (p Player) attack(opponent *Player) {
	opponent.takeDamage(p.damage)
}

func (p *Player) takeDamage(damage int) {
	damageAmt := damage - p.armor
	if damageAmt < 1 {
		damageAmt = 1
	}
	p.hitPoints -= damageAmt
}

func main() {
	// shortest path where a node is an item bought, and the edge weight is the
	// gold spent
	loadouts := getAllLoadouts()
	minCost := math.MaxInt
	var maxCost int
	bossStats := getBossStats()
	for _, l := range loadouts {
		cost := tally(l)
		if playGame(l, bossStats) {
			if cost < minCost {
				minCost = cost
			}
		} else {
			if cost > maxCost {
				maxCost = cost
			}
		}
	}

	fmt.Println("Part one:", minCost)
	fmt.Println("Part two:", maxCost)
}

func getAllLoadouts() [][]Item {
	loadouts := [][]Item{}
	addWeapon(&loadouts)
	addArmor(&loadouts)
	addRings(&loadouts)
	return loadouts
}

func addWeapon(loadouts *[][]Item) {
	weapons := getWeapons()
	for _, w := range weapons {
		*loadouts = append(*loadouts, []Item{w})
	}
}

func getWeapons() []Item {
	return []Item{
		{name: "Dagger", cost: 8, damage: 4, armor: 0},
		{name: "Shortsword", cost: 10, damage: 5, armor: 0},
		{name: "Warhammer", cost: 25, damage: 6, armor: 0},
		{name: "Longsword", cost: 40, damage: 7, armor: 0},
		{name: "Greataxe", cost: 74, damage: 8, armor: 0},
	}
}

func addArmor(loadouts *[][]Item) {
	armor := getArmor()
	for _, l := range *loadouts {
		for _, a := range armor {
			newLoadout := []Item{}
			newLoadout = append(newLoadout, a)
			newLoadout = append(newLoadout, l...)
			*loadouts = append(*loadouts, newLoadout)
		}
	}
}

func getArmor() []Item {
	return []Item{
		{name: "Leather", cost: 13, damage: 0, armor: 1},
		{name: "Chainmail", cost: 31, damage: 0, armor: 2},
		{name: "Splintmail", cost: 53, damage: 0, armor: 3},
		{name: "Bandedmail", cost: 75, damage: 0, armor: 4},
		{name: "Platemail", cost: 102, damage: 0, armor: 5},
	}
}

func addRings(loadouts *[][]Item) {
	rings := getRings()
	for _, l := range *loadouts {
		for i := 0; i < len(rings); i++ {
			newLoadout := []Item{}
			r := rings[i]
			newLoadout = append(newLoadout, r)
			newLoadout = append(newLoadout, l...)
			*loadouts = append(*loadouts, newLoadout)
			for j := i + 1; j < len(rings); j++ {
				if j < len(rings) {
					anotherNewLoadout := make([]Item, len(newLoadout))
					copy(anotherNewLoadout, newLoadout)
					r = rings[j]
					anotherNewLoadout = append(anotherNewLoadout, r)
					*loadouts = append(*loadouts, anotherNewLoadout)
				}
			}
		}
	}
}

func getRings() []Item {
	return []Item{
		{name: "Damage +1", cost: 25, damage: 1, armor: 0},
		{name: "Damage +2", cost: 50, damage: 2, armor: 0},
		{name: "Damage +3", cost: 100, damage: 3, armor: 0},
		{name: "Defense +1", cost: 20, damage: 0, armor: 1},
		{name: "Defense +2", cost: 40, damage: 0, armor: 2},
		{name: "Defense +3", cost: 80, damage: 0, armor: 3},
	}
}

func getBossStats() Player {
	f, _ := os.ReadFile("./day-21-data.txt")
	lines := strings.Split(string(f), "\n")
	hitPointsLine, damageLine, armorLine := lines[0], lines[1], lines[2]

	_, hitPointString, _ := strings.Cut(hitPointsLine, ": ")
	_, damageString, _ := strings.Cut(damageLine, ": ")
	_, armorString, _ := strings.Cut(armorLine, ": ")

	hitPoints, _ := strconv.Atoi(hitPointString)
	damage, _ := strconv.Atoi(damageString)
	armor, _ := strconv.Atoi(armorString)

	return Player{
		hitPoints: hitPoints,
		damage:    damage,
		armor:     armor,
	}
}

func playGame(loadout []Item, boss Player) bool {
	var playerArmor int
	var playerDamage int
	for _, l := range loadout {
		playerArmor += l.armor
		playerDamage += l.damage
	}
	p := Player{hitPoints: 100, armor: playerArmor, damage: playerDamage}

	for i := 0; true; i++ {
		if i%2 == 0 {
			p.attack(&boss)
			if boss.hitPoints <= 0 {
				return true
			}
		} else {
			boss.attack(&p)
			if p.hitPoints <= 0 {
				return false
			}
		}
	}
	return true
}

func tally(loadout []Item) int {
	var sum int
	for _, i := range loadout {
		sum += i.cost
	}
	return sum
}
