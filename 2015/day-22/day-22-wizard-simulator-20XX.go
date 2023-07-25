package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Player struct {
	hitPoints int
	armor     int
	damage    int
	mana      int
}

func (p Player) attack(opponent *Player, amount int) {
	opponent.takeDamage(amount)
}

func (p *Player) takeDamage(amount int) {
	damageAmount := amount - p.armor
	if damageAmount < 1 {
		damageAmount = 1
	}
	p.hitPoints -= damageAmount
}

type Spell struct {
	name   string
	cost   int
	effect Effect
}

type Effect struct {
	counter int
	effect  func(EffectArgs)
}

type EffectArgs struct {
	player       *Player
	boss         *Player
	currentCount int
}

type Path struct {
	spells        []*Spell
	player        Player
	boss          Player
}

func (p *Path) Copy() Path {
	spells := []*Spell{}
	for _, s := range p.spells {
		spell := Spell{
			name: s.name,
			cost: s.cost,
			effect: s.effect,
		}
		spells = append(spells, &spell)
	}
	return Path{
		spells: spells,
		player: Player{ hitPoints: 50, mana: 500 },
		boss: getBoss(),
	}
}

func main() {
	p := Player{mana: 500, hitPoints: 50}
	boss := getBoss()
	spells := getSpells()

	pathQueue := []Path{
		{
			spells: []*Spell{},
			player: p,
			boss:   boss,
		},
	}
	minCost := math.MaxInt
	for len(pathQueue) > 0 {
		var latest Path
		latest, pathQueue = pathQueue[0], pathQueue[1:]
		if win, complete := playGame(&latest); complete && win {
			var sum int
			for _, s := range latest.spells {
				sum += s.cost
			}
			if sum < minCost {
				minCost = sum
				fmt.Println("sum:", sum)
			}

		} else {
			for _, s := range latest.spells {
				fmt.Printf("%v\n", s)
			}
			for _, s := range spells {
				p := latest.Copy()
				p.spells = append(p.spells, &s)
				pathQueue = append(pathQueue, p)
			}
		}
	}
}

func playGame(path *Path) (win bool, complete bool) {
	spells := make([]*Spell, len(path.spells))
	copy(spells, path.spells)
	for i, s := range spells {
		// this is essentially a turn - we'll need to go back and hit the different
		// active effects first and again before the boss goes
		for _, prevSpell := range spells[:i]  {
			if prevSpell.effect.counter > 0 {
				prevSpell.effect.effect(EffectArgs{player: &path.player, boss: &path.boss, currentCount: prevSpell.effect.counter})
				prevSpell.effect.counter--
			}
		}
		s.effect.effect(EffectArgs{player: &path.player, boss: &path.boss, currentCount: s.effect.counter})
		if path.player.hitPoints < 0 {
			return false, true
		}
		if path.boss.hitPoints < 0 {
			return true, true
		}

		for _, prevSpell := range spells[:i + 1]  {
			if prevSpell.effect.counter > 0 {
				prevSpell.effect.effect(EffectArgs{player: &path.player, boss: &path.boss, currentCount: prevSpell.effect.counter})
				prevSpell.effect.counter--
			}
		}

		path.boss.attack(&path.player, path.boss.damage)
		if path.player.hitPoints < 0 {
			return false, true
		}
		if path.boss.hitPoints < 0 {
			return true, true
		}
	}

	return false, false
}

func getBoss() Player {
	f, _ := os.ReadFile("./day-22-data.txt")
	lines := strings.Split(string(f), "\n")
	hitPointsLine, damageLine := lines[0], lines[1]

	_, hitPointsStr, _ := strings.Cut(hitPointsLine, ": ")
	_, damageStr, _ := strings.Cut(damageLine, ": ")

	hitPoints, _ := strconv.Atoi(hitPointsStr)
	damage, _ := strconv.Atoi(damageStr)

	return Player{
		hitPoints: hitPoints,
		damage:    damage,
	}
}

func getSpells() []Spell {
	return []Spell{
		{
			name: "Magic Missile",
			cost: 53,
			effect: Effect{
				counter: 0,
				effect: func(e EffectArgs) {
					e.player.mana -= 53
					e.player.attack(e.boss, 4)
				},
			},
		},
		{
			name: "Drain",
			cost: 73,
			effect: Effect{
				counter: 0,
				effect: func(e EffectArgs) {
					e.player.mana -= 73
					e.player.attack(e.boss, 2)
					e.player.hitPoints += 2
				},
			},
		},
		{
			name: "Shield",
			cost: 113,
			effect: Effect{
				counter: 6,
				effect: func(e EffectArgs) {
					if e.currentCount == 6 {
						e.player.mana -= 113
						e.player.armor += 7
					} else if e.currentCount == 0 {
						e.player.armor -= 7
						if e.player.armor < 0 {
							e.player.armor = 0
						}
					}
				},
			},
		},
		{
			name: "Poison",
			cost: 173,
			effect: Effect{
				counter: 6,
				effect: func(e EffectArgs) {
					e.player.attack(e.boss, 3)
				},
			},
		},
		{
			name: "Recharge",
			cost: 229,
			effect: Effect{
				counter: 5,
				effect: func(e EffectArgs) {
					e.player.mana += 101
				},
			},
		},
	}
}
