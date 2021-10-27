const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-22-data.txt", "utf-8").split("\n");

class Player {
  constructor(hitPoints, armor) {
    this.hitPoints = hitPoints;
    this.armor = armor;
  }

  takeDamage(amount, ignoreArmor = false) {
    if(ignoreArmor){
      this.hitPoints -= Math.max(amount, 1)
    } else {
      this.hitPoints -= Math.max(amount - this.armor, 1);
    }
  }
}

class Wizard extends Player {
  constructor({mana, hitPoints, armor, spells}) {
    super(hitPoints, armor);
    this.mana = mana;
    this.spells = spells;
    this.effects = [];
    this.minimumManaReqd = spells.reduce((out, curr) => out < curr.cost ? out : curr.cost, Infinity)
  }

  heal(healHitPoints) {
    this.hitPoints += healHitPoints;
  }

  increaseArmor(increaseAmount) {
    this.armor = increaseAmount;
  }

  dropArmor(){
    this.armor = 0
  }

  plusMana(amount) {
    this.mana += amount;
  }

  runEffects(boss){
    if (this.effects.length > 0) {
      for (let effect of this.effects) {
        effect.performEffect({ boss, player: this });
      }
      // remove any effects that have run their course
      // run any cleanup functions needed
      const newEffects = []
      for (let effect of this.effects){
        if(effect.turns === 0){
          if(effect.endFn){
            effect.endFn({player: this})
          }
        } else {
          newEffects.push(effect)
        }
      }
      this.effects = newEffects
    }
  }

  attack({boss, manaSpends, spellsCast}) {
    // run effects
    this.runEffects(boss)
    // cast spell
    // you're dry!
    if(this.mana < this.minimumManaReqd) return
    // loop until finding an appropriate spell
    while(true){
      // select a spell at random
      const spellIdx = Math.floor(Math.random() * this.spells.length)
      const spell = this.spells[spellIdx]
      // only cast spells where you have enough mana and you don't have the effect actively running
      if(this.mana >= spell.cost && !this.effects.map(effect => effect.name).includes(spell.name)){
        this._castSpell({spell, boss, manaSpends})
        spellsCast.push(spell.name)
        // break out after casting the spell
        break
      }
    }
  }



  _castSpell({spell, boss, manaSpends}) {
    if(spell.effectData && spell.effectData.turns > 0) {
      const {turns, effectFn, endFn} = spell.effectData
      this.effects.push(new Effect({turns, effectFn, name: spell.name, endFn}))
    } else {
      boss.takeDamage(spell.damage, true)
      if(spell.effectData?.effectFn) spell.effectData.effectFn({boss, player: this})
    }

    this.mana -= spell.cost
    manaSpends.push(spell.cost)
  }
}

class Effect {
  constructor({turns, effectFn, name, endFn}) {
    this.turns = turns;
    this.effectFn = effectFn;
    this.name = name
    this.endFn = endFn
  }

  performEffect({boss, player}) {
    this.effectFn({boss, player});
    this.turns--
  }
}

class Spell {
  constructor(name, cost, damage, effectData) {
    this.name = name;
    this.cost = cost;
    this.damage = damage;
    this.effectData = effectData;
  }
}

class Boss extends Player {
  constructor({hitPoints, damage, armor}) {
    super(hitPoints, armor);
    this.damage = damage;
  }

  attack(player) {
    player.runEffects(this)
    player.takeDamage(this.damage);
  }
}

const playerSpells = [
  new Spell("Magic Missile", 53, 4, null),
  new Spell("Drain", 73, 2, { turns: 0, effectFn: ({ player }) => player.heal(2) }),
  new Spell("Shield", 113, 0, {
    turns: 6,
    effectFn: ({ player }) => player.increaseArmor(7),
    endFn: ({ player }) => (player.dropArmor()),
  }),
  new Spell("Poison", 173, 0, { turns: 6, effectFn: ({ boss }) => boss.takeDamage(3, true) }),
  new Spell("Recharge", 229, 0, { turns: 5, effectFn: ({ player }) => player.plusMana(101) }),
];

let leastMana = Infinity;
let winningSpellsCast = []

console.log("Running simulations...");
const totalRounds = 1_000_000_000;
for (let roundIdx = 0; roundIdx < totalRounds; roundIdx++){
  // const player = new Wizard({ mana: 500, hitPoints: 50, armor: 0, spells: playerSpells });
  // const boss = new Boss({ hitPoints: 58, damage: 9, armor: 0 });
  const player = new Wizard({ mana: 250, hitPoints: 10, armor: 0, spells: playerSpells });
  const boss = new Boss({ hitPoints: 13, damage: 8, armor: 0 });
  const manaSpends = [];
  const spellsCast = []
  let turnIdx = 0;
  while (player.hitPoints > 0 && boss.hitPoints > 0) {
    if (turnIdx % 2 === 0) {
      player.attack({ boss, manaSpends, spellsCast });
    } else {
      boss.attack(player);
    }
    turnIdx++;
  }
  if (player.hitPoints > 0) {
    const manaCount = manaSpends.reduce((out, curr) => out + curr, 0);
    if (manaCount < leastMana){
      leastMana = manaCount;
      winningSpellsCast = spellsCast
    }
  }
  if(roundIdx / totalRounds * 100 % 10 === 0){
    console.log(`${Math.round(roundIdx / totalRounds * 100)}% complete...`)
  }
}
console.log("LEAST MANA: ", leastMana);
console.log("SPELLS CAST:\n", winningSpellsCast.join(",\n"))


// too low: 987
// incorrect: 1309
// incorrect: 1341
// too high: 1475

/**
 * Part One: 1269
 * Part Two: 1309
 */