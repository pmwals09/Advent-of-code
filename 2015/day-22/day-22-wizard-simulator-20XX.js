

// // too low: 987
// // incorrect: 1309
// // incorrect: 1341
// // too high: 1475

// /**
//  * Part One: 1269
//  * Part Two: 1309
//  */

class Effect {
  constructor(effectData) {
    const { turns, effectFn, endFn } = effectData;
    // Effects are created with a timer
    this.turns = turns;
    this.effectFn = effectFn;
    this.endFn = endFn;
  }

  runEffect({ boss, player }) {
    // after they apply any effect they have,
    if(this.effectFn){
      this.effectFn({ boss, player });
    }
    // their timer is decreased by one
    if(this.turns > 0){
      this.decTurn();
    }
    // If this decreases the timer to zero,
    if (this.turns === 0) {
      // the effect ends
      if(this.endFn){
        this.endFn({ player, boss });
      }
      return false;
    }
    return true;
  }

  decTurn() {
    this.turns--;
  }
}

class Spell {
  constructor(name, manaCost, damage, effectData) {
    this.name = name;
    this.manaCost = manaCost;
    this.damage = damage;
    this.effect = new Effect(effectData);
  }

  castSpell({boss, player}){
    if(this.damage){
      boss.takeDamage(this.damage)
    }

    this.effect.runEffect({boss, player})
  }
}

const playerSpells = [
  new Spell("Magic Missile", 53, 4, {}),
  new Spell("Drain", 73, 2, { turns: 0, effectFn: ({ player }) => player.heal(2) }),
  new Spell("Shield", 113, 0, {
    turns: 6,
    effectFn: ({ player }) => player.increaseArmor(7),
    endFn: ({ player }) => (player.dropArmor()),
  }),
  new Spell("Poison", 173, 0, { turns: 6, effectFn: ({ boss }) => boss.takeDamage(3, true) }),
  new Spell("Recharge", 229, 0, { turns: 5, effectFn: ({ player }) => player.plusMana(101) }),
];

class Player {
  constructor({ hitPoints, armor }) {
    this.hitPoints = hitPoints;
    this.armor = armor;
  }

  takeDamage(amount, ignoreArmor = false) {
    if (ignoreArmor) {
      this.hitPoints -= Math.max(amount, 1);
    } else {
      this.hitPoints -= Math.max(amount - this.armor, 1);
    }
  }
}

class Wizard extends Player {
  constructor({ hitPoints, armor, mana }) {
    super({ hitPoints, armor });
    this.mana = mana;
    this.effects = [];
    this.spells = playerSpells;
    this.minimumRequiredMana = playerSpells.reduce((out, spell) => Math.min(spell.manaCost, out), Infinity);
  }

  increaseArmor(amount) {
    if (this.armor !== amount) {
      this.armor = amount;
    }
  }

  dropArmor() {
    this.armor = 0;
  }

  plusMana(amount) {
    this.mana += amount;
  }

  heal(amount) {
    this.hitPoints += 2;
  }

  takeTurn(boss, game) {
    // Effects apply at the start of both the player's turns and the boss' turns
    this.runAllEffects(boss);
    if(boss.hitPoints <= 0){
      game.end(boss)
      return false
    }
    this.castSpell({ game, boss, player: this });
    return true
  }

  runAllEffects(boss) {
    this.effects = this.effects.filter((effect) => effect.runEffect({ boss, player: this }));
  }

  castSpell({ game, boss, player }) {
    const that = this;
    // If you cannot afford to cast any spell, you lose
    if (this.mana < this.minimumRequiredMana) {
      game.end(player);
      return false;
    }
    // you must select one of your spells to cast
    const spell = selectSpell();
    // its cost is immediately deducted when you cast it
    this.mana -= spell.manaCost;
    game.spellsCast.push(spell);
    game.manaSpent += spell.manaCost;
    spell.castSpell({ boss, player });

    function selectSpell() {
      const spellIdx = Math.floor(Math.random() * that.spells.length);
      const spell = that.spells[spellIdx];
      // You cannot cast a spell that would start an effect which is already active
      if (that.mana >= spell.manaCost && !that.effects.some((effect) => effect.name === spell.name)) {
        return spell;
      } else {
        return selectSpell();
      }
    }
  }
}

class Boss extends Player {
  constructor({hitPoints, armor, damage}){
    super({hitPoints, armor})
    this.damage = damage
  }

  takeTurn(player, game){
    // Effects apply at the start of both the player's turns and the boss' turns
    player.runAllEffects(this);
    if(this.hitPoints <= 0){
      game.end(this)
      return false
    }
    player.takeDamage(this.damage);
    return true
  }
}

class Game
{
  constructor(){
    this.turn = 0
    this.player = new Wizard({mana: 500, hitPoints: 50, armor: 0})
    this.boss = new Boss({hitPoints: 58, armor: 0, damage: 9})
    this.manaSpent = 0
    this.spellsCast = []
  }

  playGame(){
    while (this.player.hitPoints > 0 && this.boss.hitPoints > 0) {
      this.takeTurn();
    }
    // What is the least amount of mana you can spend and still win the fight?
    // (Do not include mana recharge effects as "spending" negative mana.)
    if(this.player.hitPoints > 0){
      console.log("Player won!")
      console.log(this.spellsCast)
      console.log(this.manaSpent)
    } else if(this.boss.hitPoints > 0){
      console.log("Boss won...")
    }
  }

  takeTurn(){
    // player goes first
    // alternating turns
    let attacker, attacked
    if(this.turn % 2 === 0){
      attacker = this.player
      attacked = this.boss
    } else {
      attacker = this.boss
      attacked = this.player
    }
    this.turn++
    return attacker.takeTurn(attacked, this)
  }

  end(loser){
    loser.hitPoints = 0
    return false
  }
  // first at or below 0 hit points loses
  // both essentially have 0 armor
  // boss' attacks always deal at least 1 damage
}

main()

function main(){
  for(let i = 0; i < 1_000_000; i++){
    const game = new Game()
    game.playGame()
  }

  console.log("Fin.")
}
