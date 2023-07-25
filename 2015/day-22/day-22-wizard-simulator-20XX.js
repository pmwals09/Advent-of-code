console.log("Part One:", partOne());
console.log("Part Two:", partTwo());

function partOne() {
  const game = new Game();
  game.playAllGames();
  return game.leastManaSpent;
}

function partTwo() {
  const game = new Game(true);
  game.playAllGames();
  return game.leastManaSpent;
}

class Game {
  constructor(isPartTwo = false) {
    this.wizard = new Wizard({ hitPoints: 50, mana: 500 });
    this.boss = new Boss({ hitPoints: 58, damageAmount: 9 });
    this.leastManaSpent = Infinity;
    this.isPartTwo = isPartTwo;
  }

  playAllGames(wizard = this.wizard, boss = this.boss) {
    for (let i = 0; i < wizard.spells.length; i++) {
      let spellActive = false;
      for (let j = 0; j < wizard.activeSpells.length; j++) {
        if (wizard.activeSpells[j].duration > 1 && i === wizard.activeSpells[j].idx) {
          spellActive = true;
        }
      }
      if (spellActive || wizard.spells[i].cost > wizard.mana) continue;

      const newWizard = wizard.duplicate();
      const newBoss = boss.duplicate();

      this.playRound(newWizard, newBoss, i);

      if (newBoss.hitPoints <= 0) {
        this.leastManaSpent = Math.min(this.leastManaSpent, newWizard.spent);
      }

      if (this.shouldContinue(newWizard, newBoss)) this.playAllGames(newWizard, newBoss);
    }
  }

  playRound(wizard, boss, spellIdx) {
    if (this.isPartTwo) wizard.hitPoints--;
    wizard.runEffects(boss);
    wizard.attack(boss, spellIdx);
    wizard.runEffects(boss);
    boss.attack(wizard);
  }

  shouldContinue(wizard, boss) {
    const wizardMinHp = this.isPartTwo ? 1 : 0;
    return wizard.hitPoints > wizardMinHp && boss.hitPoints > 0 && wizard.spent < this.leastManaSpent;
  }
}

class Player {
  constructor(hitPoints) {
    this.hitPoints = hitPoints;
    this.spent = 0;
    this.armor = 0;
    this.activeSpells = [];
  }

  takeDamage(amount) {
    this.hitPoints -= Math.max(1, amount - this.armor);
  }
}

class Spell {
  constructor({ name, cost, startFn, effectFn, endFn, duration }) {
    this.name = name;
    this.cost = cost;
    this.start = startFn;
    this.effect = effectFn;
    this.end = endFn;
    this.duration = duration;
  }
}

const spells = [
  new Spell({
    name: "Magic Missile",
    cost: 53,
    effectFn: (_wizard, boss) => boss.takeDamage(4),
  }),
  new Spell({
    name: "Drain",
    cost: 73,
    effectFn: (wizard, boss) => {
      boss.takeDamage(2);
      wizard.hitPoints += 2;
    },
  }),
  new Spell({
    name: "Shield",
    cost: 113,
    startFn: (wizard, _boss) => (wizard.armor += 7),
    effectFn: (_wizard, _boss) => { },
    endFn: (wizard, _boss) => (wizard.armor -= 7),
    duration: 6,
  }),
  new Spell({
    name: "Poison",
    cost: 173,
    effectFn: (_wizard, boss) => boss.takeDamage(3),
    duration: 6,
  }),
  new Spell({
    name: "Recharge",
    cost: 229,
    effectFn: (wizard, _boss) => (wizard.mana += 101),
    duration: 5,
  }),
];

class Wizard extends Player {
  constructor({ hitPoints, mana }) {
    super(hitPoints);
    this.spells = spells;
    this.mana = mana;
  }

  attack(boss, spellIdx) {
    const spell = this.spells[spellIdx];
    this.spent += spell.cost;
    this.mana -= spell.cost;

    if (spell.duration) {
      const newSpell = {
        idx: spellIdx,
        effect: spell.effect,
        duration: spell.duration,
      };
      if (spell.start) {
        spell.start(this, boss);
      }
      if (spell.end) {
        newSpell.end = spell.end;
      }
      this.activeSpells.push(newSpell);
    } else {
      spell.effect(this, boss);
    }
  }

  runEffects(boss) {
    for (const spell of this.activeSpells) {
      if (spell.duration > 0) {
        spell.effect(this, boss);
        spell.duration--;

        if (spell.end && spell.duration === 0) {
          spell.end(this, boss);
        }
      }
    }
  }

  duplicate() {
    const newWizard = new Wizard({ hitPoints: this.hitPoints, mana: this.mana });
    newWizard.spent = this.spent;
    newWizard.armor = this.armor;
    for (var i = 0; i < this.activeSpells.length; i++) {
      newWizard.activeSpells.push({ ...this.activeSpells[i] });
    }
    return newWizard;
  }
}

class Boss extends Player {
  constructor({ hitPoints, damageAmount }) {
    super(hitPoints);
    this.damageAmount = damageAmount;
  }

  attack(player, _spellIdx) {
    player.takeDamage(this.damageAmount);
  }

  duplicate() {
    var newBoss = new Boss({ hitPoints: this.hitPoints, damageAmount: this.damageAmount });
    newBoss.spent = this.spent;
    newBoss.armor = this.armor;
    return newBoss;
  }
}
