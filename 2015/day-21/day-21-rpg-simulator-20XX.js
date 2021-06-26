const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-21-data.txt", "utf-8").split("\n");
const menu = fs.readFileSync(__dirname + "/day-21-store-menu.txt", "utf-8").split("\n");

const playerStats = {
  "Hit Points": 100,
  Damage: 1,
  Armor: 0,
};

const bossStats = data
  .map((ea) => ea.match(/([\w\s]+)\: (\d+)$/))
  .reduce((out, curr) => ({ ...out, [curr[1]]: +curr[2] }), {});

const allPossibleValidPurchases = makePurchases();
console.log(
  "Part 1: ",
  allPossibleValidPurchases
    .map((purchase) => applyPurchase(purchase))
    .filter((newPlayerStats) => {
      return getWinner(newPlayerStats, bossStats) === newPlayerStats;
    })
    .reduce((out, curr) => {
      return Math.min(out, curr.cost);
    }, Infinity)
);
console.log(
  "Part 2: ",
  allPossibleValidPurchases
    .map((purchase) => applyPurchase(purchase))
    .filter((newPlayerStats) => {
      return getWinner(newPlayerStats, bossStats) === bossStats;
    })
    .reduce((out, curr) => {
      return Math.max(out, curr.cost);
    }, 0)
);

function makePurchases() {
  const storeItems = parseMenuToStoreItems();

  let purchases = [];
  for (let weaponIdx = 0; weaponIdx < storeItems.weapon.length; weaponIdx++) {
    const weapon = storeItems.weapon[weaponIdx];
    for (let armorIdx = 0; armorIdx <= storeItems.armor.length; armorIdx++) {
      const armor = storeItems.armor[armorIdx];
      for (let ringIdxA = 0; ringIdxA <= storeItems.ring.length; ringIdxA++) {
        const ringA = storeItems.ring[ringIdxA];
        for (let ringIdxB = 1; ringIdxB <= storeItems.ring.length; ringIdxB++) {
          const ringB = storeItems.ring[ringIdxB];
          const purchase = [weapon, armor, ringA, ringB].filter((ea) => ea);
          if (validPurchase(purchase)) {
            purchases.push(purchase);
          }
        }
      }
    }
  }
  return purchases;

  function parseMenuToStoreItems() {
    return menu.map((ea) => ea.match(/^([\w\s\+]+)\s+(\d+)\s+(\d+)\s+(\d+)$/)).reduce(reduceMenuArrayIntoObject, {});

    function reduceMenuArrayIntoObject(out, curr, i) {
      let type;
      if (i < 5) type = "weapon";
      else if (i < 10) type = "armor";
      else type = "ring";
      if (!!out[type]) {
        return {
          ...out,
          [type]: [...out[type], { itemName: curr[1].trim(), cost: +curr[2], damage: +curr[3], armor: +curr[4], type }],
        };
      } else {
        return {
          ...out,
          [type]: [{ itemName: curr[1].trim(), cost: +curr[2], damage: +curr[3], armor: +curr[4], type }],
        };
      }
    }
  }

  function validPurchase(purchase) {
    return oneWeapon() && oneOrLessArmor() && twoOrLessRings();

    function oneWeapon() {
      return purchase.filter((ea) => ea.type === "weapon").length === 1;
    }

    function oneOrLessArmor() {
      return purchase.filter((ea) => ea.type === "armor").length < 2;
    }

    function twoOrLessRings() {
      const purchasedRings = purchase.filter((ea) => ea.type === "ring");
      return (
        purchasedRings.length < 3 &&
        (purchasedRings.length === 2 ? purchasedRings[0].itemName !== purchasedRings[1].itemName : true)
      );
    }
  }
}

function applyPurchase(purchase) {
  const newPlayerStats = { ...playerStats };
  const purchaseTotals = purchase.reduce(
    (out, item) => {
      return {
        cost: out.cost + item.cost,
        Damage: out.Damage + item.damage,
        Armor: out.Armor + item.armor,
      };
    },
    { cost: 0, Damage: 0, Armor: 0 }
  );
  return {
    ...newPlayerStats,
    ...purchaseTotals,
  };
}

function getWinner(you, boss) {
  const youTurns = numTurnsToWin({ actor: you, opponent: boss });
  const bossTurns = numTurnsToWin({ actor: boss, opponent: you });
  if (youTurns <= bossTurns) return you;
  else return boss;
}

function numTurnsToWin({ actor, opponent }) {
  const opponentHitPoints = opponent["Hit Points"];
  const hitDamage = Math.max(actor.Damage - opponent.Armor, 1);
  return Math.ceil(opponentHitPoints / hitDamage);
}
