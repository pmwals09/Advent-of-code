const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-15-data.txt", "utf-8").split("\n");

const ingredients = data.map((ea) => parseLine(ea));
const maxScoreNoCals = scoreIter(100, false);
const maxScoreCals = scoreIter(100, true);
console.log("Part 1: ", maxScoreNoCals);
console.log("Part 2: ", maxScoreCals)

function parseLine(line) {
  const lineRegEx = /(\w+)\: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)/;
  const [_, ingredient, capacity, durability, flavor, texture, calories] = line.match(lineRegEx);
  return {
    ingredient,
    capacity: +capacity,
    durability: +durability,
    flavor: +flavor,
    texture: +texture,
    calories: +calories,
  };
}

function scoreRecipe(volumes, ingredients) {
  const ingredientWithVolume = ingredients.map((ea, i) => {
    return {
      ...ea,
      capacity: ea.capacity * volumes[i],
      durability: ea.durability * volumes[i],
      flavor: ea.flavor * volumes[i],
      texture: ea.texture * volumes[i],
      calories: ea.calories * volumes[i],
    };
  });
  const sumsByCategory = ingredientWithVolume.reduce((out, curr) => {
    return {
      capacity: out.capacity + curr.capacity,
      durability: out.durability + curr.durability,
      flavor: out.flavor + curr.flavor,
      texture: out.texture + curr.texture,
      calories: out.calories + curr.calories,
    };
  });
  return {
    score: calculateScore(),
    calories: sumsByCategory.calories
  }

  function calculateScore(){
    return Object.keys(sumsByCategory).reduce((out, key) => {
      if (key !== "calories") {
        if (sumsByCategory[key] < 0) {
          return 0;
        } else {
          return out * sumsByCategory[key];
        }
      } else {
        return out;
      }
    }, 1);
  }
}

function scoreIter(totalVolume, trackCalories) {
  let bestCookie = {score: 0, calories: 0};
  for (let i = 0; i <= totalVolume; i++) {
    for (let j = 0; j <= totalVolume; j++) {
      for (let k = 0; k <= totalVolume; k++) {
        for (let l = 0; l <= totalVolume; l++) {
          if (i + j + k + l === totalVolume) {
            const {score, calories} = scoreRecipe([i, j, k, l], ingredients)
            if(trackCalories){
              if(calories === 500){
                bestCookie = score > bestCookie.score ? {score, calories} : bestCookie
              }
            } else {
              bestCookie = score > bestCookie.score ? { score, calories } : bestCookie;
            }
          }
        }
      }
    }
  }
  return bestCookie;
}
