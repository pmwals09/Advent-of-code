const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-17-data.txt", "utf-8").split("\n");

const containers = data.map((ea) => +ea);
const allCombinations = generateCombinations();
const allCombinationsNoEmpty = allCombinations.filter(combination => combination.length > 0)
console.log(
  "Part 1: ",
  allCombinationsNoEmpty
    .filter((combination) => totalCombination(combination) === 150).length
);

const minimumLength = allCombinationsNoEmpty.reduce((out, curr) => {
  const valid = totalCombination(curr) === 150
  if(valid) {
    return out > curr.length ? curr.length : out
  } else {
    return out
  }
}, Infinity)
console.log(
  "Part 2: ",
  allCombinationsNoEmpty
    .filter((combination) => totalCombination(combination) === 150)
    .filter((combination) => combination.length === minimumLength).length
);

function generateCombinations() {
  const combinations = [[]];
  for (let container of containers) {
    const last = combinations.length - 1;
    for (let i = 0; i <= last; i++) {
      combinations.push([...combinations[i], container]);
    }
  }
  return combinations;
}

function totalCombination(combination) {
  return combination.reduce((out, curr) => out + curr);
}
