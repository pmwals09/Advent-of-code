const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-19-data.txt", "utf-8").split("\n");

const { input, replacements } = parseInput(data);

console.log("Part 1: ", makeAllReplacements({ replacements, input }).size);

function parseInput(data) {
  const input = data.pop();
  data.pop();
  const replacements = data.map((ea) => {
    const [from, to] = ea.split(" => ");
    return {
      from,
      to,
    };
  });
  return {
    input,
    replacements,
  };
}

function makeAllReplacements({ input, replacements }) {
  const uniqueStrings = new Set();
  replacements
    .map((replacement) => {
      const newStrings = [];
      for (let i = 0; i < input.length; i++) {
        if (input.slice(i, i + replacement.from.length) === replacement.from) {
          const newString = input.slice(0, i) + replacement.to + input.slice(i + replacement.from.length);
          newStrings.push(newString);
        }
      }
      return newStrings;
    })
    .flat()
    .forEach((string) => {
      uniqueStrings.add(string);
    });
  return uniqueStrings;
}

const numberOfMolecules = input.match(/[A-Z]/g).length;
const exteriorCantReplace = input.match(/Ar|Rn/g).length;
const interiorCantReplace = input.match(/Y/g).length;
console.log("Part 2: ", numberOfMolecules - exteriorCantReplace - (2 * interiorCantReplace) - "e".length);
