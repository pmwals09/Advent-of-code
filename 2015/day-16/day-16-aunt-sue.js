const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-16-data.txt", "utf-8").split("\n");

const sueToMatch = {
  children: 3,
  cats: 7,
  samoyeds: 2,
  pomeranians: 3,
  akitas: 0,
  vizslas: 0,
  goldfish: 5,
  trees: 3,
  cars: 2,
  perfumes: 1,
};

function parseLine(line) {
  const itemsRegEx = /\w+\: \d+/g;
  const match = line.match(itemsRegEx);
  return {
    sueNum: +line.match(/Sue (\d+).*/)[1],
    items: match
      .map((ea) => {
        return ea.match(/(?<itemName>\w+)\: (?<itemNum>\d+)/).groups;
      })
      .reduce((out, curr) => {
        return {
          ...out,
          [curr.itemName]: +curr.itemNum,
        };
      }, {}),
  };
}

const sueData = data.map((line) => parseLine(line));

function testSue(sue) {
  let isRightSue = true;
  for (let item in sue.items) {
    if (sue.items[item] !== sueToMatch[item]) {
      isRightSue = false;
      break;
    }
  }
  return isRightSue;
}

function partTwoTestSue(sue, isPartTwo) {
  let isRightSue = true;
  for (let item in sue.items) {
    if (item === "cats" || item === "trees") {
      if (sue.items[item] <= sueToMatch[item]) {
        isRightSue = false;
        break;
      }
    } else if (item === "pomeranians" || item === "goldfish") {
      if (sue.items[item] >= sueToMatch[item]) {
        isRightSue = false;
        break;
      }
    } else if (sue.items[item] !== sueToMatch[item]) {
      isRightSue = false;
      break;
    }
  }
  return isRightSue;
}

const partOneTestedSueData = sueData.find((sue) => testSue(sue));
const partTwoTestedSueData = sueData.find((sue) => partTwoTestSue(sue));
console.log("Part 1: ", partOneTestedSueData.sueNum);
console.log("Part 2: ", partTwoTestedSueData.sueNum);
