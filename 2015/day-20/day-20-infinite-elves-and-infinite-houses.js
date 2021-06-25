const fs = require("fs");
const data = +fs.readFileSync(__dirname + "/day-20-data.txt");

const CONSTS = {
  PART_ONE: {
    NUM_PRESENTS: 10,
    MAX_HOUSES: false,
  },
  PART_TWO: {
    NUM_PRESENTS: 11,
    MAX_HOUSES: 50,
  },
};

function numberOfPresents(houseNumber, challengePart) {
  const { NUM_PRESENTS, MAX_HOUSES } = CONSTS[challengePart];
  if (houseNumber === 1) return NUM_PRESENTS;
  let factors = [];
  for (let i = 1; i <= Math.sqrt(houseNumber); i++) {
    if (houseNumber / i === i) {
      factors.push(i);
    } else if (houseNumber % i === 0) {
      factors.push(i);
      factors.push(houseNumber / i);
    }
  }
  if (MAX_HOUSES) factors = factors.filter((ea) => !(ea * 50 < houseNumber));
  return factors.reduce((out, curr) => out + curr * NUM_PRESENTS, 0);
}

function houseNumberWithNumPresents(challengePart) {
  let houseNum = 1;
  let numPresents = 0;
  while (numPresents < data) {
    houseNum++;
    numPresents = numberOfPresents(houseNum, challengePart);
  }
  return houseNum;
}
console.log("Part 1: ", houseNumberWithNumPresents("PART_ONE"));
console.log("Part 2: ", houseNumberWithNumPresents("PART_TWO"));
