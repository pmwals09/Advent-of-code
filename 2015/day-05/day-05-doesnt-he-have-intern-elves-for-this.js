const data = require("./day-05-data.json");

const forbiddenStrings = ["ab", "cd", "pq", "xy"];

const hasForbiddenStrings = (string) => {
  let hasString = false;
  forbiddenStrings.forEach((forbidString) => {
    if (string.includes(forbidString)) {
      hasString = true;
    }
  });
  return hasString;
};

const hasDoubledLetter = (string) => {
  let hasLetter = false;
  for (let i = 0; i < string.length - 1; i++) {
    if (string[i] === string[i + 1]) hasLetter = true;
  }
  return hasLetter;
};

const hasThreeVowels = (string) => {
  let count = string
    .split("")
    .reduce((out, curr) => ("aeiou".includes(curr) ? ++out : out), 0);
  return count >= 3;
};

console.log(
  data.reduce(
    (out, curr) =>
      hasDoubledLetter(curr) &&
      hasThreeVowels(curr) &&
      !hasForbiddenStrings(curr)
        ? ++out
        : out,
    0
  )
);

const pairOfTwoLetters = (string) => {
  let hasPairs = false;
  for (let i = 0; i < string.length - 2; i++) {
    if (string.substring(i + 2).includes(string.substring(i, i + 2))) {
      hasPairs = true;
    }
  }
  return hasPairs;
};

const pairSeparatedByOne = (string) => {
  let hasPair = false;
  for (let i = 0; i < string.length - 2; i++) {
    if (string[i] === string[i + 2]) {
      hasPair = true;
    }
  }
  return hasPair;
};

console.log(
  data.reduce(
    (out, curr) =>
      pairOfTwoLetters(curr) && pairSeparatedByOne(curr) ? ++out : out,
    0
  )
);
