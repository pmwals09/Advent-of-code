const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-11-data.txt", "utf-8");

function findNextPassword(password) {
  let nextPassword = increment(password);
  while (!validate(nextPassword)) {
    nextPassword = increment(nextPassword);
  }
  return nextPassword;
}

function increment(password, place = 1) {
  if (password[password.length - place] === "z") {
    const newPassword = incrementOneLetter("a");
    return increment(newPassword, place + 1);
  } else {
    return incrementOneLetter(String.fromCharCode(password.charCodeAt(password.length - place) + 1));
  }

  function incrementOneLetter(incrementedLetter) {
    return password.slice(0, password.length - place) + incrementedLetter + password.slice(password.length - place + 1);
  }
}

function validate(password) {
  return ruleOne(password) && ruleTwo(password) && ruleThree(password);
}

function ruleOne(string) {
  let longestRun = "";
  let run = "";
  for (let i = 0; i < string.length; i++) {
    if (run.length === 0) {
      run += string[i];
    } else {
      if (string.charCodeAt(i) - run.charCodeAt(run.length - 1) === 1) {
        run += string[i];
      } else {
        if (longestRun.length < run.length) {
          longestRun = run;
        }
        run = "";
      }
    }
  }
  return longestRun.length >= 3;
}

function ruleTwo(string) {
  const forbiddenCharsRegEx = /[iol]/gi;
  const match = string.match(forbiddenCharsRegEx);
  return !match;
}

function ruleThree(string) {
  const doubleLetterRegEx = /(.)\1/g;
  const match = string.match(doubleLetterRegEx);
  return match && match.length >= 2;
}

const firstPassword = findNextPassword(data)
const secondPassword = findNextPassword(firstPassword)
console.log(firstPassword);
console.log(secondPassword);
