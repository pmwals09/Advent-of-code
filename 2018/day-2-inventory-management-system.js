const data = require("./data/day-2.json");

const letterFrequency = (id) => {
  return id.split("").reduce((out, curr) => {
    if (out[curr]) {
      return { ...out, [curr]: out[curr] + 1 };
    } else {
      return { ...out, [curr]: 1 };
    }
  }, {});
};

const exactlyTwoOrThree = (frequencyCount) => {
  let counts = {
    2: 0,
    3: 0,
  };
  Object.values(frequencyCount).forEach((count) => {
    if (count === 2) {
      counts[2]++;
    } else if (count === 3) {
      counts[3]++;
    }
  });
  return counts;
};

const countTwoThreeCounts = (twosThreesArr) => {
  return twosThreesArr.reduce(
    (out, curr) => {
      if (curr[2]) out[2]++;
      if (curr[3]) out[3]++;
      return out;
    },
    { 2: 0, 3: 0 }
  );
};

const multiplyValues = (twoThreeCountObj) => {
  return Object.values(twoThreeCountObj).reduce((out, curr) => out * curr, 1);
};

console.log(
  multiplyValues(
    countTwoThreeCounts(
      data
        .map((id) => letterFrequency(id))
        .map((count) => exactlyTwoOrThree(count))
    )
  )
);

const compareStrings = (str1, str2) => {
  if (str1.length !== str2.length) return false;
  let pointer = 0;
  let commonLetters = [];
  let differences = 0
  while (pointer <= str1.length - 1 && differences < 2) {
    if (str1[pointer] === str2[pointer]) {
      commonLetters.push(str1[pointer]);
    } else {
      differences++
    }
    pointer++;
  }
  return pointer === str1.length ? commonLetters.join('') : false
};

const compareData = (data) => {
  for(let firstString = 0; firstString <= data.length; firstString++){
    for(let secondString = firstString + 1; secondString < data.length; secondString++){
      let comparisonResult = compareStrings(data[firstString], data[secondString])
      if(comparisonResult){
        return comparisonResult
      } else {
        continue
      }
    }
  }
}

console.log(compareData(data))