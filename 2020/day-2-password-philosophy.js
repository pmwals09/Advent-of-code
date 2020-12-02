const data = require("./data/day-2.json")

const parseInput = line => {
  let minOccur = line.split("-")[0]
  let remainingPieces = line.split("-")[1].split(" ")
  let [maxOccur, targetLetter, password] = remainingPieces
  return {
    minOccur: parseInt(minOccur),
    maxOccur: parseInt(maxOccur),
    targetLetter: targetLetter[0],
    password
  }
}

const parsePassword = password => {
  return password.split("").reduce((out, curr) => {
    if(out[curr]) return {...out, [curr]: out[curr] + 1}
    else return {...out, [curr]: 1}
  }, {})
}

const passwordOccurrenceTest = line => {
  const parsedInput = parseInput(line)
  let {minOccur, maxOccur, targetLetter, password} = parsedInput
  const targetLetterFrequency = parsePassword(password)[targetLetter]
  return targetLetterFrequency <= maxOccur && targetLetterFrequency >= minOccur
}

console.log(data.reduce((out, curr) => {
  return passwordOccurrenceTest(curr) ? out + 1 : out
}, 0))

const passwordPositionTest = line => {
    const parsedInput = parseInput(line);
    let { minOccur, maxOccur, targetLetter, password } = parsedInput;
    return (
      (password[minOccur - 1] === targetLetter ||
        password[maxOccur - 1] === targetLetter) &&
      !(
        password[minOccur - 1] === targetLetter &&
        password[maxOccur - 1] === targetLetter
      )
    );

}

console.log(data.reduce((out, curr) => {
  return passwordPositionTest(curr) ? out + 1 : out
}, 0))