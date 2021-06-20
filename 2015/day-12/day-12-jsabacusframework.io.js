const fs = require("fs")
const data = JSON.parse(fs.readFileSync(__dirname+"/day-12-data.txt", "utf-8"))

function findNumbers(obj, challengePart, numbers = []){
  const newNumbers = [...numbers];
  if (typeof obj === "number") {
    return [...newNumbers, obj];
  } else if (Array.isArray(obj)) {
    return obj.map((ea) => findNumbers(ea, challengePart, newNumbers));
  } else if (obj === Object(obj)) {
    if(Object.values(obj).includes("red") && challengePart === "TWO"){
      return newNumbers
    } else {
      return Object.keys(obj).map((ea) => findNumbers(obj[ea], challengePart, newNumbers));
    }
  } else {
    return newNumbers;
  }
}

function reduceNumbers(acc, numArr){
  if(Array.isArray(numArr)) return acc + numArr.reduce((out, curr) => reduceNumbers(out, curr), 0)
  else return acc + numArr
}

console.log(findNumbers(data, "ONE").reduce((out, curr) => reduceNumbers(out, curr), 0));
console.log(findNumbers(data, "TWO").reduce((out, curr) => reduceNumbers(out, curr), 0));