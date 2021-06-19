const fs = require("fs")
const data = fs.readFileSync(__dirname + "/day-10-data.txt", "utf-8")

function parseNumber(num){
  const numString = num.toString()
  const groups = []
  let currentNum = null
  let startIdx = null
  for(let i = 0; i <= numString.length; i++){
    if(numString[i] !== currentNum){
      groups.push({
        num: currentNum,
        length: i - startIdx
      })
      currentNum = numString[i]
      startIdx = i
    }
  }

  const newNum = groups.slice(1).reduce((out, curr) => out + `${curr.length}${curr.num}`, "")
  return newNum
}

function iterateParsing(iterations){
  let num = data
  for(let i = 0; i < iterations; i++){
    num = parseNumber(num)
  }
  return num
}

console.log(iterateParsing(40).length);
console.log(iterateParsing(50).length);
