const data = require("./data/day-1.json")

let floor = 0
let firstInstruction = Infinity
for(let i = 0; i < data.length; i++){
  if(data[i] === "(") floor++
  else floor--
  if(floor < 0) firstInstruction = Math.min(firstInstruction, i+1)
}

console.log(floor)
console.log(firstInstruction)