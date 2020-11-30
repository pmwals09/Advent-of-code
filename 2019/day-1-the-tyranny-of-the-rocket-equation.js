const input = require("./inputs/1.json")

const fuelInput = (weight) => {
  return Math.floor(weight / 3) - 2
}

const recursiveFuelInput = (weight) => {
  const fuelReq = Math.floor(weight/3)-2
  if(fuelReq <= 0) return 0
  else return fuelReq + recursiveFuelInput(fuelReq)
}


console.log(input.reduce((out, curr) => out + fuelInput(curr), 0))
console.log(input.reduce((out, curr) => out + recursiveFuelInput(curr),0))
