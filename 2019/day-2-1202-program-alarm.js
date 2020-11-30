const input = require("./inputs/2.json")

const intcode = (memory, overwriteAddresses=null, overwriteValues=null) => {
  if(overwriteAddresses){
    overwriteAddresses.forEach((address, i) => {
      memory[address] = overwriteValues[i]
    });
  }
  let pointer = 0
  while(pointer <= memory.length - 4){
    const instruction = memory[pointer]
    const param1 = memory[pointer + 1]
    const param2 = memory[pointer + 2]
    const param3 = memory[pointer + 3]
    if(instruction === 1){
      memory[param3] = memory[param1] + memory[param2]
    } else if(instruction === 2){
      memory[param3] = memory[param1] * memory[param2]
    } else if(instruction === 99){
      break
    } else {
    }
    pointer = pointer + 4
  }
  return memory[0]
}

console.log(intcode(input))