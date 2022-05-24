
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input).length)
  console.log("Part two:", partTwo(input))
}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-09-input.txt"), "utf-8")
  const input = rawInput.split("\n").filter(Boolean)[0]
  return input
}

function partOne(input){
  let instruction = ""
  let newString = ""
  for(let i = 0; i < input.length; i++){
    if(input[i] === "(") {
      instruction += input[i]
    } else if(input[i] === ")" && instruction.length > 0){
      const {repeatedString, stringLen} = handleRepeat(instruction, input, i)
      newString += repeatedString
      i += stringLen
      instruction = ""
    } else if(instruction.length > 0){
      instruction += input[i]
    } else {
      newString += input[i]
    }
  }
  // console.log(newString)
  return {
    newString,
    length: newString.length
  }
}

function handleRepeat(instruction, input, index){
  const [stringLen, repeat] = instruction.slice(1).split("x").map(Number);
  const stringToRepeat = input.slice(index + 1, index + 1 + stringLen);
  const repeatedString = stringToRepeat.repeat(repeat);
  return {repeatedString, stringLen}
}

/**
 * 
 * 1067451 - too low
 */
function partTwo(input){
  input = "(27x12)(20x12)(13x14)(7x10)(1x12)A";
  console.log(decompressInput(input))
  // console.log(newString)
}

function decompressInput(input){
  // console.log("decompressInput input", input)
  let instruction = "";
  let newString = "";
  for (let i = 0; i < input.length; i++) {
    if (input[i] === "(") {
      instruction += input[i];
    } else if (input[i] === ")" && instruction.length > 0) {
      const { repeatedString, stringLen } = handleRecursiveRepeat(instruction, input, i);
      newString += repeatedString;
      i += stringLen;
      instruction = "";
    } else if (instruction.length > 0) {
      instruction += input[i];
    } else {
      newString += input[i];
    }
  }
  return {repeatedString: newString, stringLen: newString.length}
}
        
function handleRecursiveRepeat(instruction, input, index) {
  // console.log("handleRecursiveRepeat input", input)
  const [stringLen, repeat] = instruction.slice(1).split("x").map(Number);
  const stringToRepeat = input.slice(index + 1, index + 1 + stringLen);
  console.log(repeat);
  const repeatedString = stringToRepeat.repeat(repeat);

  if (repeatedString.match(/\(\d+x\d+\)/)) {
    return decompressInput(repeatedString);
  } else {
    return { repeatedString, stringLen };
  }
}