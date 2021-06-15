const fs = require("fs")
const data = fs.readFileSync(__dirname + "/day-08-data.txt", "utf-8").split("\n");

function characterDifference(){
  return data.reduce((out, curr) => {
    return out + Math.abs(curr.length - eval(curr).length)
  }, 0)
}

console.log(characterDifference())

function encodeString(string){
  let encodedString = String.raw`"`
  for(let i = 0; i < string.length; i++){
    if(string[i] === '"'){
      encodedString += String.raw`\"`
    } else if(string[i] === "\\"){
      encodedString += String.raw`\\`
    } else {
      encodedString += string[i]
    }
  }
  return encodedString += String.raw`"`
}

function encodeStringLiterals(){
  return data.reduce((out, curr) => {
    return out + (encodeString(curr).length - curr.length)
  }, 0)
}

console.log(encodeStringLiterals())