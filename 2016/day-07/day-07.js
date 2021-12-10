const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))
}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-07-input.txt"), "utf-8")
  const input = rawInput
    .split("\n")
    .map(line => splitNets(line))
  return input
}

function splitNets(line){
  const hypernet = [];
  const supernet = [];
  let inHypernet = false;
  for (const char of line) {
    if (char === "[") inHypernet = openHypernet({hypernet, supernet})
    else if (char === "]") inHypernet = closeHypernet(hypernet)
    else handleChar({ char, inHypernet, hypernet, supernet });
  }
  return { hypernet: hypernet.filter(Boolean), supernet: supernet.filter(Boolean) };
}

function openHypernet({hypernet, supernet}) {
  if (!hypernet.length) {
    hypernet.push("");
  }
  supernet.push("");
  return true
}

function closeHypernet(hypernet) {
  hypernet.push("");
  return false
}

function handleChar({char, inHypernet, hypernet, supernet}){
  if (inHypernet) {
    hypernet[hypernet.length - 1] += char;
  } else {
    if (!supernet.length) {
      supernet.push(char);
    } else {
      supernet[supernet.length - 1] += char;
    }
  }
}

function partOne(input){
  return input.filter(line => validTls(line)).length
}

function validTls(line){
  const {hypernet, supernet} = line
  console.log(supernet)
  if(hasPalindrome(hypernet)){
    return false
  }
  if(hasPalindrome(supernet)){
    return true
  }
  return false
}

function hasPalindrome(stringArr){
  return stringArr.some(string => {
    for(let i = 3; i < string.length; i++){
      if(string[i] === string[i - 3] && string[i - 2] === string[i - 1] && string[i] !== string[i - 1]){
        return true
      }
    }
    return false
  })
}

function partTwo(input){
  return input.filter(line => validSsl(line)).length
}

function validSsl(line){
  const abas = getAbas(line.supernet)
  if(abas.length === 0){
    return false
  }
  const babs = getAbas(line.hypernet)
  return abas.some(aba => babs.includes(reverseAba(aba)))
}

function getAbas(netArr){
  const abas = []
  netArr.forEach(net => {
    for(let i = 2; i < net.length; i++){
      if(net[i] === net[i - 2] && net[i] !== net[i - 1]){
        abas.push(net.slice(i - 2, i + 1))
      }
    }
  })
  return abas
}

function reverseAba(aba){
  return `${aba[1]}${aba[0]}${aba[1]}`
}
