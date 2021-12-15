
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))
}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-14-input.txt"), "utf-8")
  const input = rawInput.split("\n").filter(Boolean)
  const [template, ...insertions] = input

  return {template, insertions: insertions.reduce((out, curr) => {
    const [pair, insert] = curr.split(" -> ")
    out[pair] = insert

    return out
  }, {})}
}

function partOne(input){
  const {template, insertions} = input
  let polymer = template
  for(let i = 0; i < 10; i++){
    polymer = performInsertions(polymer, insertions)
  }
  const freq = getLetterFrequencies(polymer)
  const counts = Object.values(freq)

  return Math.max(...counts) - Math.min(...counts)
}

function performInsertions(polymer, insertions) {
  let newPolymer = polymer[0]
  for(let i = 1; i < polymer.length; i++){
    const pair = `${polymer[i - 1]}${polymer[i]}`
    const insertionChar = insertions[pair]
    if(insertionChar){
      newPolymer += insertionChar
      newPolymer += polymer[i]
    } else {
      newPolymer += polymer[i]
    }
  }

  return newPolymer
}

function getLetterFrequencies(string){
  return string.split("").reduce((out, char) => {
    if(out[char]){
      out[char]++
    } else {
      out[char] = 1
    }

    return out
  }, {})
}

function partTwo(input) {
  const { template, insertions } = input;
  let polymer = getPairFreq(template)
  for(let i = 0; i < 40; i++){
    polymer = updatePairFreq(polymer, insertions);
  }
  const charCounts = getCharFreq(polymer, template)
  const charValues = Object.values(charCounts)
  
  return Math.max(...charValues) - Math.min(...charValues)
}

function getPairFreq(str){
  const pairs = {}
  for(let i = 0; i < str.length - 1; i++){
    const pair = str.slice(i, i+2)
    if(pairs[pair]){
      pairs[pair]++
    } else {
      pairs[pair] = 1
    }
  }

  return pairs
}

function updatePairFreq(pairFreq, insertions){
  const newPairFreq = {}
  for(const pair of Object.keys(pairFreq)){
    const insertion = insertions[pair]
    if(insertion){
      const newPairs = [
        pair[0] + insertion,
        insertion + pair[1]
      ]
      for(const newPair of newPairs){
        if(newPairFreq[newPair]){
          newPairFreq[newPair] += pairFreq[pair]
        } else {
          newPairFreq[newPair] = pairFreq[pair]
        }
      }
    }
  }

  return newPairFreq
}

function getCharFreq(polymer, template){
  const charCounts = Object.keys(polymer).reduce((out, pair) => {
    for(const char of pair.split("")){
      if(out[char]){
        out[char] += polymer[pair]
      } else {
        out[char] = polymer[pair]
      }
    }

    return out
  }, {})

  charCounts[template[0]]++;
  charCounts[template[template.length - 1]]++;

  Object.keys(charCounts).forEach(char => {
    charCounts[char] /= 2
  })

  return charCounts
}
