const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))

}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-10-input.txt"), "utf-8")
  const input = rawInput.split("\n").filter(Boolean)
  return input
}

function partOne(input){
  let score = 0
  const SCORE_MAP = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137
  }
  for(const line of input){
    let badChar = isCorrupted(line)
    if(!Array.isArray(badChar)){
      score += SCORE_MAP[badChar]
    }
  }
  return score
}

function isCorrupted(line){
  const openTags = []
  for(const char of line){
    if(isOpenBracket(char)){
      openTags.push(char)
    } else {
      const lastOpenTag = openTags.pop()
      if(!isValidClosingBracket({openTag: lastOpenTag, closeTag: char})){
        return char
      }
    }
  }
  return openTags
}

function isOpenBracket(char){
  return ["(", "[", "{", "<"].includes(char)
}

function isValidClosingBracket({openTag, closeTag}){
  const CLOSE_TAG_MATCH = {
    ")": "(",
    "]": "[",
    "}": "{",
    ">": "<"
  }
  return openTag === CLOSE_TAG_MATCH[closeTag]
}

function partTwo(input){
  const incomplete = input.filter(line => Array.isArray(isCorrupted(line)))
  const completionStrings = []
  for(const line of incomplete){
    handleLine({line, completionStrings})
  }
  return score(completionStrings)
}

function handleLine({line, completionStrings}){
  const closeTags = []
  const completionString = []
  for(let i = line.length - 1; i >= 0; i--){
    const char = line[i]
    if(isOpenBracket(char)){
      handleOpenBracket({char, closeTags, completionString})
    } else {
      closeTags.push(char)
    }
  }
  completionStrings.push(completionString)
}

function handleOpenBracket({char, closeTags, completionString}){
  const lastClose = closeTags.pop()
  if(!lastClose){
    completionString.push(char)
  }
}

function score(completionStrings){
  const scores = completionStrings
    .map(ea => scoreCompletionString()(reverseBrackets(ea)))
    .sort((a, b) => a - b)
  return scores[Math.floor(scores.length / 2)]
}

function scoreCompletionString(){
  return completionArr => completionArr.reduce((total, char) => scoreChar({total, char}), 0)
}

function scoreChar({total, char}){
  const BRACKET_SCORE = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4
  }
  return total * 5 + BRACKET_SCORE[char]
}

function reverseBrackets(bracketArr){
  const BRACKET_MATCH = {
    "(": ")",
    "[": "]",
    "{": "}",
    "<": ">",
  }
  return bracketArr.map(ea => BRACKET_MATCH[ea])
}