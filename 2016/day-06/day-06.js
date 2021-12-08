const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()

  console.log("Part one:", partOne())
  console.log("Part two:", partTwo())

  function partOne(){
    let message = ""
    for(let col = 0; col < input[0].length; col++){
      const colCounts = {}
      for(let row = 0; row < input.length; row++){
        const letter = input[row][col]
        if(colCounts[letter]){
          colCounts[letter]++
        } else {
          colCounts[letter] = 1
        }
      }
      message += Object.keys(colCounts).reduce((maxLetter, letter) => colCounts[letter] > colCounts[maxLetter] ? letter : maxLetter, input[0][col])
    }

    return message
  }

  function partTwo(){
    let message = ""
    for(let col = 0; col < input[0].length; col++){
      const colCounts = {}
      for(let row = 0; row < input.length; row++){
        const letter = input[row][col]
        if(colCounts[letter]){
          colCounts[letter]++
        } else {
          colCounts[letter] = 1
        }
      }
      message += Object.keys(colCounts).reduce((minLetter, letter) => colCounts[letter] < colCounts[minLetter] ? letter : minLetter, input[0][col])
    }

    return message
  }

  async function parseInput(){
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-06-input.txt"), "utf-8")
    const input = rawInput.split("\n")
    return input
  }
}
      