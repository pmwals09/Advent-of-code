
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  const nums = input.map(ea => Number(ea))

  console.log("Part 1: ", partOne())
  console.log("Part 2: ", partTwo())
  function partOne(){
    let total = 0
    for(let i = 0, j = 1; j < nums.length; i++, j++){
      if(nums[j] > nums[i]){
        total++
      }
    }
  
    return total
  }

  function partTwo(){
    let total = 0
    for(let a = 0, b = 2, i = 1, j = 3; j < nums.length; a++, b++, i++, j++){
      if(tally(nums.slice(i, j + 1)) > tally(nums.slice(a, b + 1))){
        total ++
      }
    }

    return total

    function tally(arr){
      return arr.reduce((out, curr) => out + curr)
    }
  }

  async function parseInput(){
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-01-input.txt"), "utf-8")
    const input = rawInput.split("\n")
    return input
  }
}
    