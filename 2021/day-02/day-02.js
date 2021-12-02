
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()

  console.log("Part one: ", partOne())
  console.log("Part two: ", partTwo());

  function partOne(){
    let horizontal = 0
    let depth = 0
    for(const instruction of input){
      const [direction, amount] = instruction.split(" ")
      switch(direction){
        case "forward":
          horizontal += Number(amount)
          break
        case "down":
          depth += Number(amount)
          break
        case "up":
          depth -= Number(amount)
          break
      }
    }
    return horizontal * depth
  }
  function partTwo(){
    let horizontal = 0
    let depth = 0
    let aim = 0
    for(const instruction of input){
      const [direction, amount] = instruction.split(" ")
      switch(direction){
        case "forward":
          horizontal += Number(amount)
          depth += (aim * Number(amount))
          break
        case "down":
          aim += Number(amount)
          break
        case "up":
          aim -= Number(amount)
          break
      }
    }
    return horizontal * depth
  }

  async function parseInput(){
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-02-input.txt"), "utf-8")
    const input = rawInput.split("\n")
    return input
  }
}
    