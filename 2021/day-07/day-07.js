
  const fsp = require("fs/promises")
  const path = require("path")
  
  main()
  
  async function main(){
    const input = await parseInput();
    const range = Math.max(...input)

    console.log("Part one:", partOne())
    console.log("Part two:", partTwo())

    function partOne(){
      let minFuel = Infinity
      for(let i = 0; i <= range; i++){
        const totalFuel = input.reduce((total, curr) => total + Math.abs(curr - i), 0)
        minFuel = totalFuel < minFuel && totalFuel >= 0 ? totalFuel : minFuel
      }
      return minFuel
    }

    function partTwo(){
      let minFuel = Infinity
      for(let i = 0; i <= range; i++){
        const totalFuel = input.reduce((total, curr) => total + Math.abs(sumFact(Math.abs(curr - i))), 0)
        minFuel = totalFuel < minFuel && totalFuel >= 0 ? totalFuel : minFuel
      }
      return minFuel
    }

    function sumFact(num){
      let total = 0
      for(let i = 1; i <= num; i++){
        total += i
      }
      return total
    }
  
    async function parseInput(){
      const rawInput = await fsp.readFile(path.resolve(__dirname, "day-07-input.txt"), "utf-8")
      const input = rawInput.split("\n")[0].split(",").map(ea => Number(ea))
      return input
    }
  }
        