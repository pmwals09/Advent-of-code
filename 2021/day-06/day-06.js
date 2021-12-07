
const fsp = require("fs/promises")
const path = require("path")
  
main()
  
async function main(){
  const input = await parseInput()
  console.log("Part one:", run(80))
  console.log("Part two:", run(256))

  function run(days){
    const fish = {}
    for(let i = 0; i < 9; i++){
      fish[i] = 0
    }
  
    for(const days of input){
      fish[days]++
    }
  
    for(let i = 0; i < days; i++){
      const newFish = fish["0"]
      fish["0"] = fish["1"]
      fish["1"] = fish["2"]
      fish["2"] = fish["3"]
      fish["3"] = fish["4"]
      fish["4"] = fish["5"]
      fish["5"] = fish["6"]
      fish["6"] = fish["7"] + newFish
      fish["7"] = fish["8"]
      fish["8"] = newFish
    }
  
    return Object.values(fish).reduce((total, day) => total + day)
  }

  async function parseInput(){
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-06-input.txt"), "utf-8")
    // const rawInput = "3,4,3,1,2"
    const input = rawInput.split("\n")
    return input[0].split(",").map(ea => Number(ea))
  }
}
      