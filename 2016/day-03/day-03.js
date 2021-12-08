
  const fsp = require("fs/promises")
  const path = require("path")
  
  main()
  
  async function main(){
    console.log("Part One:", await partOne())
    console.log("Part Two", await partTwo())

    async function partOne(){
      const input = await parsePartOneInput()
      const valid = input.filter(triangle => isValidTriangle(triangle)).length
  
      return  valid
    }

    async function partTwo(){
      const input = await parsePartTwoInput()
      const valid = input.filter(triangle => isValidTriangle(triangle)).length
  
      return valid
    }

    function isValidTriangle([A, B, C]){
      return (A + B > C && B + C > A && C + A > B)
    }
  
    async function parsePartOneInput(){
      const rawInput = await fsp.readFile(path.resolve(__dirname, "day-03-input.txt"), "utf-8")
      const input = rawInput.split("\n").map(triangle => triangle.split(" ").filter(ea => ea.length).map(ea => Number(ea)))
      return input
    }
    async function parsePartTwoInput(){
      const rawInput = await fsp.readFile(path.resolve(__dirname, "day-03-input.txt"), "utf-8")
      const input = rawInput.split("\n").map(triangle => triangle.split(" ").filter(ea => ea.length).map(ea => Number(ea)))
      const rotatedInput = []
      for(let i = 0; i < 3; i++){
        let newTriangle = []
        for(let j = 0; j < input.length; j++){
          if(newTriangle.length === 3){
            rotatedInput.push(newTriangle)
            newTriangle = [input[j][i]]
          } else {
            newTriangle.push(input[j][i])
          }
        }
        rotatedInput.push(newTriangle)
      }
      return rotatedInput
    }
  }
        