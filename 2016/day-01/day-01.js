
  const fsp = require("fs/promises")
  const path = require("path")
  
  main()
  /**
   * Wrong answers:
   * - 648 (too high)
   */
  
  async function main(){
    const input = await parseInput()
    class Cursor {
      constructor(){
        this.x = 0
        this.y = 0
        this.direction = 0
      }

      move(direction, distance){
        if(direction === "R"){
          this.direction = (this.direction + 90) % 360
        } else {
          this.direction = (this.direction - 90) % 360
          if(this.direction < 0){
            this.direction = this.direction + 360
          }
        }

        switch(this.direction){
          case 0:
            this.y += distance
            break
          case 90:
            this.x += distance
            break
          case 180:
            this.y -= distance
            break
          case 270:
            this.x -= distance
            break
          default:
            this.y += distance
        }
      }

      getPosition(){
        return {
          x: this.x,
          y: this.y
        }
      }
    }

    class Instruction {
      constructor(stringInstruction){
        this.direction = ""
        this.distance = ""

        for(let i = 0; i < stringInstruction.length; i++){
          if(i === 0){
            this.direction += stringInstruction[i]
          } else {
            this.distance += stringInstruction[i]
          }
        }

        this.distance = Number(this.distance)
      }
    }

    const instructions = input.map(ea => new Instruction(ea))
    
    console.log("Part one:", partOne())
    console.log("Part two:", partTwo())

    function partOne(){
      const cursor = new Cursor()
      for(let instruction of instructions){
        cursor.move(instruction.direction, instruction.distance)
      }
  
      const {x, y} = cursor.getPosition()
      return Math.abs(x) + Math.abs(y)
    }

    function partTwo(){
      const map = [[1]]
      const cursor = new Cursor()
      for(let instruction of instructions){
         
      }
  
      const {x, y} = cursor.getPosition()
      return Math.abs(x) + Math.abs(y)
    }
  
    async function parseInput(){
      const rawInput = await fsp.readFile(path.resolve(__dirname, "day-01-input.txt"), "utf-8")
      const input = rawInput.split("\n")[0]
      return input.split(", ")
    }
  }
        