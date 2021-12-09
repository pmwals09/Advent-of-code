
  const fsp = require("fs/promises")
  const path = require("path")
  
  main()
  
  async function main(){
    const input = await parseInput()

    console.log("Part one:", partOne().reduce((out, curr) => out + (curr.val + 1), 0))
    console.log("Part two:", partTwo())

    function partOne(){
      const localMins = []
      for(let row = 0; row < input.length; row++){
        for(let col = 0; col < input[0].length; col++){
          const val = +input[row][col]
          const neighbors = []
          if(row === 0){
            neighbors.push(+input[row + 1][col])
          } else if(row === input.length - 1){
            neighbors.push(+input[row - 1][col])
          } else {
            neighbors.push(+input[row + 1][col])
            neighbors.push(+input[row - 1][col])
          }

          if(col === 0){
            neighbors.push(+input[row][col + 1])
          } else if (col === input[0].length - 1){
            neighbors.push(+input[row][col - 1])
          } else {
            neighbors.push(+input[row][col + 1])
            neighbors.push(+input[row][col - 1])
          }

          if(Math.min(val, ...neighbors) === val && neighbors.filter(ea => ea === val).length === 0){
            localMins.push({val, row, col})
          }
        }
      }

      return localMins
    }

    function partTwo(){
      const localMins = partOne()
      console.log(localMins)

      function getBasin(localMin){
        // look up and recurse
        // look down and recurse
        // look left and recurse
        // look right and recurse
        // return if encounter a 9
        // return if out of bounds
      }
    }

  
    async function parseInput(){
      const rawInput = await fsp.readFile(path.resolve(__dirname, "day-09-input.txt"), "utf-8")
      // const rawInput = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
      const input = rawInput.split("\n")
      return input
    }
  }
        