const fsp = require("fs/promises")
const path = require("path")

main()

async function main() {
  const input = await parseInput()

  console.log("Part one:", partOne())
  console.log("Part two:", partTwo())

  function partOne() {
    const keypad = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]

    let code = []
    let cursor = [1, 1]
    for (let line of input) {
      for (let direction of line) {
        const [row, col] = cursor
        switch (direction) {
          case "U":
            if (row > 0) cursor[0]--
            break
          case "R":
            if (col < 2) cursor[1]++
            break
          case "D":
            if (row < 2) cursor[0]++
            break
          case "L":
            if (col > 0) cursor[1]--
            break
          default:
            cursor[0] = row
            cursor[1] = col
        }
      }
      code.push(keypad[cursor[0]][cursor[1]])
    }

    return code.join("")
  }

  function partTwo() {
    const keypad = [
      [null, null, 1, null, null],
      [null, 2, 3, 4, null],
      [5, 6, 7, 8, 9],
      [null, "A", "B", "C", null],
      [null, null, "D", null, null],
    ]

    const code = []
    const current = [2, 0]
    for(let line of input){
      for(let direction of line){
        const [row, col] = current
        let nextRow = row
        let nextCol = col
        switch(direction){
          case "U":
            nextRow--
            if(nextRow >= 0 && keypad[nextRow][nextCol]) current[0]--
            break
          case "R":
            nextCol++
            if(nextCol <= 4 && keypad[nextRow][nextCol]) current[1]++
            break
          case "D":
            nextRow++
            if(nextRow <= 4 && keypad[nextRow][nextCol]) current[0]++
            break
          case "L":
            nextCol--
            if(nextRow >= 0 && keypad[nextRow][nextCol]) current[1]--
            break
          default:
            current[0] = row
            current[1] = col
        }
      }
      code.push(keypad[current[0]][current[1]])
    }

    return code.join("")
  }

  async function parseInput() {
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-02-input.txt"), "utf-8")
    // const rawInput = "ULL\nRRDDD\nLURDL\nUUUUD"
    const input = rawInput
      .split("\n")
      .filter((ea) => ea.length > 0)
      .map((row) => row.split(""))
    return input
  }
}
