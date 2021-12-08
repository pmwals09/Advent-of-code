const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne().idSum)
  console.log("Part two:", partTwo())

  function partOne(){
    const real = []
    for(let room of input){
      if(validate(frequency(room.name.join("")), room.checksum)){
        real.push(room)
      }
    }
    return {
      rooms: real,
      idSum: real.reduce((out, curr) => out + curr.id, 0)
    }

    function frequency(string){
      return string.split("").reduce((out, curr) => {
        if(out[curr]){
          out[curr]++
          return out
        } else {
          return {
            ...out,
            [curr]: 1
          }
        }
      }, {})
    }
  
    function validate(frequencyObj, checksum){
      const expected = Object.keys(frequencyObj).sort((a, b) => {
        if(frequencyObj[a] > frequencyObj[b]){
          return -1
        } else if(frequencyObj[a] < frequencyObj[b]) {
          return 1
        } else {
          return a < b ? -1 : 1
        }
      }).slice(0, 5)
      return expected.join("") === checksum
    }
  }

  function partTwo(){
    const {rooms} = partOne()
    return rooms
      .map(room => {
        return {
          ...room,
          name: room.name.map(part => rotate(part, room.id)).join(" ")
        }
      })
      .find(room => room.name.toLowerCase().includes("northpole"))
      .id
      
    function rotate(string, num){
      let rotatedString = ""
      for(let i = 0; i < string.length; i++){
        rotatedString += String.fromCharCode(((string[i].charCodeAt(0) - 96 + num) % 26) + 96)
      }
      return rotatedString
    }
  }



  async function parseInput(){
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-04-input.txt"), "utf-8")
    const input = rawInput.split("\n").map(ea => {
      const split = ea.split("-")
      const idChecksum = split.pop()
      const [id, checksum] = idChecksum.split("[")
      return {
        name: split,
        id: Number(id),
        checksum: checksum.slice(0, -1)
      }
    })
    return input
  }
}
      