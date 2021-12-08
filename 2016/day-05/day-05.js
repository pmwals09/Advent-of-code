const fsp = require("fs/promises")
const path = require("path")
const crypto = require("crypto")

main()

async function main() {
  const input = await parseInput()

  console.log("Part one:", partOne())
  console.log("Part two:", partTwo())

  function partOne(){
    let i = 0
    let password = ""
    while(password.length < 8){
      const toHash = input + i.toString()
      const hash = crypto.createHash("md5").update(toHash).digest("hex")
      if(hash.slice(0, 5) === "00000"){
        password += hash[5]
      }
      i++
    }
  
    return password
  }

  function partTwo(){
    let i = 0
    let password = [null, null, null, null, null, null, null, null]
    while(password.filter(ea => ea !== null).length < 8){
      const toHash = input + i.toString()
      const hash = crypto.createHash("md5").update(toHash).digest("hex")
      if(hash.slice(0, 5) === "00000"){
        if(hash[5] < 8 && password[hash[5]] === null){
          password[hash[5]] = hash[6]
        }
      }
      i++
    }
  
    return password.join("")
  }

  async function parseInput() {
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-05-input.txt"), "utf-8")
    const input = rawInput.split("\n")
    return input
  }
}
