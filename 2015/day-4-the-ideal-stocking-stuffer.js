const data = require("./data/day-4.json")
const crypto = require("crypto")

const md5hash = (string) => crypto.createHash('md5').update(string).digest('hex')
let i = 0
while(md5hash(data + i).slice(0,5) !== "00000"){
  i++
}
console.log(i)

while (md5hash(data + i).slice(0,6) !== "000000") {
  i++;
}
console.log(i);