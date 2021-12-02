const https = require("https")
const fsp = require("fs/promises")
const fs = require("fs")
const path = require("path")
const {spawn} = require("child_process")

main()

async function main() {
  await parseEnv()

  const day = process.argv[2]
  const paddedDay = padLeft(day);
  const folderName = `day-${paddedDay}`;
  const folderPath = path.resolve(__dirname, folderName);

  await makePuzzleDirectory()
  writePuzzle();
  writeInput();
  createSolution();

  async function parseEnv() {
    const envPath = path.resolve(__dirname, "../.env");
    const envData = await fsp.readFile(envPath, "utf-8");
    const envPairs = envData.split("\n");
    for (const pair of envPairs) {
      const keyValDelimiter = pair.indexOf("=");
      const key = pair.substring(0, keyValDelimiter);
      const val = pair.substring(keyValDelimiter + 1);
      if (!process.env[key]) {
        process.env[key] = val;
      }
    }
  }

  async function makePuzzleDirectory() {
    if (!fs.existsSync(folderPath)) {
      await fsp.mkdir(folderPath);
    }
  }
  
  function writePuzzle(){
    // write file of puzzle
    const fileName = `day-${paddedDay}.txt`;
    const filePath = path.resolve(folderPath, fileName)
    if(!fs.existsSync(filePath)){
      const fileStream = fs.createWriteStream(filePath, {flags: "r+"})
      const puzzleReqOptions = {
        hostname: "adventofcode.com",
        path: `/2021/day/${day}`,
        port: 443,
        method: "GET"
      };
      const puzzleReq = https.request(puzzleReqOptions, res => {
        console.log(`puzzleReq statusCode: `, res.statusCode)
        res.on("error", e => console.error(e))
        res.on("data", d => fileStream.write(d))
      })
      
      puzzleReq.on("error", e => console.error(e))
      puzzleReq.end()
    }
  }

  async function writeInput(){
    const fileName = `day-${paddedDay}-input.txt`
    const filePath = path.resolve(folderPath, fileName)
    if(!fs.existsSync(filePath)){
      const fileStream = fs.createWriteStream(filePath)
      const inputReqOptions = {
        hostname: "adventofcode.com",
        path: `/2021/day/${day}/input`,
        port: 443,
        method: "GET",
        headers: {
          Cookie: [process.env.COOKIE]
        }
      };
      const inputReq = https.request(inputReqOptions, res => {
        console.log(`inputReq statusCode: `, res.statusCode);
        res.on("data", d => fileStream.write(d))
      })
      inputReq.on("error", e => console.error(e))
      inputReq.end()
    }
  }

  async function createSolution(){
    const fileName = `day-${paddedDay}.js`
    const filePath = path.resolve(folderPath, fileName)
    fsp.writeFile(
      filePath,
      `
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  console.log(await parseInput())

  async function parseInput(){
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-${paddedDay}-input.txt"), "utf-8")
    const input = rawInput.split("\\n")
    return input
  }
}
    `
    );
    const runBase = spawn(`node`, [`day-${paddedDay}.js`], {cwd: folderPath})
    runBase.stdout.on("data", (d) => process.stdout.write(d));
  }

  function padLeft(num){
    if(num < 10){
      return `0${num}`
    } else {
      return num.toString()
    }
  }
}