const https = require("https")
const fsp = require("fs/promises")
const fs = require("fs")
const path = require("path")
const {spawn} = require("child_process")
const readline = require("readline")

main()

async function main() {
  const { envPath, envPairs } = await parseEnv();
  const {day, paddedDay, year, onlyPuzzle} = await parseArgs()
  console.log(onlyPuzzle)
  const {folderName, folderPath} = await makePuzzleDirectory()
  if(onlyPuzzle){
    await writePuzzle();
    process.exit(0)
  }
  await writePuzzle();
  writeInput();
  createSolution();

  async function parseEnv() {
    const envPath = path.resolve(__dirname, ".env");
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

    return {
      envPath,
      envPairs
    }
  }

  async function parseArgs() {
    const [nodePath, selfPath, ...scriptArgs] = process.argv;

    const argObj = parseScriptArgs()

    const day = parseDay();
    const paddedDay = padLeft(day);
    const year = await parseYear();
    const onlyPuzzle = argObj["only-puzzle"]


    return {
      day,
      paddedDay,
      year,
      onlyPuzzle
    };

    function parseScriptArgs(){
      return scriptArgs.reduce((out, curr) => {
        const [key, val] = curr.split("=");
        return {
          ...out,
          [key.slice(2)]: val ? val : true,
        };
      }, {});
    }

    function parseDay() {
      class InvalidDayException extends Error {
        constructor() {
          super("You must include a day, either as the sole numerical argument or as '--day=<NUM>'");
          this.name = "InvalidDayError";
        }
      }
      let day
      if (!isNaN(scriptArgs[0]) && Number(scriptArgs[0]) < 26) {
        day = scriptArgs[0];
      } else if (argObj.day && !isNaN(argObj.day)) {
        day = argObj.day;
      } else {
        throw new InvalidDayException();
      }
      return day
    }

    async function parseYear() {
      class InvalidYearException extends Error {
        constructor() {
          super("You must include a valid year, either in the .env file or as '--year=<NUM>'");
          this.name = "InvalidDayError";
        }
      }
      let year
      if (argObj.year && !isNaN(argObj.year)) {
        year = argObj.year
        if(Number(year) !== Number(process.env.YEAR)){
          await updatePrompt()
        }
      } else if (process.env.YEAR) {
        year = process.env.YEAR
      } else {
        throw new InvalidYearException()
      }

      return year

      function updatePrompt(){
        const rl = readline.createInterface({
          input: process.stdin,
          output: process.stdout,
        });

        return new Promise((resolve, reject) => {
          rl.question("Would you like to update your .env with the new year? [y/n] > ", async (yOrN) => {
              if(!["y", "n"].includes(yOrN.toLowerCase())){
                rl.write("Invalid input - please input 'y' or 'n'")
                rl.close()
                reject()
                await updatePrompt()
              } else if(yOrN.toLowerCase() === "y") {
                const entry = `YEAR=${year}`
                if(!fs.existsSync(envPath)){
                  await fsp.writeFile(envPath, entry)
                } else {
                  const yearIndex = envPairs.findIndex(ea => ea.startsWith("YEAR"))
                  envPairs[yearIndex] = entry
                  await fsp.writeFile(envPath, envPairs.join("\n"))
                }
              }
              rl.close()
              resolve()
          })
        })

      }
    }
  }

  async function makePuzzleDirectory() {
    console.log("Create puzzle directory")
    const folderName = `day-${paddedDay}`;
    const folderPath = path.resolve(__dirname, year, folderName);
    if (!fs.existsSync(folderPath)) {
      console.log("Creating puzzle directory...")
      await fsp.mkdir(folderPath, {recursive: true});
      console.log("Puzzle directory created!")
    } else {
      console.log("Directory already exists")
    }
    return {folderName, folderPath}
  }
  
  function writePuzzle(){
    console.log("Get puzzle.")
    const fileName = `day-${paddedDay}.txt`;
    const filePath = path.resolve(folderPath, fileName)
    const fileStream = fs.createWriteStream(filePath, {flags: "w"})
    return new Promise((resolve, reject) => {
      const puzzleReqOptions = {
        hostname: "adventofcode.com",
        path: `/${year}/day/${day}`,
        port: 443,
        method: "GET",
        headers: {
          Cookie: [process.env.COOKIE],
        },
      };
      const puzzleReq = https.request(puzzleReqOptions, res => {
        console.log(`Requesting puzzle...received ${res.statusCode} response`)
        res.on("error", e => {
          console.error(e)
          reject(e)
        })
        res.on("data", d => {
          console.log("Writing response to puzzle file...")
          fileStream.write(d)
        })
        res.on("end", () => resolve())
      })
      
      puzzleReq.on("error", e => console.error(e))
      puzzleReq.end()
  
      console.log("Puzzle retrieved!")
    })
  }

  async function writeInput(){
    console.log("Get puzzle input")
    const fileName = `day-${paddedDay}-input.txt`
    const filePath = path.resolve(folderPath, fileName)
    const fileStream = fs.createWriteStream(filePath, {flags: "w"})
    const inputReqOptions = {
      hostname: "adventofcode.com",
      path: `/${year}/day/${day}/input`,
      port: 443,
      method: "GET",
      headers: {
        Cookie: [process.env.COOKIE]
      }
    };
    const inputReq = https.request(inputReqOptions, res => {
      console.log(`Requesting puzzle input...received ${res.statusCode} status code`);
      res.on("data", d => {
        console.log("Writing puzzle input response to file...")
        fileStream.write(d)
      })
    })
    inputReq.on("error", e => console.error(e))
    inputReq.end()
    console.log("Puzzle input retrieved!")
  }

  async function createSolution(){
    console.log("Create boilerplate solution file.")
    const fileName = `day-${paddedDay}.js`
    const filePath = path.resolve(folderPath, fileName)
    console.log("Writing boilerplate solution file...")
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
    console.log("Boilerplate solution file written! Testing input reading...")

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