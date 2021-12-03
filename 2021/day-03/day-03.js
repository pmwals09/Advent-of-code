
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one: ", partOne())
  console.log("Part two: ", partTwo())

  function partOne(){
    const freq = generateFrequency(input);

    let gamma = "";
    let epsilon = "";
    freq.forEach((digitObj) => {
      if (digitObj["0"] > digitObj["1"]) {
        gamma += "0";
        epsilon += "1";
      } else {
        epsilon += "0";
        gamma += "1";
      }
    });

    const gammaDigit = parseInt(gamma, 2);
    const epsilonDigit = parseInt(epsilon, 2);

    return gammaDigit * epsilonDigit;
  }

  function partTwo(){
    const genBin = progressiveFilter("0", "1")
    const scrubBin = progressiveFilter("1", "0")

    const genDigit = parseInt(genBin, 2);
    const scrubDigit = parseInt(scrubBin, 2);

    return genDigit * scrubDigit;

    function progressiveFilter(passNum, failNum) {
      let newArr = input;
      let digit = 0;
      while (newArr.length > 1) {
        const freq = generateFrequency(newArr);
        const digitCount = freq[digit];
        newArr = newArr.filter((num) => num[digit] === (digitCount["0"] > digitCount["1"] ? passNum : failNum));
        digit++;
      }

      return newArr[0];
    }
  }

  function generateFrequency(input){
    return input.reduce((out, curr) => {
      const digits = curr.split("");
      if (out.length > 0) {
        const newOut = out.map((digitObj, digitIndex) => {
          return { ...digitObj, [digits[digitIndex]]: digitObj[digits[digitIndex]] + 1 };
        });
        return newOut;
      } else {
        return digits
          .map((_) => {
            return { "0": 0, "1": 0 };
          })
          .map((digitObj, digitIndex) => {
            return { ...digitObj, [digits[digitIndex]]: 1 };
          });
      }
    }, []);
  }

  async function parseInput(){
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-03-input.txt"), "utf-8")
    const input = rawInput.split("\n")
    return input
  }
}
