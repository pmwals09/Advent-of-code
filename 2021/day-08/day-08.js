const fsp = require("fs/promises");
const path = require("path");

main();

async function main() {
  const input = await parseInput();

  console.log("Part one:", partOne());
  console.log("Part two:", partTwo());

  function partOne() {
    let count = 0;
    for (const line of input) {
      for (const outNum of line[1]) {
        if ([2, 3, 4, 7].some((num) => num === outNum.length)) {
          count++;
        }
      }
    }

    return count;
  }

  function partTwo() {
    const outputValues = [];
    for (const line of input) {
      const scrambleToDigitMap = mapScrambleToDigit(line[0]);
      outputValues.push(line[1].map(digit => scrambleToDigitMap[sortString(digit)]).join(""))
    }
    return outputValues.reduce((out, curr) => out + Number(curr), 0);
    
    function mapScrambleToDigit(scrambledDigits) {
      const segments = {}
      const one = scrambledDigits.find(ea => ea.length === 2)
      const four = scrambledDigits.find((ea) => ea.length === 4);
      const seven = scrambledDigits.find((ea) => ea.length === 3);
      const eight = scrambledDigits.find((ea) => ea.length === 7);

      const cf = one.split("")
      segments.a = seven.split("").filter(letter => !cf.includes(letter))[0]

      let fourAndSeven = new Set()
      for (let letter of [...four.split(""), ...seven.split("")]){
        fourAndSeven.add(letter)
      }
      fourAndSeven = Array.from(fourAndSeven)
      const nine = scrambledDigits.find(ea => ea.length === 6 && fourAndSeven.every(letter => ea.includes(letter)))
      segments.g = nine.split("").filter(letter => !four.includes(letter) && !seven.includes(letter))[0]

      const backwardC = [...cf, segments.a, segments.g];
      const three = scrambledDigits.find(ea => ea.length === 5 && backwardC.every(letter => ea.includes(letter)))
      segments.d = three.split("").filter(letter => !backwardC.includes(letter))[0]

      segments.b = four.split("").filter(letter => !three.includes(letter))[0]
      segments.e = eight.split("").filter(letter => !nine.includes(letter))[0]

      const six = scrambledDigits.find(ea => ea.length === 6 && ea.includes(segments.d) && ea.includes(segments.e))

      segments.c = eight.split("").filter(letter => !six.includes(letter))[0]
      segments.f = cf.filter(letter => letter !== segments.c)[0]

      return {
        [sortString(makeLetter("abcefg"))] : "0",
        [sortString(makeLetter("cf"))] : "1",
        [sortString(makeLetter("acdeg"))] : "2",
        [sortString(makeLetter("acdfg"))] : "3",
        [sortString(makeLetter("bcdf"))] : "4",
        [sortString(makeLetter("abdfg"))] : "5",
        [sortString(makeLetter("abdefg"))] : "6",
        [sortString(makeLetter("acf"))] : "7",
        [sortString(makeLetter("abcdefg"))] : "8",
        [sortString(makeLetter("abcdfg"))] : "9",
      }

      function makeLetter(str){
        return str.split("").map(letter => segments[letter]).join("")
      }
      
    }

    function sortString(str) {
      return str.split("").sort().join("");
    }
  }

  async function parseInput() {
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-08-input.txt"), "utf-8");
    const input = rawInput.split("\n").map((line) => line.split(" | ").map(ea => ea.split(" ")));
    return input;
  }
}