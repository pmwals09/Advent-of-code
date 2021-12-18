
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))
}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-17-input.txt"), "utf-8")
  const input = rawInput
    .split("\n")
    .filter(Boolean)[0]
    .split(": ")
    .filter((ea, i) => i !== 0)[0]
    .split(", ")
    .reduce((out, curr) => {
      let [coord, range] = curr.split("=")
      let [start, end] = range.split("..")
      out[coord] = {start: +start, end: +end}
      return out
    }, {})
  return input
}

function partOne(input){
  return sumRange(Math.abs(input.y.start) - 1);
}

function sumRange(inclTop) {
  return [...Array(inclTop + 1).keys()].reduce((out, curr) => out + curr)
}

function partTwo(input){
  const xLower = getXLower(input.x)
  const xUpper = input.x.end
  const yUpper = Math.abs(input.y.start) - 1;
  const yLower = input.y.start

  const options = []

  for(let i = xLower; i <= xUpper; i++){
    for(let j = yLower; j <= yUpper; j++){
      if(testVelocity([i, j], input)){
        options.push([i,j])
      }
    }
  }

  return options.length
}

function getXLower(inputX) {
  const { start, end } = inputX;
  let options = [];
  let potential = 1;
  while (sumRange(potential) <= end) {
    if (sumRange(potential) >= start) {
      options.push(potential);
    }
    potential++;
  }
  return Math.min(...options);
}

function testVelocity(velocity, input){
  const testV = [...velocity];
  let pos = [0, 0];
  while (couldStillLand(pos, input)) {
    if (maybeInLandingArea(pos, input)) {
      break;
    }
    pos[0] += testV[0];
    pos[1] += testV[1];
    if (testV[0] < 0) {
      testV[0]++;
    } else if (testV[0] > 0) {
      testV[0]--;
    }

    testV[1]--;
  }

  if (inLandingArea(pos, input)) {
    return true;
  }
  return false;
}

function couldStillLand(pos, input){
  return pos[0] <= input.x.end && pos[1] >= input.y.start;
}

function maybeInLandingArea(pos, input){
  return pos[0] >= input.x.start && pos[1] <= input.y.end;
}

function inLandingArea(pos, input){
  return pos[0] >= input.x.start && pos[0] <= input.x.end && pos[1] >= input.y.start && pos[1] <= input.y.end;
}