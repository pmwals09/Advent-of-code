
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input).numPixels)
  console.log("Part two:")
  console.log(showDisplay(partOne(input).display));
}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-08-input.txt"), "utf-8")
  const input = rawInput.split("\n").filter(Boolean)
  return input
}

function partOne(input){
  const screenDimensions = {
    width: 50,
    height: 6
  }
  let display = generateDisplay(screenDimensions)
  input.forEach(line => {
    if(line.split(" ")[0] === "rect"){
      handleRect(display, line)
    } else {
      handleRotate(display, line)
    }
  })

  return {
    display,
    numPixels: tallyPixels(display)
  }
}

function generateDisplay(screenDimensions){
  const {height, width} = screenDimensions
  return Array.from(new Array(height), () => {
    return Array.from(new Array(width), () => ".");
  });
}

function handleRect(display, line){
  const [_, dimensions] = line.split(" ")
  const [width, height] = dimensions.split("x")
  for(let row = 0; row < height; row++){
    for(let col = 0; col < width; col++){
      display[row][col] = "#"
    }
  }

}

function handleRotate(display, line) {
  let [axis, rest] = line.split("=")
  axis = axis[axis.length -1]
  let [index, amount] = rest.split("by").map(ea => ea.trim())
  if(axis === "x"){
    handleRotateColumn(display, index, amount)
  } else {
    handleRotateRow(display, index, amount)
  }
}

function handleRotateColumn(display, index, amount) {
  for(let i = 0; i < amount; i++){
    let temp = display[display.length - 1][index]
    for(let row = display.length - 1; row >= 0; row--){
      if(row === 0){
        display[row][index] = temp
      } else {
        display[row][index] = display[row - 1][index];
      }
    }
  }
}

function handleRotateRow(display, index, amount) {
  for(let i = 0; i < amount; i++){
    let temp = display[index][display[0].length - 1]
    for(let col = display[0].length - 1; col >= 0; col--){
      if(col === 0){
        display[index][col] = temp
      } else {
        display[index][col] = display[index][col - 1]
      }
    }
  }
}

function tallyPixels(display){
  return display.reduce((total, row) => {
    return total + row.reduce((rowTotal, cell) => {
      return cell === "#" ? rowTotal + 1 : rowTotal
    }, 0)
  },0);
}

function showDisplay(display){
  console.log(
    display.map((row) => row.map(cell => cell === "." ? " " : cell).join("")).join("\n")
  );
}