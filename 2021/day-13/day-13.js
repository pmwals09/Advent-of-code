const fsp = require("fs/promises")
const path = require("path")

main()

async function main() {
  const input = await parseInput()
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))
}

async function parseInput() {
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-13-input.txt"), "utf-8")
  const input = rawInput
    .split("\n")
    .filter(Boolean)
    .reduce((out, curr) => buildInput(out, curr), { holes: [], folds: [] })

  return input
}

function buildInput(out, curr){
    if (curr.startsWith("fold")) {
      handleFoldParse(out, curr)
    } else {
      handleHoleParse(out, curr)
    }

    return out
}

function handleFoldParse(out, curr){
  let [axis, num] = curr.split("=")
  axis = axis[axis.length - 1]
  out.folds.push({ axis, num: +num })
}

function handleHoleParse(out, curr){
  const [x, y] = curr.split(",")
  out.holes.push({ x: +x, y: +y })
}

class Chart {
  constructor(points) {
    this.chart = [[]]

    if (points) {
      points.forEach((point, i) => {
        const { x, y } = point
        while (this.chart.length < y + 1) {
          this.addRow()
        }
        while (this.chart[0].length < x + 1) {
          this.addCol()
        }
        this.chart[y][x] = "#"
      })
    }
  }

  addRow() {
    const newRow = []
    for (let i = 0; i < this.chart[0].length; i++) {
      newRow.push(".")
    }
    this.chart.push(newRow)
  }

  addCol() {
    for (const row of this.chart) {
      row.push(".")
    }
  }

  foldRow(foldLine) {
    const receivingStartRow = foldLine - (this.chart.length - 1 - foldLine)
    for(let i = receivingStartRow; i < foldLine; i++){
      const line = this.chart.pop()
      for(let j = 0; j < line.length; j++){
        if(line[j] === "#"){
          this.chart[i][j] = "#"
        }
      }
    }
    this.chart.pop()
  }

  foldCol(foldLine) {
    const receivingStartCol = foldLine - (this.chart[0].length - 1 - foldLine)
    for(const row of this.chart){
      for(let i = receivingStartCol; i < foldLine; i++){
        const item = row.pop()
        if(item === "#"){
          row[i] = item
        }
      }
      row.pop()
    }
  }
}

function partOne(input) {
  const chart = new Chart(input.holes)
  const fold = input.folds[0]
  performFold(fold, chart)
  return chart.chart.reduce((total, row) => total + row.reduce((colTotal, col) => (col === "#" ? colTotal + 1 : colTotal), 0), 0)
}

function partTwo(input) {
  const chart = new Chart(input.holes)
  for(const fold of input.folds){
    performFold(fold, chart)
  }
  return `\n${chart.chart.map(row => row.map(replaceDotWithSpace).join("")).join("\n")}`
}

function performFold(fold, chart){
  const {axis, num: foldLine} = fold
    if(axis === "x"){
      chart.foldCol(foldLine)
    } else {
      chart.foldRow(foldLine)
    }
}

function replaceDotWithSpace(item){
  return item === "." ? " " : item
}
