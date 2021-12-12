const fsp = require("fs/promises");
const path = require("path");

main();

async function main() {
  const input = await parseInput();
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))
  
}

async function parseInput() {
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-11-input.txt"), "utf-8");
  const input = rawInput
    .split("\n")
    .filter(Boolean)
    .map((line) => line.split(""));
  return input;
}

class Octopus {
  constructor(energy) {
    this.energy = energy;
    this.hasFlashed = false
  }

  incEnergy() {
    this.energy++;
  }

  flash() {
    this.energy = 0;
  }
}

class Point {
  constructor(x, y){
    this.x = x
    this.y = y
  }

  isEqual(point, y = null){
    if(point instanceof Point){
      return point.x === this.x && point.y === this.y
    } else if(Array.isArray(point)){
      return point[0] === this.x && point[1] === this.y
    } else if(typeof point === "number" && y && typeof y === "number"){
      return this.x === point && this.y === y
    }
  }
}

class Cell {
  constructor(val, point){
    this.val = val
    this.point = point
  }
}

class Grid {
  constructor(width = Infinity, height = Infinity, cellObj = Cell){
    this.width = width
    this.height = height
    this.cursor = new Point(0, 0)
    this.grid = []
    for(let i = 0; i < height; i++){
      this.grid.push([])
      for(let j = 0; j < width; j++){
        this.grid[i].push(new cellObj(0, new Point(j, i)))
      }
    }
  }

  adjacentCells(point = this.cursor){
    const adjacent = []
    for(let i = Math.max(point.y - 1, 0); i <= Math.min(point.y + 1, this.height - 1); i++){
      for(let j = Math.max(point.x - 1, 0); j <= Math.min(point.x + 1, this.width - 1); j++){
        if(!(point.isEqual(j, i))){
          adjacent.push(this.grid[i][j])
        }
      }
    }
    return adjacent
  }

  forEach(cb){
    for(let i = 0; i < this.height; i++){
      for(let j = 0; j < this.width; j++){
        cb(this.grid[i][j], i, j)
      }
    }
  }

  setVal(row, col, val){
    this.grid[row][col].val = val
  }

  currentCell() {
    return this.grid[this.cursor.y][this.cursor.x]
  }
}

function partOne(input){
  const grid = buildGrid(input)

  let flashes = 0
  for(let i = 0; i < 100; i++){
    flashes += runRound(grid)
  }

  return flashes
}

function partTwo(input){
  const grid = buildGrid(input)

  let i = 0
  while(true){
    i++
    const flashes = runRound(grid)

    if(flashes === 100){
      break
    }
  }

  return i
}

function buildGrid(input){
  const grid = new Grid(10, 10)
  input.forEach((row, rowI) => {
    row.forEach((col, colI) => {
      grid.setVal(rowI, colI, new Octopus(+col))
    })
  })

  return grid
}

function runRound(grid){
  incrementAllEnergy(grid)
  const flashes = handleFlashing(grid)
  clearHasFlashed(grid)
  return flashes
}

function incrementAllEnergy(grid){
  grid.forEach((cell) => {
    cell.val.incEnergy();
  });    
}

function handleFlashing(grid){
  let allFlashed = false
  while(!allFlashed){
    allFlashed = true
    grid.forEach((cell) => {
      if(cell.val.energy > 9 && !cell.val.hasFlashed){
        allFlashed = false
        handleFlash(cell, grid)
      }
    })
  }

  let flashes = 0
  grid.forEach((cell) => {
    if(cell.val.energy > 9){
      cell.val.flash()
      flashes++
    }
  })

  return flashes
}

function handleFlash(cell, grid){
  grid.cursor = cell.point
  const adjacentCells = grid.adjacentCells()
  adjacentCells.forEach(cell => cell.val.incEnergy())
  cell.val.hasFlashed = true
}

function clearHasFlashed(grid){
  grid.forEach(cell => cell.val.hasFlashed = false)
}