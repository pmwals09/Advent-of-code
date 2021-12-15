
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))
}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-15-input.txt"), "utf-8")
  const input = rawInput.split("\n").filter(Boolean).map(line => line.split("").map(Number))
  return input
}

function partOne(input){
  return dijkstra(input)
}

function partTwo(input){
  const newInput = []
  buildDown(input, newInput)
  buildAcross(input, newInput)
  
  return dijkstra(newInput)
}

function buildAcross(input, newInput){
  for(let i = 0; i < 4; i++){
    newInput.forEach((row, rowI) => {
      const originalRow = row.slice(0, input.length)
      const newRow = originalRow.map(num => {
        const newNum = num + i + 1
        return newNum > 9 ? newNum % 9 : newNum
      })
      newInput[rowI] ? newInput[rowI].push(...newRow) : newInput.push(newRow)
    })
  }
}

function buildDown(input, newInput){
  for(let i = 0; i < 5; i++){
    input.forEach(row => {
      const newRow = row.map(num => {
        const newNum = num + i
        return newNum > 9 ? newNum % 9 : newNum
      })
      newInput.push(newRow)
    })
  }
}

function dijkstra(input){
  input = mapToNodes(input)

  let unvisited = input.flatMap(node => node)
  let current = initNode(input)

  while(!currentIsLastNode(current, input)){
    visitNeighbors()
    logProgress(input, unvisited)
  }
  process.stdout.write("\n")
  return current.minDanger.val
  
  function visitNeighbors(){
    const neighbors = getNeighbors(input, current)
    
    for(const neighbor of neighbors){
      neighbor.setMinDanger(current)
    }
    
    current.visited = true
    unvisited = unvisited.filter(node => node !== current)
    
    current = getSmallestUnvisited(unvisited)
  }
}

function mapToNodes(input){
  return input.map((line, rowI) => {
    return line.map((node, colI) => {
      return new Node(node, new Point(rowI, colI))
    })
  })
}

class Node {
  /**
   * 
   * @param {number} val 
   * @param {Point} point 
   */
  constructor(val, point){
    this.val = val
    this.minDanger = {
      val: Infinity,
      via: null
    }
    this.point = point
    this.visited = false
  }

  /**
   * 
   * @param {Node} potentialParent 
   */
  setMinDanger(potentialParent){
    const newVal = potentialParent.minDanger.val + this.val
    if(newVal < this.minDanger.val){
      this.minDanger = {
        val: newVal,
        via: potentialParent
      }
    }
  }
}

class Point {
  /**
   * 
   * @param {number} row 
   * @param {number} col 
   */
  constructor(row, col){
    this.row = row
    this.col = col
  }
}

function initNode(input){
  const start = input[0][0]
  if(start.minDanger.val === Infinity){
    start.minDanger.val = 0
  }
  
  return start
}

function getNeighbors(input, current){
  return [
    input[current.point.row - 1]?.[current.point.col],
    input[current.point.row + 1]?.[current.point.col],
    input[current.point.row]?.[current.point.col + 1],
    input[current.point.row]?.[current.point.col - 1],
  ].filter(node => !!node && !node.visited)
}

function getSmallestUnvisited(unvisited){
  let smallestUnvisited = null
    for(const node of unvisited){
      if(!node.visited){
        if(!smallestUnvisited || node.minDanger.val < smallestUnvisited.minDanger.val){
            smallestUnvisited = node
        }
      }
    }
  return smallestUnvisited
}

function currentIsLastNode(current, input){
  return current.point.row === input.length - 1 && current.point.col === input[0].length - 1
}

function logProgress(input, unvisited){
  process.stdout.cursorTo(0)
    process.stdout.write((() => {
      const inputLen = input.reduce((total, row) => total + row.length, 0)
      return `${((inputLen - unvisited.length) / inputLen * 100).toFixed(2)}% visited`
    })())
    process.stdout.clearLine(1)
}