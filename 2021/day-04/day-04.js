
const fsp = require("fs/promises")
const path = require("path")

main()

class Board {
  constructor(string){
    this.board = string.split("\n").map(row => row.split(" ")).map(row => row.filter(num => num !== ""))
  }

  hasWon(nums){
    const that = this
    if(checkRows() || checkColumns()){
      return true
    }
    return false

    function checkRows(){
      return that.board.some(row => row.every(num => nums.includes(num)))
    }
    function checkColumns(){
      for(let colIdx = 0; colIdx < that.board[0].length; colIdx++){
        if(that.board.every(row => nums.includes(row[colIdx]))){
          return true
        }
      }
      return false
    }
  }

  scoreBoard(nums){
    const uncalledNums = this.board
      .reduce((out, row) => ([...out, ...row]))
      .filter(num => !nums.includes(num))
    const uncalledSum = uncalledNums.reduce((out, num) => (out + Number(num)), 0)
    return uncalledSum * Number(nums[nums.length - 1])
  }
}

async function main(){
  const [numbers, ...boards] = await parseInput()
  let parsedBoards = boards.map(ea => new Board(ea))

  console.log("Part one:", partOne())
  console.log("Part two:", partTwo())

  function partOne(){
    let winningBoard
    let lastNum = 0
    for(let i = 0; i < numbers.length; i++){
      winningBoard = parsedBoards.find(board => board.hasWon(numbers.slice(0, i)))
      if(winningBoard){
        lastNum = i
        break
      }
    }
  
    return winningBoard.scoreBoard(numbers.slice(0, lastNum))
  }

  function partTwo(){
    let winningBoards = [];
    let lastNum = 0;
    for (let i = 0; i < numbers.length; i++) {
      for(const board of parsedBoards){
        if(board.hasWon(numbers.slice(0, i))){
          lastNum = i
          winningBoards.push(board)
        }
      }
      parsedBoards = parsedBoards.filter(board => !winningBoards.includes(board))
    }
    const winningBoard = winningBoards[winningBoards.length - 1];
    return winningBoard.scoreBoard(numbers.slice(0, lastNum));
  }

  async function parseInput(){
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-04-input.txt"), "utf-8")
    const [numbers, ...boardStrings] = rawInput.split("\n\n")
    return [numbers.split(","), ...boardStrings];
  }
}