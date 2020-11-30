const data = require("./data/day-3.json")

const chooseMove = move => {
  switch (move) {
    case "^":
      moveUp(historyArr, currentPosition)
      break;
    case ">":
      moveRight(historyArr, currentPosition)
      break;
    case "v":
      moveDown(historyArr, currentPosition)
      break;
    case "<":
      moveLeft(historyArr, currentPosition)
      break;
    default:
      return "Somethign went wrong..."
  }
}

const moveUp = (historyArr = [1], currentPosition) => {
  if(currentPosition[0] === 0){
    const newRow = historyArr[0].map(ea => ea = 0)
    historyArr.push(newRow)
  } else {
    // move up a row
  }
  // increment where you land
  historyArr[currentPosition[0]][currentPosition[1]] = 1
}

const moveRight = (historyArr, currentPosition) => {
  // if you're at the end of the row, add a column
  // else move up a column
  // increment where you land
}

const moveDown = (historyArr, currentPosition) => {
  // if you're in the last row, add a row
  // else move down a row
  // increment where you land
}

const moveLeft = (historyArr, currentPosition) => {
  // if you're in the first col, add a col
  // else move down a col
  // increment where you land
}

const addRow = () => {

}

const addCol = () => {

}

const incrementPosition = () => {
  
}