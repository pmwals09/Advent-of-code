const data = require("./data/day-3.json")

const chooseMove = (move, historyArr, currentPosition) => {
  switch (move) {
    case "^":
      return moveUp(historyArr, currentPosition)
    case ">":
      return moveRight(historyArr, currentPosition)
    case "v":
      return moveDown(historyArr, currentPosition)
    case "<":
      return moveLeft(historyArr, currentPosition)
    default:
      return "Something went wrong in chooseMove..."
  }
}

const incrementCurrentPosition = (historyArr, currentPosition) => {
  historyArr[currentPosition[0]][currentPosition[1]]++
}

const moveUp = (historyArr = [[1]], currentPosition = [0,0]) => {
  if (currentPosition[0] === 0) {
    addRow(historyArr, "top")
  } else {
    currentPosition[0]--
  }
  incrementCurrentPosition(historyArr, currentPosition)
  return historyArr
}

const moveRight = (historyArr = [[1]], currentPosition = [0,0]) => {
  if(currentPosition[1] === historyArr[0].length - 1){
    addCol(historyArr, 'right')
  }
  currentPosition[1]++
  incrementCurrentPosition(historyArr, currentPosition)
  return historyArr
}

const moveDown = (historyArr = [[1]], currentPosition = [0,0]) => {
  if (currentPosition[0] === historyArr.length - 1) {
    addRow(historyArr, "bottom")
  }
  currentPosition[0]++
  incrementCurrentPosition(historyArr, currentPosition)
  return historyArr
}

const moveLeft = (historyArr = [[1]], currentPosition = [0,0]) => {
  if(currentPosition[1] === 0) {
    addCol(historyArr, 'left')
  }
  incrementCurrentPosition(historyArr, currentPosition)
  return historyArr
}

const addRow = (historyArr, whereToAdd) => {
  const newRow = historyArr[0].map((ea) => (ea = 0))
  switch (whereToAdd) {
    case "top":
      historyArr.unshift(newRow)
      break
    case "bottom":
      historyArr.push(newRow)
      break
    default:
      return "something went wrong..."
  }
}

const addCol = (historyArr, whereToAdd) => {
  switch (whereToAdd) {
    case "left":
      historyArr.map((ea) => ea.unshift(0))
      break
    case "right":
      historyArr.map((ea) => ea.push(0))
      break
    default:
      return "something went wrong..."
  }
}

const followDirections = directionString => {
  let historyArr = [[1]]
  currentPosition = [0,0]
  directionString.split('').forEach(move => {
    historyArr = chooseMove(move, historyArr, currentPosition)
  });
  return historyArr
}

console.log(followDirections(data).map(row => row.filter(house => house !== 0)).reduce((out, curr) => out + curr.length, 0))
