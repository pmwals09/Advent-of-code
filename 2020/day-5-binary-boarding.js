const data = require("./data/day-5.json")

const parseLine = boardingPass => {
  const rowHalving = boardingPass.slice(0,7)
  const colHalving = boardingPass.slice(7)
  return [rowHalving, colHalving]
}

const parseRow = rowHalving => {
  let upperBound = 127
  let lowerBound = 0
  for(let i = 0; i < rowHalving.length; i++){
    halving = (upperBound + 1 - lowerBound) / 2
    rowHalving[i] === "F" ? upperBound -= halving : lowerBound += halving
  }
  return upperBound
}

const parseCol = colHalving => {
  let upperBound = 7
  let lowerBound = 0
  for(let i = 0; i < colHalving.length; i++){
    halving = (upperBound + 1 - lowerBound) / 2
    colHalving[i] === "L" ? (upperBound -= halving) : (lowerBound += halving);
  }
  return upperBound
}

const calcSeatId = (row, col) => {
  return row * 8 + col
}

console.log(data.reduce((out, curr) => {
  const [rowHalving, colHalving] = parseLine(curr)
  const [row, col] = [parseRow(rowHalving), parseCol(colHalving)]
  return Math.max(out, calcSeatId(row, col))
}, -Infinity))

const seatIds = data.map(ea => {
  const [rowHalving, colHalving] = parseLine(ea);
  const [row, col] = [parseRow(rowHalving), parseCol(colHalving)];
  return calcSeatId(row, col)
}).sort()

const findMissingSeatId = seatIds => {
  for (let i = Math.min(...seatIds); i <= Math.max(...seatIds); i++) {
    if (
      seatIds.includes(i + 1) &&
      seatIds.includes(i - 1) &&
      !seatIds.includes(i)
    )
      return i;
  }
}

console.log(findMissingSeatId(seatIds))