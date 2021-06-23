const fs = require("fs");
const data = fs
  .readFileSync(__dirname + "/day-18-data.txt", "utf-8")
  .split("\n")
  .map((row) => row.split("").map((ea) => (ea === "#" ? 1 : 0)));

let board = data;
for (let i = 0; i < 100; i++) {
  board = updateBoard(board, false);
}
console.log("Part 1: ", tallyBoard(board));

let partTwoBoard = data;
partTwoBoard = turnOnCorners(partTwoBoard);
for(let i = 0; i < 100; i++){
  partTwoBoard = updateBoard(partTwoBoard, true)
}
console.log("Part 2: ", tallyBoard(partTwoBoard));

function updateBoard(board, isPartTwo) {
  let nextBoard = copyBoard(board);
  for (let row = 0; row < nextBoard.length; row++) {
    for (let col = 0; col < nextBoard[row].length; col++) {
      const adjacentSum = sumAllAdjacentCells({ row, col, board });
      if (board[row][col] === 1) {
        if (adjacentSum !== 2 && adjacentSum !== 3) {
          nextBoard[row][col] = 0;
        }
      } else {
        if (adjacentSum === 3) {
          nextBoard[row][col] = 1;
        }
      }
      if (isPartTwo) {
        nextBoard = turnOnCorners(nextBoard);
      }
    }
  }
  return nextBoard;
}

function sumAllAdjacentCells({ row, col, board }) {
  let topRow = getTopRow({ row, col, board });
  let midRow = getMidRow({ row, col, board });
  let bottomRow = getBottomRow({ row, col, board });
  const allAdjacentCells = [...topRow, ...midRow, ...bottomRow];
  return allAdjacentCells.reduce((out, curr) => out + curr);
}

function getTopRow({ row, col, board }) {
  if (row === 0) {
    return [0, 0, 0];
  } else {
    if (col === 0) {
      return [0, ...board[row - 1].slice(0, 2)];
    } else if (col === board[row].length - 1) {
      return [...board[row - 1].slice(-2), 0];
    } else {
      return board[row - 1].slice(col - 1, col + 2);
    }
  }
}

function getMidRow({ row, col, board }) {
  if (col === 0) {
    return [0, board[row][col + 1]];
  } else if (col === board[row].length - 1) {
    return [board[row][col - 1], 0];
  } else {
    return [board[row][col - 1], board[row][col + 1]];
  }
}

function getBottomRow({ row, col, board }) {
  if (row === board.length - 1) {
    return [0, 0, 0];
  } else {
    if (col === 0) {
      return [0, ...board[row + 1].slice(0, 2)];
    } else if (col === board[row].length - 1) {
      return [...board[row + 1].slice(-2), 0];
    } else {
      return board[row + 1].slice(col - 1, col + 2);
    }
  }
}

function turnOnCorners(board) {
  const boardCopy = copyBoard(board);
  boardCopy[0][0] = 1;
  boardCopy[0][boardCopy[0].length - 1] = 1;
  boardCopy[boardCopy.length - 1][0] = 1;
  boardCopy[boardCopy.length - 1][boardCopy[0].length - 1] = 1;
  return boardCopy;
}

function copyBoard(board) {
  return [...board.map((ea) => [...ea])];
}

function tallyBoard(board) {
  return board.reduce((total, row) => {
    return (
      total +
      row.reduce((rowTotal, cell) => {
        return rowTotal + cell;
      }, 0)
    );
  }, 0);
}
