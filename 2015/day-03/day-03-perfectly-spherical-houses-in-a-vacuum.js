const fs = require("fs")

const data = fs.readFileSync("./day-03-data.txt", { encoding: "utf8" })

const chooseMove = (move, historySet, currentPosition) => {
  switch (move) {
    case "^":
      return moveUp(historySet, currentPosition);
    case ">":
      return moveRight(historySet, currentPosition);
    case "v":
      return moveDown(historySet, currentPosition);
    case "<":
      return moveLeft(historySet, currentPosition);
    default:
      return "Something went wrong in chooseMove...";
  }
};

const moveUp = (historySet = [[1]], currentPosition = [0, 0]) => {
  currentPosition[1]++
  historySet.add(currentPosition.toString())
};

const moveRight = (historySet = [[1]], currentPosition = [0, 0]) => {
  currentPosition[0]++
  historySet.add(currentPosition.toString())
};

const moveDown = (historySet = [[1]], currentPosition = [0, 0]) => {
  currentPosition[1]--
  historySet.add(currentPosition.toString())
};

const moveLeft = (historySet = [[1]], currentPosition = [0, 0]) => {
  currentPosition[0]--
  historySet.add(currentPosition.toString())
};

const followDirections = (directionString) => {
  let historySet = new Set();
  historySet.add('0,0')
  currentPosition = [0, 0];
  directionString.split("").forEach((move) => {
    chooseMove(move, historySet, currentPosition);
  });
  return historySet;
};

console.log(
  followDirections(data).size
);

const followDirectionsPair = (directionString) => {
  let historySet = new Set();
  historySet.add('0,0')
  let santaPosition = [0, 0];
  let roboPosition = [0, 0];
  directionString.split("").forEach((move, i) => {
    if (i % 2 === 0) {
      chooseMove(move, historySet, santaPosition);
    } else {
      chooseMove(move, historySet, roboPosition);
    }
  });
  return historySet.size;
};

console.log(followDirectionsPair(data))
