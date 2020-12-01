const data = require("./data/day-6.json");

const parseLine = (line) => {
  let split = line.split(" ");
  let end = split.pop();
  end = { x: end.split(",")[0], y: end.split(",")[1] };
  split.pop();
  let start = split.pop();
  start = {x: start.split(',')[0], y: start.split(',')[1]}
  const action = split.join(" ");
  return {
    action,
    start,
    end,
  };
};

const selectAction = (parsedLine, lightsSet) => {
  switch (parsedLine.action) {
    case "turn on":
      turnOn(parsedLine, lightsSet);
      break;
    case "turn off":
      turnOff(parsedLine, lightsSet);
      break;
    case "toggle":
      toggle(parsedLine, lightsSet);
      break;
    default:
      return "Something went wrong in selectAction";
  }
};

const turnOn = (parsedLine, lightsSet) => {
  for(let y = parsedLine.start.y; y <= parsedLine.end.y; y++){
    for(let x = parsedLine.start.x; x <= parsedLine.end.x; x++){
      lightsSet.add(`${x},${y}`)
    }
  }
};

const turnOff = (parsedLine, lightsSet) => {
  for (let y = parsedLine.start.y; y <= parsedLine.end.y; y++) {
    for (let x = parsedLine.start.x; x <= parsedLine.end.x; x++) {
      lightsSet.delete(`${x},${y}`)
    }
  }
};

const toggle = (parsedLine, lightsSet) => {
  for (let y = parsedLine.start.y; y <= parsedLine.end.y; y++) {
    for (let x = parsedLine.start.x; x <= parsedLine.end.x; x++) {
      lightsSet.has(`${x},${y}`)
        ? lightsSet.delete(`${x},${y}`)
        : lightsSet.add(`${x},${y}`);
    }
  }
};

const followDirections = directions => {
  let lightsSet = new Set()
  directions.forEach(direction => {
    selectAction(parseLine(direction), lightsSet)
  })
  return lightsSet.size
}

// console.log(parseLine(data[0]));
console.log(
  followDirections(data)
)

// 418954 -> too high