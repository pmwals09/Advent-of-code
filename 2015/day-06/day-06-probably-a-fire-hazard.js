const fs = require("fs")

const data = fs.readFileSync("./day-06-data.txt", {encoding: "utf8"}).split("\n");
data.pop()

const parseLine = (line) => {
  let split = line.split(" ");
  let end = split.pop();
  end = { x: parseInt(end.split(",")[0]), y: parseInt(end.split(",")[1]) };
  split.pop();
  let start = split.pop();
  start = {x: parseInt(start.split(',')[0]), y: parseInt(start.split(',')[1])}
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

const turnOn = (parsedLine, lights) => {
  for(let y = parsedLine.start.y; y <= parsedLine.end.y; y++){
    for(let x = parsedLine.start.x; x <= parsedLine.end.x; x++){
      lights[`${x},${y}`] = lights[`${x},${y}`] ? lights[`${x},${y}`] + 1 : 1
    }
  }
};

const turnOff = (parsedLine, lights) => {
  for (let y = parsedLine.start.y; y <= parsedLine.end.y; y++) {
    for (let x = parsedLine.start.x; x <= parsedLine.end.x; x++) {
      const nextVal = lights[`${x},${y}`] - 1;
      lights[`${x},${y}`] = lights[`${x},${y}`] ? (nextVal < 0 ? 0 : nextVal) : 0
    }
  }
};

const toggle = (parsedLine, lights) => {
  for (let y = parsedLine.start.y; y <= parsedLine.end.y; y++) {
    for (let x = parsedLine.start.x; x <= parsedLine.end.x; x++) {
      lights[`${x},${y}`] = lights[`${x},${y}`] ? lights[`${x},${y}`] + 2 : 2;
    }
  }
};

const followDirections = directions => {
  let lights = {}
  directions.forEach(direction => {
    selectAction(parseLine(direction), lights)
  })
  return Object.values(lights).reduce((out, curr) => out + curr, 0)
}

console.log(
  followDirections(data)
)



