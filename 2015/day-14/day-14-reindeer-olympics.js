const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-14-data.txt", "utf-8").split("\n");
const DURATION = 2503;

function parseLine(line) {
  const lineRegEx = /(\w+) can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds./;
  const [_, reindeerName, flyingSpeed, flyingDuration, restDuration, ...rest] = line.match(lineRegEx);
  return {
    reindeerName,
    flyingSpeed: +flyingSpeed,
    flyingDuration: +flyingDuration,
    restDuration: +restDuration,
  };
}

class Reindeer {
  constructor(reindeerData) {
    const { reindeerName, flyingSpeed, flyingDuration, restDuration } = reindeerData;
    this.name = reindeerName;
    this.flyingSpeed = flyingSpeed;
    this.flyingDuration = flyingDuration;
    this.restDuration = restDuration;
    this.score = 0;
  }

  get cycleTime() {
    return this.flyingDuration + this.restDuration;
  }

  getDistance(time) {
    const numberOfCycles = Math.floor(time / this.cycleTime);
    const timeOverLastCycle = time - numberOfCycles * this.cycleTime;

    const fullCycleDistance = numberOfCycles * this.flyingDuration * this.flyingSpeed;
    const partialCycleDistance = Math.min(timeOverLastCycle, this.flyingDuration) * this.flyingSpeed;

    return fullCycleDistance + partialCycleDistance;
  }
}

const allReindeer = data.map((ea) => new Reindeer(parseLine(ea)));

for (let i = 1; i <= DURATION; i++) {
  const distances = allReindeer.sort((a, b) => b.getDistance(i) - a.getDistance(i));
  const tiedForLead = distances.filter((reindeer) => reindeer.getDistance(i) === distances[0].getDistance(i));
  tiedForLead.forEach((ea) => ea.score++);
}

console.log(
  "Part 1: ",
  allReindeer.sort((a, b) => b.getDistance(DURATION) - a.getDistance(DURATION))[0].getDistance(DURATION)
);
console.log("Part 2: ", allReindeer.sort((a, b) => b.score - a.score)[0]);
