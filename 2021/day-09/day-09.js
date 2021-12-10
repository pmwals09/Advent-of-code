
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()

  console.log("Part one:", partOne(input).reduce((out, curr) => out + (curr.val + 1), 0))
  console.log("Part two:", partTwo(input))
}

async function parseInput() {
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-09-input.txt"), "utf-8");
  const input = rawInput.split("\n");
  return input;
}

function partOne(input) {
  const localMins = [];
  for (let row = 0; row < input.length; row++) {
    for (let col = 0; col < input[0].length; col++) {
      const val = +input[row][col];
      const neighbors = getNeighbors({ row, col, input });

      if (isLocalMin({ val, neighbors })) {
        localMins.push({ val, row, col });
      }
    }
  }

  return localMins;
}

function getNeighbors({ row, col, input }) {
  const neighbors = [];
  neighbors.push({ row: row + 1, col, val: +input[row + 1]?.[col] });
  neighbors.push({ row: row - 1, col, val: +input[row - 1]?.[col] });
  neighbors.push({ row, col: col + 1, val: +input[row]?.[col + 1] });
  neighbors.push({ row, col: col - 1, val: +input[row]?.[col - 1] });
  return neighbors.filter((point) => !isNaN(point.val));
}

function isLocalMin({ val, neighbors }) {
  return (
    Math.min(val, ...neighbors.map((neighbor) => neighbor.val)) === val &&
    neighbors.filter((ea) => ea.val === val).length === 0
  );
}

function partTwo(input) {
const localMins = partOne(input);
const basins = localMins
  .map((minPoint) => makeBasin(minPoint, input))
  .map((ea) => flatten(ea))
  .map((ea) => dedupe(ea));
return largestProduct(basins)
}

function makeBasin(point, input, visited = []) {
  if (point.val === 9) return visited;
  visited.push(point);

  const neighbors = getNeighbors({...point, input});
  const newNeighbors = neighbors.filter((neighbor) => hasVisited({ visited, neighbor }));
  return newNeighbors.map((neighbor) => makeBasin(neighbor, input, visited));
}

function hasVisited({ visited, neighbor }) {
  return !visited.some((vPoint) => areSamePoints(vPoint, neighbor));
}

function flatten(items) {
  const flat = [];

  items.forEach((item) => {
    if (Array.isArray(item)) {
      flat.push(...flatten(item));
    } else {
      flat.push(item);
    }
  });

  return flat;
}

function dedupe(basinPoints) {
  const uniques = [];
  basinPoints.forEach((point) => {
    if (!uniques.some((uniquePoint) => areSamePoints(point, uniquePoint))) {
      uniques.push(point);
    }
  });
  return uniques;
}

function largestProduct(basins){
  return basins
    .map((ea) => ea.length)
    .sort((a, b) => b - a)
    .slice(0, 3)
    .reduce((out, curr) => out * curr);
}

function areSamePoints(pointA, pointB){
  return pointA.row === pointB.row && pointA.col === pointB.col;
}