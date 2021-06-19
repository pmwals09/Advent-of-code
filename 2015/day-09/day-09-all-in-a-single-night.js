const fs = require("fs");
const data = fs.readFileSync(__dirname + "/day-09-data.txt", "utf-8").split("\n");
// const data = fs.readFileSync(__dirname + "/sampleData.txt", "utf-8").split("\n");

function parseLine(line) {
  const [locations, distance] = line.split(" = ");
  const [from, to] = locations.split(" to ");
  return {
    from,
    to,
    distance,
  };
}

class Graph {
  constructor() {
    this.adjacencyList = {};
  }

  addVertex(vertexName) {
    if (!this.adjacencyList[vertexName]) this.adjacencyList[vertexName] = [];
  }

  addEdge(vertex1, vertex2, weight) {
    if (!this.adjacencyList[vertex1]) this.addVertex(vertex1);
    if (!this.adjacencyList[vertex2]) this.addVertex(vertex2);

    this.adjacencyList[vertex1].push({ node: vertex2, weight: +weight });
    this.adjacencyList[vertex2].push({ node: vertex1, weight: +weight });
  }

  allPaths() {
    const thisAdjacencyList = this.adjacencyList;
    const allPaths = Object.keys(this.adjacencyList).map((location) => fromNode(location));
    return allPaths.flat(6).flatMap((ea) => ea);

    function fromNode(startingNodeName, visited = []) {
      const newVisited = [...visited];
      newVisited.push(startingNodeName);
      const remainingNeighbors = Object.keys(thisAdjacencyList).filter(
        (location) => !newVisited.includes(location)
      );
      if (remainingNeighbors.length === 0) return newVisited;
      else return remainingNeighbors.map((neighbor) => fromNode(neighbor, newVisited));
    }
  }

  calculatePathDistance(pathArr) {
    return pathArr.reduce((out, curr, i) => {
      if (i === 0) {
        return 0;
      } else {
        const distanceFromPrevious = this.adjacencyList[pathArr[i - 1]].find(
          (neighbor) => neighbor.node === curr
        ).weight;
        return out + distanceFromPrevious;
      }
    }, 0);
  }
}

const locationGraph = new Graph();
data.forEach((line) => {
  const parsedLine = parseLine(line);
  locationGraph.addEdge(parsedLine.from, parsedLine.to, parsedLine.distance);
});

const allPaths = locationGraph.allPaths();

console.log(Math.min(...allPaths.map((ea) => locationGraph.calculatePathDistance(ea))));
console.log(Math.max(...allPaths.map((ea) => locationGraph.calculatePathDistance(ea))));
