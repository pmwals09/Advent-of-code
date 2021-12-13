const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))
}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-12-input.txt"), "utf-8")
  const input = rawInput.split("\n").filter(Boolean)
  return input
}

class BfsNode {
  constructor(path, visited){
    this.path = path
    this.visited = visited
    this.nodeName = path[path.length - 1]
  }
}

class Graph {
  constructor(challengePart){
    this.adjacencyList = {}
    this.challengePart = challengePart;
  }

  _addNode(node){
    if(!this.adjacencyList[node]){
      this.adjacencyList[node] = []
    }
  }

  addEdge(node1, node2){
    this._addNode(node1)
    this._addNode(node2)
    if(!this.adjacencyList[node1].includes(node2)){
      this.adjacencyList[node1].push(node2)
    }
    if(!this.adjacencyList[node2].includes(node1)){
      this.adjacencyList[node2].push(node1)
    }
  }

  traverse(){
    const queue = [
      new BfsNode(["start"], {"start": [true]})
    ]
    const paths = []
    
    while(queue.length){
      const {path, visited, nodeName} = queue.pop()
      if(nodeName === "end"){
        paths.push(path)
        continue
      }
      for(const neighborName of this.adjacencyList[nodeName]){
        if(this._shouldVisit(neighborName, visited)){
          const newVisited = this._getNewVisited(visited, neighborName);
          const newPath = this._getNewPath(path, neighborName);
          queue.push(new BfsNode(newPath, newVisited))
        }
      }
    }

    return paths
  }

  _shouldVisit(neighborName, visited){
    if(neighborName.toLowerCase() === neighborName){
      if(this.challengePart === "ONE"){
        return this._handleShouldVisitPartOne(visited, neighborName);
      } else {
        return this._handleShouldVisitPartTwo(visited, neighborName);
      }
    }

    return true
  }

  _handleShouldVisitPartOne(visited, neighborName){
    if (visited[neighborName]) {
      return false;
    }

    return true
  }

  _handleShouldVisitPartTwo(visited, neighborName){
    const hasUsedRevisit = this._getHasUsedRevisit(visited);
    const maxedOnVisits = visited[neighborName] && hasUsedRevisit;
    if (maxedOnVisits || neighborName === "start") {
      return false;
    }

    return true
  }

  _getHasUsedRevisit(visited){
    return Object.keys(visited)
      .filter((key) => key.toLowerCase() === key)
      .map((key) => visited[key])
      .some((val) => val.length > 1);
  }

  _getNewVisited(visited, neighborName){
    const newVisited = JSON.parse(JSON.stringify(visited));
    if (newVisited[neighborName]) {
      newVisited[neighborName].push(true);
    } else {
      newVisited[neighborName] = [true];
    }

    return newVisited
  }

  _getNewPath(path, neighborName){
    const newPath = path.slice(0);
    newPath.push(neighborName);

    return newPath
  }
}
      
function partOne(input){
  const graph = buildGraph(input, "ONE")

  return graph.traverse().length
}

function partTwo(input){
  const graph = buildGraph(input, "TWO")
  
  return graph.traverse().length
}

function buildGraph(input, challengePart){
  const graph = new Graph(challengePart)
  for (const pair of input) {
    const [node1, node2] = pair.split("-");
    graph.addEdge(node1, node2);
  }

  return graph
}