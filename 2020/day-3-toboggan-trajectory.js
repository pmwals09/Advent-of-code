const data = require("./data/day-3.json")

const move = (currentPos, right = 3, down = 1) => {
  return [currentPos[0] + down, currentPos[1] + right]
}

const addTerrain = (terrain, template) => {
  let newTerrain = terrain.map((row, i) => {
    return row.concat(template[i])
  })
  return newTerrain
}

const isTree = (terrain, position) => {
  if (terrain[position[0]][position[1]] === "#") {
    return true
  } else {
    return false
  }
}

const traverseHill = (
  terrain = data,
  currentPos = [0, 0],
  numTrees = 0,
  right = 3,
  down = 1,
  template = data
) => {
  if (currentPos[0] >= terrain.length) return numTrees
  else {
    let newTrees = numTrees
    if (isTree(terrain, currentPos)) {
      newTrees = ++numTrees
    }
    let newTerrain = terrain
    if (currentPos[1] >= (terrain[0].length - (right))) {
      newTerrain = addTerrain(terrain, template)
    }
    let newPos = move(currentPos, right, down)
    return traverseHill(newTerrain, newPos, newTrees, right, down, template)
  }
}

console.log(traverseHill())

const slopes = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2],
]

console.log(
  slopes.reduce((out, curr) => {
    return out * traverseHill(data, [0, 0], 0, curr[0], curr[1], data)
  }, 1)
)
