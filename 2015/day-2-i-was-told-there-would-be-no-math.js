const data = require("./data/day-2.json")

const parseLine = line => {
  return {
    l: line.split("x")[0],
    w: line.split("x")[1],
    h: line.split("x")[2],
  }
}

const transformValues = (obj, cb) => {
  Object.keys(obj).forEach(key => {
    obj[key] = cb(obj[key])
  })
  return obj
}

const calculateSides = parsedLine => {
  return [
    parsedLine.l * parsedLine.w,
    parsedLine.w * parsedLine.h,
    parsedLine.h * parsedLine.l
  ]
}

const findSmallestSide = sidesArr => {
  return sidesArr.reduce((out, curr) => out < curr ? out : curr)
}

const wrappingPaperPerBox = line => {
  let boxDimensions = transformValues(parseLine(line), parseInt)
  let boxSideAreas = calculateSides(boxDimensions)
  let smallestSide = findSmallestSide(boxSideAreas)
  return boxSideAreas.reduce((out, curr) => out + (2*curr), 0) + smallestSide
}

console.log(data.reduce((out, curr) => out + wrappingPaperPerBox(curr), 0))

