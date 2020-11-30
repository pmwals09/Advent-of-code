const data = require("./data/day-3.json");

const transformValues = (obj, cb) => {
  Object.keys(obj).forEach(key => {
    obj[key] = cb(obj[key])
  })
  return obj
}
const parseInput = (dataPoint) => {
  return {
    claimNumber: dataPoint.split("#")[1].split("@")[0].trim(),
    xMargin: dataPoint.split("@")[1].split(",")[0],
    yMargin: dataPoint.split(",")[1].split(":")[0],
    width: dataPoint.split(":")[1].split("x")[0].trim(),
    height: dataPoint.split("x")[1].trim()
  };
};

const parsedData = data.map(claim => transformValues(parseInput(claim), parseInt))

console.log(parsedData)