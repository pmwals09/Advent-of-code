const data = require("./data/day-1.json")

const checkTwoDates = (data) => {
  let nums = []
  for (let i = 0; i < data.length; i++) {
    let num = parseInt(data[i])
    if (data.includes((2020 - num).toString())) {
      nums = [num, (2020 - num)]
    }
  }
  return nums[0] * nums[1]
}

console.log(checkTwoDates(data))

const checkThreeDates = data => {
  let nums = []
  for(let i = 0; i < data.length-2; i++) {
    for(let j = i + 1; j < data.length -1; j++ ){
      let num1 = parseInt(data[i])
      let num2 = parseInt(data[j])
      if(data.includes((2020 - num1 - num2).toString())){
        nums = [num1, num2, 2020 - num1 - num2]
      }
    }
  }
  return nums
}

console.log(checkThreeDates(data).reduce((out, curr) => out * curr))