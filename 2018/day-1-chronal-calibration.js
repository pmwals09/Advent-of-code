const day1Data = require("./data/day-1.json");

// console.log(
//   day1Data.reduce((out, curr) => {
//     if (curr[0] === "+") {
//       return out + parseInt(curr.slice(1));
//     } else {
//       return out - parseInt(curr.slice(1));
//     }
//   }, 0)
// );

let freqsHit = [0]
let currentFreq = 0
let currentFreqIndex = 0
noOldFreq = true
while(noOldFreq){
  freq = day1Data[currentFreqIndex]
  currentFreq += freq[0] === "+" ? parseInt(freq.slice(1)) : -parseInt(freq.slice(1))
  if(freqsHit.includes(currentFreq)){
    noOldFreq = false
    console.log(currentFreq)
  } else {
    freqsHit.push(currentFreq)
  }
  if(currentFreqIndex === day1Data.length - 1){
    currentFreqIndex = 0
  } else {
    currentFreqIndex++
  }
}