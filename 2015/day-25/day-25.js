main()
  
function main(){
  const input = {
    row: 2947, column: 3029
  }
  const INIT = 20151125
  const loops = getOrdinal(input.row, input.column);
  
  let tally = INIT
  for(let i = 0; i < loops - 1; i++){
    tally = nextNum(tally)
    if(i % 10000 === 0) console.log(i, "of", loops, ":", tally)
  }

  console.log("Part one:", tally)

  function nextNum(num){
    return (num * 252533) % 33554393;
  }

  function getOrdinal(row, col){
    let num = 1
    for(let i = 0; i < row; i++){
      num += i
    }

    for(let i = 0; i < col - 1; i++){
      num += ((row + 1) + i)
    }

    return num
  }
}
      