const fsp = require("fs/promises");
const path = require("path");
const util = require("util");

main();

async function main() {
  const input = await parseInput();
  console.log("Part one:", partOne(input));
  console.log("Part two:", partTwo(input));
}

async function parseInput() {
  // const rawInput = await fsp.readFile(path.resolve(__dirname, "day-18-input.txt"), "utf-8");
  // const rawInput = "[1,1]\n[2,2]\n[3,3]\n[4,4]"
  const rawInput = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]"
  const input = rawInput.split("\n").filter(Boolean);
  return input;
}

function partOne(input) {
  input = "[[[[[9,8],1],2],3],4]";
  let depth = 0
  for(let i = 0; i < input.length; i++){
    if(input[i] === "["){
      depth++
    } else if(input[i] === "]"){
      depth--
    } else if(!isNaN(input[i])){
      if(depth >= 5){
        // explode
        // get the right and left numbers - they may be double-digit
        let left = ""
        let right = ""
        let j = i
        while(input[j] !== ","){
          left += input[j]
          j++
        }
        j++
        while(input[j] !== "]"){
          right += input[j]
          j++
        }
        
        // find the next number to the left and add the left digit to it
        let nextLeft = ""
        let nextLeftRightI = 0
        for(let k = i - 1; k >= 0; k--){
          if(!isNaN(input[k])){
            nextLeftRightI = k
          }
        }

        while(!isNaN(input[nextLeftRightI])){
          nextLeft = input[nextLeftRightI] + nextLeft
          nextLeftRightI--
        }

        if(nextLeft){
          // splice in the sum of the two numbers. May need to increment 
          // counters above if it results in > 10
        }

        // find the next number to the right and add the right digit to it

        // replace the exploded pair with a 0



        console.log("Should explode")
      } else if(!isNaN(input[i + 1])){
        console.log("Should split")
      }
    }
  }
  // const snailNums = input.map((num) => parseSnailNumber(num));
  // let result = snailNums[0]
  // for(let i = 1; i < snailNums.length; i++){
  //   result = addSnailNums(result, snailNums[i])
  // }
  // console.log(result)
}

function parseSnailNumber(strNum) {
  let num = new SnailNumber();
  let levelDeep = 0;
  strNum.split(",").forEach((numWBracket, i) => {
    if (numWBracket[0] === "[") {
      // count how many opening brackets we have for nesting
      let openBrackets = 0;
      for (let i = 0; i < numWBracket.length - 1; i++) {
        if (numWBracket[i] === "[") {
          openBrackets++;
        }
      }

      if (num.x) {
        num.y = new SnailNumber();
        prevNum = num;
        num = num.y;
        num.parent = prevNum;
        levelDeep++;
        num.depth = levelDeep;
        for (let i = 0; i < openBrackets - 1; i++) {
          num.x = new SnailNumber();
          prevNum = num;
          num = num.x;
          num.parent = prevNum;
          levelDeep++;
          num.depth = levelDeep;
        }

        num.x = Number(numWBracket.slice(openBrackets));
      } else {
        for (let i = 0; i < openBrackets - 1; i++) {
          num.x = new SnailNumber();
          prevNum = num;
          num = num.x;
          num.parent = prevNum;
          levelDeep++;
          num.depth = levelDeep;
        }

        num.x = Number(numWBracket.slice(openBrackets));
      }
    } else {
      let newYStr = "";
      for (let i = 0; i < numWBracket.length; i++) {
        if (!isNaN(numWBracket[i])) {
          newYStr += numWBracket[i];
        }
      }
      num.y = +newYStr;

      // get number of closing brackets
      let closeBrackets = 0;
      for (let i = 1; i < numWBracket.length; i++) {
        if (numWBracket[i] === "]") {
          closeBrackets++;
        }
      }

      while (closeBrackets && num.parent) {
        num = num.parent;
        closeBrackets--;
        levelDeep--;
      }
    }
  });

  return num;
}

function addSnailNums(num1, num2) {
  const newNum = new SnailNumber(num1, num2)
  newNum.x.parent = newNum
  newNum.y.parent = newNum
  newNum.traverse().forEach(num => num.depth++)
  return reduceSnailNum(newNum)
}

function reduceSnailNum(snailNum) {
  let nums = snailNum.traverse();

  console.log(
    "Should run:",
    nums.some((num) => num.shouldExplode() || num.shouldSplit())
  );
  while(nums.some(num => num.shouldExplode() || num.shouldSplit())){
    console.log(
      "Should run:",
      nums.some((num) => num.shouldExplode() || num.shouldSplit())
    );
    nums.forEach((num) => {
      if (num.shouldExplode()) {
        num.explode();
      }
      if (num.shouldSplit()) {
        num.split();
      }
    });

    nums = snailNum.traverse()
  }

  return snailNum
}

function partTwo(input) {
  // console.log(input)
}

class SnailNumber {
  constructor(x, y, parent = null, depth = 0) {
    this.x = x;
    this.y = y;
    this.parent = parent;
    this.depth = depth;
  }

  toString() {
    let string = "[";
    if (this.x instanceof SnailNumber) {
      string += this.x.toString();
    } else {
      string += `${this.x},`;
    }

    if (this.y instanceof SnailNumber) {
      string += this.y.toString();
    } else {
      string += `${this.y}],`;
    }
    return string;
  }

  add(num1, num2) {
    return [num1, num2];
  }

  reduce() {
    if (this._shouldExplode()) {
      this._explode();
    } else if (this._shouldSplit()) {
      this._split();
    } else {
    }
  }

  shouldExplode() {
    return this.depth >= 4;
  }

  explode() {
    // const origX = this.x;
    // const origY = this.y;
    // const origParent = this.parent;
    let target = this.parent
    let prev = this
    while(target){
      if(target.x && target.x !== prev){
        break
      } else {
        prev = target
        target = target.parent
      }
    }
    if(target){
      if(typeof target.x === "number"){
        target.x += this.x
      } else {
        while(typeof target.y !== "number"){
          target = target.y
        }
        target.y += this.x
      }
    }

    target = this.parent
    prev = this
    while(target){
      if(target.y && target.y !== prev){
        break
      } else {
        prev = target
        target = target.parent
      }
    }
    if(target){
      if(typeof target.y === "number"){
        target.y += this.y
      } else {
        while(typeof target.x !== "number"){
          target = target.x
        }
        target.x += this.y
      }
    }

    const isX = this.parent.x === this;
    if (isX) {
      this.parent.x = 0
    } else {
      this.parent.y = 0
    }
  }

  shouldSplit() {
    return [this.x, this.y].some((int) => !isNaN(int) && int >= 10);
    // If any regular number is 10 or greater, the leftmost such regular number splits.
  }

  split() {
    if (!isNaN(this.x) && this.x > 10) {
      console.log(this);
      const newX = Math.floor(this.x / 2);
      const newY = Math.ceil(this.x / 2);
      this.x = new SnailNumber(newX, newY, this, this.depth);
      console.log(this);
    }
    if(!isNaN(this.y) && this.y > 10) {
      console.log(this)
      const newX = Math.floor(this.y / 2)
      const newY = Math.ceil(this.y / 2)
      this.y = new SnailNumber(newX, newY, this, this.depth)
      console.log(this)
    }
    // To split a regular number, replace it with a pair; the left element of
    // the pair should be the regular number divided by two and rounded down,
    // while the right element of the pair should be the regular number divided
    // by two and rounded up. For example, 10 becomes [5,5], 11 becomes [5,6],
    // 12 becomes [6,6], and so on.
  }

  traverse(){
    let nodes = []
    innerTraverse(this)
    return nodes

    function innerTraverse(node){
      nodes.push(node);
      if (node.x instanceof SnailNumber) {
        innerTraverse(node.x);
      }
      if (node.y instanceof SnailNumber) {
        innerTraverse(node.y);
      }
    }
  }
}

/**
 * [2,[0,[9,[5,9]]]]
 * SnailNumber {
 *  x: 2,
 *  y: SnailNumber {
 *    x: 0,
 *    y: SnailNumber {
 *      x: 9,
 *      y: SnailNumber {
 *        x: 5,
 *        y: 9
 *      }
 *    }
 *  }
 * }
 * [[2,[1,8]],3]
 * SnailNumber {
 *  x: SnailNumber {
 *    x: 2,
 *    y: SnailNumber: {
 *      x: 1,
 *      y: 8
 *    }
 *  },
 *    y: 3
 * }
 * [[[[7,2],6],[[7,8],3]],[9,[[6,9],2]]]
 *
 * SnailNumber {
 *  x: SnailNumber {
 *    x: SnailNumber {
 *      x: SnailNumber {
 *        x: 7,
 *        y: 2
 *      },
 *      y: 6
 *    },
 *    y: SnailNumber {
 *      x: SnailNumber {
 *        x: 7,
 *        y: 8
 *      },
 *      y: 3
 *    },
 *  },
 *  y: SnailNumber {
 *    x: 9,
 *    y: SnailNumber {
 *      x: SnailNumber {
 *        x: 6,
 *        y: 9
 *      },
 *      y: 2
 *    }
 *  }
 * }
 */
