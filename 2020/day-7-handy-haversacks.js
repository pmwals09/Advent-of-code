const data = require("./data/day-7.json")

/**
 * @typedef Child
 * @type {object}
 * @property {number} qty
 * @property {string} color
 */

/**
 * @typedef Bag
 * @type {object}
 * @property {string} color
 * @property {Child[]} children
 */

/**
 *
 * @param {string} line
 * @returns {Bag}
 */
const parseLine = (line) => {
  const [color, children] = line.split(" bags contain ")
  return {
    qty: 1,
    color,
    children: parseChildren(children),
  }
}

/**
 *
 * @param {string} children
 * @returns {Child[] | null}
 */
const parseChildren = (children) => {
  if (children.match(/\d+[a-zA-Z\s]+bags?/)) {
    return children
      .split(",")
      .map((child) => child.match(/(\d+)\s([a-zA-Z\s]+)\sbags?/))
      .map((parsedChild) => {
        return {
          qty: parseInt(parsedChild[1]),
          color: parsedChild[2],
        }
      })
  } else {
    return null
  }
}

/**
 *
 * @param {Bag[]} data
 * @param {Child} bag
 * @returns {Bag}
 */
const getChildsChildren = (data, bag) => {
  const newBag = data.find((parent) => parent.color === bag.color)
  if (newBag === undefined) {
    console.log(bag)
  }
  return newBag
}

/**
 *
 * @param {Bag[]} data
 * @param {string} color
 * @param {Bag} bag
 * @returns {boolean[]}
 */
const validateBag = (data, color, bag) => {
  if (!bag || !bag.children) return [false]
  else if (bag.children.some((child) => child.color === color)) return [true]
  else {
    return bag.children.map((child) =>
      validateBag(data, color, getChildsChildren(data, child))
    )
  }
}

/**
 *
 * @param {boolean[][]} results
 * @returns {boolean[]}
 */
const flattenResults = (results) => {
  if (!Array.isArray(results)) return results
  else {
    return results.reduce((out, curr) => {
      if (Array.isArray(curr)) return [...out, ...flattenResults(curr)]
      else return [...out, curr]
    }, [])
  }
}

const parsedData = data.map((line) => parseLine(line))

// console.log(
//   parsedData
//     .map((bag) => validateBag(parsedData, "shiny gold", bag))
//     .map((tree) => {
//       return flattenResults(tree)
//     })
//     .filter((results) => results.some((result) => result === true)).length
// )

/**
 *
 * @param {Bag[]} data
 * @param {Bag} topBag
 * @returns {number}
 */
const countInnerBags = (data, topBag) => {
  if (topBag.children) {
    return [
      ...topBag.children,
      topBag.children.map((child) =>{
        return countInnerBags(
          data,
          buildChildren(
            data,
            {...data.find((bag) => bag.color === child.color), qty: child.qty}
          )
        )
      }),
    ]
  }
}

// need to keep track of qty of each parent to multiply on the way down, i.e.:
/**
 * shiny gold
 *      |
 *      V
 * 5 wavy chartreuse
 *      |
 *      V
 * 5 muted blue bags, 1 bright crimson bag, 1 pale gray bag, 1 shiny silver bag
 * = 5 wavy chartreuse, 25 muted blue, 5 bright crimson, 5 pale gray, 5 shiny silver = 45 bags
 *
 *
 */

const buildChildren = (data, parent) => {
  if (parent.children) {
    return parent.children
      .map((child) => data.find((bag) => bag.color === child.color))
      .map((child) => {
        return { ...child, parent }
      })
  }
}
const targetBag = parsedData.find((bag) => bag.color === "shiny gold")
console.log(
  countInnerBags(parsedData, targetBag)
  // .filter(ea => ea !== undefined)
  // .reduce((out, curr) => out * curr.qty, 1)
)

// 121 too low
