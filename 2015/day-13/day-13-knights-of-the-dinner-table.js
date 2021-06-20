const fs = require("fs")
const data = fs.readFileSync(__dirname+"/day-13-data.txt", "utf-8").split("\n")

class Table {
  constructor(data){
    this.adjacencyList = {}
    data.forEach((line) => {
      this.addRelationship(line);
    });
  }

  addRelationship(line){
    const {primary, happinessUnits, neighbor} = Table.parseLine(line)
    if(this.adjacencyList[primary]){
      this.adjacencyList[primary] = {...this.adjacencyList[primary], [neighbor]: happinessUnits}
    } else {
      this.adjacencyList[primary] = {[neighbor]: happinessUnits}
    }
  }

  static parseLine(line){
    const [primary, rest] = line.split(" would ")
    const [happinessTrend, neighbor] = rest.split(" happiness units by sitting next to ")
    const [direction, amount] = happinessTrend.split(" ")
    return {
      primary,
      happinessUnits: direction === "gain" ? +amount : +amount * -1,
      neighbor: neighbor.slice(0, neighbor.length - 1)
    }
  }

  buildAllSeatingArrangements(){
    const thisAdjacencyList = this.adjacencyList
    const allSeats = Object.keys(this.adjacencyList).map(guest => seatGuest(guest))
    return allSeats.flat(Object.keys(thisAdjacencyList).length - 2).flatMap(ea => ea)

    function seatGuest(guestName, seatedGuests = []){
      const newSeatedGuests = [...seatedGuests]
      newSeatedGuests.push(guestName)
      const remainingGuests = Object.keys(thisAdjacencyList).filter(
        guest => !newSeatedGuests.includes(guest)
      )
      if(remainingGuests.length === 0) return newSeatedGuests
      else return remainingGuests.map(guest => seatGuest(guest, newSeatedGuests))
    }
  }

  scoreSeatingArrangement(seatArr){
    const middleSeats = seatArr.map((guest, i) => {
      const leftGuest = this.adjacencyList[guest][seatArr[i - 1]]
      const rightGuest = this.adjacencyList[guest][seatArr[i + 1]];
      return {
        guest,
        leftScore: leftGuest,
        rightScore: rightGuest
      }
    })
    const leftMost = middleSeats[0]
    const rightMost = middleSeats[middleSeats.length - 1]
    leftMost.leftScore = this.adjacencyList[leftMost.guest][rightMost.guest]
    rightMost.rightScore = this.adjacencyList[rightMost.guest][leftMost.guest]

    return middleSeats.reduce((out, curr) => out + curr.leftScore + curr.rightScore, 0)
  }

  scoreTable(){
    const allArrangements = this.buildAllSeatingArrangements()
    return allArrangements.reduce((out, arrangement) => {
      return Math.max(out, this.scoreSeatingArrangement(arrangement))
    }, 0)
  }
}

const table = new Table(data)
console.log("Part 1: ",table.scoreTable())

const newTable = new Table(data)
Object.keys(newTable.adjacencyList).forEach(guest => {
  newTable.addRelationship(`You would gain 0 happiness units by sitting next to ${guest}.`)
  newTable.addRelationship(`${guest} would gain 0 happiness units by sitting next to You.`)
})
console.log("Part 2: ", newTable.scoreTable())
