const fsp = require("fs/promises");
const path = require("path");

main();

async function main() {
  const lineStrings = await parseInput();

  class Line {
    constructor({ x1, y1, x2, y2 }) {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }
    static parseLine(lineString) {
      const [start, end] = lineString.split(" -> ");
      const [startX, startY] = start.split(",");
      const [endX, endY] = end.split(",");
      return {
        x1: Number(startX),
        y1: Number(startY),
        x2: Number(endX),
        y2: Number(endY),
      };
    }

    isHorizontal() {
      return this.y1 === this.y2;
    }

    isVertical() {
      return this.x1 === this.x2;
    }
  }

  class Cursor {
    constructor(line) {
      this.x = line.x1;
      this.y = line.y1;
      this.xDir = line.x1 < line.x2 ? 1 : line.x1 === line.x2 ? 0 : -1;
      this.yDir = line.y1 < line.y2 ? 1 : line.y1 === line.y2 ? 0 : -1;
    }

    next() {
      this.x += this.xDir;
      this.y += this.yDir;
    }
  }

  class VentMap {
    constructor() {
      this.ventMap = [[0]];
    }

    markVents(line) {
      const cursor = new Cursor(line);
      while (cursor.x !== line.x2 || cursor.y !== line.y2) {
        this.expandMap(cursor).markVent(cursor)
        cursor.next();
      }
      this.expandMap(cursor).markVent(cursor)
    }

    expandMap(cursor){
      while (cursor.x >= this.ventMap[0].length) {
        this.addCol();
      }
      while (cursor.y >= this.ventMap.length) {
        this.addRow();
      }

      return this
    }

    markVent(cursor){
      this.ventMap[cursor.y][cursor.x]++

      return this
    }

    addRow() {
      const rowLen = this.ventMap[0]?.length || 1;
      const newRow = [];
      for (let i = 0; i < rowLen; i++) {
        newRow.push(0);
      }
      this.ventMap.push(newRow);
    }

    addCol() {
      for (const row of this.ventMap) {
        row.push(0);
      }
    }

    countVents() {
      return this.ventMap.reduce(
        (ventTotal, row) => ventTotal + row.reduce((rowTotal, col) => rowTotal + (col >= 2 ? 1 : 0), 0),
        0
      );
    }
  }

  console.log("Part one:", partOne());
  console.log("Part two:", partTwo());

  function partOne() {
    const lines = lineStrings
      .map((lineString) => new Line(Line.parseLine(lineString)))
      .filter((line) => line.isHorizontal() || line.isVertical());
    const ventMap = new VentMap()
    for(const line of lines){
      ventMap.markVents(line)
    }
    return ventMap.countVents();
  }

  function partTwo() {
    const lines = lineStrings.map((lineString) => new Line(Line.parseLine(lineString)));
    const ventMap = new VentMap()
    for(const line of lines){
      ventMap.markVents(line)
    }
    return ventMap.countVents();
  }

  async function parseInput() {
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-05-input.txt"), "utf-8");
    const input = rawInput.split("\n");
    return input;
  }
}
