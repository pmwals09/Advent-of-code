const fsp = require("fs/promises");
const path = require("path");

main();

async function main() {
  const lineStrings = await parseInput();
  console.log("Part one:", partOne());
  console.log("Part two:", partTwo());

  function partOne() {
    const lines = lineStrings
      .map((lineString) => parseLineString(lineString))
      .filter((line) => isHorizontal(line) || isVertical(line));
    const vents = buildVentCounts(lines);
    return tallyVents(vents);
  }

  function partTwo() {
    const lines = lineStrings.map((lineString) => parseLineString(lineString));
    const vents = buildVentCounts(lines);
    return tallyVents(vents);
  }

  function parseLineString(lineString) {
    const [start, end] = lineString.split(" -> ");
    const [startX, startY] = start.split(",");
    const [endX, endY] = end.split(",");
    return {
      start: {
        x: Number(startX),
        y: Number(startY),
      },
      end: {
        x: Number(endX),
        y: Number(endY),
      },
    };
  }

  function buildVentCounts(lines) {
    const vents = [[0]];
    for (const line of lines) {
      expandVents({ line, vents });
      countVents({ line, vents });
    }
    return vents;
  }

  function expandVents({ line, vents }) {
    if (!xInBounds({ line, vents })) {
      widen();
    }
    if (!yInBounds({ line, vents })) {
      heighten();
    }
    function widen() {
      while (!xInBounds({ line, vents })) {
        addCol();
      }

      function addCol() {
        for (const row of vents) {
          row.push(0);
        }
      }
    }
    function heighten() {
      while (!yInBounds({ line, vents })) {
        addRow();
      }

      function addRow() {
        const rowLen = vents[0]?.length || 1;
        const newRow = [];
        for (let i = 0; i < rowLen; i++) {
          newRow.push(0);
        }
        vents.push(newRow);
      }
    }
  }

  function xInBounds({ line, vents }) {
    return line.start.x + 1 <= vents[0].length && line.end.x + 1 <= vents[0].length;
  }

  function yInBounds({ line, vents }) {
    return line.start.y + 1 <= vents.length && line.end.y + 1 <= vents.length;
  }

  function countVents({ line, vents }) {
    if (isHorizontal(line)) {
      const row = line.start.y;
      if (isForward(line)) {
        for (let col = line.start.x; col <= line.end.x; col++) {
          vents[row][col] += 1;
        }
      } else {
        for (let i = line.end.x; i <= line.start.x; i++) {
          vents[row][i] += 1;
        }
      }
    } else if (isVertical(line)) {
      const col = line.start.x;
      if (isDown(line)) {
        for (let row = line.start.y; row <= line.end.y; row++) {
          vents[row][col] += 1;
        }
      } else {
        for (let row = line.end.y; row <= line.start.y; row++) {
          vents[row][col] += 1;
        }
      }
    } else {
      if (isForward(line) && isDown(line)) {
        for (let row = line.start.y, col = line.start.x; row <= line.end.y; row++, col++) {
          vents[row][col] += 1;
        }
      } else if (isForward(line)) {
        for (let row = line.start.y, col = line.start.x; row >= line.end.y; row--, col++) {
          vents[row][col] += 1;
        }
      } else if (isDown(line)) {
        for (let row = line.start.y, col = line.start.x; row <= line.end.y; row++, col--) {
          vents[row][col] += 1;
        }
      } else {
        for (let row = line.start.y, col = line.start.x; row >= line.end.y; row--, col--) {
          vents[row][col] += 1;
        }
      }
    }
  }
  function isHorizontal(line) {
    return line.start.y === line.end.y;
  }
  function isVertical(line) {
    return line.start.x === line.end.x;
  }
  function isForward(line) {
    return line.start.x < line.end.x;
  }
  function isDown(line) {
    return line.start.y < line.end.y;
  }

  function tallyVents(vents) {
    return vents.reduce(
      (ventsTotal, row) => ventsTotal + row.reduce((rowTotal, col) => rowTotal + (col >= 2 ? 1 : 0), 0),
      0
    );
  }

  async function parseInput() {
    const rawInput = await fsp.readFile(path.resolve(__dirname, "day-05-input.txt"), "utf-8");
    const input = rawInput.split("\n");
    return input;
  }
}
