const fs = require("fs")
const data = fs.readFileSync("./day-07-data.txt", {encoding: "utf8"}).split("\n");

function executeNode({circuit, targetNode}) {
  targetNode.sources = targetNode.sources.map(sourceKey => {
    const parsedSourceKey = parseInt(sourceKey, 2)
    return isNaN(parsedSourceKey) ? executeCircuit({targetNode: circuit[sourceKey], circuit}) : sourceKey
  })
  switch (targetNode.gateType) {
    case "AND":
      return and16(...targetNode.sources);
    case "OR":
      return or16(...targetNode.sources);
    case "LSHIFT":
      return lshift(...targetNode.sources, parseInt(targetNode.gateArgs[0]))
    case "RSHIFT":
      return rshift(...targetNode.sources, parseInt(targetNode.gateArgs[0]));
    case "NOT":
      return not(...targetNode.sources)
    default:
      console.log("unhandled gate type:", targetNode.gateType)
  }
}

function convertTo16String(num) {
  let string = num.toString(2);
  let leftPad = 16 - string.length;
  if (leftPad <= 0) {
    return string;
  } else {
    return "0".repeat(leftPad) + string;
  }
}

function and16(binString1, binString2) {
  let result = "";
  for (let i = 0; i < binString1.length; i++) {
    if (binString1[i] === "1" && binString2[i] === "1") {
      result += "1";
    } else {
      result += "0";
    }
  }
  return result;
}

function or16(binString1, binString2) {
  let result = "";
  for (let i = 0; i < binString1.length; i++) {
    if (binString1[i] === "1" || binString2[i] === "1") {
      result += "1";
    } else {
      result += "0";
    }
  }
  return result;
}

function rshift(binString, numShift) {
  return "0".repeat(numShift) + binString.slice(0, binString.length - numShift);
}

function lshift(num, numShift) {
  return num.slice(numShift) + "0".repeat(numShift);
}

function not(binString) {
  let result = "";
  for (let i = 0; i < binString.length; i++) {
    if (binString[i] === "0") {
      result += "1";
    } else {
      result += "0";
    }
  }
  return result;
}

function followInstructions(instructions) {
  const circuit = {};
  instructions.forEach((instruction) => parseLine(instruction, circuit));
  function parseLine(line, circuit) {
    const [sourceInstruction, destination] = line.split(" -> ");
    const instructionParts = sourceInstruction.split(" ");
    let gate, sources;
    if (instructionParts.length === 1) {
      const value = instructionParts[0]
      const parsedValue = parseInt(value)
      circuit[destination] = { value: isNaN(parsedValue) ? value : convertTo16String(parsedValue) };
    } else if (instructionParts.length === 2) {
      circuit[destination] = {
        gateType: "NOT",
        sources: [instructionParts[1]],
      }
    } else {
      const gateType = instructionParts[1];
      if (gateType.includes("SHIFT")) {
        circuit[destination] = {
          gateType,
          sources: [instructionParts[0]],
          gateArgs: [parseInt(instructionParts[2])],
        }
      } else {
        const source1 = instructionParts[0]
        const parsedSource1 = parseInt(source1)
        const source2 = instructionParts[2]
        const parsedSource2 = parseInt(source2)
        circuit[destination] = {
          gateType,
          sources: [
            isNaN(parsedSource1) ? source1 : convertTo16String(parsedSource1),
            isNaN(parsedSource2) ? source2 : convertTo16String(parsedSource2),
          ],
        }
      }
    }
  }
  return circuit
}

function executeCircuit({targetNode, circuit}) {
  const targetNodeIntValue = parseInt(targetNode.value, 2);
  if (!!targetNode.value && (!!targetNodeIntValue || targetNodeIntValue === 0)) {
    return targetNode.value;
  } else if (!!targetNode.value && isNaN(targetNodeIntValue)) {
    return executeCircuit({targetNode: circuit[targetNode.value], circuit});
  } else {
    return executeNode({circuit, targetNode});
  }
}

const partOneCircuit = followInstructions(data);
const partOneBinary = executeCircuit({ targetNode: partOneCircuit.a, circuit: partOneCircuit });
console.log(`Part 1: ${partOneBinary} -> ${parseInt(partOneBinary, 2)}`)
const partTwoCircuit = followInstructions(data);
partTwoCircuit.b.value = convertTo16String(16076);
const partTwoBinary = executeCircuit({ targetNode: partTwoCircuit.a, circuit: partTwoCircuit });
console.log(`Part 2: ${partTwoBinary} -> ${parseInt(partTwoBinary, 2)}`)
