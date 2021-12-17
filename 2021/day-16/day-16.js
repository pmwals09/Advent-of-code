
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const input = await parseInput()
  console.log("Part one:", partOne(input))
  console.log("Part two:", partTwo(input))
}

async function parseInput(){
  const rawInput = await fsp.readFile(path.resolve(__dirname, "day-16-input.txt"), "utf-8")
  const input = rawInput.split("\n").filter(Boolean)[0]
  return input
}

function partOne(input){
  input = convertInput(input);
  const tree = parsePacket(input);
  return tallyVersions(tree)
}

function partTwo(input) {
  input = convertInput(input);
  const tree = parsePacket(input);
  return tree[0].value
}

function convertInput(input){
  let newString = ""
  for(const char of input){
    newString += hexToBin(char)
  }

  return newString
}

function hexToBin(char){
  return parseInt(char, 16).toString(2).padStart(4, "0")
}

function parsePacket(packet, numPackets = null){
  const packets = []
  let numPacketsParsed = 0
  while (shouldContinueParsing(packet, numPacketsParsed, numPackets)) {
    const version = getVersion(packet);
    const type = getType(packet);
    if (type === "100") {
      packet = handleValuePacket(packet, packets, version, type);
    } else {
      packet = handleOperatorPacket(packet, packets, version, type);
    }
    numPacketsParsed++;
  }
  return packets
}

function shouldContinueParsing(packet, numPacketsParsed, numPackets) {
  if (numPackets) {
    return numPackets !== numPacketsParsed
  } else {
    return !packet.match(/^0+$/) && packet.length;
  }
}

function getVersion(packet){
  return packet.slice(0, 3);
}

function getType(packet){
  return packet.slice(3, 6);
}

function handleValuePacket(packet, packets, version, type){
  const vals = [];
  let newVal = "";
  let valStart = 6;
  const PACKET_LENGTH = 5;
  while (!newVal.startsWith("0")) {
    newVal = packet.slice(valStart, valStart + PACKET_LENGTH);
    vals.push(newVal.slice(1));
    valStart += PACKET_LENGTH;
  }
  packets.push(
    new Packet({
      version,
      type,
      value: parseInt(vals.join(""), 2),
      packetLength: valStart
    })
  );
  return packet.slice(valStart);
}

const operators = {
  "000": (subPackets) => {
    return subPackets.reduce((out, curr) => out + curr.value, 0);
  },
  "001": (subPackets) => {
    return subPackets.reduce((out, curr) => out * curr.value, 1);
  },
  "010": (subPackets) => {
    return Math.min(...subPackets.map((ea) => ea.value));
  },
  "011": (subPackets) => {
    return Math.max(...subPackets.map((ea) => ea.value));
  },
  101: (subPackets) => {
    return subPackets[0].value > subPackets[1].value ? 1 : 0;
  },
  110: (subPackets) => {
    return subPackets[0].value < subPackets[1].value ? 1 : 0;
  },
  111: (subPackets) => {
    return subPackets[0].value === subPackets[1].value ? 1 : 0;
  },
};

function handleOperatorPacket(packet, packets, version, type){
  const lengthType = packet[6];
  let subPackets = []
  if (lengthType === "1") {
    subPackets = handleNumSubPackets(packet);
  } else {
    subPackets = handleLenSubPackets(packet);
  }
  const packetLength =
    subPackets.reduce((out, curr) => out + curr.packetLength, 0) +
    version.length +
    type.length +
    1 +
    getSubPacketBitsLen(lengthType);

  packets.push(
    new Packet({
      version,
      type,
      children: subPackets,
      packetLength,
      value: operators[type](subPackets)
    })
  );
  return packet.slice(packetLength);
}

function handleNumSubPackets(packet){
  const subPacketLenBits = getSubPacketBitsLen("1");
  const subPacketsStart = 7 + subPacketLenBits
  const subPacketNum = parseInt(packet.slice(7, 7 + subPacketLenBits), 2)
  const subPackets = parsePacket(packet.slice(subPacketsStart), subPacketNum)
  return subPackets
}

function handleLenSubPackets(packet){
  const subPacketLenBits = getSubPacketBitsLen("0");
  const subPacketsStart = 7 + subPacketLenBits;
  const subPacketLen = parseInt(packet.slice(7, subPacketsStart), 2);
  const subPackets = parsePacket(packet.slice(subPacketsStart, subPacketsStart + subPacketLen));
  return subPackets
}

function getSubPacketBitsLen(lengthType){
  if(lengthType === "1"){
    return 11
  } else {
    return 15
  }
}

function tallyVersions(tree){
  const versions = [tree[0].version]
  parseChildren(tree[0])

  function parseChildren(tree){
    if(tree.children){
      tree.children.forEach(child => {
        versions.push(child.version)
        parseChildren(child)
      })
    }
  }
  return sumBinaries(versions)
}

function sumBinaries(versions) {
  return versions
    .map((version) => parseInt(version, 2))
    .reduce((out, curr) => out + curr);
}

class Packet {
  constructor({version, type, value = null, children = [], packetLength}){
    this.version = version
    this.type = type
    this.value = value
    this.children = children
    this.packetLength = packetLength
  }
}