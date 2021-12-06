
  const fsp = require("fs/promises")
  const path = require("path")
  
  main()  
  async function main(){
    let program = await parseInput()
    console.log("Part one:", runProgram())
    console.log("Part two:", runProgram(1))

    function runProgram(a = 0){
      const registers = {
        a: a,
        b: 0,
      };
      const instructions = {
        hlf: (r, _o) => (registers[r] = Math.floor(registers[r] / 2)),
        tpl: (r, _o) => (registers[r] *= 3),
        inc: (r, _o) => registers[r]++,
        jmp: (_r, o) => (i += Number(o) - 1),
        jie: (r, o) => (i += (registers[r] % 2 === 0 ? Number(o) : 1) - 1),
        jio: (r, o) => (i += (registers[r] === 1 ? Number(o) : 1) - 1),
      };
      let i = 0;

      do {
        const { verb, register, offset } = parseInstruction(program[i]);
        instructions[verb](register, offset);
        i++;
      } while (i < program.length);

      return registers.b;
    }

    function parseInstruction(instruction) {
      const [verb, ...rest] = instruction.split(" ");
      let register, offset;
      switch (verb) {
        case "jmp":
          offset = rest[0];
          break;
        case "jie":
        case "jio":
          register = rest[0][0];
          offset = rest[1];
          break;
        default:
          register = rest[0];
      }

      return {
        verb,
        register,
        offset,
      };
    }    

    async function parseInput(){
      const rawInput = await fsp.readFile(path.resolve(__dirname, "day-23-input.txt"), "utf-8")
      // const rawInput = "inc a\njio a, +2\ntpl a\ninc a"
      const input = rawInput.split("\n")
      return input
    }
  }
        