using System;
using System.IO;

class Program
{
    static void Main()
    {
      Input input = new Input("day-02-input.txt");
      Instruction[] instructions = input.ParseInput();
      Solution solution = new Solution(instructions);
      PartOneSubmarine submarine1 = new PartOneSubmarine();
      System.Console.WriteLine(solution.RunSolution(submarine1));
      PartTwoSubmarine submarine2 = new PartTwoSubmarine();
      System.Console.WriteLine(solution.RunSolution(submarine2));
    }
}

class Solution
{
  private readonly Instruction[] _instructions;
  public Solution(Instruction[] instructions)
  {
    _instructions = instructions;
  }

  public int RunSolution(Submarine submarine)
  {
    foreach (Instruction instruction in _instructions)
    {
        submarine.FollowInstruction(instruction);
    }
    return submarine.Depth * submarine.HorizontalDistance;
  }
}

class PartOneSubmarine : Submarine
{
  public PartOneSubmarine() : base()
  {
  }

  public override void FollowInstruction(Instruction instruction)
  {
    switch (instruction.Direction)
    {
      case "forward":
        HorizontalDistance += instruction.Amount;
        break;
      case "down":
        Depth += instruction.Amount;
        break;
      case "up":
        Depth -= instruction.Amount;
        break;
      default:
        break;
    }
  }

}

class PartTwoSubmarine : Submarine
{
  public int Aim {get; private set;}
  public PartTwoSubmarine() : base()
  {
    Aim = 0;
  }

  public override void FollowInstruction(Instruction instruction)
  {
    switch(instruction.Direction)
    {
      case "forward":
        HorizontalDistance += instruction.Amount;
        Depth += (Aim * instruction.Amount);
        break;
      case "up":
        Aim -= instruction.Amount;
        break;
      case "down":
        Aim += instruction.Amount;
        break;
      default:
        break;
    }
  }
}

class Submarine
{
  public int Depth { get; protected set; }
  public int HorizontalDistance { get; protected set; }
  public Submarine()
  {
    Depth = 0;
    HorizontalDistance = 0;
  }

  public virtual void FollowInstruction(Instruction instruction)
  {
    
  }
}

class Input
{
  public readonly string[] _lines;
  public Input(string filepath)
  {
    _lines = File.ReadAllLines(filepath);
  }

  public Instruction[] ParseInput()
  {
    Instruction[] instructions = new Instruction[_lines.Length];
    for (int i = 0; i < _lines.Length; i++)
    {
        instructions[i] = new Instruction(_lines[i]);
    }

    return instructions;
  }

}

class Instruction
{
  public string Direction;
  public int Amount;
  public Instruction(string line)
  {
    string[] parts = line.Split(" ");
    Direction = parts[0];
    Amount = int.Parse(parts[1]);
  }

  public override string ToString()
  {
    return $"{Direction} {Amount}";
  }
}