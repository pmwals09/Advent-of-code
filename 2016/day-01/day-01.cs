using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;

class Program
{
  static void Main()
  {
    string input = File.ReadAllLines("day-01-input.txt")[0];
    PartOne partOne = new PartOne(input);
    partOne.Main();

    PartTwo partTwo = new PartTwo(input);
    partTwo.Main();
  }
}

class PartOne : PuzzlePart
{
  public PartOne(string input) : base(input){}

  public void Main()
  {
    foreach (Instruction instruction in this._instructions)
    {
      this._cursor.FollowInstruction(instruction);
    }
    int totalDistance = this._cursor.Pos.DistanceFromOrigin;
    Console.WriteLine($"Part One: {totalDistance}");
  }

}

class PartTwo : PuzzlePart
{
  public PartTwo(string input) : base(input){}

  public void Main()
  {
    bool hasCrossed = false;
    Point crossPoint = null;
    foreach(Instruction instruction in this._instructions)
    {
      this._cursor.FollowInstruction(instruction);
      for (int i = 0; i < this._cursor.Visited.Count; i++)
      {
        for (int j = i + 1; j < this._cursor.Visited.Count; j++)
        {
          if(this._cursor.Visited[i].PtEquals(this._cursor.Visited[j]))
          {
            hasCrossed = true;
            crossPoint = this._cursor.Visited[i];
          }
        }
      }

      if(hasCrossed)
      {
        break;
      }
    }

    int totalDistance = crossPoint.DistanceFromOrigin;
    Console.WriteLine($"Part Two: {totalDistance}");
  }
}

class PuzzlePart
{
  protected List<Instruction> _instructions { get; set; }
  protected Cursor _cursor { get; set; }
  public PuzzlePart(string input)
  {
    _instructions = ParseInput(input);
    _cursor = new Cursor();
  }

  private List<Instruction> ParseInput(string input)
  {
    string[] instructions = input.Split(", ");
    List<Instruction> res = new List<Instruction>();
    foreach (var instruction in instructions)
    {
      res.Add(new Instruction(instruction));
    }

    return res;
  }
}

class Instruction
{
  public char Turn { get; set; }
  public int Distance { get; set; }
  public Instruction(string instruction)
  {
    Turn = instruction[0];
    Distance = Int32.Parse(instruction.Substring(1));
  }

  public override string ToString()
  {
    return $"turn: {Turn}, distance: {Distance}";
  }
}

class Cursor
{
  public Point Pos { get; set; }
  public int Heading { get; set; }
  public List<Point> Visited { get; set; }

  public Cursor(int x = 0, int y = 0, int heading = 0)
  {
    Pos = new Point(x, y);
    Heading = heading;
    Visited = new List<Point>();
    Visited.Add(Pos);
  }

  public void FollowInstruction(Instruction instruction)
  {
    ChangeHeading(instruction.Turn);
    Move(instruction.Distance);
  }

  private void ChangeHeading(char direction)
  {
    if(direction == 'R')
    {
      Heading = (Heading + 90) % 360;
    }
    else
    {
      Heading = (Heading - 90) % 360;
      if(Heading < 0)
      {
        Heading = Heading + 360;
      }
    }
  }

  private void Move(int distance)
  {
    switch(Heading)
    {
      case 0:
        for (int i = 0; i < distance; i++)
        {
          Visited.Add(new Point(Pos.X, Pos.Y + i + 1));
        }
        Pos = new Point(Pos.X, Pos.Y + distance);
        break;
      case 90:
        for (int i = 0; i < distance; i++)
        {
          Visited.Add(new Point(Pos.X + i + 1, Pos.Y));
        }
        Pos = new Point(Pos.X + distance, Pos.Y);
        break;
      case 180:
        for (int i = distance - 1; i >= 0; i--)
        {
          Visited.Add(new Point(Pos.X, Pos.Y - i - 1));
        }
        Pos = new Point(Pos.X, Pos.Y - distance);
        break;
      case 270:
        for (int i = distance - 1; i >= 0; i--)
        {
          Visited.Add(new Point(Pos.X - i - 1, Pos.Y));
        }
        Pos = new Point(Pos.X - distance, Pos.Y);
        break;
      default:
        for (int i = 0; i < distance; i++)
        {
          Visited.Add(new Point(Pos.X, Pos.Y + i + 1));
        }
        Pos = new Point(Pos.X, Pos.Y + distance);
        break;
    }
  }
}

class Point
{
  public int X { get; set; }
  public int Y { get; set; }
  public int DistanceFromOrigin
  {
    get { return Math.Abs(X) + Math.Abs(Y); }
  }
  public Point(int x, int y)
  {
    X = x;
    Y = y;
  }

  public bool PtEquals(Point other)
  {
    return this.X == other.X && this.Y == other.Y;
  }

  public override string ToString()
  {
    return $"x: {X}, y: {Y}";
  }
}

