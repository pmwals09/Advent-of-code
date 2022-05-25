using System;
using System.IO;
using System.Collections.Generic;

class Program
{
  static void Main()
  {
    PartOneInput p1Input = new PartOneInput("day-03-input.txt");
    p1Input.ParseInput();
    PartOne partOne = new PartOne(p1Input);
    partOne.Main();

    PartTwoInput p2Input = new PartTwoInput("day-03-input.txt");
    p2Input.ParseInput();
    PartTwo partTwo = new PartTwo(p2Input);
    partTwo.Main();
  }
}

class PartOneInput : Input
{
  public PartOneInput(string filename) : base(filename){}
}

class PartTwoInput : Input
{
  public PartTwoInput(string filename) : base(filename){}

  public override void ParseInput()
  {
    List<List<string>> rotatedLines = new List<List<string>>();
    foreach(string line in _filelines)
    {
      string[] stringLine = System.Text.RegularExpressions.Regex.Split(line, @"\s+"); ;
      for(int i = 0; i < stringLine.Length; i++)
      {
        if(rotatedLines.Count < i + 1){
          rotatedLines.Add(new List<string>());
        }  

        rotatedLines[i].Add(stringLine[i]);
      }
    }

    List<int> group = new List<int>();
    foreach(List<string> line in rotatedLines)
    {
      foreach(string item in line)
      {
        int intItem;
        if(Int32.TryParse(item, out intItem))
        {
          if(group.Count >= 3)
          {
            input.Add(group);
            group = new List<int>();
          } 
          group.Add(intItem);
        }
      }
    }
    if(group.Count >= 3)
    {
      input.Add(group);
    } 
  }
}
class Input
{
  public string[] _filelines;
  public List<List<int>> input = new List<List<int>>();
  public Input(string filename)
  {
    _filelines = File.ReadAllLines(filename);
  }

  public virtual void ParseInput()
  {
    foreach(string line in _filelines)
    {
      string[] stringLine = System.Text.RegularExpressions.Regex.Split(line, @"\s+"); ;
      List<int> intLine = new List<int>();
      foreach(string side in stringLine)
      {
        int intSide;
        if(Int32.TryParse(side, out intSide)){
          intLine.Add(intSide);
        }
      }
      input.Add(intLine);
    }
  }
}

class PartOne : PuzzlePart
{
  public PartOne(Input input) : base(input){}
}

class PartTwo : PuzzlePart
{
  public PartTwo(Input input) : base(input){}
}

class PuzzlePart
{
  public List<List<int>> _input = new List<List<int>>();
  public int _count = 0;
  public PuzzlePart(Input input)
  {
    _input = input.input;
  }

  public void Main()
  {
    foreach(List<int> sides in _input)
    {
      Triangle triangle = new Triangle(sides);
      if(triangle.IsPossible){
        _count += 1;
      }
    }
    System.Console.WriteLine(this.GetType().Name);
    System.Console.WriteLine(_count);
  }
}
class Triangle
{

  private readonly List<int> _sides;
  public bool IsPossible
  {
    get
    {
      return _sides[0] < _sides[1] + _sides[2] &&
        _sides[1] < _sides[0] + _sides[2] &&
        _sides[2] < _sides[0] + _sides[1];
    }
  }

  public Triangle(List<int> sides)
  {
    _sides = sides;
  }
}