using System;
using System.IO;
using System.Collections.Generic;

class Program
{
  static void Main()
  {
    string[] input = File.ReadAllLines("day-03-input.txt");

    PartOne partOne = new PartOne(input);
    partOne.Main();

    PartTwo partTwo = new PartTwo(input);
    partTwo.Main();
  }
}

class PartOne : PuzzlePart
{
  public PartOne(string[] input) : base(input)
  {
  }

  public void Main()
  {
    System.Console.WriteLine("Part One:");
    base.Main();
  }
}

class PartTwo : PuzzlePart
{
  public PartTwo(string[] input) : base(input)
  {
  }
}

class PuzzlePart
{
  public List<List<int>> _input = new List<List<int>>();
  public int _count = 0;
  public PuzzlePart(string[] input)
  {
    foreach(string line in input)
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
      _input.Add(intLine);
    }
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