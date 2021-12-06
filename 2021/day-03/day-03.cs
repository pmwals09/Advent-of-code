using System;

class Program
{
  static void Main()
  {
    Input input = new Input("day-03-input.txt");
    input.PrintInput(10);
    Frequencies freq = new Frequencies(input);
  }
}

class Input : BaseInput
{
  public Input(string filepath) : base(filepath)
  {
  }
  public string[] ParseInput()
  {
    return _lines;
  }

  public void PrintInput(int limit = 0)
  {
    if(limit == 0)
    {
      foreach(string line in _lines)
      {
          System.Console.WriteLine(line);
      }
    }
    else
    {
      int lineIndex = 0;
      foreach(string line in _lines)
      {
        if(lineIndex < limit)
        {
          System.Console.WriteLine(line);
          lineIndex++;
        }
        else
        {
          break;
        }
      }
    }
  }
}

class Frequencies
{
  private readonly string[] _binaryNums;
  public DigitCounts[] DigitCounts { get; private set; }
  public Frequencies(string[] binaryNums)
  {
    _binaryNums = binaryNums;
    DigitCounts = DigitCounts[12];
  }

  // public DigitCounts[] GenerateFrequencies()
  public bool GenerateFrequencies()
  {

    return true;
  }
}

class DigitCounts
{
  public int Ones { get; set; }
  public int Zeroes {get; set;}
  public DigitCounts()
  {
    Ones = 0;
    Zeroes = 0;
  }

  public void IncOnes()
  {
    Ones++;
  }

  public void IncZeroes()
  {
    Zeroes++;
  }
}