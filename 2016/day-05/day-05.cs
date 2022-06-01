using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;

class Program
{
  static void Main()
  {
    Input input = new Input("day-05-input.txt");
    PartOne partOne = new PartOne(input);
    partOne.Main();
  }
}

class Input
{
  public string DoorId;
  public Input(string filepath)
  {
    DoorId = File.ReadAllText(filepath);
  }
}

class PartOne : PuzzlePart
{
  public PartOne(Input input) : base(input)
  {
  }

  public void Main()
  {
    while(Password.Length < 8)
    {
      MD5 hasher = MD5.Create();
      byte[] convertedInput = Encoding.ASCII.GetBytes($"{_input}{Index}");
      byte[] hash = hasher.ComputeHash(convertedInput);
      string hexString = String.Join("", BitConverter.ToString(hash).Split("-"));
      if(hexString.Substring(0,5) == "00000")
      {
        Password += hexString[5];
      }
      Index++;
    }
    System.Console.WriteLine($"Password: {Password}");
  }
}

class PartTwo : PuzzlePart
{
  public PartTwo(Input input) : base(input)
  {
  }
  
  public void Main()
  {

  }
}

class PuzzlePart
{

  protected string _input;
  public string Password { get; set; }
  public int Index { get; set; }
  public PuzzlePart(Input input)
  {
    _input = input.DoorId;
    // _input = "abc";
    Index = 0;
    Password = "";
  }
}