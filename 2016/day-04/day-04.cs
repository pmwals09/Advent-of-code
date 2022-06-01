using System;
using System.IO;
using System.Collections.Generic;
using System.Text;

class Program
{
  static void Main()
  {
    Input input = new Input("day-04-input.txt");
    PartOne partOne = new PartOne(input);
    partOne.Main();
    PartTwo partTwo = new PartTwo(input);
    partTwo.Main();
  }
}

class Input
{
  public string[] lines = new string[1];
  public List<Room> rooms = new List<Room>();
  public Input(string filename)
  {
    lines = File.ReadAllLines(filename);

    foreach(string line in lines)
    {
      Room room = new Room(line);
      room.Parse();
      rooms.Add(room);
    }
  }
}

class Room
{
  
  public string _roomInput;
  public string roomName;
  public Dictionary<char, int> freq = new Dictionary<char, int>();
  public string checksum;
  public int sectorId;
  public bool IsValid
  {
    get
    {
      List<KeyValuePair<char, int>> freqEntries = new List<KeyValuePair<char, int>>(freq);
      freqEntries.Sort(delegate(KeyValuePair<char, int> a, KeyValuePair<char, int> b)
      {
        if(a.Value == b.Value)
        {
          if(a.Key < b.Key) return -1;
          else return 1;
        }
        else if(a.Value > b.Value) return -1;
        else return 1;
      });

      bool res = true;
      for(int i = 0; i < 5; i++)
      {
        if(freqEntries[i].Key != checksum[i]) res = false;
      }
      return res;
    }
  }
  public Room(string roomInput)
  {
    _roomInput = roomInput;
  }

  public void Parse()
  {
    char[] brackets = new char[] { '[', ']'};
    string[] splitRoomInput = _roomInput.Split(brackets);
    checksum = splitRoomInput[1];
    string remaining = splitRoomInput[0];

    string[] roomParts = remaining.Split("-");
    sectorId = Int32.Parse(roomParts[roomParts.Length - 1]);
    StringBuilder builder = new StringBuilder();
    for(int i = 0; i < roomParts.Length - 1; i++)
    {
      foreach(char letter in roomParts[i])
      {
        builder.Append(letter);
        if(freq.ContainsKey(letter))
        {
          freq[letter]++;
        }
        else
        {
          freq.Add(letter, 1);
        }
      }
      if(i != roomParts.Length - 2) builder.Append('-');
    }

    roomName = builder.ToString();
  }
}

class PartOne : PuzzlePart
{
  public PartOne(Input input) : base(input)
  {
  }

  public void Main()
  {
    int sum = 0;
    foreach(Room room in _input.rooms)
    {
      if(room.IsValid) sum += room.sectorId;
    }
    System.Console.WriteLine(sum);
  }
}

class PartTwo : PuzzlePart
{
  public PartTwo(Input input) : base(input)
  {
  }

  public void Main()
  {
    foreach(Room room in _input.rooms)
    {
      if(room.IsValid)
      {
        string[] roomNameParts = room.roomName.Split("-");
        int rotation = room.sectorId % 26;
        string solution = "";
        foreach(string part in roomNameParts)
        {
          foreach(char letter in part)
          {
            int newLetterCode = (int) letter + rotation;
            if(newLetterCode > 122)
            {
              newLetterCode = newLetterCode - (123 - 97);
            }
            solution += (char) newLetterCode;
          }
          solution += " ";
        }

        if(solution.Contains("north"))
        {
          System.Console.WriteLine($"{solution} - {room.sectorId}");
        }
      }
    }
  }
}

class PuzzlePart
{
  public Input _input;
  public PuzzlePart(Input input)
  {
    _input = input;
  }
}
