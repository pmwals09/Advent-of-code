using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] input = File.ReadAllLines("day-02-input.txt");

        Keypad partOneKeypad = new PartOneKeypad(new string[3,3]{
            {"1", "2", "3"},
            {"4", "5", "6"},
            {"7", "8", "9"}
        });
        PartOne partOne = new PartOne(input, partOneKeypad);
        partOne.Main();

        Keypad partTwoKeypad = new PartTwoKeypad(new string[5,5]{
            {"", "", "1", "", ""},
            {"", "2", "3", "4", ""},
            {"5", "6", "7", "8", "9"},
            {"", "A", "B", "C", ""},
            {"", "", "D", "", ""}
        });
        PartTwo partTwo = new PartTwo(input, partTwoKeypad);
        partTwo.Main();
    }
}

class PartOne : PuzzlePart
{
    public PartOne(string[] input, Keypad keypad) : base(input, keypad)
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
    public PartTwo(string[] input, Keypad keypad) : base(input, keypad)
    {
    }

    public void Main()
    {
        System.Console.WriteLine("Part Two:");
        base.Main();
    }
}

class PuzzlePart
{
    private readonly string[] _input;
    private Keypad _keypad;
    private string _solution = "";
    public PuzzlePart(string[] input, Keypad keypad)
    {
        _input = input;
        _keypad = keypad;
    }

    public void Main()
    {
        foreach (string line in _input)
        {
            foreach (char direction in line)
            {
                _keypad.Move(direction);
            }
            _solution += _keypad.pad[_keypad.cursor.point.Y, _keypad.cursor.point.X];
        }
        System.Console.WriteLine(_solution);
    }
}

class PartOneKeypad : Keypad
{
    public PartOneKeypad(string[,] keypad) : base(keypad)
    {
    }
}

class PartTwoKeypad : Keypad
{
    public PartTwoKeypad(string[,] keypad) : base(keypad)
    {
    }

    public override bool InBounds()
    {
        return cursor.point.X >= 0 && cursor.point.X < pad.GetLength(1) &&
          cursor.point.Y >= 0 && cursor.point.Y < pad.GetLength(0) && Value.Length > 0;
    }
}

class Keypad
{
    public string Value 
    {
        get
        {
            return pad[cursor.point.Y, cursor.point.X];
        }
    }
    public string[,] pad;
    public Cursor cursor;
    public Keypad(string[,] keypad)
    {
        pad = keypad;
        for(int i = 0; i < keypad.GetLength(0); i++)
        {
            for(int j = 0; j < keypad.GetLength(1); j++)
            {
                if(keypad[i,j] == "5")
                {
                    cursor = new Cursor(j, i);
                }
            }
        }
    }

    public void Move(char direction)
    {
        cursor.Move(direction);
        if(!InBounds()){
            if (direction == 'U') Move('D');
            else if (direction == 'R') Move('L');
            else if (direction == 'D') Move('U');
            else if (direction == 'L') Move('R');
        }
    }

    public virtual bool InBounds()
    {
        return cursor.point.X >= 0 && cursor.point.X < pad.GetLength(1) &&
          cursor.point.Y >= 0 && cursor.point.Y < pad.GetLength(0);

    }
}

class Cursor
{
    public Point point { get; set; }
    public Cursor(int row, int col)
    {
      point = new Point(row, col);
    }

    public void Move(char direction)
    {
        switch (direction)
        {
            case 'U':
                point.Y = point.Y - 1;
                break;
            case 'R':
                point.X = point.X + 1;
                break;
            case 'D':
                point.Y = point.Y + 1;
                break;
            case 'L':
                point.X = point.X - 1;
                break;
            default:
                break;
        }
    }
}

class Point
{
    public int X { get; set; }
    public int Y { get; set; }
    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }

    public override string ToString()
    {
        return $"x: {X}, y: {Y}";
    }
}
