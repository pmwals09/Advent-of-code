using System;
using System.Collections;
using System.IO;

class BaseInput
{
  public readonly string[] _lines;
  public BaseInput(string filepath)
  {
    _lines = File.ReadAllLines(filepath);
  }
}