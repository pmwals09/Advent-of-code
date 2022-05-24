# frozen_string_literal: true

# Shared puzzle logic
class PuzzlePart
  attr_reader :instructions, :keypad

  def initialize
    @instructions = File.readlines('./day-02-input.txt').map(&:strip).map { |x| x.split('') }
  end
end

# cursor location
class Cursor
  def initialize(point)
    @y, @x = point
  end

  def move(direction)
    case direction
    when 'U'
      @y -= 1 if @y - 1 >= 0
    when 'R'
      @x += 1 if @x + 1 < 3
    when 'D'
      @y += 1 if @y + 1 < 3
    when 'L'
      @x -= 1 if @x - 1 >= 0
    end
  end

  def to_a
    [@y, @x]
  end
end

# Part one
class PartOne < PuzzlePart
  def initialize
    super
    @cursor = Cursor.new([1, 1])
    @keypad = (0..2).map { |ea| (ea * 3..ea * 3 + 2).map { |n| n + 1 } }
  end

  def main
    numbers = []
    @instructions.each do |num|
      num.each do |x|
        @cursor.move x
      end
      y, x = @cursor.to_a
      numbers << @keypad[y][x]
    end
    numbers
  end
end

# Part two
class PartTwo < PuzzlePart
  def initialize
    super
    @cursor = Cursor.new([2, 0])
    @keypad = [
      [nil, nil, 1, nil, nil],
      [nil, 2, 3, 4, nil],
      [5, 6, 7, 8, 9],
      [nil, 'A', 'B', 'C', nil],
      [nil, nil, 'D', nil, nil]
    ]
  end

  def main
    numbers = []
    @instructions.each do |num|
      num.each do |x|
        # need to add a limit to the cursor for validation
        # need to tell if making a move is valid before it's made
      end
    end
    numbers
  end
end

part_one = PartOne.new
p part_one.main

