# frozen_string_literal: true

require 'pry'

# Point logic, esp. comparisons
class Point
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def eql_pt(point)
    point.x == @x && point.y == @y
  end

  def to_s
    "x: #{@x}, y: #{@y}"
  end
end

# Manages x/y position
class Cursor
  attr_accessor :pos, :facing, :visited

  def initialize
    @pos = Point.new(0, 0)
    @facing = :u
    @visited = [@pos]
  end

  def move(direction, amount = 1)
    @facing = direction
    case direction
    when :r
      (@pos.x + 1..@pos.x + amount).each { |x| visited << Point.new(x, @pos.y) }
      @pos = Point.new(@pos.x + amount, @pos.y)
    when :d
      (@pos.y - 1).downto(@pos.y - amount).each { |y| visited << Point.new(@pos.x, y) }
      @pos = Point.new(@pos.x, @pos.y - amount)
    when :l
      (@pos.x - 1).downto(@pos.x - amount).each { |x| visited << Point.new(x, @pos.y) }
      @pos = Point.new(@pos.x - amount, @pos.y)
    when :u
      (@pos.y + 1..@pos.y + amount).each { |y| visited << Point.new(@pos.x, y) }
      @pos = Point.new(@pos.x, @pos.y + amount)
    else
      raise 'Invalid direction selected'
    end
  end

  def to_s
    "#{@pos}, facing: #{@facing}"
  end
end

# Manage a single instruction
class Instruction
  attr_reader :direction, :distance

  def initialize(instruction)
    @direction = instruction[0].chomp.downcase.to_sym
    @distance = instruction[1..-1].chomp.to_i
  end

  def to_s
    "#{@direction} #{@distance}"
  end
end

# Shared puzzle part logic
class PuzzlePart
  def initialize
    @instructions = File.read('./day-01-input.txt').split(', ').map { |x| Instruction.new(x) }
    @cursor = Cursor.new
  end

  DIRECTIONS = %i[u r d l].freeze
  def make_turn(direction, facing)
    current_idx = DIRECTIONS.index facing
    if direction == :r && current_idx == DIRECTIONS.length - 1
      DIRECTIONS[0]
    elsif direction == :r
      DIRECTIONS[current_idx + 1]
    else
      DIRECTIONS[current_idx - 1]
    end
  end

  def take_turn(instruction)
    turn = make_turn(instruction.direction, @cursor.facing)
    @cursor.move(turn, instruction.distance)
  end
end

# Part One logic/main loop
class PartOne < PuzzlePart
  def main
    @instructions.each do |ea|
      take_turn ea
    end

    puts "Part one: #{@cursor.pos.x.abs + @cursor.pos.y.abs}"
  end
end

# Part Two logic/main loop
class PartTwo < PuzzlePart
  def main
    crossover_point = nil
    @instructions.each do |ea|
      take_turn ea
      next unless crossed_over?

      crossover_point = find_crossover_point
      break
    end

    puts "Part two: #{crossover_point.x.abs + crossover_point.y.abs}"
  end

  def crossed_over?
    @cursor.visited.uniq(&:to_s).length != @cursor.visited.length
  end

  def find_crossover_point
    @cursor.visited.detect do |visited|
      @cursor.visited.count do |counted|
        counted.x == visited.x && counted.y == visited.y
      end > 1
    end
  end
end

PartOne.new.main
PartTwo.new.main
