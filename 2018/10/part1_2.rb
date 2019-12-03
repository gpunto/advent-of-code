class Point
  attr :x, :y, :vx, :vy

  def initialize(x, y, vx, vy)
    @x = x
    @y = y
    @vx = vx
    @vy = vy
  end

  def advance
    Point.new(x + vx, y + vy, vx, vy)
  end

  def inspect
    "(#{@x}, #{@y}) <#{@vx}, #{@vy}>"
  end
end

class Box
  attr :t, :l, :b, :r

  def initialize(bounds_x, bounds_y)
    @l = bounds_x[0]
    @r = bounds_x[1]
    @t = bounds_y[0]
    @b = bounds_y[1]
  end

  def size
    (@r - @l) * (@b - @t)
  end
end

class Board
  def initialize(points)
    @points = points
  end

  def bounding_box
    mmx = @points.map(&:x).minmax
    mmy = @points.map(&:y).minmax
    Box.new(mmx, mmy)
  end

  def display
    grouped = @points.group_by(&:y)
    bounds = bounding_box()

    (bounds.t..bounds.b)
      .map { |row_i| grouped.fetch(row_i, []) }
      .each { |row| print_row(row, bounds.l, bounds.r) }
  end

  def print_row(row, min, max)
    grouped = row.group_by(&:x)

    puts (min..max)
           .map { |col_i| char_to_print(grouped.key?(col_i)) }
           .join
  end

  def char_to_print(has_point)
    return "#" if has_point
    return " "
  end

  def advance
    @points = @points.map(&:advance)
  end
end

def parse(line)
  parsed = line
    .match(/position=<[ ]*(-?\d+),[ ]*(-?\d+)> velocity=<[ ]*(-?\d+),[ ]*(-?\d+)>/)
    .captures
    .map(&:to_i)

  Point.new(parsed[0], parsed[1], parsed[2], parsed[3])
end

points = File.foreach("input.txt")
  .map { |line| parse(line) }

board = Board.new(points)

# p (1...20000).min_by {
#   board.advance
#   board.bounding_box.size
# }

10009.times { board.advance }
board.display
