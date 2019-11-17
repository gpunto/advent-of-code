class Cell
  attr_reader :level, :x, :y

  def initialize(x, y, serial_number)
    @x = x
    @y = y
    @level = power_level(serial_number)
  end

  private

  def power_level(serial_number)
    rack_id = @x + 10
    hundreds(((rack_id * @y) + serial_number) * rack_id) - 5
  end

  def hundreds(num)
    (num % 1000) / 100
  end
end

def build_grid(size, serial_number)
  (1..size).map { |x|
    (1..size).map { |y|
      Cell.new(x, y, serial_number)
    }
  }
end

def total_power(grid, square_size, top, left)
  (top...top + square_size).sum { |i|
    (left...left + square_size).sum { |j|
      grid[i][j].level
    }
  }
end

def solve(grid, serial_number, square_size)
  size = grid.size - square_size
  (0..size).flat_map { |top|
    (0..size).map { |left|
      [top + 1, left + 1, total_power(grid, square_size, top, left)]
    }
  }.max_by { |top, left, power| power }
end

serial_number = 9435
grid_size = 300
grid = build_grid(grid_size, serial_number)

# Part 1
p solve(grid, serial_number, 3)
