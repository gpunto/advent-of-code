# O(n^3) 2D partial sum: copied from r/adventofcode
def power_level(x, y, serial_number)
  rack_id = x + 10
  hundreds(((rack_id * y) + serial_number) * rack_id) - 5
end

def hundreds(num)
  (num % 1000) / 100
end

def solve(serial_number)
  best = -9999999999
  sum = Array.new(301) { Array.new(301) { 0 } }
  bx = 0
  by = 0
  bs = 0

  for y in 1..300
    for x in 1..300
      level = power_level(x, y, serial_number)
      sum[y][x] = level + sum[y - 1][x] + sum[y][x - 1] - sum[y - 1][x - 1]
    end
  end

  for s in 1..300
    for y in s..300
      for x in s..300
        total = sum[y][x] - sum[y - s][x] - sum[y][x - s] + sum[y - s][x - s]

        if (total > best)
          best = total
          bx = x
          by = y
          bs = s
        end
      end
    end
  end

  puts "#{bx - bs + 1}, #{by - bs + 1}, #{bs}"
end

solve(9435)