class Board
  attr :circle, :current

  def initialize()
    @circle = [0]
    @current = 0
  end

  def put(num)
    if num % 23 == 0
      put_23(num)
    else
      put_regular(num)
    end
  end

  def put_regular(num)
    @current = ((@current + 1) % @circle.size) + 1
    @circle.insert(@current, num)
    return 0
  end

  def put_23(num)
    @current = @current - 7
    @current = @circle.size + @current if @current < 0
    removed = @circle.delete_at(@current)
    return num + removed
  end

  def inspect
    arr = @circle.join(", ")
    "(#{@circle[@current]}) - #{arr}"
  end
end

def read_specs(line)
  line.match(/(\d+) players; last marble is worth (\d+) points/)
    .captures.map { |c| c.to_i }
end

specs = read_specs(File.read("input.txt"))
players = specs[0]
last_marble = specs[1]

board = Board.new
scores = Array.new(players) { 0 }

for i in 1..last_marble
  player = i % players
  worth = board.put(i)
  scores[player] += worth
end

p scores.max
