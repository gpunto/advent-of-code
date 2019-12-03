require_relative "./doubly_linked_list"

class Board
  attr :circle

  def initialize()
    @circle = DoublyLinkedList.new
    @circle.insert(0)
  end

  def put(num)
    if num % 23 == 0
      put_23(num)
    else
      put_regular(num)
    end
  end

  def put_regular(num)
    @circle.move_next
    @circle.insert(num)
    0
  end

  def put_23(num)
    @circle.move(-7)
    removed = @circle.remove
    num + removed
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

for i in 1..last_marble * 100
  player = i % players
  worth = board.put(i)
  scores[player] += worth
end

p scores.max
