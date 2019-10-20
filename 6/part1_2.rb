Tuple = Struct.new(:_1, :_2)

class Point
    attr_accessor :x, :y

    def initialize(x, y)
        @x = x
        @y = y
    end

    def dist(p2)
       (@x - p2.x).abs + (@y - p2.y).abs
    end

    def at_min_dist?(points)
        min_dists = points
                    .select { |p| self != p }
                    .map { |p| Tuple.new(dist(p), p) }
                    .sort_by { |t| t._1 }
                    .take(2)
        min_dists[0]._2 unless min_dists[0]._1 == min_dists[1]._1
    end

    def total_dist_from(points)
        points.sum { |p| dist(p) }
    end

    def inspect
        "(#{@x}, #{@y})"
    end
end

def parse_point(line)
    pair = line.split(", ")
    x = pair[0].to_i
    y = pair[1].to_i
    Point.new(x, y)
end

def to_point(linear, cols)
    x = linear / cols
    y = linear % cols
    Point.new(x, y)
end

def board_size(points)
    board_size = points.inject(Point.new(0, 0)) { |br, point|
        br.x = point.x if(br.x < point.x)
        br.y = point.y if(br.y < point.y)
        br
    }
    board_size.x = board_size.x + 1
    board_size.y = board_size.y + 1
    board_size
end

def contains_edges?(cell_and_nearest_list, board_size)
    cell_and_nearest_list.any? { |c_n|
        cell = c_n._1
        cell.x == 0 ||
            cell.y == 0 ||
            cell.x == board_size.x - 1 ||
            cell.y == board_size.y - 1
    }
end

def board_cells(board_size)
    (0...board_size.x * board_size.y)
        .map { |l| to_point(l, board_size.y) }
end

def part1(points, board_size)
    p board_cells(board_size)
        .map { |cell|
            nearest = cell.at_min_dist?(points)
            Tuple.new(cell, nearest)
        }
        .select { |c_n| c_n._2 != nil }
        .group_by { |c_n| c_n._2 }
        .select { |_, c_n_list| !contains_edges?(c_n_list, board_size) }
        .map { |c_n, c_n_list| [c_n, c_n_list.size] }
        .sort_by { |pair| pair[1] }
        .last[1]
end

def part2(points, board_size)
    board_cells(board_size)
        .select { |cell| cell.total_dist_from(points) < 10000 }
        .size
end

points = File.foreach("input.txt")
    .map { |line| parse_point(line) }

board_size = board_size(points)

p part2(points, board_size)