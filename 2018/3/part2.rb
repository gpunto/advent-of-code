require 'set'

class Claim
    attr_accessor :id, :left, :top, :right, :bottom

    def initialize(specs)
        @id, @left, @top, width, height = specs
        @right = @left + width
        @bottom = @top + height
    end

    def overlaps?(other)
        !(@left > other.right || other.left > @right ||
            @top > other.bottom || other.top > @bottom)
    end

    def to_s
        "##{id}: #{left}, #{right}, #{top}, #{bottom}"
    end
end

def read_specs(line)
    line.match(/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/i)
        .captures.map(&:to_i)
end

claims = File.foreach("input.txt")
    .map(&method(:read_specs))
    .flat_map(&Claim.method(:new))

puts claims.find { |current| 
    claims.select { |other| other != current }
        .none?(&current.method(:overlaps?))
}
