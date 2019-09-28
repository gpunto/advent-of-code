require 'set'

def read_specs(line)
    line.match(/#\d+ @ (\d+),(\d+): (\d+)x(\d+)/i)
        .captures.map {|c| c.to_i}
end

def to_cells(specs)
    left, top, width, height = specs
    cells = []

    for i in left...(left + width) do
        for j in top...(top+height) do
            cells << [i, j]
        end
    end

    cells
end

ones = Set[]
mores = Set[]

File.foreach("input.txt")
    .map {|line| read_specs(line)}
    .flat_map {|specs| to_cells(specs)}
    .each {|cell|
        if ones.include?(cell)
            ones.delete(cell)
            mores.add(cell)
        elsif !mores.include?(cell)
            ones.add(cell)
        end
    }

p mores.size