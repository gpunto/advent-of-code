def to_2_and_3(chars)
    chars.group_by {|c| c}
        .map {|k,v| v.count}
        .select {|i| i == 2 || i == 3}
        .uniq
end

counts = File.foreach("input.txt")
    .map {|line| to_2_and_3(line.chars)}
    .inject([0, 0]) { |acc, curr|
        if curr.include?(2)
            acc[0] += 1
        end
        if curr.include?(3)
            acc[1] += 1
        end

        acc
    }

puts counts[0] * counts[1]