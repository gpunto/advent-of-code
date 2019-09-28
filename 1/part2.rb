require 'set'
seen = Set[0]

current_freq = 0
found = false

while(!found) do
    File.foreach("input.txt")
        .map {|line| line.to_i}
        .each {|i|
            current_freq += i
            found = seen.include?(current_freq)
            break if(found)
            seen.add(current_freq)
        }
end

puts current_freq