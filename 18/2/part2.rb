def common_chars(a, b)
    a.chars.zip(b.chars)
        .select {|pair| pair[0] == pair[1]}
        .map {|pair| pair[0]}
end

def distance(a, b)
    a.length - common_chars(a, b).size
end

lines = File.readlines("input.txt")

common = ""
for i in 0...lines.length do
    for j in (i+1)...lines.length do
        if(distance(lines[i], lines[j]) == 1)
            common = common_chars(lines[i], lines[j]).join("")
            break
        end
        j += 1
    end
    i += 1
end

puts common