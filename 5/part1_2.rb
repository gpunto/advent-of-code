$a_u = 'A'.ord
$a_l = 'a'.ord

def reacts?(x1, x2)
    x1i = x1.ord
    x2i = x2.ord
    
    x1i - $a_l == x2i - $a_u ||
    x1i - $a_u == x2i - $a_l
end

def remove_units(from, num)
    low = ($a_l + num).chr
    up = ($a_u + num).chr
    from.gsub(/#{low}|#{up}/, "")
end

def react(what)
    content = what.chars
    prev_size = 0

    while prev_size != content.size
        prev_size = content.size
        i = content.size - 1
        while i > 0 do
            prev = content[i - 1]
            curr = content[i]
            if reacts?(prev, curr)
                content.delete_at(i - 1)
                content.delete_at(i - 1)
                i -= 1
            end
            i -= 1
        end
    end

    content.size
end

original = File.read("input.txt")

p (0...26)
        .map { |n| remove_units(original, n) }
        .map { |content| react(content) }
        .sort