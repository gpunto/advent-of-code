puts File.foreach("input.txt")
    .map {|line| line.to_i}
    .reduce(0, :+)