class Node
  attr_accessor :children, :metadata, :len

  def initialize(children, metadata, len)
    @children = children
    @metadata = metadata
    @len = len
  end

  def metadata_sum
    @metadata.sum + @children.sum(&:metadata_sum)
  end

  def value
    if children.empty?
      @metadata.sum
    else
      @metadata.map { |i| i - 1 }
        .select { |i| i.between?(0, @children.size - 1) }
        .map { |i| @children[i] }
        .sum { |n| n.value }
    end
  end
end

def node_list_len(nodes)
  nodes.sum { |n| n.len }
end

def make_nodes(arr, siblings)
  return [] if arr.empty?

  children_count = arr[0]
  metadata_count = arr[1]

  if siblings == 0
    if (children_count == 0)
      children = []
    else
      children = make_nodes(arr.slice(2...arr.size - metadata_count), children_count - 1)
    end
    len = 2 + metadata_count + node_list_len(children)
    node = Node.new(children, arr.slice(len - metadata_count, metadata_count), len)
    res = [node]
  elsif children_count == 0
    len = 2 + metadata_count
    siblings_next = [0, siblings - 1].max
    children = []
    node = Node.new(children, arr.slice(len - metadata_count, metadata_count), len)
    res = [node].concat make_nodes(arr.slice(len...arr.size), siblings_next)
  else
    children = make_nodes(arr.slice(2...arr.size), children_count - 1)
    len = 2 + metadata_count + node_list_len(children)
    sibling_nodes = make_nodes(arr.slice(len, arr.size), siblings - 1)
    node = Node.new(children, arr.slice(len - metadata_count, metadata_count), len)
    res = [node].concat sibling_nodes
  end
  res
end

numbers = File.read("input.txt")
  .split(" ")
  .map { |s| s.to_i }

tree = make_nodes(numbers, 0)[0]

puts "Part 1: #{tree.metadata_sum()}"
puts "Part 2: #{tree.value()}"
