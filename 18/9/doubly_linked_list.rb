
class Node
  attr_accessor :val, :next, :prev

  def initialize(val)
    @val = val
  end
end

class DoublyLinkedList
  attr_accessor :head

  def initialize
    @head = nil
  end

  def insert(val)
    to_add = Node.new(val)
    if head == nil
      to_add.next = to_add
      to_add.prev = to_add
    else
      curr_next = @head.next
      to_add.prev = @head
      to_add.next = curr_next
      @head.next = to_add
      curr_next.prev = to_add
    end
    @head = to_add
  end

  def remove
    return nil if head == nil
    val = @head.val
    if @head.prev == @head
      @head = nil
    else
      @head.prev.next = @head.next
      @head.next.prev = @head.prev
      @head = @head.next
    end
    val
  end

  def move(steps)
    if steps > 0
      steps.times { move_next }
    elsif steps < 0
      steps.abs.times { move_prev }
    end
  end

  def move_next
    return if @head == nil
    @head = @head.next
  end

  def move_prev
    return if @head == nil
    @head = @head.prev
  end
end
