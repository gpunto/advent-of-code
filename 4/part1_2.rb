class Guard
    attr_accessor :id, :sleeping

    def initialize(id)
        @id = id
        @sleeping = Array.new(60, 0)
        @start = -1
    end
    
    def fall_asleep(minute)
        @start = minute
    end
    
    def wake_up(minute)
        if @start >= 0
            (@start...minute).each { |i| @sleeping[i] += 1 }
            @start = -1
        end
    end

    def total_sleep
        @sleeping.sum
    end

    def max_minute
        @sleeping.each_with_index.max_by {|e, i| e}[1]
    end

    def max_sleep
        @sleeping[max_minute]
    end
end

class Entry
    attr_accessor :month, :day, :hour, :min, :event

    def initialize(row)
        pattern_row = /\[\d{4}-(\d\d)-(\d\d) (\d\d):(\d\d)\] (.+)/i
        capts = row.match(pattern_row).captures
        @month = capts[0].to_i
        @day = capts[1].to_i
        @hour = capts[2].to_i
        @min = capts[3].to_i
        @event = capts[4]
    end
    
    def <=>(other)
        if @month != other.month
            @month - other.month
        elsif @day != other.day
            @day - other.day
        elsif @hour != other.hour
            @hour - other.hour
        elsif @min != other.min
            @min - other.min
        else
            0
        end
    end

    def guard?
        event.start_with?("Guard")
    end
    def wakes?
        event.start_with?("wakes")
    end
    def sleeps?
        event.start_with?("falls")
    end
end

def parse_guard_id(guard_event)
    pattern_guard = /Guard #(\d+) begins shift/i
    guard_event.match(pattern_guard).captures[0].to_i
end

$guards = {}
$current = nil

File.foreach("input.txt")
    .map { |r| Entry.new(r) }
    .sort
    .each { |e|
        if e.guard?
            id = parse_guard_id(e.event)
            $current = $guards[id] ||= Guard.new(id)
        elsif e.sleeps?
            $current.fall_asleep(e.min)
        elsif e.wakes?
            $current.wake_up(e.min)
        end
    }

chosen1 = $guards.values.max_by { |guard| guard.total_sleep }
puts "Strategy 1: chosen id #{chosen1.id}, minute #{chosen1.max_minute}, product = #{chosen1.id * chosen1.max_minute}"

chosen2 = $guards.values.max_by {|guard| guard.max_sleep }
puts "Strategy 2: chosen id #{chosen2.id}, minute #{chosen2.max_minute}, product = #{chosen2.id * chosen2.max_minute}"