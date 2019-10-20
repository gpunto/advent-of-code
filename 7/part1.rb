class Step
    attr_accessor :dependency, :name

    def initialize(dependency, name)
        @dependency = dependency
        @name = name
    end

    def inspect
        "#{@dependency} -> #{@name}"
    end
end 

class Flow
    attr :hash

    def initialize
        @hash = {}
    end

    def insert(step)
        add_or_put(step.name, step.dependency)
    end

    def add_or_put(name, dependency)
        current = @hash[name]
        if(current == nil)
            @hash[name] = [dependency]
        else
            current.push(dependency)
        end
        put_if_no_dep(dependency)
    end

    def put_if_no_dep(name)
        @hash[name] = [] if(@hash[name] == nil)
    end

    def execute
        h = @hash.clone
        res = []
        res << execute_pass(h) while !h.empty?
        res.join
    end

    def execute_pass(h)
        to_execute = h.select { |name, deps| deps.empty? }.keys.sort.first
        h.delete_if { |name, _| to_execute == name }
        h.update(h) { |_, deps| deps.select { |name| to_execute != name } }
        to_execute
    end
end

def parse_step(line)
    line.scan(/Step ([A-Z]) must be finished before step ([A-Z]) can begin./)
        .map { |pair| Step.new(pair[0], pair[1]) }
        .first
end

flow = File.foreach("input.txt")
    .map { |line| parse_step(line) }
    .inject(Flow.new) { |flow, step| flow.insert(step); flow }

puts flow.execute