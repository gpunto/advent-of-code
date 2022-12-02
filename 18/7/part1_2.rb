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
        to_execute = find_to_execute(h).first
        remove_completed(h, to_execute)
        to_execute
    end

    def find_to_execute(h)
        h.select { |name, deps| deps.empty? }.keys.sort
    end

    def remove_completed(h, completed)
        h.delete_if { |name, _| completed == name }
        h.each { |_, deps| deps.delete(completed); deps }
    end

    def execute_with_workers(workers)
        h = @hash.clone
        free_workers = workers
        executing = {}
        total_time = 0

        while !h.empty?
            to_execute = find_to_execute(h)
            
            for task in to_execute do
                if executing[task] == nil
                    executing[task] = time_for(task)
                    free_workers -= 1
                    break if free_workers == 0
                end
            end

            min_time = executing.min_by { |_, time| time }[1]
            completed = executing.select { |_, time| time == min_time }.keys

            executing.delete_if { |task, _| completed.include? task } 
            executing.update(executing) { |_, time| time - min_time }
            completed.each { |task| remove_completed(h, task) }
            free_workers += completed.size
            total_time += min_time
        end
        total_time
    end

    def time_for(task)
        60 + task.ord - 'A'.ord + 1
    end
end

def parse_step(line)
    line.scan(/Step ([A-Z]) must be finished before step ([A-Z]) can begin./)
        .map { |pair| Step.new(pair[0].chr, pair[1].chr) }
        .first
end

flow = File.foreach("input.txt")
    .map { |line| parse_step(line) }
    .inject(Flow.new) { |flow, step| flow.insert(step); flow }

p flow.execute_with_workers(5)