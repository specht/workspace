# The one queue for all Kata runs (docs/plan-katas.md): at most parallel
# runs at a time, the others wait, first come, first served. Each person has
# at most one run: a new one replaces theirs, waiting or running (a running
# one is stopped through the hook its job registered, KataRunner.run's
# cancel:). A waiting ticket knows how many are ahead of it (»Noch 2 vor
# dir«).
#
# A request does not wait for its run (each waiting request would hold a
# thread of the web server): it gets the ticket's id and asks again (find),
# until the run is done. Finished tickets are dropped after KEEP seconds.
#
# Plain Ruby with threads, tested on its own (test/kata_queue_test.rb).
require 'securerandom'

class KataQueue
    KEEP = 600

    class Ticket
        attr_reader :id, :owner, :result, :error, :created_at, :finished_at
        # whatever the caller wants to keep with the ticket (the task, what
        # has been stored already)
        attr_accessor :data

        def initialize(owner, job)
            @id = SecureRandom.hex(12)
            @created_at = Time.now
            @memo = {}
            @once = Mutex.new
            @owner = owner
            @job = job
            @state = :waiting
            @stops = []
            @mutex = Mutex.new
            @done = ConditionVariable.new
        end

        def state
            @mutex.synchronize { @state }
        end

        # For the job: stop, a lambda that ends the run from outside.
        def on_stop(stop)
            run_now = false
            @mutex.synchronize do
                @stops << stop
                run_now = @state == :cancelled
            end
            stop.call if run_now
        end

        def cancelled?
            state == :cancelled
        end

        # The block's value, computed only the first time for key (storing a
        # result once, however often the page asks).
        def once(key)
            @once.synchronize { @memo.fetch(key) { @memo[key] = yield } }
        end

        # Waits until the ticket is done or cancelled (at most timeout
        # seconds); returns the state then.
        def wait(timeout = nil)
            deadline = timeout && Time.now + timeout
            @mutex.synchronize do
                while @state == :waiting || @state == :running
                    left = deadline && deadline - Time.now
                    break if left && left <= 0

                    @done.wait(@mutex, left)
                end
                @state
            end
        end

        def start
            @mutex.synchronize do
                return false unless @state == :waiting

                @state = :running
            end
            true
        end

        def run
            @result = @job.call(self)
        rescue StandardError => e
            @error = e
        ensure
            finish(:done)
        end

        def cancel
            stops = @mutex.synchronize do
                return unless @state == :waiting || @state == :running

                @state = :cancelled
                @finished_at = Time.now
                @done.broadcast
                @stops.dup
            end
            stops.each(&:call)
        end

        private

        def finish(state)
            @mutex.synchronize do
                @state = state unless @state == :cancelled
                @finished_at = Time.now
                @done.broadcast
            end
        end
    end

    attr_reader :parallel

    def initialize(parallel:)
        @parallel = [parallel.to_i, 1].max
        @mutex = Mutex.new
        @available = ConditionVariable.new
        @waiting = []
        @current = {} # owner => their ticket, waiting or running
        @tickets = {} # id => ticket, until KEEP seconds after it ended
        @workers = []
    end

    # Queues job (a lambda that gets the ticket) for owner; their earlier
    # run, waiting or running, is cancelled.
    def submit(owner, &job)
        ticket = Ticket.new(owner, job)
        previous = nil
        @mutex.synchronize do
            prune
            previous = @current[owner]
            @waiting.delete(previous) if previous
            @current[owner] = ticket
            @tickets[ticket.id] = ticket
            @waiting << ticket
            start_workers
            @available.signal
        end
        previous&.cancel
        ticket
    end

    # The ticket with this id, or nil (unknown, or ended long ago).
    def find(id)
        @mutex.synchronize { @tickets[id.to_s] }
    end

    # How many runs wait before ticket; nil when it no longer waits.
    def ahead(ticket)
        @mutex.synchronize { @waiting.index(ticket) }
    end

    def waiting
        @mutex.synchronize { @waiting.size }
    end

    private

    def prune
        limit = Time.now - KEEP
        @tickets.delete_if { |_, t| t.finished_at && t.finished_at < limit }
    end

    def start_workers
        while @workers.size < @parallel
            @workers << Thread.new { work }
        end
    end

    def work
        loop do
            ticket = @mutex.synchronize do
                @available.wait(@mutex) while @waiting.empty?
                @waiting.shift
            end
            next unless ticket.start

            ticket.run
            @mutex.synchronize { @current.delete(ticket.owner) if @current[ticket.owner].equal?(ticket) }
        end
    end
end
