# The one queue for all Kata runs (docs/plan-katas.md): at most parallel
# runs at a time, the others wait, first come, first served. Each person has
# at most one run: a new one replaces theirs, waiting or running (a running
# one is stopped through the hook its job registered, KataRunner.run's
# cancel:). A waiting ticket knows how many are ahead of it (»Noch 2 vor
# dir«).
#
# Plain Ruby with threads, tested on its own (test/kata_queue_test.rb).
class KataQueue
    class Ticket
        attr_reader :owner, :result, :error

        def initialize(owner, job)
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
                @done.broadcast
                @stops.dup
            end
            stops.each(&:call)
        end

        private

        def finish(state)
            @mutex.synchronize do
                @state = state unless @state == :cancelled
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
        @workers = []
    end

    # Queues job (a lambda that gets the ticket) for owner; their earlier
    # run, waiting or running, is cancelled.
    def submit(owner, &job)
        ticket = Ticket.new(owner, job)
        previous = nil
        @mutex.synchronize do
            previous = @current[owner]
            @waiting.delete(previous) if previous
            @current[owner] = ticket
            @waiting << ticket
            start_workers
            @available.signal
        end
        previous&.cancel
        ticket
    end

    # How many runs wait before ticket; nil when it no longer waits.
    def ahead(ticket)
        @mutex.synchronize { @waiting.index(ticket) }
    end

    def waiting
        @mutex.synchronize { @waiting.size }
    end

    private

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
