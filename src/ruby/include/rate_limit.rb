# How often something may happen per key (an address, a client) within a
# time window: in memory, so a restart starts over, and nothing is written
# anywhere. allow? counts a try only when it is allowed.
class RateLimit
    def initialize(limit:, period:)
        @limit = limit
        @period = period
        @hits = {}
        @mutex = Mutex.new
    end

    def allow?(key, now: Time.now.to_f)
        @mutex.synchronize do
            prune(now) if @hits.size > 10_000
            times = (@hits[key] ||= [])
            times.shift while !times.empty? && times.first <= now - @period
            return false if times.size >= @limit

            times << now
            true
        end
    end

    private

    def prune(now)
        @hits.delete_if { |_key, times| times.empty? || times.last <= now - @period }
    end
end
