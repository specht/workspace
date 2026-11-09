require 'set'

# Running Workspaces for /statistik (docs/datenschutz.md, »Nutzung ohne
# Namen«): every few minutes the server sees which Workspaces run. For the
# current hour it keeps in memory which ones it has seen, and in the
# database only two numbers per hour (WorkspaceHour): running, how many
# different Workspaces ran in that hour, and peak, how many at once. Who it
# was is never stored. A restart in the middle of an hour forgets who was
# seen; the stored numbers then only grow, never shrink. Hours older than
# KEEP_DAYS go. Plain Ruby, tested on its own (test/usage_stats_test.rb); the
# server's part is in usage_stats_page.rb.
module WorkspaceUsage
    KEEP_DAYS = 400
    SECONDS = 300                  # between two looks

    # The Workspaces seen in the current hour, in memory only.
    class Tally
        def initialize
            @hour = nil
            @seen = Set.new
            @peak = 0
        end

        # tags: the Workspaces running now; { hour:, running:, peak: }
        def add(tags, now)
            hour = now - now % 3600
            if hour != @hour
                @hour = hour
                @seen = Set.new
                @peak = 0
            end
            @seen.merge(tags)
            @peak = [@peak, tags.size].max
            { :hour => @hour, :running => @seen.size, :peak => @peak }
        end
    end
end
