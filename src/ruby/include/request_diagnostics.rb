# Diagnostics for a slow site (docs/entwicklung.md, »Wenn die Seite langsam
# ist«), in the log of the Ruby container, three kinds of lines:
#
#   SLOW 2345 ms GET /admin 200 neo4j 14× 2100 ms (slowest 1800 ms: MATCH (u:User) …) roster lock 0 ms
#     a request that took longer than SLOW_MS: where its time went;
#   STUCK 15 s GET /klasse (thread 47) at main.rb:903 … / roster_store.rb:54 …
#     a request still running after STUCK_SECONDS, with where it is now
#     (its thread's backtrace, the site's own lines), once per request;
#   STATS 5 min: 1234 requests, 12 slow, p50 40 ms, p95 900 ms, max 4100 ms, …
#     every REPORT_SECONDS: the requests, Neo4j, the Katas' queue, threads,
#     the memory of this process and of the machine.
#
# Cheap enough to stay switched on: two clock readings per request and per
# Neo4j query, one thread that looks every few seconds.
module RequestDiagnostics
    SLOW_MS = 1000
    STUCK_SECONDS = 10
    REPORT_SECONDS = 300
    WATCH_SECONDS = 5
    KEY = :hs_request_diagnostics

    @mutex = Mutex.new
    @in_flight = {} # thread => { method:, path:, started:, reported: }
    @durations = []
    @neo4j_ms = 0.0
    @neo4j_count = 0
    @slow = 0

    class << self
        attr_accessor :queue # the Katas' queue, for STATS (KataQueue)

        def now = Process.clock_gettime(Process::CLOCK_MONOTONIC)

        # The counters of the request on this thread, or nil outside one.
        def current = Thread.current[KEY]

        def start(method, path)
            data = { :method => method, :path => path, :started => now, :neo4j_ms => 0.0, :neo4j_count => 0,
                     :slowest_ms => 0.0, :slowest => nil, :roster_ms => 0.0, :reported => false }
            Thread.current[KEY] = data
            @mutex.synchronize { @in_flight[Thread.current] = data }
            data
        end

        def finish(data, status)
            Thread.current[KEY] = nil
            ms = (now - data[:started]) * 1000
            @mutex.synchronize do
                @in_flight.delete(Thread.current)
                @durations << ms
                @neo4j_ms += data[:neo4j_ms]
                @neo4j_count += data[:neo4j_count]
                @slow += 1 if ms > SLOW_MS
            end
            return unless ms > SLOW_MS

            slowest = data[:slowest] ? " (slowest #{data[:slowest_ms].round} ms: #{data[:slowest]})" : ''
            STDERR.puts "SLOW #{ms.round} ms #{data[:method]} #{data[:path]} #{status} " \
                        "neo4j #{data[:neo4j_count]}× #{data[:neo4j_ms].round} ms#{slowest} roster lock #{data[:roster_ms].round} ms"
        end

        # How long a Neo4j query of this request took (Neo4jTiming).
        def neo4j(ms, query)
            data = current or return
            data[:neo4j_ms] += ms
            data[:neo4j_count] += 1
            return unless ms > data[:slowest_ms]

            data[:slowest_ms] = ms
            data[:slowest] = query.to_s.gsub(/\s+/, ' ').strip[0, 120]
        end

        # How long this request waited for the roster's lock (Main.load_invitations).
        def roster_wait(ms)
            data = current or return
            data[:roster_ms] += ms
        end

        def start_watch
            Thread.new do
                last_report = now
                loop do
                    sleep WATCH_SECONDS
                    begin
                        report_stuck
                        if now - last_report >= REPORT_SECONDS
                            report_stats(now - last_report)
                            last_report = now
                        end
                    rescue StandardError => e
                        STDERR.puts "RequestDiagnostics: #{e.class}: #{e.message}"
                    end
                end
            end
        end

        def report_stuck
            stuck = @mutex.synchronize do
                @in_flight.select { |_, d| !d[:reported] && now - d[:started] > STUCK_SECONDS }.each_value { |d| d[:reported] = true }
            end
            stuck.each do |thread, data|
                lines = (thread.backtrace || []).select { |l| l.include?('/src/ruby/') }.first(8)
                                                .map { |l| l.sub(%r{\A.*/src/ruby/}, '').sub(/:in /, ' ') }
                STDERR.puts "STUCK #{(now - data[:started]).round} s #{data[:method]} #{data[:path]} (thread #{thread.object_id}) " \
                            "neo4j #{data[:neo4j_count]}× #{data[:neo4j_ms].round} ms, at #{lines.empty? ? '?' : lines.join(' < ')}"
            end
        end

        def report_stats(seconds)
            durations, neo4j_ms, neo4j_count, slow, in_flight = @mutex.synchronize do
                taken = [@durations, @neo4j_ms, @neo4j_count, @slow, @in_flight.size]
                @durations = []
                @neo4j_ms = 0.0
                @neo4j_count = 0
                @slow = 0
                taken
            end
            sorted = durations.sort
            at = ->(q) { sorted.empty? ? 0 : sorted[[(sorted.size * q).ceil - 1, 0].max].round }
            queue = @queue ? ", kata queue #{@queue.waiting} waiting #{@queue.running} running" : ''
            STDERR.puts "STATS #{(seconds / 60).round} min: #{sorted.size} requests, #{slow} slow, " \
                        "p50 #{at.(0.5)} ms, p95 #{at.(0.95)} ms, max #{(sorted.last || 0).round} ms, #{in_flight} running now; " \
                        "neo4j #{neo4j_count} queries, #{neo4j_count.zero? ? 0 : (neo4j_ms / neo4j_count).round(1)} ms each#{queue}; " \
                        "#{Thread.list.size} threads, this process #{memory('/proc/self/status', 'VmRSS')}, " \
                        "machine #{memory('/proc/meminfo', 'MemAvailable')} available, load #{load}"
        end

        def memory(file, field)
            kb = File.read(file)[/^#{field}:\s+(\d+)/, 1].to_i
            kb < 1024 * 1024 ? "#{kb / 1024} MB" : "#{(kb / 1024.0 / 1024).round(1)} GB"
        rescue StandardError
            '?'
        end

        def load
            File.read('/proc/loadavg').split.first(3).join(' ')
        rescue StandardError
            '?'
        end
    end

    # Around every request (config.ru), outside Sinatra, so that its filters
    # count too. A WebSocket or a streamed download counts until its
    # response starts.
    class Middleware
        def initialize(app)
            @app = app
        end

        def call(env)
            data = RequestDiagnostics.start(env['REQUEST_METHOD'], env['PATH_INFO'])
            status = nil
            begin
                status, headers, body = @app.call(env)
                [status, headers, body]
            ensure
                RequestDiagnostics.finish(data, status || 'error')
            end
        end
    end

    # Every Neo4j query of a request, timed (prepended to Neo4jBolt, which
    # the site and its helpers include); a query inside another counts once.
    module Neo4jTiming
        def neo4j_query(query, data = {}, &block)
            return super if Thread.current[:hs_in_neo4j_query]

            Thread.current[:hs_in_neo4j_query] = true
            started = RequestDiagnostics.now
            begin
                super
            ensure
                Thread.current[:hs_in_neo4j_query] = false
                RequestDiagnostics.neo4j((RequestDiagnostics.now - started) * 1000, query)
            end
        end
    end
end
