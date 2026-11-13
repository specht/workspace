require 'json'
require_relative 'workspace_usage'
require_relative 'phone_games'
require_relative 'usage_stats'

# Calls of the Telefonspiel for /statistik: the line in the browser
# (src/phone/line/line.py) and the real line (src/phone/runner/runner.py)
# write one line per call into /internal/phone-calls (record_call in
# src/phone/core/game.py): the hour it began, the line, how long it took,
# the numbers of the games it reached. Every five minutes the website adds
# them up per hour and deletes the files: in the database are only numbers
# per hour and line (PhoneHour: calls, seconds) and per hour, line and game
# (PhoneGameHour: calls), KEEP_DAYS long. Never who called.
module PhoneCalls
    DIR = '/internal/phone-calls'
    LINES = UsageStats::PHONE_LINES
    KEEP_DAYS = WorkspaceUsage::KEEP_DAYS
    # a call is at most a quarter of an hour (GAME_SECONDS); more is a
    # broken line
    MAX_SECONDS = 3600

    # One line of a file as an entry, or nil.
    def self.parse(line)
        e = JSON.parse(line)
        return nil unless e.is_a?(Hash) && LINES.key?(e['line']) && e['hour'].is_a?(Integer) && e['hour'] % 3600 == 0
        return nil unless e['seconds'].is_a?(Integer) && e['seconds'].between?(0, MAX_SECONDS)
        return nil unless e['games'].is_a?(Array) && e['games'].all? { |g| g.is_a?(String) && g.match?(/\A\d{3,4}\z/) }

        e
    rescue JSON::ParserError
        nil
    end

    # The entries added up: [per hour and line, per hour, line and game];
    # a call that reached no game counts at the game '' (only the menu).
    def self.tally(entries)
        hours = Hash.new { |h, k| h[k] = { :calls => 0, :seconds => 0 } }
        games = Hash.new(0)
        entries.each do |e|
            key = [e['hour'], e['line']]
            hours[key][:calls] += 1
            hours[key][:seconds] += e['seconds']
            (e['games'].empty? ? [''] : e['games'].uniq).each { |g| games[key + [g]] += 1 }
        end
        [hours.map { |(hour, line), v| { 'hour' => hour, 'line' => line, 'calls' => v[:calls], 'seconds' => v[:seconds] } },
         games.map { |(hour, line, game), n| { 'hour' => hour, 'line' => line, 'game' => game, 'calls' => n } }]
    end

    # The files into the database, then away. A file is renamed first, so
    # that the line writes the next calls into a new one; one left renamed
    # by an interrupted run is taken the next time.
    def self.collect(neo4j, dir: DIR, now: Time.now.to_i)
        return unless File.directory?(dir)

        Dir[File.join(dir, '*.jsonl')].each { |f| File.rename(f, "#{f}.#{now}.collecting") }
        files = Dir[File.join(dir, '*.collecting')]
        entries = files.flat_map { |f| File.readlines(f, chomp: true).filter_map { |l| parse(l) } }
        hours, games = tally(entries)
        unless hours.empty?
            neo4j.neo4j_query(<<~END_OF_QUERY, :rows => hours)
                UNWIND $rows AS r
                MERGE (h:PhoneHour {hour: r.hour, line: r.line})
                SET h.calls = coalesce(h.calls, 0) + r.calls, h.seconds = coalesce(h.seconds, 0) + r.seconds;
            END_OF_QUERY
            neo4j.neo4j_query(<<~END_OF_QUERY, :rows => games)
                UNWIND $rows AS r
                MERGE (h:PhoneGameHour {hour: r.hour, line: r.line, game: r.game})
                SET h.calls = coalesce(h.calls, 0) + r.calls;
            END_OF_QUERY
        end
        files.each { |f| File.delete(f) }
        before = now - KEEP_DAYS * 86_400
        neo4j.neo4j_query('MATCH (h:PhoneHour) WHERE h.hour < $before DELETE h;', :before => before)
        neo4j.neo4j_query('MATCH (h:PhoneGameHour) WHERE h.hour < $before DELETE h;', :before => before)
    end

    # The titles of the published games by number (published.json of the line)
    def self.titles(path = File.join(PhoneGames::ROOT, 'published.json'))
        games = (JSON.parse(File.read(path))['games'] rescue nil)
        Array(games).select { |g| g.is_a?(Hash) }.to_h { |g| [g['number'].to_s, g['title'].to_s] }
    end
end
