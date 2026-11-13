require 'minitest/autorun'
require 'tmpdir'
require 'json'
require_relative '../include/phone_calls'

# The calls of the Telefonspiel for /statistik (PhoneCalls): the line's
# lines are added up per hour and game, and the files go; nothing but
# numbers reaches the database, and nothing about who called.
class PhoneCallsTest < Minitest::Test
    HOUR = Time.utc(2026, 11, 13, 9).to_i

    class FakeNeo4j
        attr_reader :queries

        def initialize = @queries = []

        def neo4j_query(query, params = {})
            @queries << [query, params]
            []
        end
    end

    def line(games, line: 'telefon', seconds: 60, hour: HOUR)
        JSON.generate('hour' => hour, 'line' => line, 'seconds' => seconds, 'games' => games)
    end

    def test_only_well_formed_lines_count
        assert PhoneCalls.parse(line(['123']))
        assert PhoneCalls.parse(line([]))
        assert_nil PhoneCalls.parse(line(['123'], :line => 'pager'))
        assert_nil PhoneCalls.parse(line(['12']))
        assert_nil PhoneCalls.parse(line(['123'], :seconds => 99_999))
        assert_nil PhoneCalls.parse(line(['123'], :hour => HOUR + 5))
        assert_nil PhoneCalls.parse('{"hour": 1, "caller": "+49301234"}')
        assert_nil PhoneCalls.parse('not json')
    end

    def test_a_forwarded_call_counts_once_per_game_and_once_as_a_call
        entries = [line(['123', '204']), line(['123']), line([]), line(['204'], :line => 'browser', :seconds => 30)].map { |l| PhoneCalls.parse(l) }
        hours, games = PhoneCalls.tally(entries)
        assert_equal({ 'hour' => HOUR, 'line' => 'telefon', 'calls' => 3, 'seconds' => 180 }, hours.find { |h| h['line'] == 'telefon' })
        count = ->(game, l = 'telefon') { games.find { |g| g['game'] == game && g['line'] == l }&.dig('calls') }
        assert_equal 2, count.('123')
        assert_equal 1, count.('204')
        assert_equal 1, count.('')
        assert_equal 1, count.('204', 'browser')
    end

    def test_collecting_writes_numbers_and_deletes_the_files
        Dir.mktmpdir do |dir|
            File.write(File.join(dir, 'telefon.jsonl'), [line(['123']), line(['123']), 'broken'].join("\n") + "\n")
            File.write(File.join(dir, 'browser.jsonl.1.collecting'), line([], :line => 'browser') + "\n")
            neo4j = FakeNeo4j.new
            PhoneCalls.collect(neo4j, :dir => dir, :now => HOUR + 600)
            assert_empty Dir.children(dir)
            rows = neo4j.queries.flat_map { |_, p| p[:rows] || [] }
            assert_equal 2, rows.find { |r| r['line'] == 'telefon' && !r.key?('game') }['calls']
            assert_equal 1, rows.find { |r| r['line'] == 'browser' && r['game'] == '' }['calls']
            # numbers only: hour, line, game, calls, seconds
            assert rows.all? { |r| (r.keys - %w[hour line game calls seconds]).empty? }
        end
    end

    def test_a_crashed_game_keeps_its_newest_error_for_its_author
        Dir.mktmpdir do |dir|
            crash = { 'game' => '741', 'text' => "start.rb:8: syntax error, unexpected `or'" }
            File.write(File.join(dir, 'telefon.jsonl'),
                       JSON.generate('hour' => HOUR, 'line' => 'telefon', 'seconds' => 3, 'games' => ['741'], 'errors' => [crash, { 'game' => 'x' }]) + "\n")
            neo4j = FakeNeo4j.new
            PhoneCalls.collect(neo4j, :dir => dir, :now => HOUR + 600)
            query, params = neo4j.queries.find { |q, _| q.include?('last_error') }
            assert_includes query, "state: 'published'"
            assert_equal ['741', crash['text'], HOUR, 1], params.values_at(:number, :text, :at, :count)
        end
        # an error line is kept short
        long = PhoneCalls.parse(JSON.generate('hour' => HOUR, 'line' => 'browser', 'seconds' => 1, 'games' => [],
                                              'errors' => [{ 'game' => '123', 'text' => 'x' * 5000 }]))
        assert_equal PhoneCalls::MAX_ERROR, long['errors'].first['text'].size
    end

    def test_the_page_shows_calls_per_day_and_per_game
        now = HOUR + 3600
        phone = [{ :hour => HOUR, :line => 'telefon', :calls => 3, :seconds => 180 }, { :hour => HOUR, :line => 'browser', :calls => 1, :seconds => 30 }]
        games = [{ :hour => HOUR, :line => 'telefon', :game => '123', :calls => 2 }, { :hour => HOUR, :line => 'browser', :game => '123', :calls => 1 },
                 { :hour => HOUR, :line => 'telefon', :game => '', :calls => 1 }]
        summary = UsageStats.summary([], :days => 7, :now => now, :phone => phone, :phone_games => games)
        today = summary[:phone].last
        assert_equal 4, today[:people]
        assert_equal({ 'telefon' => 3, 'browser' => 1 }, today[:lines])
        assert_equal %w[123], summary[:phone_games].map { |g| g[:game] }.first(1)
        html = UsageStats.phone_html(summary, { '123' => 'Die Schatzsuche' })
        assert_includes html, 'Die Schatzsuche'
        assert_includes html, 'kein Spiel gewählt'
        assert_includes html, '4 Anrufe'
        assert_includes UsageStats.course_html(summary, nil), 'Anrufe beim Telefonspiel'
    end
end
