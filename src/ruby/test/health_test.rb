require 'minitest/autorun'
require_relative '../include/health'

class HealthTest < Minitest::Test
    NOW = Time.new(2026, 10, 19, 12, 0, 0).to_i

    def test_backup_is_unknown_until_it_reports_and_bad_when_too_old
        assert_equal :unknown, Health.backup(nil, :now => NOW, :max_hours => 26).state
        fresh = Health.backup(NOW - 5 * 3600, :now => NOW, :max_hours => 26)
        assert_equal :ok, fresh.state
        assert_includes fresh.text, 'vor 5 Stunden'
        stale = Health.backup(NOW - 30 * 3600, :now => NOW, :max_hours => 26)
        assert_equal :bad, stale.state
        assert_includes stale.text, 'Älter als 26 Stunden'
    end

    def test_starts_warn_on_a_failure_and_are_bad_when_many_fail
        ok = ->(ago, seconds) { { :at => NOW - ago, :ok => true, :seconds => seconds } }
        failed = ->(ago) { { :at => NOW - ago, :ok => false, :seconds => 1 } }
        since = NOW - 7 * 86_400
        assert_equal :ok, Health.starts([], :now => NOW, :since => since).state
        good = Health.starts([ok.(60, 4), ok.(120, 6), ok.(180, 20)], :now => NOW, :since => since)
        assert_equal :ok, good.state
        assert_includes good.text, '3 Starts in 24 Stunden, keiner fehlgeschlagen'
        assert_includes good.text, 'Median 6.0 s, höchstens 20 s'
        assert_equal :warn, Health.starts([ok.(60, 4), failed.(100)], :now => NOW, :since => since).state
        many = Array.new(10) { ok.(60, 4) } + [failed.(10), failed.(20), failed.(30)]
        assert_equal :bad, Health.starts(many, :now => NOW, :since => since).state
        # Older than a day does not count.
        assert_equal :ok, Health.starts([failed.(90_000)], :now => NOW, :since => since).state
        # The history begins with the server's start.
        assert_includes Health.starts([ok.(60, 4)], :now => NOW, :since => NOW - 3600).text, 'seit dem Neustart um'
    end

    def test_a_queue_waiting_long_is_bad
        assert_equal :ok, Health.queue([], :running => 0, :now => NOW).state
        assert_equal :ok, Health.queue([NOW - 10], :running => 1, :now => NOW).state
        assert_equal :warn, Health.queue([NOW - 90, NOW - 5], :running => 1, :now => NOW).state
        bad = Health.queue([NOW - 400], :running => 1, :now => NOW)
        assert_equal :bad, bad.state
        assert_includes bad.text, '1 Start wartet'
    end

    def test_hourly_jobs_late_or_missing
        since = NOW - 86_400
        assert_equal :ok, Health.jobs(NOW - 1800, NOW - 600, :now => NOW, :since => since).state
        assert_equal :bad, Health.jobs(NOW - 3 * 3600, NOW - 600, :now => NOW, :since => since).state
        assert_equal :bad, Health.jobs(NOW - 1800, nil, :now => NOW, :since => since).state
        # Just started: not yet run is fine.
        assert_equal :ok, Health.jobs(nil, nil, :now => NOW, :since => NOW - 300).state
    end

    def test_mails_once_a_day_for_what_stops_lessons
        checks = [
            Health.probe('mysql', 'MySQL', 'timeout'),
            Health.probe('neo4j', 'Neo4j', nil),
            Health::Check.new(:key => 'queue', :label => 'Warteschlange', :state => :bad, :text => ''),
            Health.backup(NOW - 40 * 3600, :now => NOW, :max_hours => 26),
        ]
        assert_equal %w[mysql backup], Health.to_mail(checks, {}, :today => '2026-10-19').map(&:key)
        assert_equal %w[backup], Health.to_mail(checks, { 'mysql' => '2026-10-19' }, :today => '2026-10-19').map(&:key)
        assert_equal :bad, Health.worst(checks)
        assert_equal :ok, Health.worst([])
    end
end
