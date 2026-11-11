require 'minitest/autorun'
require_relative '../include/server_load'

# The server's load for /statistik (ServerLoad): what is read from the
# machine, what goes into the database and what the page shows.
class ServerLoadTest < Minitest::Test
    NOW = Time.new(2026, 11, 7, 12).to_i
    MEMINFO = "MemTotal:       64000000 kB\nMemFree:         1000000 kB\nMemAvailable:   48000000 kB\n"
    DF = <<~END_OF_DF
        Filesystem     1024-blocks      Used Available Capacity Mounted on
        overlay          200000000 120000000  70000000      64% /
        /dev/sdb1        500000000 440000000  20000000      96% /user
    END_OF_DF

    def test_the_processors_count_from_one_look_to_the_next
        before = ServerLoad.cpu_times("cpu  100 0 100 700 100 0 0 0 0 0\ncpu0 1 2 3\n")
        after = ServerLoad.cpu_times("cpu  400 0 200 1300 100 0 0 0 0 0\n")
        assert_equal [200, 1000], before
        # 400 more busy of 1000 more in all; waiting for the disk is idle
        assert_equal 40.0, ServerLoad.cpu_percent(before, after)
        assert_nil ServerLoad.cpu_percent(nil, after)
        assert_nil ServerLoad.cpu_percent(after, after)
    end

    def test_memory_in_use_is_what_is_not_available
        assert_equal [16_000_000 * 1024, 64_000_000 * 1024], ServerLoad.memory(MEMINFO)
    end

    def test_the_disks_count_the_reserved_blocks_as_used
        disks = ServerLoad.disks(DF)
        assert_equal [130_000_000 * 1024, 200_000_000 * 1024], disks['/']
        assert_equal [480_000_000 * 1024, 500_000_000 * 1024], disks['/user']
    end

    def test_the_first_look_after_a_start_has_no_processors
        stats = ["cpu  100 0 100 700 100 0 0 0\n", "cpu  400 0 200 1300 100 0 0 0\n"]
        reader = ServerLoad::Reader.new(:stat => -> { stats.shift }, :meminfo => -> { MEMINFO }, :df => -> { DF })
        first = reader.read
        assert_nil first[:cpu]
        assert_equal 500_000_000 * 1024, first[:user_total]
        assert_equal 40.0, reader.read[:cpu]
        refute_includes ServerLoad.store_query(first), 'cpu'
        refute_includes ServerLoad.sample_query(first), 'cpu'
    end

    def test_an_hour_adds_up_and_keeps_its_maxima
        sample = { :cpu => 12.5, :ram_used => 1, :ram_total => 2, :root_used => 1, :root_total => 2, :user_used => nil, :user_total => nil }
        query = ServerLoad.store_query(sample)
        assert_includes query, 'MERGE (h:ServerHour {hour: $hour})'
        assert_includes query, 'h.cpu_sum = coalesce(h.cpu_sum, 0) + $cpu'
        assert_includes query, 'h.root_used = $root_used'
        refute_includes query, 'user_used'
        assert_equal 'CREATE (:ServerSample {ts: $ts, ram_used: $ram_used, ram_total: $ram_total, cpu: $cpu, root_used: $root_used, root_total: $root_total});',
                     ServerLoad.sample_query(sample)
    end

    def hour(days_ago, hour_of_day, cpu, samples: 12, max: nil, ram: 32)
        t = Time.at(NOW - days_ago * 86_400)
        { :hour => Time.new(t.year, t.month, t.day, hour_of_day).to_i, :cpu_sum => cpu * samples, :cpu_samples => samples, :cpu_max => max || cpu,
          :ram_max => ram * 1e9, :ram_total => 64e9, :root_used => 100e9, :root_total => 200e9, :user_used => 450e9, :user_total => 500e9 }
    end

    def test_a_day_shows_its_busiest_hour_its_mean_and_the_most_memory
        hours = [hour(1, 9, 10.0, :ram => 16), hour(1, 10, 50.0, :max => 90.0, :ram => 48), hour(1, 11, 30.0, :samples => 6)]
        days = ServerLoad.periods(hours, 7, NOW)
        assert_equal 7, days.size
        day = days[-2]
        assert_equal 50.0, day[:cpu_hour]
        assert_equal 90.0, day[:cpu_max]
        # (120 + 600 + 180) / 30 looks
        assert_equal 30.0, day[:cpu_mean]
        assert_equal 75.0, ServerLoad.share(day[:ram])
        assert_equal 90.0, ServerLoad.share(day[:user])
        refute days.first[:counted]
        assert_nil days.first[:cpu_hour]
        # a year goes by calendar weeks
        assert(ServerLoad.periods(hours, 365, NOW).all? { |p| p[:from].monday? })
    end

    def test_the_rhythm_is_the_mean_per_weekday_and_hour
        rhythm = ServerLoad.rhythm([hour(1, 10, 20.0), hour(8, 10, 40.0)])
        friday = Time.at(NOW - 86_400)
        assert_equal 30.0, rhythm[(friday.wday + 6) % 7][10]
        assert_nil rhythm[0][3]
    end

    def test_the_page_links_the_days_with_their_looks
        today = Date.new(2026, 11, 7)
        periods = ServerLoad.periods([hour(1, 10, 50.0)], 30, NOW)
        html = ServerLoad.html(periods, :since => NOW - 86_400, :today => today)
        assert_includes html, 'Festplatte der Workspaces'
        assert_includes html, "href='/statistik?server=2026-11-06'"
        # days without a look are no links
        refute_includes html, "href='/statistik?server=2026-11-05'"
        assert_includes ServerLoad.html([], :since => nil, :today => today), 'Noch keine Zahlen'
        # the processors stand in the course above, beside the students
        title, _, value, tip = ServerLoad::CHARTS.first
        assert_equal 'Prozessoren', title
        day = ServerLoad.columns(periods, today, value, tip).find { |c| c[:href] }
        assert_includes day[:tip], 'in der vollsten Stunde 50,0 %'
        assert_equal '/statistik?server=2026-11-06', day[:href]
    end

    def test_a_day_in_five_minute_steps
        day = Date.new(2026, 11, 6)
        start = Time.new(2026, 11, 6).to_i
        samples = [{ :ts => start + 10 * 3600 + 60, :cpu => 42.0, :ram_used => 32e9, :ram_total => 64e9, :root_used => 1e9, :root_total => 2e9 }]
        html = ServerLoad.day_html(day, samples, :today => Date.new(2026, 11, 7))
        assert_equal 2 * 288, html.scan("class='usage-col").size
        assert_includes html, '10:00 Uhr: 42,0 %'
        assert_includes html, '10:00 Uhr: 32,0 GB von 64,0 GB (50,0 %)'
        assert_includes html, "href='/statistik?server=2026-11-07'"
        assert_includes html, "href='/statistik?server=2026-11-05'"
        assert_includes ServerLoad.day_html(day, [], :today => day), 'keine Messungen'
    end
end
