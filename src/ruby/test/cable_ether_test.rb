require 'minitest/autorun'
require_relative '../include/cable_ether'

class CableEtherTest < Minitest::Test
    def setup
        @now = 1000.0
        @timers = []
        @ether = CableEther.new(:clock => -> { @now }, :later => ->(seconds, &block) { @timers << [seconds, block] })
        @inbox = Hash.new { |h, k| h[k] = [] }
    end

    def connect(name, teacher: false, address: nil)
        id = @ether.join(->(message) { @inbox[name] << message }, :teacher => teacher, :address => address)
        @ids ||= {}
        @ids[name] = id
        id
    end

    def send_as(name, message)
        @ether.receive(@ids[name], message.is_a?(String) ? message : JSON.generate(message))
    end

    def last(name, type)
        @inbox[name].reverse.find { |m| m[:t] == type }
    end

    def test_hello_says_how_many_lamps_and_only_teachers_get_the_words
        connect(:ada)
        connect(:teacher, :teacher => true)
        hello = last(:ada, 'hello')
        assert_equal 5, hello[:lamps]
        assert_equal false, hello[:teacher]
        assert_nil hello[:words]
        assert_equal CableEther::WORDS, last(:teacher, 'hello')[:words]
    end

    def test_words_are_letters_only_without_repeats
        words = CableEther::WORDS
        assert_equal words.uniq, words
        assert words.all? { |w| w.match?(/\A[A-Z]{5,11}\z/) }
        assert words.size.between?(80, 100)
    end

    def test_a_lamp_lights_on_its_frequency_only
        connect(:ada)
        connect(:bob)
        connect(:eve)
        send_as(:ada, { :t => 'tune', :f => 503 })
        send_as(:bob, { :t => 'tune', :f => 503 })
        send_as(:eve, { :t => 'tune', :f => 504 })
        @inbox.each_value(&:clear)

        send_as(:ada, { :t => 'lamp', :i => 3, :on => true })
        assert_equal [3], last(:bob, 'state')[:on]
        assert_equal [3], last(:ada, 'state')[:on]
        assert_nil last(:eve, 'state')

        send_as(:ada, { :t => 'lamp', :i => 3, :on => false })
        assert_equal [], last(:bob, 'state')[:on]
    end

    def test_a_lamp_stays_on_while_anyone_holds_it
        connect(:ada)
        connect(:bob)
        send_as(:ada, { :t => 'tune', :f => 420 })
        send_as(:bob, { :t => 'tune', :f => 420 })
        send_as(:ada, { :t => 'lamp', :i => 1, :on => true })
        send_as(:bob, { :t => 'lamp', :i => 1, :on => true })
        send_as(:ada, { :t => 'lamp', :i => 1, :on => false })
        assert_equal [1], @ether.on_lamps(420)
        send_as(:bob, { :t => 'lamp', :i => 1, :on => false })
        assert_equal [], @ether.on_lamps(420)
    end

    def test_tuning_away_leaving_and_release_let_go
        connect(:ada)
        connect(:bob)
        send_as(:ada, { :t => 'tune', :f => 777 })
        send_as(:bob, { :t => 'tune', :f => 777 })
        send_as(:ada, { :t => 'lamp', :i => 2, :on => true })
        send_as(:ada, { :t => 'tune', :f => 778 })
        assert_equal [], last(:bob, 'state')[:on]
        assert_equal [], last(:ada, 'state')[:on], 'the new frequency starts dark'

        send_as(:bob, { :t => 'lamp', :i => 4, :on => true })
        send_as(:bob, { :t => 'lamp', :i => 5, :on => true })
        send_as(:bob, { :t => 'release' })
        assert_equal [], @ether.on_lamps(777)

        send_as(:ada, { :t => 'tune', :f => 777 })
        send_as(:bob, { :t => 'lamp', :i => 1, :on => true })
        @ether.leave(@ids[:bob])
        assert_equal [], last(:ada, 'state')[:on]
        assert_equal [777], @ether.frequencies
    end

    def test_empty_frequencies_are_forgotten
        connect(:ada)
        send_as(:ada, { :t => 'tune', :f => 123 })
        send_as(:ada, { :t => 'tune', :f => 124 })
        assert_equal [124], @ether.frequencies
        @ether.leave(@ids[:ada])
        assert_equal [], @ether.frequencies
        assert_equal 0, @ether.client_count
    end

    def test_nonsense_is_ignored
        connect(:ada)
        send_as(:ada, { :t => 'tune', :f => 99 })
        send_as(:ada, { :t => 'tune', :f => '503' })
        send_as(:ada, { :t => 'tune', :f => 1000 })
        assert_equal [], @ether.frequencies
        send_as(:ada, { :t => 'tune', :f => 503 })
        send_as(:ada, { :t => 'lamp', :i => 6, :on => true })
        send_as(:ada, { :t => 'lamp', :i => '1', :on => true })
        send_as(:ada, 'not json')
        send_as(:ada, '[1, 2]')
        send_as(:ada, JSON.generate({ :t => 'lamp', :i => 1, :on => true, :x => 'y' * 300 }))
        assert_equal [], @ether.on_lamps(503)
    end

    def test_only_teachers_change_the_lamps_or_watch
        connect(:ada)
        send_as(:ada, { :t => 'lamps', :n => 2, :delay => 0 })
        send_as(:ada, { :t => 'watch' })
        assert_equal 5, @ether.lamps
        assert_equal 'teachers_only', last(:ada, 'error')[:reason]
        assert_nil last(:ada, 'overview')
    end

    def test_a_change_without_delay_applies_at_once_and_drops_higher_lamps
        connect(:ada)
        connect(:bob)
        connect(:teacher, :teacher => true)
        send_as(:ada, { :t => 'tune', :f => 300 })
        send_as(:bob, { :t => 'tune', :f => 300 })
        send_as(:ada, { :t => 'lamp', :i => 5, :on => true })
        send_as(:ada, { :t => 'lamp', :i => 2, :on => true })

        send_as(:teacher, { :t => 'lamps', :n => 4, :delay => 0 })
        assert_equal 4, @ether.lamps
        assert_equal 4, last(:bob, 'lamps')[:n]
        assert_equal [2], last(:bob, 'state')[:on]
        send_as(:ada, { :t => 'lamp', :i => 5, :on => true })
        assert_equal [2], @ether.on_lamps(300)
    end

    def test_a_scheduled_change_counts_down_and_can_be_cancelled
        connect(:ada)
        connect(:teacher, :teacher => true)
        send_as(:teacher, { :t => 'lamps', :n => 3, :delay => 180 })
        assert_equal({ :t => 'pending', :n => 3, :in => 180.0 }, last(:ada, 'pending'))
        assert_equal 5, @ether.lamps
        assert_equal 180, @timers.last[0]

        @now += 60
        connect(:late)
        assert_equal({ :n => 3, :in => 120.0 }, last(:late, 'hello')[:pending])
        @ether.tick
        assert_equal 5, @ether.lamps, 'too early'

        send_as(:teacher, { :t => 'cancel' })
        assert_equal({ :t => 'pending', :n => nil }, last(:ada, 'pending'))
        @now += 120
        @timers.last[1].call
        assert_equal 5, @ether.lamps, 'cancelled'
    end

    def test_a_scheduled_change_applies_when_its_time_comes
        connect(:ada)
        connect(:teacher, :teacher => true)
        send_as(:teacher, { :t => 'lamps', :n => 3, :delay => 10 })
        send_as(:teacher, { :t => 'lamps', :n => 2, :delay => 20 })
        @now += 10
        @timers[0][1].call
        assert_equal 5, @ether.lamps, 'the first change was replaced'
        @now += 10
        @timers[1][1].call
        assert_equal 2, @ether.lamps
        assert_equal 2, last(:ada, 'lamps')[:n]
        assert_nil @ether.pending
    end

    def test_delay_is_capped
        connect(:teacher, :teacher => true)
        send_as(:teacher, { :t => 'lamps', :n => 1, :delay => 99_999 })
        assert_equal 300, @timers.last[0]
        send_as(:teacher, { :t => 'lamps', :n => 9, :delay => 0 })
        assert_equal 5, @ether.lamps
    end

    def test_teachers_watch_the_frequencies
        connect(:teacher, :teacher => true)
        connect(:ada)
        send_as(:teacher, { :t => 'watch' })
        send_as(:ada, { :t => 'tune', :f => 503 })
        send_as(:ada, { :t => 'lamp', :i => 1, :on => true })
        overview = last(:teacher, 'overview')
        assert_equal 2, overview[:clients]
        assert_equal [{ :f => 503, :people => 1, :on => [1] }], overview[:freqs]
    end

    def test_holding_lamps_is_rate_limited_but_letting_go_is_not
        connect(:ada)
        send_as(:ada, { :t => 'tune', :f => 600 })
        sent = 0
        200.times do
            send_as(:ada, { :t => 'lamp', :i => 1, :on => true })
            send_as(:ada, { :t => 'lamp', :i => 1, :on => false })
        end
        sent = @inbox[:ada].count { |m| m[:t] == 'state' && m[:on] == [1] }
        assert_operator sent, :<=, CableEther::BURST
        send_as(:ada, { :t => 'lamp', :i => 1, :on => false })
        assert_equal [], @ether.on_lamps(600)
        @now += 1
        send_as(:ada, { :t => 'lamp', :i => 2, :on => true })
        assert_equal [2], @ether.on_lamps(600)
    end

    def test_connections_are_capped_in_total_and_per_address
        ether = CableEther.new(:max_clients => 3, :max_per_address => 2)
        inbox = []
        out = ->(message) { inbox << message }
        assert ether.join(out, :address => 'a')
        assert ether.join(out, :address => 'a')
        assert_nil ether.join(out, :address => 'a')
        assert_equal 'full', inbox.last[:reason]
        id = ether.join(out, :address => 'b')
        assert id
        assert_nil ether.join(out, :address => 'c')
        ether.leave(id)
        assert ether.join(out, :address => 'c')
    end

    def test_ping_gets_a_pong
        connect(:ada)
        send_as(:ada, { :t => 'ping' })
        assert last(:ada, 'pong')
    end
end
