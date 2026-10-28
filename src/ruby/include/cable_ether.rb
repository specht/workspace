require 'json'
require 'set'

# The ether of »Datenübertragung per Funk« (/radio, /ether): one public room
# for the whole installation, as in the original cable simulator. Everyone
# picks a frequency (10.0 to 99.9 kHz, kept in tenths: 100 to 999) and sees
# the lamps of that frequency; a lamp is on while at least one person on the
# frequency holds its button. Teachers set how many lamps there are, now or
# after a countdown that every client shows.
#
# Nothing is stored: the state lives in this object and goes with a restart.
# The web side (cable_page.rb) hands in each connection with a block that
# sends a message to it; every change sends the new state to whoever needs
# it, inside the lock, so the messages of one frequency arrive in order.
#
# Messages from a client (JSON, at most MAX_MESSAGE_BYTES):
#   { t: 'tune', f: 503 }           listen and send on 50.3 kHz
#   { t: 'lamp', i: 3, on: true }   hold (on) or let go of lamp 3
#   { t: 'release' }                let go of every lamp (blur, hidden tab)
#   { t: 'ping' }                   keeps the connection open (nginx ends a silent one after a minute)
#   teachers only:
#   { t: 'watch' }                  the list of frequencies, from now on
#   { t: 'lamps', n: 4, delay: 180 } four lamps in three minutes (delay 0: now)
#   { t: 'cancel' }                 forget the pending change
# Messages to a client:
#   { t: 'hello', lamps:, pending:, teacher:, words: (teachers only) }
#   { t: 'state', f:, on: [1, 3] }  the lamps that are on, on your frequency
#   { t: 'lamps', n: }              the number of lamps now
#   { t: 'pending', n:, in: } / { t: 'pending', n: nil }
#   { t: 'overview', clients:, freqs: [{ f:, people:, on: }] }   teachers who watch
#   { t: 'pong' }, { t: 'error', reason: }
class CableEther
    MAX_LAMPS = 5
    FREQ_MIN = 100
    FREQ_MAX = 999
    MAX_DELAY = 300
    MAX_MESSAGE_BYTES = 256
    # Holding and letting go of lamps and turning the dial: a burst of
    # BURST, then RATE per second. Letting go always passes, so a lamp never
    # stays on because a message was dropped.
    BURST = 60
    RATE = 25.0
    MAX_CLIENTS = 2000
    # A school is one address to the outside: several classes with a phone
    # and a laptop each.
    MAX_PER_ADDRESS = 300

    # The words the teacher hands out, one per round: made up, so nobody can
    # guess the rest from the first letters; only A to Z, 5 to 11 letters,
    # a few with double letters (»KWIBBLE«, »FROZZLEBOP«), which a protocol has to tell
    # apart from one letter sent twice by mistake.
    WORDS = %w[
        ALGORUNE ARCADENIX ARCANFORGE BINARYTHR BITSHIFTUS BLIPZOR BLORBMAX BLORP
        BLUNDERWYRM BOOMSHACK BOSSRUSHER BOUNTYHUNT BUGSORCER BYTEFORGE CACHEWYLD
        CHIPMAGUS COREHEXUS CRUNCHIFY CRYPTOVM CRYPTOWARD CYBERMANC DAEMONKEY
        DARKFYRE DATAWYRM DEBUGORC DRACOMANC DUNGEONEX ELDRISPELL FELWRAITH FIZBO
        FLIBBERDAB FLINKUS FLOOBERWINK FRAMEWIZ FROZZLEBOP GAMEMANCER GLARV
        GLIMMO GLITCHIFY GLITCHMAGE GLITCHPORT GROBNIK HASHFYRE HYPERSPIN
        IMPULSEZAP JOYSTORM KABOOMZAP KRYZO KWIBBLE LEVELWARP LITCHARM LOOTSCROLL
        MAGELORD MEEPZOR MEGAFRAZZLE MEGAZORP MIMICGOLD MOXIUM MYTHICKEY
        NECROVAULT NEURALHEX PIXELFORGE PIXELQUEST PORTALRAM POWERUPIA QUABBIT
        QUAZZLEPOP QUIXL QUOBEL RETROFYRE RUNEBLADE SAVEGEM SHADOWHEX SHIELDBOOST
        SKELREIGN SNORFY SPLATZOID SPRITECODE SYSCALLIX THUNDABLAST TILESHIFT
        TRAXUL VECTORIUM VORPIX VRUNK WIZZLEFUZZ WUBEX WYVERNAIR XPBOOSTUS ZAPTOR
        ZORPLEX ZYMOX
    ].freeze

    Client = Struct.new(:id, :out, :teacher, :address, :freq, :held, :watching, :tokens, :tokens_at)

    attr_reader :lamps

    # clock: seconds as a Float; later: runs a block after so many seconds
    # (a thread; the tests collect the blocks and call them).
    def initialize(clock: -> { Time.now.to_f },
                   later: ->(seconds, &block) { Thread.new { sleep(seconds); block.call } },
                   max_clients: MAX_CLIENTS, max_per_address: MAX_PER_ADDRESS)
        @clock = clock
        @later = later
        @max_clients = max_clients
        @max_per_address = max_per_address
        @mutex = Mutex.new
        @clients = {}
        @tuned = {}
        @per_address = Hash.new(0)
        @lamps = MAX_LAMPS
        @pending = nil
        @next_id = 0
    end

    # A new connection; out.call(hash) sends a message to it. Returns its id,
    # or nil when the ether is full (the client then hears why).
    def join(out, teacher: false, address: nil)
        @mutex.synchronize do
            full = @clients.size >= @max_clients ||
                (address && @per_address[address] >= @max_per_address)
            if full
                say(out, { :t => 'error', :reason => 'full' })
                return nil
            end
            @next_id += 1
            client = Client.new(@next_id, out, teacher == true, address, nil, Set.new, false, BURST.to_f, @clock.call)
            @clients[client.id] = client
            @per_address[address] += 1 if address
            hello = { :t => 'hello', :lamps => @lamps, :pending => pending_message_fields, :teacher => client.teacher }
            hello[:words] = WORDS if client.teacher
            say(out, hello)
            overview_changed
            client.id
        end
    end

    def leave(id)
        @mutex.synchronize do
            client = @clients.delete(id)
            next unless client

            if client.address
                @per_address[client.address] -= 1
                @per_address.delete(client.address) if @per_address[client.address] <= 0
            end
            untune(client)
            overview_changed
        end
    end

    # A message as the client sent it (a String).
    def receive(id, data)
        data = data.to_s
        return if data.bytesize > MAX_MESSAGE_BYTES

        message = begin
            JSON.parse(data)
        rescue JSON::ParserError
            nil
        end
        return unless message.is_a?(Hash)

        @mutex.synchronize do
            client = @clients[id]
            next unless client

            case message['t']
            when 'tune'
                tune(client, message['f']) if spend(client)
            when 'lamp'
                lamp(client, message['i'], message['on'] == true)
            when 'release'
                release(client)
            when 'ping'
                say(client.out, { :t => 'pong' })
            when 'watch'
                next refuse(client) unless client.teacher

                client.watching = true
                say(client.out, overview)
            when 'lamps'
                next refuse(client) unless client.teacher

                schedule(message['n'], message['delay'])
            when 'cancel'
                next refuse(client) unless client.teacher

                cancel
            end
        end
    end

    # Applies a pending change whose time has come; the timer of schedule
    # calls it, and calling it early does nothing.
    def tick
        @mutex.synchronize do
            next unless @pending && @clock.call >= @pending[:at] - 0.05

            n = @pending[:n]
            @pending = nil
            apply(n)
        end
    end

    # For the tests and the teacher's list.
    def pending
        @mutex.synchronize { @pending && @pending.dup }
    end

    def on_lamps(freq)
        @mutex.synchronize { lamps_on(freq) }
    end

    def people(freq)
        @mutex.synchronize { (@tuned[freq] || Set.new).size }
    end

    def client_count
        @mutex.synchronize { @clients.size }
    end

    def frequencies
        @mutex.synchronize { @tuned.keys.sort }
    end

    private

    def say(out, message)
        out.call(message)
    rescue StandardError
        nil
    end

    def refuse(client)
        say(client.out, { :t => 'error', :reason => 'teachers_only' })
    end

    # One message from the budget of a client; false when it is used up.
    def spend(client)
        now = @clock.call
        client.tokens = [BURST.to_f, client.tokens + (now - client.tokens_at) * RATE].min
        client.tokens_at = now
        return false if client.tokens < 1

        client.tokens -= 1
        true
    end

    def valid_freq?(freq)
        freq.is_a?(Integer) && freq >= FREQ_MIN && freq <= FREQ_MAX
    end

    def tune(client, freq)
        return unless valid_freq?(freq)
        return if client.freq == freq

        untune(client)
        client.freq = freq
        (@tuned[freq] ||= Set.new) << client.id
        say(client.out, state_message(freq))
        overview_changed
    end

    # Off the current frequency: what the client held goes off there.
    def untune(client)
        freq = client.freq
        return unless freq

        had_lamps = !client.held.empty?
        client.held.clear
        client.freq = nil
        members = @tuned[freq]
        members&.delete(client.id)
        if members.nil? || members.empty?
            @tuned.delete(freq)
        elsif had_lamps
            broadcast_state(freq)
        end
    end

    def lamp(client, index, on)
        return unless client.freq && index.is_a?(Integer) && index >= 1 && index <= @lamps

        before = lamps_on(client.freq)
        if on
            return unless spend(client)

            client.held << index
        else
            client.held.delete(index)
        end
        return if lamps_on(client.freq) == before

        broadcast_state(client.freq)
        overview_changed
    end

    def release(client)
        return if client.held.empty?

        client.held.clear
        broadcast_state(client.freq) if client.freq
        overview_changed
    end

    def lamps_on(freq)
        on = Set.new
        (@tuned[freq] || []).each { |id| on.merge(@clients[id].held) }
        on.to_a.sort
    end

    def state_message(freq)
        { :t => 'state', :f => freq, :on => lamps_on(freq) }
    end

    def broadcast_state(freq)
        message = state_message(freq)
        (@tuned[freq] || []).each { |id| say(@clients[id].out, message) }
    end

    def broadcast(message)
        @clients.each_value { |client| say(client.out, message) }
    end

    def pending_message_fields
        return nil unless @pending

        { :n => @pending[:n], :in => [(@pending[:at] - @clock.call).round(1), 0].max }
    end

    def schedule(n, delay)
        return unless n.is_a?(Integer) && n >= 1 && n <= MAX_LAMPS

        delay = delay.is_a?(Numeric) ? delay.to_f.clamp(0, MAX_DELAY) : 0.0
        if delay <= 0
            @pending = nil
            broadcast({ :t => 'pending', :n => nil })
            apply(n)
            return
        end
        @pending = { :n => n, :at => @clock.call + delay }
        broadcast({ :t => 'pending' }.merge(pending_message_fields))
        overview_changed
        @later.call(delay) { tick }
    end

    def cancel
        return unless @pending

        @pending = nil
        broadcast({ :t => 'pending', :n => nil })
        overview_changed
    end

    # Fewer lamps: what was held above the new number goes off.
    def apply(n)
        @lamps = n
        changed = []
        @tuned.each do |freq, ids|
            before = lamps_on(freq)
            ids.each { |id| @clients[id].held.delete_if { |i| i > n } }
            changed << freq if lamps_on(freq) != before
        end
        broadcast({ :t => 'lamps', :n => n })
        changed.each { |freq| broadcast_state(freq) }
        overview_changed
    end

    def overview
        {
            :t => 'overview',
            :clients => @clients.size,
            :lamps => @lamps,
            :pending => pending_message_fields,
            :freqs => @tuned.keys.sort.map { |freq| { :f => freq, :people => @tuned[freq].size, :on => lamps_on(freq) } },
        }
    end

    def overview_changed
        watchers = @clients.values.select(&:watching)
        return if watchers.empty?

        message = overview
        watchers.each { |client| say(client.out, message) }
    end
end
