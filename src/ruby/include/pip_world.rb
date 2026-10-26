# Pip unter Funkelbach (docs/plan-katas.md, section 2): the world Pip walks
# through, turn by turn, on the server. The program a student writes sends
# one command at a time (PipCheck); the world answers and writes down every
# step, so a run is exactly reproducible, the check judges the trace and
# the browser only plays it back. Nothing the program does can change the
# world except through these commands.
#
# A map is text, one character per tile:
#
#   #  wall               .  floor             P  Pip (facing: in the task)
#   o  Funkenstein        L  the lamp (goal)   G  Glibber, moving east–west
#   g  Glibber, moving north–south            H  lever      T  spikes (raised)
#   p  paw prints          ~  puddle            B  Bello
#
# A step is move, turn_left, take, wait or pull. After each step every
# Glibber moves one tile, turning back at walls; one on Pip's tile catches
# Pip. Reaching the lamp or Bello ends the run, like the end of a level, so
# a loop need not stop exactly there. say costs no step. Sensors (on_stone,
# on_lever, front_clear, glibber_ahead, on_lamp, trail_ahead, puddle_ahead,
# bello_here) never change anything; glibber_ahead also sees a Glibber that
# is about to move onto the tile ahead, so »wait while one is ahead« is
# safe. Walking into a wall, taking where there is no stone, pulling
# without a lever, a Glibber and too many steps end the run with that
# event; with dry: true, so does stepping into a puddle (»trockene Füße«).
#
# While a program is only being loaded (quiet: a check that calls one of
# its functions afterwards, PipCheck), commands do nothing and are not
# written down.
#
# Plain Ruby, tested on its own (test/pip_world_test.rb).
class PipWorld
    DIRECTIONS = %w[east north west south].freeze # turn_left goes one to the right in this list
    DELTA = { 'east' => [1, 0], 'north' => [0, -1], 'west' => [-1, 0], 'south' => [0, 1] }.freeze
    STEPS = %w[move turn_left take wait pull].freeze
    SENSORS = %w[on_stone on_lever front_clear glibber_ahead on_lamp trail_ahead puddle_ahead bello_here].freeze
    MAX_STEPS = 1000
    MAX_SAY = 80
    # pulls that lower the spikes (T, a gate in the world); one more raises
    # them again (»Drei Hebel«)
    PULLS_TO_OPEN = 3

    class Error < StandardError; end

    attr_reader :width, :height, :pip, :stones, :taken, :steps, :trace, :ended, :pulls, :said
    attr_accessor :quiet

    def initialize(map, facing: 'east', dry: false)
        rows = map.to_s.lines.map(&:chomp).reject(&:empty?)
        raise Error, 'map is empty' if rows.empty?

        @height = rows.size
        @width = rows.map(&:size).max
        @walls = {}
        @stones = {}
        @floor_marks = {}
        @glibbers = []
        @levers = {}
        @gates = {}
        @lamp = nil
        @bello = nil
        start = nil
        rows.each_with_index do |row, y|
            row.ljust(@width, '#').each_char.with_index do |c, x|
                case c
                when '#' then @walls[[x, y]] = true
                when 'o' then @stones[[x, y]] = true
                when 'L' then @lamp = [x, y]
                when 'P' then start = [x, y]
                when 'G' then @glibbers << { :x => x, :y => y, :dx => 1, :dy => 0 }
                when 'g' then @glibbers << { :x => x, :y => y, :dx => 0, :dy => 1 }
                when 'H' then @levers[[x, y]] = true
                when 'T' then @gates[[x, y]] = true
                when 'p' then @floor_marks[[x, y]] = :trail
                when '~' then @floor_marks[[x, y]] = :puddle
                when 'B' then @bello = [x, y]
                when '.', ' ' then nil
                else raise Error, "unknown tile #{c.inspect} in row #{y + 1}"
                end
            end
        end
        raise Error, 'the map has no P' unless start
        raise Error, "facing must be one of #{DIRECTIONS.join(', ')}" unless DIRECTIONS.include?(facing)

        @pip = { :x => start[0], :y => start[1], :facing => facing }
        @dry = dry
        @quiet = false
        @start_stones = @stones.keys
        @taken = 0
        @steps = 0
        @pulls = 0
        @said = []
        @ended = nil
        @trace = [frame(:start, nil)]
    end

    # What the map shows at the start, for the browser to draw, also after
    # a run (the stones Pip took are still in it).
    def layout
        {
            :width => @width, :height => @height,
            :walls => @walls.keys, :stones => @start_stones, :lamp => @lamp, :bello => @bello,
            :levers => @levers.keys, :gates => @gates.keys,
            :trail => trail_directions,
            :puddles => @floor_marks.select { |_, v| v == :puddle }.keys,
        }
    end

    # The paw prints with the way Bello went: [x, y, angle], the angle in
    # degrees clockwise from north, towards the next tile of the trail on
    # the way to Bello (puddles on the trail count as part of it). At a
    # corner a print points halfway between the way in and the way out, so
    # the trail bends instead of turning on the spot. Prints that lead to
    # no Bello point north.
    def trail_directions
        prints = @floor_marks.select { |_, v| v == :trail }.keys
        distance = {}
        if @bello
            distance[@bello] = 0
            queue = [@bello]
            until queue.empty?
                x, y = queue.shift
                DELTA.each_value do |dx, dy|
                    n = [x + dx, y + dy]
                    next if distance.key?(n) || !@floor_marks.key?(n)

                    distance[n] = distance[[x, y]] + 1
                    queue << n
                end
            end
        end
        prints.map do |x, y|
            here = distance[[x, y]]
            next [x, y, 0] unless here

            steps = DELTA.values.select { |dx, dy| distance[[x + dx, y + dy]] }
            out = steps.find { |dx, dy| distance[[x + dx, y + dy]] == here - 1 }
            back = steps.find { |dx, dy| distance[[x + dx, y + dy]] == here + 1 }
            # the way in is the way from the tile before to this one
            vx, vy = out
            vx, vy = vx - back[0], vy - back[1] if back
            [x, y, (Math.atan2(vx, -vy) * 180 / Math::PI).round % 360]
        end
    end

    def stones_left
        @stones.size
    end

    def gate_open?
        @pulls == PULLS_TO_OPEN
    end

    def at_lamp?
        @lamp == [@pip[:x], @pip[:y]]
    end

    def at_bello?
        !@bello.nil? && @bello == [@pip[:x], @pip[:y]]
    end

    # how many tiles Pip has walked
    def moves
        @trace.count { |f| f[:event] == :move }
    end

    # Runs one command; line is the line of the program it came from.
    # Returns what the program gets back: 'ok', true or false for a sensor,
    # or 'end' once the run is over (the event is in ended).
    def command(name, line: nil, text: nil)
        return 'end' if @ended
        return SENSORS.include?(name) ? false : 'ok' if @quiet

        if name == 'say'
            @said << text.to_s[0, MAX_SAY]
            @trace << frame(:say, line, :text => text.to_s[0, MAX_SAY])
            return 'ok'
        end
        return sense(name) if SENSORS.include?(name)
        raise Error, "unknown command #{name}" unless STEPS.include?(name)

        @steps += 1
        if @steps > MAX_STEPS
            finish(:steps, line)
            return 'end'
        end
        case name
        when 'move'
            x, y = ahead
            if blocked?(x, y)
                finish(@gates[[x, y]] ? :gate : :wall, line)
                return 'end'
            end
            @pip[:x], @pip[:y] = x, y
            if @dry && @floor_marks[here] == :puddle
                finish(:puddle, line)
                return 'end'
            end
            event = :move
        when 'turn_left'
            @pip[:facing] = DIRECTIONS[(DIRECTIONS.index(@pip[:facing]) + 1) % 4]
            event = :turn
        when 'take'
            unless @stones.delete(here)
                finish(:no_stone, line)
                return 'end'
            end
            @taken += 1
            event = :take
        when 'wait'
            event = :wait
        when 'pull'
            unless @levers[here]
                finish(:no_lever, line)
                return 'end'
            end
            @pulls += 1
            event = :pull
        end
        # stepping onto a Glibber: caught before it moves on (else the two
        # could swap tiles and pass through each other)
        if caught?
            finish(:caught, line)
            return 'end'
        end
        # a frame shows the whole step: Pip's, then the Glibbers'; the step
        # onto the lamp or to Bello is the frame of the end itself, so that
        # the replay shows Pip walk up to it
        move_glibbers
        reached = (:lamp if at_lamp?) || (:bello if at_bello?)
        @trace << frame(event, line) unless reached && !caught?
        if caught?
            finish(:caught, line)
            return 'end'
        end
        if reached
            finish(reached, line)
            return 'end'
        end
        'ok'
    end

    # The program ended by itself.
    def done(line = nil)
        finish(:done, line) unless @ended
    end

    private

    def here
        [@pip[:x], @pip[:y]]
    end

    def ahead
        dx, dy = DELTA[@pip[:facing]]
        [@pip[:x] + dx, @pip[:y] + dy]
    end

    def blocked?(x, y)
        @walls[[x, y]] || x.negative? || y.negative? || x >= @width || y >= @height || (@gates[[x, y]] && !gate_open?)
    end

    def sense(name)
        x, y = ahead
        case name
        when 'on_stone' then @stones.key?(here)
        when 'on_lever' then @levers.key?(here)
        when 'front_clear' then !blocked?(x, y)
        when 'glibber_ahead' then @glibbers.any? { |g| [g[:x], g[:y]] == [x, y] || next_position(g) == [x, y] }
        when 'on_lamp' then at_lamp?
        # Bello stands at the end of his trail
        when 'trail_ahead' then @floor_marks[[x, y]] == :trail || @bello == [x, y]
        when 'puddle_ahead' then @floor_marks[[x, y]] == :puddle
        when 'bello_here' then @bello == here
        end
    end

    # Where a Glibber will be after the next step, without moving it.
    def next_position(g)
        copy = g.dup
        step_glibber(copy)
        [copy[:x], copy[:y]]
    end

    def move_glibbers
        @glibbers.each { |g| step_glibber(g) }
    end

    # One step of a Glibber: on, or back where the way is blocked; stuck
    # between two blocked tiles, it stays.
    def step_glibber(g)
        blocked = ->(x, y) { @walls[[x, y]] || @gates[[x, y]] || @stones.key?([x, y]) || x.negative? || y.negative? || x >= @width || y >= @height }
        nx, ny = g[:x] + g[:dx], g[:y] + g[:dy]
        if blocked.call(nx, ny)
            g[:dx], g[:dy] = -g[:dx], -g[:dy]
            nx, ny = g[:x] + g[:dx], g[:y] + g[:dy]
            return if blocked.call(nx, ny)
        end
        g[:x], g[:y] = nx, ny
    end

    def caught?
        @glibbers.any? { |g| g[:x] == @pip[:x] && g[:y] == @pip[:y] }
    end

    def finish(event, line)
        @ended = event
        @trace << frame(event, line)
    end

    def frame(event, line, extra = {})
        {
            :event => event, :line => line, :step => @steps,
            :pip => @pip.dup, :glibbers => @glibbers.map { |g| [g[:x], g[:y]] },
            :stones => @stones.keys, :pulls => @pulls,
        }.merge(extra)
    end
end
