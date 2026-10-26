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
#   M  the Mampfer         *  a lamp that burns already (its light)
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
# The Mampfer (Teil 3): after each step of Pip (a turn, a wait and taking
# a stone are steps too, say is none), it goes one tile along a shortest
# way to Pip, so Pip standing still is caught. The way is a
# breadth-first search from Pip (neighbours in the order of DIRECTIONS),
# and the Mampfer steps to its tile's predecessor, the next tile towards
# Pip. It never steps into light, the 3 × 3 tiles around a lamp that burns
# (*); Pip may. Pip on its tile, or it on Pip's, ends the run (chomped).
# cave_map and facing ask what Pip knows from the Wichtel's map: the map
# as it is now (rows of the legend, Pip, the Mampfer, Glibbers and the
# stones where they are) and where Pip looks; like a sensor, no step.
# When the plan ends with Pip neither at the goal nor caught (PipCheck asks
# for that), Pip stands still and the world goes on (chase): wait after
# wait, without a line, until the Mampfer has Pip or can come no nearer
# (Pip in the light, no way); chased counts these steps, and their frames
# say after: true (the player shows »nach dem Plan«).
#
# While a program is only being loaded (quiet: a check that calls one of
# its functions afterwards, PipCheck), commands do nothing and are not
# written down; only say still shows.
#
# Plain Ruby, tested on its own (test/pip_world_test.rb).
class PipWorld
    DIRECTIONS = %w[east north west south].freeze # turn_left goes one to the right in this list
    DELTA = { 'east' => [1, 0], 'north' => [0, -1], 'west' => [-1, 0], 'south' => [0, 1] }.freeze
    STEPS = %w[move turn_left take wait pull].freeze
    SENSORS = %w[on_stone on_lever front_clear glibber_ahead on_lamp trail_ahead puddle_ahead bello_here].freeze
    QUERIES = %w[cave_map facing].freeze
    MAX_STEPS = 1000
    MAX_SAY = 80
    # pulls that lower the spikes (T, a gate in the world); one more raises
    # them again (»Drei Hebel«)
    PULLS_TO_OPEN = 3

    class Error < StandardError; end

    attr_reader :width, :height, :pip, :stones, :taken, :steps, :trace, :ended, :pulls, :said, :chased
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
        @mampfers = []
        @burning = {}
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
                when 'M' then @mampfers << { :x => x, :y => y }
                when '*' then @burning[[x, y]] = true
                when '.', ' ' then nil
                else raise Error, "unknown tile #{c.inspect} in row #{y + 1}"
                end
            end
        end
        raise Error, 'the map has no P' unless start
        raise Error, 'the Mampfer cannot stand in the light' if @mampfers.any? { |m| light?(m[:x], m[:y]) }
        raise Error, "facing must be one of #{DIRECTIONS.join(', ')}" unless DIRECTIONS.include?(facing)

        @pip = { :x => start[0], :y => start[1], :facing => facing }
        @dry = dry
        @quiet = false
        @start_stones = @stones.keys
        @taken = 0
        @steps = 0
        @pulls = 0
        @said = []
        @chased = 0
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
            :burning => @burning.keys,
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

        # say changes nothing in the world, so it counts while the program
        # is only being loaded too
        if name == 'say'
            @said << text.to_s[0, MAX_SAY]
            @trace << frame(:say, line, :text => text.to_s[0, MAX_SAY])
            return 'ok'
        end
        return query(name) if QUERIES.include?(name)
        return SENSORS.include?(name) ? false : 'ok' if @quiet
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
            if chomped?
                finish(:chomped, line)
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
        # at the lamp it is too late for the Mampfer
        move_mampfers unless reached
        @trace << frame(event, line) unless reached && !caught?
        if caught?
            finish(:caught, line)
            return 'end'
        end
        if chomped?
            finish(:chomped, line)
            return 'end'
        end
        if reached
            finish(reached, line)
            return 'end'
        end
        'ok'
    end

    # The plan is over, but not the run: Pip stands still, and while a
    # Mampfer can still come nearer, the world goes on, a wait (with no line
    # of the plan) at a time, until it has Pip. Every wait brings a Mampfer
    # one tile nearer on a way that stays the same, so it ends within as
    # many waits as the map has tiles; the bound is that, and MAX_STEPS.
    def chase
        @chasing = true
        (@width * @height).times do
            break if @ended || @steps >= MAX_STEPS || !mampfer_coming?

            @chased += 1
            command('wait')
        end
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

    # In the light of a lamp that burns: its tile and the eight around it.
    def light?(x, y)
        @burning.keys.any? { |bx, by| (bx - x).abs <= 1 && (by - y).abs <= 1 }
    end

    def chomped?
        @mampfers.any? { |m| m[:x] == @pip[:x] && m[:y] == @pip[:y] }
    end

    # Breadth-first search from Pip over the tiles the Mampfer may walk:
    # for every tile it reaches, its predecessor, the tile one step nearer
    # to Pip.
    def towards_pip
        predecessor = { here => nil }
        queue = [here]
        until queue.empty?
            x, y = queue.shift
            DIRECTIONS.each do |direction|
                dx, dy = DELTA[direction]
                n = [x + dx, y + dy]
                next if predecessor.key?(n) || @walls[n] || @gates[n] || light?(*n) ||
                        n[0].negative? || n[1].negative? || n[0] >= @width || n[1] >= @height

                predecessor[n] = [x, y]
                queue << n
            end
        end
        predecessor
    end

    # One tile each, towards Pip; never into the light, not even onto Pip
    # standing in it, and not at all without a way.
    def move_mampfers
        return if @mampfers.empty?

        predecessor = towards_pip
        @mampfers.each do |m|
            to = predecessor[[m[:x], m[:y]]]
            next if to.nil? || light?(*to)

            m[:x], m[:y] = to
        end
    end

    # Would a Mampfer move after the next step of Pip standing still?
    def mampfer_coming?
        return false if @mampfers.empty?

        predecessor = towards_pip
        @mampfers.any? do |m|
            to = predecessor[[m[:x], m[:y]]]
            !to.nil? && !light?(*to)
        end
    end

    # What cave_map and facing answer.
    def query(name)
        return @pip[:facing] if name == 'facing'

        rows = Array.new(@height) { |y| Array.new(@width) { |x| @walls[[x, y]] ? '#' : '.' } }
        put = ->(p, c) { rows[p[1]][p[0]] = c }
        @floor_marks.each { |p, mark| put.call(p, mark == :trail ? 'p' : '~') }
        @levers.each_key { |p| put.call(p, 'H') }
        @gates.each_key { |p| put.call(p, 'T') }
        @burning.each_key { |p| put.call(p, '*') }
        put.call(@lamp, 'L') if @lamp
        put.call(@bello, 'B') if @bello
        @stones.each_key { |p| put.call(p, 'o') }
        @glibbers.each { |g| put.call([g[:x], g[:y]], g[:dx].zero? ? 'g' : 'G') }
        @mampfers.each { |m| put.call([m[:x], m[:y]], 'M') }
        put.call(here, 'P')
        rows.map(&:join)
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
            :mampfers => @mampfers.map { |m| [m[:x], m[:y]] },
        }.merge(@chasing ? { :after => true } : {}).merge(extra)
    end
end
