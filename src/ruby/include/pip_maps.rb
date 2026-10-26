# Maps for Pip made from a seed (docs/plan-katas.md, »The world«): where a
# chapter is about a general solution, a check runs the plan on its fixed
# maps and on some made for the person and the task version (KataCheck.seed),
# so that no list of steps fits them all. A map in tasks.yaml that says
#
#   - generate: maze
#     size: [6, 5]          # cells across and down
#
# becomes a map in PipWorld's legend here. maze: a perfect maze (one way
# between any two cells, dug depth-first), Pip in the top left corner
# looking east, the lamp in the cell farthest from Pip.
#
# chase (Teil 3, the Mampfer): a maze with loops (some walls between cells
# knocked out), the lamp at about two thirds of the longest way from Pip,
# and the Mampfer where the
# simulation shows it is fair: every shortest way to the lamp escapes,
# walked with left turns only (a right turn costs three steps), and both
# wall-following plans of Teil 2 (a hand on the left wall, on the right)
# are caught or never arrive. A maze where no place for the Mampfer does
# that, or with more than MAX_WAYS shortest ways, is dug anew.
#
# Plain Ruby, tested on its own (test/pip_maps_test.rb).
require_relative 'pip_world'

module PipMaps
    KINDS = %w[maze chase].freeze
    # loops in a chase maze; shortest ways it may have; places tried for
    # the Mampfer; mazes dug before giving up
    BRAIDS = 5
    MAX_WAYS = 12
    PLACES = 12
    TRIES = 40
    LAMP_SHARE = 0.66
    MAX_SIZE = 9

    class Error < StandardError; end

    # The map for entry (a Hash from tasks.yaml) and seed; the index of the
    # entry goes into the seed, so two generated maps of a task differ.
    def self.make(entry, seed, index)
        rng = Random.new(seed * 31 + index)
        case entry['generate']
        when 'maze' then maze(*entry['size'], rng)
        when 'chase' then chase(*entry['size'], rng)
        else raise Error, "generate must be one of #{KINDS.join(', ')}"
        end
    end

    def self.errors(entry)
        return ["generate must be one of #{KINDS.join(', ')}"] unless KINDS.include?(entry['generate'])

        size = entry['size']
        unless size.is_a?(Array) && size.size == 2 && size.all? { |n| n.is_a?(Integer) && n.between?(2, MAX_SIZE) }
            return ["size must be [across, down], each 2 to #{MAX_SIZE}"]
        end

        []
    end

    # A maze of w × h cells: cell (cx, cy) is tile (2cx+1, 2cy+1), the walls
    # between them are dug away.
    def self.maze(w, h, rng)
        rows = Array.new(2 * h + 1) { Array.new(2 * w + 1, '#') }
        seen = { [0, 0] => true }
        stack = [[0, 0]]
        rows[1][1] = '.'
        until stack.empty?
            cx, cy = stack.last
            options = [[1, 0], [-1, 0], [0, 1], [0, -1]].map { |dx, dy| [cx + dx, cy + dy] }
                                                       .select { |nx, ny| nx.between?(0, w - 1) && ny.between?(0, h - 1) && !seen[[nx, ny]] }
            if options.empty?
                stack.pop
                next
            end
            nx, ny = options[rng.rand(options.size)]
            rows[cy + ny + 1][cx + nx + 1] = '.'
            rows[2 * ny + 1][2 * nx + 1] = '.'
            seen[[nx, ny]] = true
            stack << [nx, ny]
        end
        far = last_reached(rows, [1, 1])
        rows[1][1] = 'P'
        rows[far[1]][far[0]] = 'L'
        { 'map' => rows.map(&:join).join("\n"), 'facing' => rows[1][2] == '#' ? 'south' : 'east' }
    end

    # The open tile a breadth-first search from start reaches last (maze:
    # as it has always placed the lamp, so the mazes stay the same).
    def self.last_reached(rows, start)
        distance = { start => 0 }
        queue = [start]
        last = start
        until queue.empty?
            x, y = last = queue.shift
            [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dx, dy|
                n = [x + dx, y + dy]
                next if rows[n[1]][n[0]] == '#' || distance.key?(n)

                distance[n] = distance[[x, y]] + 1
                queue << n
            end
        end
        last
    end

    # The open tile farthest from start, walking.
    def self.farthest(rows, start)
        distance = distances(rows, start)
        distance.max_by { |p, d| [d, -p[1], -p[0]] }.first
    end

    # How many steps every open tile is from start (breadth-first).
    def self.distances(rows, start)
        distance = { start => 0 }
        queue = [start]
        until queue.empty?
            x, y = queue.shift
            PipWorld::DIRECTIONS.each do |d|
                dx, dy = PipWorld::DELTA[d]
                n = [x + dx, y + dy]
                next if rows[n[1]].nil? || rows[n[1]][n[0]].nil? || rows[n[1]][n[0]] == '#' || distance.key?(n)

                distance[n] = distance[[x, y]] + 1
                queue << n
            end
        end
        distance
    end

    def self.chase(w, h, rng)
        TRIES.times do
            map = chase_once(w, h, rng)
            return map if map
        end
        raise Error, 'no fair maze for the Mampfer'
    end

    def self.chase_once(w, h, rng)
        base = maze(w, h, rng)
        rows = base['map'].split("\n").map { |row| row.tr('PL', '..') }
        start = [1, 1]
        # loops: walls between two cells, inside the border, knocked out
        walls = []
        rows.each_with_index do |row, y|
            row.each_char.with_index do |c, x|
                inside = x > 0 && y > 0 && x < row.size - 1 && y < rows.size - 1
                walls << [x, y] if c == '#' && inside && (x + y).odd?
            end
        end
        walls.shuffle(:random => rng).first(BRAIDS).each { |x, y| rows[y][x] = '.' }
        # the lamp at about two thirds of the longest way from Pip: far, but
        # with corners enough left for a right turn's three steps
        from_start = distances(rows, start)
        reach = from_start.values.max
        lamp = from_start.keys.shuffle(:random => rng).min_by { |p| (from_start[p] - LAMP_SHARE * reach).abs }
        to_lamp = distances(rows, lamp)
        ways = shortest_ways(rows, start, lamp, to_lamp)
        return nil if ways.nil? || ways.empty?

        rows[start[1]][start[0]] = 'P'
        rows[lamp[1]][lamp[0]] = 'L'
        facing = rows[1][2] == '#' ? 'south' : 'east'
        # the Mampfer chases from behind: its places farthest from the lamp
        # first, not right beside Pip; PLACES of them
        from_pip = distances(rows, start)
        open = to_lamp.keys.select { |x, y| rows[y][x] == '.' && from_pip[[x, y]] >= 3 }.shuffle(:random => rng)
        open.sort_by { |p| -to_lamp[p] }.first(PLACES).each do |m|
            test = rows.map(&:dup)
            test[m[1]][m[0]] = 'M'
            map = test.join("\n")
            next unless ways.all? { |way| walked?(map, facing, way) }
            next if follows?(map, facing, 1) || follows?(map, facing, 3)

            return { 'map' => map, 'facing' => facing }
        end
        nil
    end

    # Every shortest way from a to b (lists of tiles), nil if more than
    # MAX_WAYS; to_b: the distances to b.
    def self.shortest_ways(rows, a, b, to_b)
        ways = []
        walk = lambda do |path|
            return if ways.size > MAX_WAYS

            here = path.last
            return ways << path.dup if here == b

            PipWorld::DIRECTIONS.each do |d|
                dx, dy = PipWorld::DELTA[d]
                n = [here[0] + dx, here[1] + dy]
                next unless to_b[n] == to_b[here] - 1

                path << n
                walk.call(path)
                path.pop
            end
        end
        walk.call([a]) if to_b[a]
        ways.size > MAX_WAYS ? nil : ways
    end

    # Pip walks way, turning left until it looks the right way: at the lamp?
    def self.walked?(map, facing, way)
        world = PipWorld.new(map, :facing => facing)
        way.each_cons(2) do |(ax, ay), (bx, by)|
            want = PipWorld::DIRECTIONS.find { |d| PipWorld::DELTA[d] == [bx - ax, by - ay] }
            world.command('turn_left') until world.pip[:facing] == want || world.ended
            world.command('move') unless world.ended
            break if world.ended
        end
        world.ended == :lamp
    end

    # The wall-following plan of Teil 2 (»Das Labyrinth«): a turn to the
    # hand's side (turn: 1 left, 3 right), then the other way until the
    # front is clear, a step; at the lamp?
    def self.follows?(map, facing, turn)
        world = PipWorld.new(map, :facing => facing)
        until world.ended
            turn.times { world.command('turn_left') unless world.ended }
            4.times do
                break if world.ended || world.command('front_clear') == true

                (4 - turn).times { world.command('turn_left') unless world.ended }
            end
            world.command('move') unless world.ended
        end
        world.ended == :lamp
    end
end
