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
# Plain Ruby, tested on its own (test/pip_maps_test.rb).
module PipMaps
    KINDS = %w[maze].freeze
    MAX_SIZE = 9

    class Error < StandardError; end

    # The map for entry (a Hash from tasks.yaml) and seed; the index of the
    # entry goes into the seed, so two generated maps of a task differ.
    def self.make(entry, seed, index)
        rng = Random.new(seed * 31 + index)
        case entry['generate']
        when 'maze' then maze(*entry['size'], rng)
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
        far = farthest(rows, [1, 1])
        rows[1][1] = 'P'
        rows[far[1]][far[0]] = 'L'
        { 'map' => rows.map(&:join).join("\n"), 'facing' => rows[1][2] == '#' ? 'south' : 'east' }
    end

    # The open tile farthest from start, walking.
    def self.farthest(rows, start)
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
end
