require 'minitest/autorun'
require_relative '../include/pip_maps'
require_relative '../include/pip_world'

# Pip's generated maps: valid for PipWorld, the same for the same seed, and
# solved within the step limit by the hand on the wall (Das Labyrinth).
class PipMapsTest < Minitest::Test
    MAZE = { 'generate' => 'maze', 'size' => [6, 5] }.freeze

    # left hand on the wall: turn left, then right until the way is free
    def wall_follower(world)
        until world.ended
            world.command('turn_left')
            3.times { world.command('turn_left') } until world.ended || world.command('front_clear')
            world.command('move')
        end
        world.ended
    end

    def test_same_seed_same_maze
        assert_equal PipMaps.make(MAZE, 7, 1), PipMaps.make(MAZE, 7, 1)
        refute_equal PipMaps.make(MAZE, 7, 1), PipMaps.make(MAZE, 7, 2)
        refute_equal PipMaps.make(MAZE, 7, 1), PipMaps.make(MAZE, 8, 1)
    end

    def test_every_maze_is_solved_by_the_hand_on_the_wall
        300.times do |seed|
            m = PipMaps.make(MAZE, seed, 1)
            world = PipWorld.new(m['map'], :facing => m['facing'])
            assert_equal :lamp, wall_follower(world), "seed #{seed}:\n#{m['map']}"
        end
    end

    def test_a_maze_has_one_way_between_cells
        m = PipMaps.make(MAZE, 3, 1)['map'].lines.map(&:chomp)
        open = m.join.count('.') + 2
        # a perfect maze of 30 cells: 30 cell tiles and 29 dug walls
        assert_equal 59, open
        assert_equal [13, 11], [m.first.size, m.size]
    end

    def test_errors
        assert_empty PipMaps.errors(MAZE)
        refute_empty PipMaps.errors('generate' => 'cave', 'size' => [3, 3])
        refute_empty PipMaps.errors('generate' => 'maze', 'size' => [30, 3])
    end
end
