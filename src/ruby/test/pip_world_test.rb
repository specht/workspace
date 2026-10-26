require 'minitest/autorun'
require_relative '../include/pip_world'

class PipWorldTest < Minitest::Test
    SHAFT = <<~MAP
        #####
        #P.o#
        ###.#
        ###L#
        #####
    MAP

    def test_walking_to_the_lamp_and_taking_a_stone
        w = PipWorld.new(SHAFT, :facing => 'east')
        assert_equal 'ok', w.command('move', :line => 1)
        assert_equal 'ok', w.command('move', :line => 2)
        assert_equal true, w.command('on_stone')
        assert_equal 'ok', w.command('take', :line => 3)
        assert_equal 0, w.stones_left
        3.times { w.command('turn_left') }
        assert_equal 'south', w.pip[:facing]
        w.command('move')
        w.command('move')
        assert w.at_lamp?
        # the lamp ends the run, like the end of a level
        assert_equal :lamp, w.ended
        assert_equal 'end', w.command('move')
        assert_equal 8, w.steps
        assert_equal %i[start move move take turn turn turn move lamp], w.trace.map { |f| f[:event] }
        assert_equal 3, w.trace[3][:line]
        # the map to play the run on still has the stone Pip took
        assert_equal [[3, 1]], w.layout[:stones]
        assert_equal [], w.trace.last[:stones]
    end

    def test_a_wall_ends_the_run
        w = PipWorld.new(SHAFT)
        w.command('turn_left')
        assert_equal false, w.command('front_clear')
        assert_equal 'end', w.command('move', :line => 4)
        assert_equal :wall, w.ended
        assert_equal 4, w.trace.last[:line]
        assert_equal 'end', w.command('move')
    end

    def test_taking_where_there_is_no_stone_ends_the_run
        w = PipWorld.new(SHAFT)
        assert_equal 'end', w.command('take')
        assert_equal :no_stone, w.ended
    end

    def test_a_glibber_moves_each_step_and_catches_pip
        corridor = "#######\n#P...G#\n#######\n"
        w = PipWorld.new(corridor)
        assert_equal true, w.command('front_clear')
        w.command('wait')                      # Glibber 5 -> bounces at the wall: 4
        assert_equal [[4, 1]], w.trace.last[:glibbers]
        w.command('move')                      # Pip 2, Glibber 3
        assert_equal true, w.command('glibber_ahead')
        assert_equal 'end', w.command('move')  # Pip steps onto 3, where the Glibber is now
        assert_equal :caught, w.ended
    end

    # A Glibber crossing the corridor: glibber_ahead also sees the one that
    # is about to step onto the tile ahead, so »wait while one is ahead« is
    # safe; it passes every eight steps.
    def test_waiting_while_a_glibber_is_ahead_is_safe
        map = "#########\n####.####\n####.####\n#P..g..L#\n####.####\n####.####\n#########\n"
        (0..7).each do |delay|
            w = PipWorld.new(map)
            delay.times { w.command('wait') }
            30.times do
                break if w.ended
                w.command('glibber_ahead') ? w.command('wait') : w.command('move')
            end
            assert_equal :lamp, w.ended, "after #{delay} waits"
        end
    end

    def test_three_pulls_open_the_gate_and_a_fourth_closes_it
        w = PipWorld.new("#####\n#PHT.L#\n#######\n".lines.map { |l| l.chomp.ljust(7, '#') }.join("\n"))
        w.command('move')
        3.times { w.command('pull') }
        assert w.gate_open?
        w.command('pull')
        refute w.gate_open?
        assert_equal false, w.command('front_clear')
    end

    def test_say_costs_no_step_and_is_kept_short
        w = PipWorld.new(SHAFT)
        assert_equal 'ok', w.command('say', :text => 'Hallo ' * 50, :line => 1)
        assert_equal 0, w.steps
        assert_equal PipWorld::MAX_SAY, w.said.first.size
    end

    def test_too_many_steps_end_the_run
        w = PipWorld.new(SHAFT)
        PipWorld::MAX_STEPS.times { w.command('turn_left') }
        assert_equal 'end', w.command('turn_left')
        assert_equal :steps, w.ended
    end

    def test_a_map_needs_pip_and_known_tiles
        assert_raises(PipWorld::Error) { PipWorld.new("###\n#.#\n###") }
        assert_raises(PipWorld::Error) { PipWorld.new("#P?#") }
        assert_raises(PipWorld::Error) { PipWorld.new('#P#', :facing => 'up') }
    end

    def test_layout_for_the_browser
        layout = PipWorld.new(SHAFT).layout
        assert_equal [5, 5], [layout[:width], layout[:height]]
        assert_equal [[3, 1]], layout[:stones]
        assert_equal [3, 3], layout[:lamp]
    end

    def test_bello_ends_the_run_and_puddles_wet_only_when_dry
        w = PipWorld.new("#######\n#P~.B.#\n#######")
        assert_equal true, w.command('puddle_ahead')
        w.command('move')
        w.command('move')
        assert_equal 'end', w.command('move')
        assert_equal :bello, w.ended
        assert w.at_bello?
        dry = PipWorld.new("#######\n#P~.B.#\n#######", :dry => true)
        assert_equal 'end', dry.command('move')
        assert_equal :puddle, dry.ended
    end

    def test_trail_and_lever_sensors
        w = PipWorld.new("######\n#PpH.#\n######")
        assert_equal true, w.command('trail_ahead')
        w.command('move')
        assert_equal false, w.command('trail_ahead')
        assert_equal false, w.command('on_lever')
        w.command('move')
        assert_equal true, w.command('on_lever')
    end

    # while a program is only loaded, its commands change nothing
    def test_quiet_while_loading
        w = PipWorld.new(SHAFT)
        w.quiet = true
        assert_equal 'ok', w.command('move')
        assert_equal false, w.command('front_clear')
        # say changes nothing, so it shows while loading too
        w.command('say', :text => 'Hallo')
        assert_equal ['Hallo'], w.said
        w.quiet = false
        w.command('move')
        assert_equal 1, w.moves
        assert_equal [2, 1], [w.pip[:x], w.pip[:y]]
    end

    # the prints point the way to Bello, across a puddle and round the
    # corners, where they point halfway between the way in and the way out
    def test_paw_prints_point_the_way_to_bello
        w = PipWorld.new("#######\n#Pp~p.#\n####pB#\n#######")
        assert_equal [[2, 1, 90], [4, 1, 135], [4, 2, 135]], w.layout[:trail].sort
    end

    # the Mampfer: one tile after each step of Pip, along the shortest way,
    # round the pillar on the side that is shorter; a turn and a wait are
    # steps too, so Pip standing still is caught
    def test_the_mampfer_takes_the_shortest_way_after_each_step
        w = PipWorld.new("#######\n#M....#\n#.###.#\n#...P.#\n#######", :facing => 'west')
        w.command('move')
        assert_equal [[1, 2]], w.trace.last[:mampfers]
        w.command('turn_left')
        assert_equal [[1, 3]], w.trace.last[:mampfers]
        w.command('say', :text => 'Hilfe!')
        assert_equal [[1, 3]], w.trace.last[:mampfers], 'say is no step'
        w.command('wait')
        assert_equal [[2, 3]], w.trace.last[:mampfers]
        assert_equal 'end', w.command('wait')
        assert_equal :chomped, w.ended
    end

    def test_the_mampfer_chomps_pip
        w = PipWorld.new("#####\n#P.M#\n#####")
        assert_equal 'end', w.command('move', :line => 1)
        assert_equal :chomped, w.ended
        assert_equal 1, w.trace.last[:line]
        # walking into it is as bad
        w = PipWorld.new("#####\n#PM.#\n#####")
        w.command('move')
        assert_equal :chomped, w.ended
    end

    # the light of a lamp that burns: the 3 × 3 tiles round it; the
    # Mampfer does not step in, not even onto Pip standing there
    def test_the_mampfer_keeps_out_of_the_light
        w = PipWorld.new("#########\n#M.....P#\n#...*...#\n#########", :facing => 'west')
        3.times { w.command('move') }
        assert_equal [[1, 1]], w.trace.last[:mampfers], 'the light cuts the way'
        # Pip at the edge of the light: the Mampfer comes up to it
        w.command('move')
        assert_equal [3, 1], [w.pip[:x], w.pip[:y]]
        assert_equal [[2, 1]], w.trace.last[:mampfers]
        assert_nil w.ended
        w = PipWorld.new("#######\n#MP..*#\n#######")
        3.times { w.command('move') }
        assert_equal [[3, 1]], w.trace.last[:mampfers]
        assert_nil w.ended
        assert_equal [[5, 1]], w.layout[:burning]
        assert_raises(PipWorld::Error) { PipWorld.new("######\n#P.M*#\n######") }
    end

    # what the Wichtel's map says, now: no step
    def test_cave_map_and_facing
        w = PipWorld.new("######\n#P.o.#\n####M#\n####L#\n######")
        assert_equal ['######', '#P.o.#', '####M#', '####L#', '######'], w.command('cave_map')
        assert_equal 'east', w.command('facing')
        w.command('move')
        assert_equal ['######', '#.PoM#', '####.#', '####L#', '######'], w.command('cave_map')
        assert_equal 1, w.steps
    end
end
