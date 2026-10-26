require 'minitest/autorun'
require_relative '../include/pip_check'
require_relative '../include/kata_runner'

# Pip's plans with real runs in the Workspace image; the spec everywhere.
class PipCheckTest < Minitest::Test
    RUN = lambda do |files:, argv:, timeout:, dialog: nil|
        KataRunner.run(:files => files, :argv => argv, :timeout => timeout, :dialog => dialog)
    end

    SHAFT = "#####\n#P.o#\n###.#\n###L#\n#####\n"

    def check(language, maps: [{ 'map' => SHAFT, 'facing' => 'east' }], commands: %w[move turn_left take], goal: { 'lamp' => true, 'stones' => 'all' })
        { 'type' => 'pip', 'language' => language, 'commands' => commands, 'maps' => maps, 'goal' => goal }
    end

    def needs_docker
        available = system('docker', 'image', 'inspect', KataRunner::IMAGE, :out => File::NULL, :err => File::NULL)
        skip "no Docker image #{KataRunner::IMAGE}" unless available
    end

    def test_the_spec
        assert_empty PipCheck.spec_errors(check('ruby'))
        refute_empty PipCheck.spec_errors(check('ruby', :commands => %w[fly]))
        refute_empty PipCheck.spec_errors(check('ruby', :maps => [{ 'map' => "###\n#.#\n###" }]))
        refute_empty PipCheck.spec_errors(check('ruby', :goal => {}))
    end

    def test_a_plan_in_each_language
        needs_docker
        plans = {
            'ruby' => "move\nmove\ntake\nturn_left\nturn_left\nturn_left\nmove\nmove\n",
            'python' => "move()\nmove()\ntake()\nfor i in range(3):\n    turn_left()\nmove()\nmove()\n",
            'javascript' => "move();\nmove();\ntake();\nfor (let i = 0; i < 3; i++) turnLeft();\nmove();\nmove();\n",
        }
        plans.each do |language, plan|
            status, message, _, replay = PipCheck.check({}, check(language), plan, RUN)
            assert_equal :correct, status, "#{language}: #{message}"
            assert_equal :lamp, replay[:ended]
            assert_equal 8, replay[:frames].last[:step]
        end
    end

    def test_mistakes_name_the_line
        needs_docker
        status, message, _, replay = PipCheck.check({}, check('python'), "move()\nturn_left()\nmove()\n", RUN)
        assert_equal :wrong, status
        assert_equal 'Hier in Zeile 3 läuft Pip gegen die Wand.', message
        assert_equal :wall, replay[:ended]
        status, message, = PipCheck.check({}, check('ruby'), "move\ntake\n", RUN)
        assert_equal 'Hier in Zeile 2 greift Pip ins Leere: Hier liegt kein Funkenstein.', message
        status, message, = PipCheck.check({}, check('ruby'), "move\nmove\n", RUN)
        assert_equal 'Hier liegt am Ende noch ein Funkenstein da.', message
        status, message, = PipCheck.check({}, check('javascript'), "move();\njump();\n", RUN)
        assert_equal :wrong, status
        assert_includes message, '`jump` kennt JavaScript an dieser Stelle nicht.'
    end

    def test_several_maps_need_a_general_plan
        needs_docker
        maps = [{ 'map' => "######\n#Po..#\n######" }, { 'map' => "######\n#P.o.#\n######" }]
        c = check('ruby', :maps => maps, :commands => %w[move take on_stone], :goal => { 'stones' => 'all' })
        status, message, _, replay = PipCheck.check({}, c, "move\ntake\n", RUN)
        assert_equal :wrong, status
        assert_includes message, 'In Karte 2'
        assert_equal 2, replay[:map]
        status, message, = PipCheck.check({}, c, "3.times do\n  move\n  take if on_stone?\nend\n", RUN)
        assert_equal :correct, status, message
        assert_equal 'Pip schafft alle 2 Karten.', message
    end

    def test_an_endless_loop_stops_at_the_step_limit
        needs_docker
        c = check('python', :commands => %w[move turn_left], :goal => { 'lamp' => true })
        status, message, = PipCheck.check({}, c, "while True:\n    turn_left()\n", RUN)
        assert_equal :wrong, status
        assert_includes message, 'Läuft Pip im Kreis?'
    end

    # chapter 7: the check calls the student's own function; the plan's
    # own commands while it is loaded change nothing
    def test_a_function_of_ones_own_is_called_on_each_map
        needs_docker
        room = "#####\n#...#\n#.P.#\n#...#\n#####"
        calls = { 'ruby' => 'turn_right', 'python' => 'turn_right()', 'javascript' => 'turnRight();' }
        plans = {
            'ruby' => "def turn_right\n  3.times { turn_left }\nend\n\nturn_right\nmove\n",
            'python' => "def turn_right():\n    for i in range(3):\n        turn_left()\n\nturn_right()\nmove()\n",
            'javascript' => "function turnRight() {\n  for (let i = 0; i < 3; i++) turnLeft();\n}\n\nturnRight();\nmove();\n",
        }
        plans.each do |language, plan|
            maps = [{ 'map' => room, 'facing' => 'east', 'call' => calls[language], 'expect' => { 'facing' => 'south', 'moves' => 0 } },
                    { 'map' => room, 'facing' => 'north', 'call' => calls[language], 'expect' => { 'facing' => 'east' } }]
            c = { 'type' => 'pip', 'language' => language, 'commands' => %w[move turn_left], 'maps' => maps }
            assert_empty PipCheck.spec_errors(c)
            status, message, = PipCheck.check({}, c, plan, RUN)
            assert_equal :correct, status, "#{language}: #{message}"
            wrong = plan.sub('3', '1')
            status, message, = PipCheck.check({}, c, wrong, RUN)
            assert_equal "In Karte 1 schaut Pip nach `#{calls[language]}` nach oben statt nach unten.", message, language
        end
    end

    def test_generated_mazes_and_saying_the_count
        needs_docker
        maze = { 'map' => "#####\n#P..#\n###L#\n#####" }
        c = { 'type' => 'pip', 'language' => 'python', 'commands' => %w[move turn_left front_clear],
              'maps' => [maze, { 'generate' => 'maze', 'size' => [4, 3] }], 'goal' => { 'lamp' => true } }
        assert_empty PipCheck.spec_errors(c)
        follower = "while True:\n    turn_left()\n    while not front_clear():\n        for i in range(3):\n            turn_left()\n    move()\n"
        [0, 1, 2].each do |seed|
            status, message, = PipCheck.check({}, c, follower, RUN, :seed => seed)
            assert_equal :correct, status, "seed #{seed}: #{message}"
        end
        status, message, = PipCheck.check({}, c, "move()\nmove()\nturn_left()\nturn_left()\nturn_left()\nmove()\n", RUN)
        assert_includes message, 'In Karte 2'
        counting = { 'type' => 'pip', 'language' => 'ruby', 'commands' => %w[move take on_stone front_clear],
                     'maps' => [{ 'map' => "######\n#Po.o#\n######" }], 'goal' => { 'say' => 'stones' } }
        plan = "n = 0\nwhile front_clear?\n  move\n  if on_stone?\n    take\n    n += 1\n  end\nend\nsay n\n"
        assert_equal :correct, PipCheck.check({}, counting, plan, RUN).first
        assert_equal 'Hier sagt Pip am Ende »1«, aber Pip hat 2 Funkensteine aufgehoben.', PipCheck.check({}, counting, plan.sub('say n', 'say 1'), RUN)[1]
    end
end

