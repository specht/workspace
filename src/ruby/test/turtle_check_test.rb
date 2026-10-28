require 'minitest/autorun'
require_relative '../include/turtle_check'

# The turtle (check type turtle, TurtleCheck): the commands the browser
# reports, replayed by TurtleWorld and compared with the picture of the
# task, however they were drawn; and the same commands of each language
# (static/include/turtle/turtle.rb, .py, .js), run on this machine.
class TurtleCheckTest < Minitest::Test
    SQUARE = { 'type' => 'turtle', 'language' => 'ruby', 'draw' => '4.times { forward 100; left 90 }' }.freeze

    def status(check, commands)
        TurtleCheck.check(check, 'code', [commands]).first
    end

    def test_the_world_draws_like_the_browser
        world = TurtleWorld.draw("forward 100\nleft 90\npen_up\nforward 50\npen_down\ncolor 'gold'\nwidth 4\nback 20")
        assert_equal [[0.0, 0.0, 100.0, 0.0, '#000000', 2], [100.0, 50.0, 100.0, 30.0, '#ffd700', 4.0]], world.segments.map(&:to_a)
        assert_equal '#ff8800', TurtleWorld.color_hex('#F80')
        assert_nil TurtleWorld.color_hex('rot')
    end

    def test_any_order_direction_or_pieces_draw_the_same_square
        assert_equal :correct, status(SQUARE, [['forward', 100], ['left', 90]] * 4)
        # the other way round, one side in two halves, one side twice
        other = [['left', 90], ['forward', 100], ['right', 90], ['forward', 50], ['forward', 50], ['right', 90],
                 ['forward', 100], ['right', 90], ['forward', 100], ['back', 100], ['forward', 100]]
        assert_equal :correct, status(SQUARE, other)
        # 360 small steps round a circle stay within the tolerance
        circle = { 'type' => 'turtle', 'language' => 'ruby', 'draw' => '360.times { forward 2; left 1 }' }
        assert_equal :correct, status(circle, [['forward', 2], ['left', 1]] * 360)
    end

    def test_what_is_missing_or_too_much_is_named_and_shown
        result, message, show = TurtleCheck.check(SQUARE, 'code', [[['forward', 100], ['left', 90]] * 3])
        assert_equal :wrong, result
        assert_match(/fehlt ein Stück von etwa 9\d Schritten/, message)
        assert_equal 1, show[:missing].size
        assert_empty show[:extra]

        _, message, show = TurtleCheck.check(SQUARE, 'code', [[['forward', 100], ['left', 120]] * 3])
        assert_match(/fehlt .*, und .* ist zu viel/, message)
        refute_empty show[:extra]
        assert_equal 'Die Schildkröte hat nichts gezeichnet.', TurtleCheck.check(SQUARE, 'code', [[]])[1]
    end

    def test_colours_count_only_when_asked
        red = [['color', 'red']] + [['forward', 100], ['left', 90]] * 4
        assert_equal :correct, status(SQUARE, red)
        assert_equal :wrong, status(SQUARE.merge('colors' => true), red)
        assert_match(/nicht alle Farben/, TurtleCheck.check(SQUARE.merge('colors' => true), 'code', [red])[1])
    end

    def test_cases_and_free_pictures
        check = { 'type' => 'turtle', 'language' => 'ruby',
                  'cases' => [{ 'call' => 'square(50)', 'draw' => '4.times { forward 50; left 90 }' },
                              { 'call' => 'square(20)', 'draw' => '4.times { forward 20; left 90 }' }] }
        assert_equal %w[square(50) square(20)], TurtleCheck.runs(check)
        good = TurtleCheck.check(check, 'code', [[['forward', 50], ['left', 90]] * 4, [['forward', 20], ['left', 90]] * 4])
        assert_equal [:correct, 'Alle 2 Aufrufe zeichnen genau das Richtige.'], good[0, 2]
        bad = TurtleCheck.check(check, 'code', [[['forward', 50], ['left', 90]] * 4, [['forward', 50], ['left', 90]] * 4])
        assert_match(/\ABei `square\(20\)`/, bad[1])
        assert_equal 'square(20)', bad[2][:call]

        free = { 'type' => 'turtle', 'language' => 'ruby', 'free' => { 'min_lines' => 3, 'min_colors' => 2 } }
        assert_equal :wrong, status(free, [['forward', 10]] * 3)
        assert_equal :correct, status(free, [['forward', 10], ['color', 'red'], ['forward', 10], ['forward', 10]])
    end

    def test_reports_that_do_not_fit_are_not_checked
        assert_equal :invalid, TurtleCheck.check(SQUARE, 'code', nil).first
        assert_equal :invalid, TurtleCheck.check(SQUARE, 'code', [[['jump', 3]]]).first
        assert_equal :invalid, TurtleCheck.check(SQUARE, ' ', [[]]).first
    end

    def test_task_files_are_checked
        assert_empty TurtleCheck.spec_errors(SQUARE)
        assert_includes TurtleCheck.spec_errors({ 'type' => 'turtle', 'language' => 'ruby', 'draw' => 'forwad 3' }).join, 'forwad'
        assert_includes TurtleCheck.spec_errors({ 'type' => 'turtle', 'language' => 'lua', 'draw' => 'forward 3' }).join, 'language'
        assert_includes TurtleCheck.spec_errors({ 'type' => 'turtle', 'language' => 'ruby' }).join, 'exactly one'
    end

    def test_every_language_draws_the_same_with_its_own_commands
        programs = {
            'ruby' => "def square(size)\n  4.times do\n    forward size\n    left 90\n  end\nend\nsquare(30)\n",
            'python' => "def square(size):\n    for i in range(4):\n        forward(size)\n        left(90)\nsquare(30)\n",
        }
        programs['javascript'] = "function square(size) {\n  for (let i = 0; i < 4; i++) { forward(size); left(90); }\n}\nsquare(30);\n" if system('node --version', :out => File::NULL, :err => File::NULL)
        programs.each do |language, code|
            runs, error = TurtleCheck.record(language, code, [nil, 'square(50)'])
            assert_nil error, language
            assert_equal [['forward', 30], ['left', 90]] * 4, runs[0], language
            # the call runs without what the program draws by itself
            assert_equal [['forward', 50], ['left', 90]] * 4, runs[1], language
        end
        _, error = TurtleCheck.record('ruby', 'forward "100"', [nil])
        assert_match(/forward braucht eine Zahl/, error)
    end
end
