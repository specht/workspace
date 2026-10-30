require 'minitest/autorun'
require 'json'
require_relative '../include/own_functions'
require_relative '../include/task_spec'

# Own functions (OwnFunctions): a later card names a function of an earlier
# one with {{own task name}}; nobody sees the earlier solution before
# solving it, and who solved it finds their own code there.
class OwnFunctionsTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    TASKS = {
        'turn-right' => {
            'id' => 'turn-right', 'title' => 'Rechtsherum',
            'solution' => "```ruby\ndef turn_right\n  3.times do\n    turn_left\n  end\nend\n```\n",
            'java' => {
                'title' => 'Rechtsherum',
                'solution' => "```java\nvoid turnRight() {\n    for (int i = 0; i < 3; i++) {\n        turnLeft();\n    }\n}\n```\n",
            },
        },
        'stairs' => {
            'id' => 'stairs', 'title' => 'Die Treppe', 'check' => { 'type' => 'pip' },
            'starter' => "{{own turn-right turn_right}}\n\n# Die Treppe\n",
            'java' => { 'starter' => "{{own turn-right turnRight}}\n\nvoid main() {\n    // Die Treppe\n}\n" },
        },
    }.freeze

    def tasks
        data = { 'tasks' => JSON.parse(JSON.generate(TASKS.values)) }
        OwnFunctions.prepare!(data)['tasks'].to_h { |t| [t['id'], t] }
    end

    MINE = "def turn_right\n  turn_left\n  turn_left\n  turn_left\nend\n\nturn_right\n"

    def test_the_page_shows_only_the_frame
        stairs = tasks['stairs']
        assert_equal "def turn_right\n  # noch leer: deine Lösung aus »Rechtsherum«\nend\n\n# Die Treppe\n", stairs['starter']
        assert_equal "void turnRight() {\n    // noch leer: deine Lösung aus »Rechtsherum«\n}\n\nvoid main() {\n    // Die Treppe\n}\n", stairs['java']['starter']
        refute_includes stairs['starter'] + stairs['java']['starter'], 'turn_left'
        assert_equal TASKS['stairs']['starter'], stairs['starter_own']
    end

    def test_who_solved_it_finds_their_own_function
        starters, = OwnFunctions.for_person(tasks, { 'turn-right' => { 'ruby' => MINE } }, {})
        assert_equal "def turn_right\n  turn_left\n  turn_left\n  turn_left\nend\n\n# Die Treppe\n", starters['stairs']['ruby']
        # not solved in Java: the frame there
        assert_includes starters['stairs']['java'], 'noch leer'
    end

    def test_a_saved_frame_takes_the_own_function
        saved = "def turn_right\n  # noch leer: deine Lösung aus »Rechtsherum«\nend\n\n3.times do\n  turn_right\nend\n"
        _, answers = OwnFunctions.for_person(tasks, { 'turn-right' => { 'ruby' => MINE } }, { 'stairs' => JSON.generate('ruby' => saved) })
        assert_equal "def turn_right\n  turn_left\n  turn_left\n  turn_left\nend\n\n3.times do\n  turn_right\nend\n", JSON.parse(answers['stairs'])['ruby']
    end

    def test_the_solution_from_an_old_starter_becomes_the_frame
        saved = "def turn_right\n  3.times do\n    turn_left\n  end\nend\n\nturn_right\n"
        _, answers = OwnFunctions.for_person(tasks, {}, { 'stairs' => JSON.generate('ruby' => saved) })
        assert_equal "def turn_right\n  # noch leer: deine Lösung aus »Rechtsherum«\nend\n\nturn_right\n", JSON.parse(answers['stairs'])['ruby']
    end

    def test_what_the_person_wrote_stays_without_a_solution
        saved = "def turn_right\n  turn_left\n  turn_left\n  turn_left\nend\n"
        _, answers = OwnFunctions.for_person(tasks, {}, { 'stairs' => JSON.generate('ruby' => saved) })
        assert_equal saved, JSON.parse(answers['stairs'])['ruby']
    end

    def test_a_function_left_as_given_takes_the_one_solved_anew
        first = "def turn_right\n  turn_left\n  turn_left\n  turn_left\nend\n"
        anew = "def turn_right\n  2.times { turn_left }\n  turn_left\nend\n"
        saved = "#{first}\nturn_right\n"
        given = JSON.generate('ruby' => OwnFunctions.filled(TASKS['stairs']['starter'], 'ruby', tasks,
            :mine => OwnFunctions.mine({ 'turn-right' => { 'ruby' => first } }, 'ruby')))
        _, answers = OwnFunctions.for_person(tasks, { 'turn-right' => { 'ruby' => anew } }, { 'stairs' => JSON.generate('ruby' => saved) }, { 'stairs' => given })
        assert_equal "#{anew}\nturn_right\n", JSON.parse(answers['stairs'])['ruby']
    end

    def test_a_function_changed_in_the_later_card_stays
        first = "def turn_right\n  turn_left\n  turn_left\n  turn_left\nend\n"
        changed = "def turn_right\n  say \"rechts\"\n  3.times { turn_left }\nend\n"
        given = JSON.generate('ruby' => { 'turn_right' => first.chomp })
        saved = "#{changed}\nturn_right\n"
        _, answers = OwnFunctions.for_person(tasks, { 'turn-right' => { 'ruby' => first } }, { 'stairs' => JSON.generate('ruby' => saved) }, { 'stairs' => given })
        assert_equal saved, JSON.parse(answers['stairs'])['ruby']
        # the frame left as it was: the own function comes in
        given = JSON.generate('ruby' => OwnFunctions.filled(TASKS['stairs']['starter'], 'ruby', tasks))
        saved = "#{tasks['stairs']['starter']}turn_right\n"
        _, answers = OwnFunctions.for_person(tasks, { 'turn-right' => { 'ruby' => first } }, { 'stairs' => JSON.generate('ruby' => saved) }, { 'stairs' => given })
        assert_includes JSON.parse(answers['stairs'])['ruby'], first
    end

    def test_functions_in_each_language
        python = "def tree(n):\n    if n > 0:\n        forward(n)\n\n    back(n)\n\ntree(80)\n"
        assert_equal "def tree(n):\n    if n > 0:\n        forward(n)\n\n    back(n)", OwnFunctions.extract(python, 'tree', 'python')
        java = "void tree(int n) {\n    if (n > 0) {\n        forward(n);\n    }\n}\n\nvoid main() {\n}\n"
        assert_equal [0, 4], OwnFunctions.span(java, 'tree', 'java')
        js = "function tree(n) {\n  if (n > 0) { forward(n); }\n}\ntree(80);\n"
        assert_equal "function tree(n) {\n  if (n > 0) { forward(n); }\n}", OwnFunctions.extract(js, 'tree', 'javascript')
        assert_nil OwnFunctions.extract("turn_right\n", 'turn_right', 'ruby')
    end

    def test_a_missing_task_or_function_is_an_error
        data = { 'tasks' => [{ 'id' => 'a', 'starter' => "{{own nowhere f}}\n" }] }
        assert_raises(ArgumentError) { OwnFunctions.prepare!(data) }
        data = { 'tasks' => JSON.parse(JSON.generate(TASKS.values)) }
        data['tasks'][1]['starter'] = "{{own turn-right walk}}\n"
        assert_raises(ArgumentError) { OwnFunctions.prepare!(data) }
    end

    # No later card of Pip or the turtle spells out a function the student
    # writes in an earlier card (docs/checkliste.md).
    DEFINED = /^(?:def|function|[A-Za-z_][\w<>\[\]]*)\s+([A-Za-z_]\w*)\s*[(:\n{]/

    def test_no_card_gives_an_earlier_solution_away
        %w[pip-2 turtle-1 turtle-2 turtle-3].each do |slug|
            data = TaskSpec.load_file(File.join(CONTENT, slug, 'tasks.yaml'))
            data['tasks'].each_with_index do |earlier, i|
                (['ruby'] + CodeLanguages::LANGUAGES).uniq.each do |language|
                    variant = CodeLanguages.variant(earlier, language)
                    next unless language == 'ruby' || earlier[language]

                    code = OwnFunctions.solution_code(variant['solution'])
                    code.scan(DEFINED).flatten.uniq.reject { |name| name == 'main' }.each do |name|
                        whole = OwnFunctions.extract(code, name, language)
                        next unless whole && whole.lines.size > 2

                        data['tasks'][i + 1..].each do |later|
                            starter = CodeLanguages.variant(later, language)['starter'].to_s
                            refute_includes starter, whole, "#{slug}/#{later['id']} (#{language}) gives #{name} from #{earlier['id']} away"
                        end
                    end
                end
            end
        end
    end
end
