require 'json'
require 'minitest/autorun'
require_relative '../include/circuit_check'
require_relative '../include/task_spec'

# Circuits of the Logik-Werkstatt (check type circuit, CircuitCheck): the
# netlist the editor sends, read, flattened and run round by round. The
# circuits in fixtures/circuits.json run the same way in the page's engine
# (test/static/logic_circuit.test.cjs).
class CircuitCheckTest < Minitest::Test
    FIXTURES = JSON.parse(File.read(File.join(__dir__, 'fixtures', 'circuits.json')))

    XOR = { 'type' => 'circuit', 'inputs' => %w[a b], 'outputs' => %w[y], 'expect' => { 'y' => 'a ^ b' },
            'gates' => %w[nand], 'max_gates' => 4, 'blocks' => false }.freeze
    XOR_NAND = "in a; in b; nand x a b; nand l a x; nand r x b; nand g l r; out y g"
    HA = "block HA\n in a\n in b\n xor x a b\n and k a b\n out s x\n out c k\nend\n"
    FULL = { 'type' => 'circuit', 'inputs' => %w[a b c_in], 'outputs' => %w[s c_out],
             'expect' => { 's' => 'a ^ b ^ c_in', 'c_out' => '(a + b + c_in) >> 1' }, 'min_blocks' => 2 }.freeze

    def check(spec, text)
        CircuitCheck.check({}, spec, text)
    end

    # the outputs of every row, from a circuit whose gates are all 0
    def table(text)
        circuit, errors = CircuitCheck.parse(text)
        assert_empty errors
        flat = CircuitCheck.flatten(circuit)
        inputs = circuit.ins
        (0...(1 << inputs.size)).map do |row|
            flat.reset
            inputs.each_with_index { |name, i| flat.set(name, (row >> (inputs.size - 1 - i)) & 1) }
            return :unstable unless flat.settle

            circuit.outs.map { |o| flat.output(o) }
        end
    end

    def test_the_fixtures_give_the_tables_the_page_gives
        FIXTURES['tables'].each do |f|
            assert_equal(f['unstable'] ? :unstable : f['rows'], table(f['text']), f['name'])
        end
    end

    def test_a_circuit_with_memory_keeps_its_state
        FIXTURES['sequences'].each do |f|
            circuit, = CircuitCheck.parse(f['text'])
            flat = CircuitCheck.flatten(circuit)
            f['steps'].each do |step|
                step['set'].each { |name, bit| flat.set(name, bit) }
                assert flat.settle, f['name']
                assert_equal step['q'], flat.output('q'), "#{f['name']}: #{step}"
            end
        end
    end

    def test_a_netlist_that_cannot_be_read_says_why_as_the_page_does
        FIXTURES['errors'].each do |f|
            _, errors = CircuitCheck.parse(f['text'])
            assert errors.first.to_s.include?(f['message']), "#{f['message']}: #{errors.inspect}"
        end
    end

    def test_a_right_circuit_passes_and_says_what_was_read
        status, message, read = check(XOR, XOR_NAND)
        assert_equal :correct, status
        assert_equal 'Alle 4 Zeilen der Wahrheitstabelle stimmen.', message
        assert_includes read, 'Gatter: 4 (4 NAND)'
    end

    def test_the_first_wrong_row_is_named
        status, message, = check(XOR, 'in a; in b; nand x a b; out y x')
        assert_equal :wrong, status
        assert_equal 'Bei a = 0 und b = 0 leuchtet die Lampe y, sie sollte aus sein.', message
        _, message, = check(XOR.merge('gates' => %w[nand and]), 'in a; in b; and x a b; out y x')
        assert_equal 'Bei a = 0 und b = 1 ist die Lampe y aus, sie sollte leuchten.', message
    end

    def test_other_gates_too_many_gates_and_blocks_are_refused
        _, message, = check(XOR, 'in a; in b; xor x a b; out y x')
        assert_equal 'In dieser Aufgabe gibt es nur NAND. Deine Schaltung hat auch XOR.', message
        five = 'in a; in b; nand na a a; nand nb b b; nand l a nb; nand r na b; nand o l r; out y o'
        status, message, = check(XOR, five)
        assert_equal :wrong, status
        assert_includes message, 'Aber deine Schaltung hat 5 Gatter; es geht mit 4.'
        _, message, = check(XOR, "block X\n in a\n in b\n nand g a b\n out o g\nend\nin a; in b; X x a b; out y x.o")
        assert_includes message, 'ohne Bausteine'
    end

    def test_gates_inside_blocks_count_and_blocks_can_be_required
        text = "#{HA}in a; in b; in c_in; HA h1 a b; HA h2 h1.s c_in; or o h1.c h2.c; out s h2.s; out c_out o"
        status, _, read = check(FULL.merge('max_gates' => 5), text)
        assert_equal :correct, status
        assert_includes read, '4 davon in Bausteinen'
        assert_includes read, 'Bausteine: 2 × HA'
        _, message, = check(FULL.merge('max_gates' => 4), text)
        assert_includes message, 'Aber deine Schaltung hat 5 Gatter, 4 davon in Bausteinen; es geht mit 4.'
        flat = 'in a; in b; in c_in; xor x1 a b; xor x2 x1 c_in; and k1 a b; and k2 x1 c_in; or o k1 k2; out s x2; out c_out o'
        _, message, = check(FULL, flat)
        assert_includes message, 'Gefragt sind mindestens 2, in deiner Schaltung steht keiner.'
        _, message, = check(FULL.merge('gates' => %w[or]), text)
        assert_includes message, 'auch in deinen Bausteinen'
    end

    def test_switches_and_lamps_must_be_the_tasks_and_every_input_wired
        assert_equal 'Der Schalter b fehlt. Die Aufgabe braucht die Schalter a und b.', check(XOR, 'in a; nand x a a; out y x')[1]
        assert_includes check(XOR, 'in a; in b; in c; nand x a b; out y x')[1], 'Den Schalter c gibt es in dieser Aufgabe nicht'
        assert_equal 'Die Lampe y hat noch keine Leitung.', check(XOR, CircuitCheck.starter(XOR))[1]
        assert_includes check(XOR, 'in a; in b; nand x a -; out y x')[1], 'Am NAND x ist ein Eingang ohne Leitung.'
        assert_includes check(FULL, "block HA\n in a\n in b\n and k a -\n out s k\n out c k\nend\nin a; in b; in c_in; HA h a b; out s h.s; out c_out h.c")[1],
                        'Am UND k im Baustein HA ist ein Eingang ohne Leitung.'
    end

    def test_numbers_of_lamps_show_the_whole_number
        spec = { 'type' => 'circuit', 'inputs' => %w[a1 a0 b1 b0], 'outputs' => %w[c s1 s0],
                 'numbers' => { 'a' => %w[a1 a0], 'b' => %w[b1 b0], 'sum' => %w[c s1 s0] }, 'expect' => { 'sum' => 'a + b' } }
        assert_empty CircuitCheck.spec_errors(spec)
        wrong = 'in a1; in a0; in b1; in b0; xor x0 a0 b0; and k0 a0 b0; xor x1 a1 b1; and k1 a1 b1; out s0 x0; out s1 x1; out c k1'
        _, message, = check(spec, wrong)
        assert_equal 'Bei a = 01 (1) und b = 01 (1) zeigen die Lampen c s1 s0 `000`, richtig wäre `010` (2).', message
    end

    def test_a_sequence_names_the_step_and_what_must_hold
        spec = { 'type' => 'circuit', 'inputs' => %w[d takt], 'outputs' => %w[q],
                 'sequence' => [{ 'd' => 1, 'takt' => 1, 'q' => 1 }, { 'takt' => 0, 'q' => 1 }, { 'd' => 0, 'q' => 1, 'say' => 'Q hält.' }] }
        assert_empty CircuitCheck.spec_errors(spec)
        assert_equal :correct, check(spec, FIXTURES['sequences'][0]['text'])[0]
        assert_equal 'Im 3. Schritt (d = 0 und takt = 0) ist die Lampe q aus, sie sollte leuchten. Q hält.', check(spec, 'in d; in takt; out q d')[1]
        _, message, = check(spec, 'in d; in takt; not n n; and a n d; out q a')
        assert_includes message, 'Im 1. Schritt (d = 1 und takt = 1) kommt deine Schaltung nicht zur Ruhe'
    end

    def test_what_cannot_be_read_does_not_count
        assert_equal :invalid, check(XOR, '')[0]
        assert_equal :invalid, check(XOR, 'in a; flub')[0]
        assert_equal :invalid, check(XOR, "in a\n" * 2100)[0]
    end

    def test_expressions_follow_the_precedence_of_c
        value = ->(text, env = {}) { CircuitCheck::Expr.value(CircuitCheck::Expr.parse(text), env) }
        assert_equal 7, value.('1 + 2 * 3')
        assert_equal 1, value.('1 | 0 & 0')
        assert_equal 1, value.('2 > 1 == 1')
        assert_equal 5, value.('s ? b : a', { 's' => 1, 'a' => 4, 'b' => 5 })
        assert_equal 0, value.('!3')
        assert_equal 1, value.('(5 >> 2) & 1')
        assert_raises(ArgumentError) { CircuitCheck::Expr.parse('a +') }
        assert_raises(ArgumentError) { CircuitCheck::Expr.parse('(a') }
    end

    def test_task_spec_knows_the_check_and_its_mistakes
        task = { 'id' => 'x', 'title' => 'XOR', 'prompt' => 'Bau es.', 'check' => XOR }
        assert_empty TaskSpec.validate_single_task(task)
        bad = ->(check) { TaskSpec.validate_single_task(task.merge('check' => XOR.merge(check))).join("\n") }
        assert_includes bad.('expect' => { 'y' => 'a ^ q' }), 'unknown names q'
        assert_includes bad.('expect' => { 'z' => 'a' }), 'every lamp once'
        assert_includes bad.('gates' => %w[nand xnor]), 'gates must list'
        assert_includes bad.('sequence' => [{ 'a' => 1 }]), 'either expect'
        assert_includes bad.('max_gates' => 0), 'max_gates must be a positive number'
        assert_includes bad.('colour' => 'red'), 'unknown keys for circuit: colour'
    end
end
