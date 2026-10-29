require 'minitest/autorun'
require 'tmpdir'
require 'open3'
require_relative '../include/prolog_check'

# Check type prolog (PrologCheck): the spec, the reading of the harness's
# answers, and, where SWI-Prolog is installed, the harness itself.
class PrologCheckTest < Minitest::Test
    CHECK = {
        'file' => 'prolog/familie.pl',
        'cases' => [
            { 'query' => 'grandparent(G, c)', 'answers' => ['G = a'] },
            { 'query' => 'parent(c, X)', 'answers' => [false] },
            { 'query' => 'ancestor(a, c)', 'answers' => [true] },
        ],
    }.freeze

    def output(cases, error: nil)
        result = error ? { 'error' => error, 'cases' => [] } : { 'cases' => cases }
        "noise\n#{PrologCheck::RESULT_MARK}#{JSON.generate(result)}\n"
    end

    def test_spec
        assert_empty PrologCheck.spec_errors(CHECK)
        refute_empty PrologCheck.spec_errors('file' => 'x.rb', 'cases' => CHECK['cases'])
        refute_empty PrologCheck.spec_errors('file' => 'x.pl', 'cases' => [{ 'query' => 'a', 'answers' => 'b' }])
        refute_empty PrologCheck.spec_errors('file' => 'x.pl', 'cases' => [{ 'query' => 'a', 'answers' => [true], 'seconds' => 99 }])
    end

    def test_answers_count_as_a_set_unless_ordered
        out = output([{ 'answers' => ['G = a', 'G = a'] }, { 'answers' => ['false'] }, { 'answers' => ['true'] }])
        status, message, = PrologCheck.evaluate({}, CHECK, out)
        assert_equal :correct, status, message
        ordered = CHECK.merge('cases' => [{ 'query' => 'is_in(X, [a, b])', 'answers' => ['X = a', 'X = b'], 'ordered' => true }])
        status, message, = PrologCheck.evaluate({}, ordered, output([{ 'answers' => ['X = b', 'X = a'] }]))
        assert_equal :wrong, status
        assert_includes message, 'Reihenfolge'
    end

    def test_messages_name_what_is_missing_and_why
        status, message, = PrologCheck.evaluate({}, CHECK, output([{ 'answers' => ['false'] }]))
        assert_equal :wrong, status
        assert_includes message, 'keine Antwort'
        assert_includes message, '`G = a`'
        _, message, = PrologCheck.evaluate({}, CHECK, output([{ 'error' => 'time' }]))
        assert_includes message, 'hört nicht auf'
        _, message, = PrologCheck.evaluate({}, CHECK.merge('slow' => 'Zu langsam.'), output([{ 'error' => 'time' }]))
        assert_equal 'Zu langsam.', message
        _, message, = PrologCheck.evaluate({}, CHECK, output([{ 'error' => 'unknown', 'message' => 'grandparent/2' }]))
        assert_includes message, '`grandparent/2`'
        _, message, = PrologCheck.evaluate({}, CHECK, output([], error: { 'kind' => 'syntax', 'line' => 4, 'message' => "Syntax error: Operator expected\n" }))
        assert_includes message, 'Zeile 4'
    end

    def test_the_harness_with_swi_prolog
        skip 'no swipl here' unless system('swipl --version', :out => File::NULL, :err => File::NULL)
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, 'prolog'))
            File.write(File.join(dir, 'prolog', 'familie.pl'), <<~PL)
                parent(a, b).
                parent(b, c).
                grandparent(G, C) :- parent(G, P), parent(P, C).
                ancestor(X, Y) :- ancestor(X, Z), parent(Z, Y).
                main :- write(student).
            PL
            cases = PrologCheck.cases(CHECK)
            out, = Open3.capture2e('sh', '-c', PrologCheck.command('prolog/familie.pl', cases), :chdir => dir)
            data = PrologCheck.parse(out)
            assert data, out
            assert_equal ['G = a'], data['cases'][0]['answers']
            assert_equal ['false'], data['cases'][1]['answers']
            # the left recursion runs into the time limit
            assert_equal 'time', data['cases'][2]['error']
        end
    end
end
