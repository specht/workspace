require 'minitest/autorun'
require 'open3'
require 'tmpdir'
require 'fileutils'
require_relative '../include/unit_tests_check'
require_relative '../include/task_spec'

# Runs the script of check type unit_tests with the local frameworks, on the
# Notenrechner of »Testen« (src/content/testen/loesung): the full tests pass
# the right version and catch the broken ones, tests that miss a boundary
# let one through, and what is missing or broken is said.
class UnitTestsCheckTest < Minitest::Test
    CONTENT = File.expand_path('../../content/testen', __dir__)
    FILES = { 'ruby' => 'rb', 'python' => 'py', 'javascript' => 'js' }.freeze
    # one change each in loesung/noten.*, the same mistake in every language
    MUTANTS = {
        'ruby' => [['points.between?(0, 15)', 'points.between?(0, 16)'], ['tenths = (1020', 'tenths = (1021']],
        'python' => [['0 <= points <= 15', '0 <= points <= 16'], ['tenths = (1020', 'tenths = (1021']],
        'javascript' => [['points > 15', 'points > 16'], ['Math.floor((1020', 'Math.floor((1021']],
    }.freeze

    def available?(language)
        command = { 'ruby' => 'ruby -e "require \'minitest\'"', 'python' => 'python3 -m pytest --version',
                    'javascript' => 'node --version' }[language]
        system(command, :out => File::NULL, :err => File::NULL)
    end

    def setup
        @root = Dir.mktmpdir
        FileUtils.mkdir_p(File.join(@root, 'notenrechner'))
    end

    def teardown
        FileUtils.rm_rf(@root) if @root
    end

    def check(language)
        ext = FILES[language]
        data = { 'tasks' => [{ 'id' => 'tests', 'check' => {
            'type' => 'unit_tests', 'language' => language, 'file' => "notenrechner/test_noten.#{ext}",
            'correct' => "loesung/noten.#{ext}",
            'mutants' => MUTANTS[language].each_with_index.map { |pair, i| { 'replace' => pair, 'say' => "Fehler #{i}" } },
        } }] }
        assert_empty UnitTestsCheck.resolve!(data, CONTENT)
        data['tasks'][0]['check']
    end

    def run_check(check, tests)
        File.write(File.join(@root, check['file']), tests) if tests
        output, = Open3.capture2e('sh', '-c', UnitTestsCheck.command_for_check(check), :chdir => @root)
        UnitTestsCheck.evaluate({}, check, output)
    end

    def reference_tests(language)
        File.read(File.join(CONTENT, 'loesung', "test_noten.#{FILES[language]}"))
    end

    def test_the_full_tests_pass_and_catch_every_mistake
        FILES.each_key do |language|
            next unless available?(language)

            status, message, read = run_check(check(language), reference_tests(language))
            assert_equal :correct, status, "#{language}: #{message}\n#{read}"
            assert_includes message, 'Deine 6 Tests bestehen'
            assert_includes message, 'allen 2 fehlerhaften'
        end
    end

    # without the test at 16 the first mistake slips through; without the
    # tests of the table the second
    def test_a_missing_boundary_lets_a_mistake_through
        FILES.each_key do |language|
            next unless available?(language)

            c = check(language)
            tests = reference_tests(language)
            no_16 = tests.lines.reject { |l| l.match?(/grade\(16\)|grade, 16/) }
            no_16 = language == 'python' ? no_16.join.sub(/    with pytest\.raises\(ValueError\):\n(?=    with|\n)/, '') : no_16.join
            status, message, = run_check(c, no_16)
            assert_equal :wrong, status, language
            assert_equal 'Fehler 0', message, language
        end
    end

    def test_a_wrong_expectation_fails_with_the_right_version
        skip 'no minitest' unless available?('ruby')
        status, message, read = run_check(check('ruby'), reference_tests('ruby').sub("assert_equal '6', grade(0)", "assert_equal '5', grade(0)"))
        assert_equal :wrong, status
        assert_includes message, 'Mit dem richtigen `noten.rb` bestehen nicht alle deine Tests'
        assert_includes read, 'Expected: "5"'
        refute_includes read, '/tmp/'
    end

    def test_no_test_yet_and_no_file
        skip 'no minitest' unless available?('ruby')
        status, message, = run_check(check('ruby'), "require 'minitest/autorun'\nrequire_relative 'noten'\n")
        assert_equal :wrong, status
        assert_includes message, 'läuft noch kein Test'
        FileUtils.rm_f(File.join(@root, 'notenrechner', 'test_noten.rb'))
        status, message, = run_check(check('ruby'), nil)
        assert_equal :wrong, status
        assert_includes message, '`notenrechner/test_noten.rb` gibt es nicht'
    end

    # what students load with wget is what the recipes and examples use
    def test_the_downloads_are_the_given_files
        FILES.each_value do |ext|
            assert_equal File.read(File.join(CONTENT, 'beispiele', "noten.#{ext}")),
                File.read(File.expand_path("../../static/testen/noten.#{ext}", __dir__)), "noten.#{ext}"
        end
    end

    def test_resolve_and_spec
        data = { 'tasks' => [{ 'id' => 'x', 'check' => {
            'type' => 'unit_tests', 'language' => 'ruby', 'file' => 'n/test_noten.rb', 'correct' => 'loesung/noten.rb',
            'mutants' => [{ 'replace' => ['points', 'p'], 'say' => 'x' }],
        } }] }
        errors = UnitTestsCheck.resolve!(data, CONTENT)
        assert_equal 1, errors.size
        assert_match(/»points« is \d+ times/, errors.first)
        data['tasks'][0]['check']['correct'] = 'loesung/nicht-da.rb'
        assert_includes UnitTestsCheck.resolve!(data, CONTENT).first, 'is not there'

        good = { 'type' => 'unit_tests', 'language' => 'ruby', 'file' => 'n/test_noten.rb', 'correct' => 'loesung/noten.rb',
                 'mutants' => [{ 'replace' => %w[a b], 'say' => 'x' }] }
        assert_empty UnitTestsCheck.spec_errors(good)
        refute_empty UnitTestsCheck.spec_errors(good.merge('language' => 'java'))
        refute_empty UnitTestsCheck.spec_errors(good.merge('file' => 'n/test_noten.py'))
        refute_empty UnitTestsCheck.spec_errors(good.merge('correct' => '../x.rb'))
        refute_empty UnitTestsCheck.spec_errors(good.merge('mutants' => [{ 'replace' => %w[a a], 'say' => 'x' }]))
        refute_empty UnitTestsCheck.spec_errors(good.merge('mutants' => []))
    end
end
