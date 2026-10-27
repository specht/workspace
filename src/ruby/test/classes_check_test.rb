require 'minitest/autorun'
require_relative '../include/classes_check'

# The check classes (ClassesCheck): the task against the summary the
# Klassen-Werkstatt's core prints (node classes-core.js --json), here written
# by hand in that shape.
class ClassesCheckTest < Minitest::Test
    def member(name, params, returns, visibility = '+')
        { 'name' => name, 'constructor' => returns.nil?, 'returns' => returns, 'visibility' => visibility,
          'params' => params.map { |n, t| { 'name' => n, 'type' => t } } }
    end

    def summary
        {
            'version' => '0.1.0', 'errors' => [], 'warnings' => [],
            'classes' => [
                { 'name' => 'Medium', 'abstract' => true, 'extends' => nil,
                  'attributes' => [{ 'name' => 'title', 'type' => 'String', 'visibility' => '-' },
                                   { 'name' => 'available', 'type' => 'boolean', 'visibility' => '-' }],
                  'methods' => [member('Medium', [%w[title String]], nil), member('lend', [], 'void'), member('isAvailable', [], 'boolean')] },
                { 'name' => 'Book', 'abstract' => false, 'extends' => 'Medium',
                  'attributes' => [{ 'name' => 'author', 'type' => 'String', 'visibility' => '-' }],
                  'methods' => [member('Book', [%w[title String], %w[author String]], nil)] },
                { 'name' => 'Member', 'abstract' => false, 'extends' => nil, 'attributes' => [],
                  'methods' => [member('borrow', [%w[medium Medium]], 'void')] },
            ],
            'associations' => [{ 'name' => 'loans', 'from' => 'Member', 'to' => 'Medium', 'fromMult' => '1', 'toMult' => '0..*', 'directed' => false }],
        }
    end

    CHECK = {
        'type' => 'classes', 'file' => 'bibliothek/bibliothek.classes',
        'classes' => [
            { 'name' => 'Medium', 'abstract' => true, 'attributes' => ['- title: String', 'available'], 'methods' => ['+ lend(): void', 'isAvailable(): boolean'] },
            { 'name' => 'Book', 'extends' => 'Medium', 'methods' => ['+ Book(title: String, author: String)'] },
            { 'name' => 'Member', 'methods' => ['borrow(m: Medium): void'] },
        ],
        'associations' => [{ 'between' => %w[Medium Member], 'multiplicities' => ['0..*', '1'], 'directed' => false }],
    }.freeze

    def result(check = CHECK, changed = summary)
        status, problem, message, = ClassesCheck.evaluate({}, check, changed)
        [status, problem, message]
    end

    def test_the_whole_diagram_passes
        assert_equal [:correct, nil, 'Passt.'], result
        assert_empty ClassesCheck.spec_errors(CHECK)
    end

    def test_what_is_missing_or_different
        s = summary
        s['classes'].reject! { |c| c['name'] == 'Book' }
        assert_equal [:wrong, :missing_class, 'Es fehlt die Klasse `Book`.'], result(CHECK, s)

        s = summary
        s['classes'][1]['name'] = 'book'
        assert_match(/Es gibt `book`; Groß- und Kleinschreibung zählen/, result(CHECK, s)[2])

        s = summary
        s['classes'][0]['abstract'] = false
        assert_equal :abstract, result(CHECK, s)[1]

        s = summary
        s['classes'][1]['extends'] = nil
        assert_equal '`Book` soll von `Medium` erben: `class Book extends Medium`.', result(CHECK, s)[2]

        s = summary
        s['classes'][0]['attributes'][0]['type'] = 'int'
        assert_equal '`Medium.title` hat den Typ `int`, gefragt ist `String`.', result(CHECK, s)[2]

        s = summary
        s['classes'][0]['attributes'][0]['visibility'] = '+'
        assert_equal '`Medium.title` soll privat sein (`-`).', result(CHECK, s)[2]

        s = summary
        s['classes'][1]['methods'] = []
        assert_equal '`Book` hat noch keinen Konstruktor.', result(CHECK, s)[2]

        s = summary
        s['classes'][2]['methods'] = [member('borrow', [%w[title String]], 'void')]
        assert_equal :method, result(CHECK, s)[1]

        s = summary
        s['associations'][0]['toMult'] = '1'
        assert_equal :multiplicity, result(CHECK, s)[1]

        s = summary
        s['associations'] = []
        assert_equal 'Es fehlt eine Assoziation zwischen `Medium` und `Member`.', result(CHECK, s)[2]
    end

    def test_errors_in_the_file_come_first
        s = summary
        s['errors'] = [{ 'line' => 3, 'msg' => 'Vor `title` fehlt `+` oder `-`.' }]
        status, problem, message = result(CHECK, s)
        assert_equal [:wrong, :errors], [status, problem]
        assert_includes message, 'Zeile 3: Vor `title` fehlt `+` oder `-`.'
    end

    def test_direction
        check = CHECK.merge('associations' => [{ 'between' => %w[Member Medium], 'directed' => true }])
        assert_equal :direction, result(check)[1]
        s = summary
        s['associations'][0]['directed'] = true
        assert_equal [:correct, nil, 'Passt.'], result(check, s)
    end

    def test_the_read_out_uses_the_files_own_notation
        read = ClassesCheck.describe(summary, 'bibliothek/bibliothek.classes')
        assert_includes read, "class Book extends Medium\n  - author: String\n  + Book(title: String, author: String)"
        assert_includes read, 'association loans: Member 1 -- 0..* Medium'
    end

    def test_spec_errors
        assert_includes ClassesCheck.spec_errors('file' => 'x.classes'), 'classes needs classes or associations'
        assert_includes ClassesCheck.spec_errors('classes' => [{ 'name' => 'Book', 'colour' => 'red' }]), 'classes[0]: unknown keys: colour'
        assert_includes ClassesCheck.spec_errors('associations' => [{ 'between' => %w[A B], 'multiplicities' => ['1', 'viele'] }]),
                        'associations[0].multiplicities must be two, like ["1", "0..*"]'
    end
end
