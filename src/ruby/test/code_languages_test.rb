require 'minitest/autorun'
require 'nokogiri'
require 'redcarpet'
require_relative '../include/code_languages'
require_relative '../include/task_spec'

class CodeLanguagesTest < Minitest::Test
    PAGE = <<~MD
        Für alle.

        <!-- language: ruby -->
        ```ruby
        def compress(data)
        ```

        Ein Hash mit Arrays als Schlüsseln.
        <!-- /language -->
        <!-- language: python -->
        ```python
        def compress(data):
        ```

        Ein `dict` mit Bytes als Schlüsseln.
        <!-- /language -->

        Wieder für alle.

        ```markdown
        <!-- language: ruby -->
        ```
    MD

    def render(markdown)
        html = Redcarpet::Markdown.new(Redcarpet::Render::HTML, :fenced_code_blocks => true).render(CodeLanguages.mark(markdown))
        CodeLanguages.wrap(Nokogiri::HTML(html))
    end

    # a typical wrong solution is Ruby code; the Java variant does not get it
    def test_a_variant_keeps_feedback_but_not_wrong_solutions_of_another_language
        task = { 'id' => 'add', 'check' => { 'type' => 'function' },
                 'feedback' => [{ 'when' => { 'same_as' => "def add(a, b)\nend\n" }, 'say' => 'Ruby' },
                                { 'when' => { 'equals' => 3 }, 'say' => 'neutral' }],
                 'java' => { 'check' => { 'type' => 'function', 'language' => 'java' } },
                 'dart' => { 'check' => { 'type' => 'function', 'language' => 'dart' },
                             'feedback' => [{ 'when' => { 'same_as' => "int add(int a, int b) {}\n" }, 'say' => 'Dart' }] } }
        assert_equal ['neutral'], CodeLanguages.variant(task, 'java')['feedback'].map { |f| f['say'] }
        assert_equal ['Dart'], CodeLanguages.variant(task, 'dart')['feedback'].map { |f| f['say'] }
        assert_equal ['Ruby', 'neutral'], CodeLanguages.variant(task, 'ruby')['feedback'].map { |f| f['say'] }
        refute CodeLanguages.variant(task.merge('feedback' => task['feedback'].first(1)), 'java').key?('feedback')
    end

    def test_parts_are_wrapped_with_their_markdown
        root = render(PAGE)
        boxes = root.css('div.code-language')
        assert_equal %w[ruby python], boxes.map { |b| b['data-language'] }
        assert_includes boxes[0].text, 'def compress(data)'
        assert_includes boxes[0].text, 'Arrays als Schlüsseln'
        assert_includes boxes[1].at_css('code').text, 'def compress(data):'
        assert_equal 'dict', boxes[1].at_css('p code').text
        refute root.at_css('div.code-language-start, div.code-language-end')
        texts = root.css('body > p').map(&:text)
        assert_includes texts, 'Für alle.'
        assert_includes texts, 'Wieder für alle.'
    end

    def test_a_part_can_be_for_several_languages
        root = render("<!-- language: javascript dart java -->\n\nA\n\n<!-- /language -->\n")
        assert_equal 'javascript dart java', root.at_css('div.code-language')['data-language']
        warnings = []
        CodeLanguages.mark("<!-- language: ruby, kotlin -->\n", warn: ->(w) { warnings << w })
        assert_equal ['<!-- language: kotlin -->: unknown language'], warnings
    end

    def test_parts_right_after_each_other_stand_side_by_side
        root = CodeLanguages.pair(render(PAGE))
        compare = root.at_css('div.code-compare')
        assert_equal %w[ruby python], compare.css('> div.code-language').map { |b| b['data-language'] }
        assert_equal 1, root.css('div.code-compare').size
    end

    def test_parts_apart_or_alone_stay_as_they_are
        root = CodeLanguages.pair(render("<!-- language: ruby -->\n\nA\n\n<!-- /language -->\n\nDazwischen.\n\n<!-- language: python -->\n\nB\n\n<!-- /language -->\n"))
        refute root.at_css('div.code-compare')
        assert_equal 2, root.css('div.code-language').size
    end

    def test_markers_in_code_blocks_stay_text
        root = render(PAGE)
        assert_includes root.css('pre code').last.text, '<!-- language: ruby -->'
    end

    def test_unclosed_and_stray_markers_are_reported
        warnings = []
        html = Redcarpet::Markdown.new(Redcarpet::Render::HTML).render(CodeLanguages.mark("<!-- language: ruby -->\n\nA\n\n<!-- /language -->\n\nB\n\n<!-- /language -->\n\n<!-- language: python -->\n\nC\n"))
        CodeLanguages.wrap(Nokogiri::HTML(html), :warn => ->(m) { warnings << m })
        assert warnings.any? { |w| w.include?('without <!-- /language -->') }
        assert warnings.any? { |w| w.include?('<!-- /language --> without') }
    end

    def test_page_languages_from_meta
        assert_equal %w[ruby python], CodeLanguages.of_page({ 'languages' => %w[Ruby Python] })
        assert_equal [], CodeLanguages.of_page({ 'languages' => ['ruby'] })
        assert_equal [], CodeLanguages.of_page({ 'language' => 'Ruby' })
        assert_equal [], CodeLanguages.of_page(nil)
    end

    def test_own_choice_then_class_then_ruby
        assert_equal 'python', CodeLanguages.resolve('python', { '10a' => 'ruby' })
        assert_equal 'python', CodeLanguages.resolve(nil, { '10a' => nil, 'LK' => 'python' })
        assert_equal 'ruby', CodeLanguages.resolve(nil, { '10a' => nil })
        assert_equal 'ruby', CodeLanguages.resolve('cobol', {})
        # JavaScript, picked on a Kata page, is passed over where a page
        # does not offer it: the class's language comes next
        assert_equal 'python', CodeLanguages.resolve('javascript', { '10a' => 'python' }, %w[ruby python])
        assert_equal 'javascript', CodeLanguages.resolve('javascript', { '10a' => 'python' }, %w[ruby python javascript])
        assert_equal 'ruby', CodeLanguages.resolve('javascript', {}, %w[ruby python])
        assert_equal 'python', CodeLanguages.resolve(nil, {}, %w[python])
    end

    # A class ranks its languages: each page shows the first it offers;
    # only the first class with a list counts; the person's own choices first
    def test_ranked_class_languages
        assert_equal 'javascript', CodeLanguages.resolve(nil, { '10a' => %w[javascript python] }, %w[ruby python javascript])
        assert_equal 'python', CodeLanguages.resolve(nil, { '10a' => %w[javascript python] }, %w[ruby python])
        assert_equal 'ruby', CodeLanguages.resolve(nil, { '10a' => %w[java] }, %w[ruby python])
        assert_equal 'dart', CodeLanguages.resolve(nil, { '10a' => %w[java] }, %w[dart javascript])
        assert_equal 'python', CodeLanguages.resolve(nil, { '9b' => nil, '10a' => %w[python], 'LK' => %w[java] }, %w[ruby python java])
        assert_equal 'ruby', CodeLanguages.resolve(['javascript', 'ruby'], { '10a' => %w[python] }, %w[ruby python])
        assert_equal 'javascript', CodeLanguages.resolve([nil, nil], { '10a' => %w[javascript] }, %w[ruby javascript])
    end

    # Before the list there was one language per class: it reads as a list
    def test_ranked_reads_the_old_value
        assert_equal %w[python], CodeLanguages.ranked('python')
        assert_equal %w[javascript ruby], CodeLanguages.ranked(%w[javascript cobol ruby javascript])
        assert_nil CodeLanguages.ranked(nil)
        assert_nil CodeLanguages.ranked([])
        assert_nil CodeLanguages.ranked('cobol')
    end

    TASK = {
        'id' => 'kompressor', 'title' => 'Der Kompressor', 'prompt' => 'Schreib `compress` in `lzw.rb`.',
        'check' => { 'type' => 'ruby', 'file' => 'lzw/lzw.rb', 'cases' => [{ "compress('A')" => '[65]' }] },
        'hint' => 'Ein Hash.',
        'python' => {
            'prompt' => 'Schreib `compress` in `lzw.py`.',
            'check' => { 'type' => 'python', 'file' => 'lzw/lzw.py', 'cases' => [{ "compress(b'A')" => '[65]' }] },
        },
    }.freeze

    def test_variant_replaces_only_what_differs
        python = CodeLanguages.variant(TASK, 'python')
        assert_equal 'python', python['check']['type']
        assert_includes python['prompt'], 'lzw.py'
        assert_equal 'Ein Hash.', python['hint']
        refute python.key?('python')
        ruby = CodeLanguages.variant(TASK, 'ruby')
        assert_equal 'ruby', ruby['check']['type']
        assert_equal %w[python], CodeLanguages.variant_languages(TASK)
    end

    def test_one_version_for_all_languages
        plain = TASK.reject { |k, _| k == 'python' }
        assert_equal TaskSpec.check_version(plain), TaskSpec.check_version(plain.dup), 'a task without variants keeps its version'
        refute_equal TaskSpec.check_version(plain), TaskSpec.check_version(TASK)
        changed = TASK.merge('python' => TASK['python'].merge('check' => TASK['python']['check'].merge('file' => 'lzw/other.py')))
        refute_equal TaskSpec.check_version(TASK), TaskSpec.check_version(changed)
        reworded = TASK.merge('python' => TASK['python'].merge('prompt' => 'Anders gesagt.'))
        assert_equal TaskSpec.check_version(TASK), TaskSpec.check_version(reworded)
    end

    def test_spec_checks_every_variant
        data = { 'activity' => 'lzw', 'tasks' => [TASK] }
        assert_empty TaskSpec.validate(data, activity: 'lzw')
        broken = TASK.merge('python' => TASK['python'].merge('check' => TASK['python']['check'].merge('file' => 'lzw/lzw.rb')))
        errors = TaskSpec.validate({ 'activity' => 'lzw', 'tasks' => [broken] }, activity: 'lzw')
        assert errors.any? { |e| e.include?('python: check: file must end in .py') }, errors.inspect
        extra = TASK.merge('python' => TASK['python'].merge('title' => 'Anders'))
        errors = TaskSpec.validate({ 'activity' => 'lzw', 'tasks' => [extra] }, activity: 'lzw')
        assert errors.any? { |e| e.include?('only prompt, check') }, errors.inspect
    end

    def test_switch_presses_the_current_language
        html = CodeLanguages.switch_html(%w[ruby python], 'python')
        assert_includes html, "data-set-language='python' aria-pressed='true'"
        assert_includes html, "data-set-language='ruby' aria-pressed='false'"
    end
end
