require 'minitest/autorun'
require_relative '../include/trusted_template'

class TrustedTemplateTest < Minitest::Test
    def test_renders_directives_from_trusted_source
        value = 21
        assert_equal 'answer: 42', TrustedTemplate.render('answer: #{value * 2}', binding)
    end

    def test_does_not_evaluate_directives_returned_by_a_directive
        value = '#{raise "executed"}'
        assert_equal value, TrustedTemplate.render('#{value}', binding)
    end

    def test_does_not_evaluate_directives_in_page_output
        student_code = '#{7 * 6}'
        page = '<pre>#{student_code}</pre>'
        layout = '<main>#{CONTENT}</main>'

        assert_equal '<main><pre>#{7 * 6}</pre></main>',
            TrustedTemplate.render_page(layout, page, binding)
    end

    def test_keeps_escaped_directives_literal
        assert_equal '&#35;{<example>}', TrustedTemplate.render('#{<example>}', binding)
    end

    def test_rejects_unclosed_directives
        assert_raises(ArgumentError) { TrustedTemplate.render('oops #{1 + 2', binding) }
    end
end
