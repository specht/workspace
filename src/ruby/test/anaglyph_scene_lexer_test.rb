require 'minitest/autorun'
begin
    require_relative '../include/anaglyph_scene_lexer'
rescue LoadError
    # rouge is a gem of the Ruby service; the test runs there.
end

class AnaglyphSceneLexerTest < Minitest::Test
    def setup
        skip 'rouge not installed' unless defined?(Rouge::Lexers::AnaglyphScene)
    end

    def tokens(source)
        Rouge::Lexers::AnaglyphScene.new.lex(source).reject { |_, v| v.strip.empty? }.map { |t, v| [t.qualname, v] }
    end

    def test_a_colour_code_is_a_value_not_a_comment
        t = tokens("fill = #4aa03f\nshape = box\n")
        assert_equal [['Name.Property', 'fill'], ['Punctuation', '='], ['Literal.String', '#4aa03f'],
                      ['Name.Property', 'shape'], ['Punctuation', '='], ['Literal.String', 'box']], t
    end

    def test_numbers_names_and_comments
        t = tokens("; die Sonne\nfill = 1, 0.5, 0\nmove = -150, 0, 0\ngroup\nend\n")
        assert_includes t, ['Comment.Single', '; die Sonne']
        assert_includes t, ['Literal.Number', '0.5']
        assert_includes t, ['Literal.Number', '-150']
        assert_includes t, ['Punctuation', ',']
        assert_includes t, ['Keyword', 'group']
        assert_includes t, ['Keyword', 'end']
    end

    def test_loops_and_expressions
        t = tokens("loop x from -100 to 100 step 100\n    shape = box\n    move = (x - 2) * 100, 0, 0\nend\n")
        assert_includes t, ['Keyword', 'loop']
        assert_includes t, ['Name.Variable', 'x']
        assert_includes t, ['Keyword', 'from']
        assert_includes t, ['Keyword', 'step']
        assert_includes t, ['Literal.Number', '-100']
        assert_includes t, ['Operator', '*']
        assert_includes t, ['Operator', '-']
        assert_includes t, ['Literal.String', 'x']
        assert_includes t, ['Keyword', 'end']
    end

    def test_a_hash_line_is_a_comment
        assert_equal [['Comment.Single', '# alt']], tokens("# alt\n")
    end
end
