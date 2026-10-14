require 'minitest/autorun'
begin
    require_relative '../include/minicpu_lexer'
rescue LoadError
    # rouge is a gem of the Ruby service; the test runs there.
end

class MiniCPULexerTest < Minitest::Test
    def setup
        skip 'rouge not installed' unless defined?(Rouge::Lexers::MiniCPU)
    end

    def tokens(source)
        Rouge::Lexers::MiniCPU.new.lex(source).reject { |_, v| v.strip.empty? }.map { |t, v| [t.qualname, v] }
    end

    def test_instructions_registers_labels_and_values
        t = tokens(".cpu 8s\nloop:   lda a, [SP+1]      ; Kommentar\n        STA [X], A\n        JNZ loop")
        assert_includes t, ['Comment.Preproc', '.cpu']
        assert_includes t, ['Literal.Number.Integer', '8s']
        assert_includes t, ['Name.Label', 'loop']
        assert_includes t, ['Keyword', 'lda']
        assert_includes t, ['Name.Builtin', 'a']
        assert_includes t, ['Name.Builtin', 'SP']
        assert_includes t, ['Name.Builtin', 'X']
        assert_includes t, ['Comment.Single', '; Kommentar']
        assert_includes t, ['Name', 'loop']
    end

    def test_directives_numbers_and_text
        t = tokens("WIDTH .equ 16\n.db 0x2A, 0b1010_0101, \"Hi\", 0\nLDI B, 'A' + WIDTH")
        assert_includes t, ['Name', 'WIDTH']
        assert_includes t, ['Comment.Preproc', '.equ']
        assert_includes t, ['Literal.Number.Hex', '0x2A']
        assert_includes t, ['Literal.Number.Bin', '0b1010_0101']
        assert_includes t, ['Literal.String.Double', '"Hi"']
        assert_includes t, ['Literal.String.Char', "'A'"]
    end
end
