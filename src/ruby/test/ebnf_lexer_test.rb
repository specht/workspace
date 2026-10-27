require 'minitest/autorun'
begin
    require_relative '../include/ebnf_lexer'
rescue LoadError
    # rouge is a gem of the Ruby service; the test runs there.
end

class EBNFLexerTest < Minitest::Test
    def setup
        skip 'rouge not installed' unless defined?(Rouge::Lexers::EBNF)
    end

    def tokens(source)
        Rouge::Lexers::EBNF.new.lex(source).reject { |_, v| v.strip.empty? }.map { |t, v| [t.qualname, v] }
    end

    # The defined name, the nonterminals, the terminals and the signs of the
    # notation each look different; a quoted bracket is a terminal.
    def test_the_parts_of_a_rule
        t = tokens(%(expression = term { "+" term }\nfactor     = number | "(" expression ")"))
        assert_includes t, ['Name.Function', 'expression']
        assert_includes t, ['Name.Function', 'factor']
        assert_includes t, ['Name.Class', 'term']
        assert_includes t, ['Name.Class', 'number']
        assert_includes t, ['Name.Class', 'expression']
        assert_includes t, ['Literal.String.Double', '"+"']
        assert_includes t, ['Literal.String.Double', '"("']
        assert_includes t, ['Literal.String.Double', '")"']
        %w[= { } |].each { |o| assert_includes t, ['Operator', o] }
    end

    def test_a_derivation_and_a_gap
        t = tokens(%(sentence\n⇒ "Pip" verb object\ndigit = "0" | … | "9"))
        assert_includes t, ['Name.Class', 'sentence']
        assert_includes t, ['Operator', '⇒']
        assert_includes t, ['Literal.String.Double', '"Pip"']
        assert_includes t, ['Punctuation', '…']
        assert_includes t, ['Name.Function', 'digit']
    end

    def test_nothing_is_an_error
        source = %(sentence = subject verb object\nlist = "[" [ item { "," item } ] "]" (* Kommentar *)\nx ::= 'a' | x 'a')
        assert_empty(Rouge::Lexers::EBNF.new.lex(source).select { |t, _| t == Rouge::Token::Tokens::Error })
    end
end
