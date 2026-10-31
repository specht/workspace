require 'minitest/autorun'
begin
    require_relative '../include/er_lexer'
rescue LoadError
    # rouge is a gem of the Ruby service; the test runs there.
end

class ErLexerTest < Minitest::Test
    def setup
        skip 'rouge not installed' unless defined?(Rouge::Lexers::ER)
    end

    def tokens(source)
        Rouge::Lexers::ER.new.lex(source).reject { |_, v| v.strip.empty? }.map { |t, v| [t.qualname, v] }
    end

    def test_entities_attributes_and_flags
        t = tokens("entity movie  # Filme\n  id: int, primary key\n  title: varchar(200), optional\n  code: text, key, unique\n")
        assert_includes t, ['Keyword', 'entity']
        assert_includes t, ['Name.Class', 'movie']
        assert_includes t, ['Comment.Single', '# Filme']
        assert_includes t, ['Name.Attribute', 'title']
        assert_includes t, ['Keyword.Type', 'varchar']
        assert_includes t, ['Literal.Number', '200']
        assert_includes t, ['Keyword.Pseudo', 'primary key']
        assert_includes t, ['Keyword.Pseudo', 'key']
        assert_includes t, ['Keyword.Reserved', 'optional']
        assert_includes t, ['Keyword.Reserved', 'unique']
    end

    def test_relationships_and_their_attributes
        t = tokens("relationship credit: movie, crew, job as movie_crew\nrelationship loan: customer n:m copy\n  borrowed: date\n")
        assert_includes t, ['Name.Function', 'credit']
        assert_includes t, ['Keyword', 'as']
        assert_includes t, ['Name.Class', 'movie_crew']
        assert_includes t, ['Literal.Number', 'n:m']
        assert_includes t, ['Name.Attribute', 'borrowed']
        assert_includes t, ['Keyword.Type', 'date']
    end

    def test_min_max_cardinalities
        t = tokens("relationship has_copy: movie (0,n) : (1,1) copy\n")
        assert_includes t, ['Literal.Number', '(0,n)']
        assert_includes t, ['Literal.Number', '(1,1)']
        assert_includes t, ['Name.Class', 'copy']
    end

    def test_every_er_block_in_the_content_lexes_without_errors
        Dir[File.expand_path('../../content/**/*.md', __dir__)].each do |file|
            File.read(file, encoding: 'UTF-8').scan(/^```er\n(.*?)^```/m).each do |(block)|
                errors = Rouge::Lexers::ER.new.lex(block).select { |t, _| t == Rouge::Token::Tokens::Error }
                assert_empty errors, file
            end
        end
    end
end
