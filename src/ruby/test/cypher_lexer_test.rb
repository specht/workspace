require 'minitest/autorun'
begin
    require_relative '../include/cypher_lexer'
rescue LoadError
    # rouge is a gem of the Ruby service; the test runs there.
end

class CypherLexerTest < Minitest::Test
    def setup
        skip 'rouge not installed' unless defined?(Rouge::Lexers::WorkspaceCypher)
    end

    def tokens(source)
        Rouge::Lexers::WorkspaceCypher.new.lex(source).reject { |_, v| v.strip.empty? }.map { |t, v| [t.qualname, v] }
    end

    # Variable, label, relationship type and property key look different.
    def test_the_parts_of_a_pattern
        t = tokens("MATCH (m:Movie {title: 'Back to the Future'})-[r:IN_GENRE]->(:Genre)\nRETURN m.year, type(r)")
        assert_includes t, ['Keyword', 'MATCH']
        assert_includes t, ['Name', 'm']
        assert_includes t, ['Name.Class', 'Movie']
        assert_includes t, ['Name.Class', 'Genre']
        assert_includes t, ['Name.Attribute', 'title']
        assert_includes t, ['Literal.String.Single', "'Back to the Future'"]
        assert_includes t, ['Name', 'r']
        assert_includes t, ['Name.Function', 'IN_GENRE']
        assert_includes t, ['Name.Attribute', 'year']
        assert_includes t, ['Name.Builtin', 'type']
    end

    def test_alternatives_lengths_and_schema
        t = tokens("MATCH p = shortestPath((a)-[*..8]-(b)), (x)-[:ACTOR|DIRECTOR]->(y)\n" \
                   "CREATE CONSTRAINT movie_id IF NOT EXISTS FOR (m:Movie) REQUIRE m.id IS NODE KEY; // Kommentar")
        assert_includes t, ['Name.Builtin', 'shortestPath']
        assert_includes t, ['Literal.Number', '8']
        assert_includes t, ['Name.Function', 'ACTOR']
        assert_includes t, ['Name.Function', 'DIRECTOR']
        %w[CONSTRAINT IF NOT EXISTS FOR REQUIRE IS NODE KEY].each { |k| assert_includes t, ['Keyword', k] }
        assert_includes t, ['Name.Class', 'Movie']
        assert_includes t, ['Comment.Single', '// Kommentar']
    end

    def test_every_cypher_block_in_the_content_is_fully_lexed
        Dir[File.expand_path('../../content/**/*.md', __dir__)].each do |file|
            File.read(file, encoding: 'UTF-8').scan(/^```cypher\n(.*?)^```/m).each do |(block)|
                left = Rouge::Lexers::WorkspaceCypher.new.lex(block)
                    .select { |t, v| [Rouge::Token::Tokens::Error, Rouge::Token::Tokens::Text].include?(t) && !v.strip.empty? }
                assert_empty left, "#{file}: #{left.map(&:last).join(' ')}"
            end
        end
    end
end
