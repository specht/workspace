require 'minitest/autorun'
begin
    require_relative '../include/uml_lexer'
rescue LoadError
    # rouge is a gem of the Ruby service; the test runs there.
end

class UmlLexerTest < Minitest::Test
    def setup
        skip 'rouge not installed' unless defined?(Rouge::Lexers::UML)
    end

    def tokens(source)
        Rouge::Lexers::UML.new.lex(source).reject { |_, v| v.strip.empty? }.map { |t, v| [t.qualname, v] }
    end

    def test_classes_and_members
        t = tokens("# Eine Bibliothek\nclass Book extends Medium (abstract)\n  - author: String\n  + borrow(book: Book): boolean\n")
        assert_includes t, ['Comment.Single', '# Eine Bibliothek']
        assert_includes t, ['Keyword', 'class']
        assert_includes t, ['Name.Class', 'Book']
        assert_includes t, ['Keyword', 'extends']
        assert_includes t, ['Name.Class', 'Medium']
        assert_includes t, ['Keyword.Pseudo', '(abstract)']
        assert_includes t, ['Operator', '-']
        assert_includes tokens("class Book\n  + available?(): boolean\n  + renew!(): void\n"), ['Name.Attribute', 'available?']
        assert_includes t, ['Name.Attribute', 'author']
        assert_includes t, ['Keyword.Type', 'String']
        assert_includes t, ['Name.Attribute', 'borrow']
        assert_includes t, ['Keyword.Type', 'boolean']
    end

    def test_associations
        t = tokens("association loans: Member 0..1 -- 0..3 Book\nassociation owns: Library 1 -> * Medium\n")
        assert_includes t, ['Name.Function', 'loans']
        assert_includes t, ['Literal.Number', '0..1']
        assert_includes t, ['Literal.Number', '0..3']
        assert_includes t, ['Operator', '--']
        assert_includes t, ['Operator', '->']
        assert_includes t, ['Literal.Number', '*']
        assert_includes t, ['Name.Class', 'Library']
    end
end
