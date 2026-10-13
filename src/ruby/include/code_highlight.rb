require 'rouge'
require_relative 'er_lexer'
require_relative 'cypher_lexer'

module CodeHighlight
    # Fenced code blocks: Rouge by the fence's language. A fence without a
    # language is plain text, like ```text. Used for page content and for the
    # task cards, which are inserted after the page is rendered.
    # root: a Nokogiri document or fragment.
    def self.apply(root)
        root.css('pre').each do |pre|
            code = pre.css('code').first
            next if code.nil?
            language = code.attr('class')
            lineno = false
            if language =~ /_lineno$/
                lineno = true
                language = language.sub(/_lineno$/, '')
            end
            wrap = false
            if language =~ /_wrap$/
                wrap = true
                language = language.sub(/_wrap$/, '')
            end

            pre['class'] = [pre['class'], 'wrap-code'].compact.join(' ') if wrap

            formatter = if lineno
                Rouge::Formatters::HTMLTable.new(Rouge::Formatters::HTML.new)
                # Rouge::Formatters::HTMLLineTable.new(Rouge::Formatters::HTML.new)
            else
                Rouge::Formatters::HTML.new
            end
            lexer = nil
            case language
            when 'bash'
                lexer = Rouge::Lexers::Shell.new
            when 'basic'
                lexer = Rouge::Lexers::VisualBasic.new
            when 'bibtex'
                lexer = Rouge::Lexers::BibTeX.new
            when 'c'
                lexer = Rouge::Lexers::C.new
            when 'clisp'
                lexer = Rouge::Lexers::CommonLisp.new
            when 'cpp'
                lexer = Rouge::Lexers::Cpp.new
            when 'cs'
                lexer = Rouge::Lexers::CSharp.new
            when 'css'
                lexer = Rouge::Lexers::CSS.new
            when 'cypher'
                lexer = Rouge::Lexers::WorkspaceCypher.new
                pre['class'] = [pre['class'], 'code-cypher'].compact.join(' ')
            when 'dart'
                lexer = Rouge::Lexers::Dart.new
            when 'er'
                lexer = Rouge::Lexers::ER.new
                pre['class'] = [pre['class'], 'code-er'].compact.join(' ')
            when 'erlang'
                lexer = Rouge::Lexers::Erlang.new
            when 'fortran'
                lexer = Rouge::Lexers::Fortran.new
            when 'go'
                lexer = Rouge::Lexers::Go.new
            when 'html'
                lexer = Rouge::Lexers::HTML.new
            when 'ini'
                lexer = Rouge::Lexers::INI.new
            when 'java'
                lexer = Rouge::Lexers::Java.new
            when 'js'
                lexer = Rouge::Lexers::Javascript.new
            when 'json'
                lexer = Rouge::Lexers::JSON.new
            when 'lua'
                lexer = Rouge::Lexers::Lua.new
            when 'markdown'
                lexer = Rouge::Lexers::Markdown.new
            when 'nasm'
                lexer = Rouge::Lexers::Nasm.new
            when 'pascal'
                lexer = Rouge::Lexers::Pascal.new
            when 'python'
                lexer = Rouge::Lexers::Python.new
            when 'ruby'
                lexer = Rouge::Lexers::Ruby.new
            when 'rust'
                lexer = Rouge::Lexers::Rust.new
            when 'smalltalk'
                lexer = Rouge::Lexers::Smalltalk.new
            when 'sql'
                lexer = Rouge::Lexers::SQL.new
            when 'svelte'
                lexer = Rouge::Lexers::Svelte.new
            when 'tex'
                lexer = Rouge::Lexers::TeX.new
            when 'text'
                lexer = Rouge::Lexers::PlainText.new
            end
            lexer ||= Rouge::Lexers::PlainText.new if language.nil? || language.empty?
            next if lexer.nil?
            pre.content = ''
            # if lineno
            #     pre << "<div style='border-bottom: 1px solid white;display: inline-table;width: 100%;margin-bottom: 0.5em;padding-bottom: 0.5em;font-style: italic;font-weight: normal;'>HelloWorld.java (124 Bytes)</div>"
            # end
            pre << formatter.format(lexer.lex(code.text))
        end
        root
    end
end
