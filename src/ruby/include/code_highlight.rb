require 'rouge'
require 'nokogiri'
require_relative 'er_lexer'
require_relative 'uml_lexer'
require_relative 'cypher_lexer'
require_relative 'anaglyph_scene_lexer'
require_relative 'minicpu_lexer'
require_relative 'backrooms_room_lexer'
require_relative 'pseudocode_lexer'
require_relative 'ebnf_lexer'

module CodeHighlight
    # From this many lines on, a code block is set a little smaller.
    LONG_CODE_LINES = 20
    # A line that starts with »~ « (or is only »~«) is context: code that is
    # already in the file, shown faint around the new code so it is clear
    # where that goes. It cannot be selected, and the copy button leaves it
    # out. »~ …« stands for code in between (docs/aktivitaet-schreiben.md).
    CONTEXT = /\A~(?: |$)/

    # A piece of a file starts or ends in the middle of something: a lone
    # »}« that closes what the block does not show is an error to the lexer
    # (C, CSS) and would look unlike every other brace. Brackets on their own
    # are punctuation.
    LONE_BRACKETS = /\A[\]\)\}\[\(\{]+\z/

    def self.tokens(lexer, text)
        lexer.lex(text).map do |token, value|
            token == Rouge::Token::Tokens::Error && value =~ LONE_BRACKETS ? [Rouge::Token::Tokens::Punctuation, value] : [token, value]
        end
    end

    # [text without the markers, line numbers (from 1) that are context]
    def self.split_context(text)
        context = []
        lines = text.lines.each_with_index.map do |line, i|
            next line unless line =~ CONTEXT

            context << i + 1
            line.sub(/\A~ ?/, '')
        end
        # A blank line where context and new code meet belongs to the
        # context, even one the step adds: the cut runs right up to the
        # first and last line of code. One between two new lines stays.
        unless context.empty?
            blank = ->(n) { n.between?(1, lines.size) && lines[n - 1].strip.empty? }
            loop do
                more = (1..lines.size).select do |n|
                    !context.include?(n) && blank.(n) && (context.include?(n - 1) || context.include?(n + 1))
                end
                break if more.empty?
                context.concat(more)
            end
            context.sort!
        end
        [lines.join, context]
    end

    # Fenced code blocks: Rouge by the fence's language. A fence without a
    # language is plain text, like ```text. Used for page content and for the
    # task cards, which are inserted after the page is rendered.
    # root: a Nokogiri document or fragment.
    def self.apply(root)
        root.css('pre').each do |pre|
            code = pre.css('code').first
            next if code.nil?
            # Suffixes after the language, in any order: _lineno, _wrap and
            # _copy (a copy button, for what is no use to type: clone
            # addresses, downloads, setup commands, and big blocks from about
            # 15 lines that go into a file, docs/aktivitaet-schreiben.md).
            language = code.attr('class')
            flags = []
            while language =~ /_(lineno|wrap|copy)$/
                flags << $1
                language = language.sub(/_(lineno|wrap|copy)$/, '')
            end
            lineno = flags.include?('lineno')
            wrap = flags.include?('wrap')

            pre['class'] = [pre['class'], 'wrap-code'].compact.join(' ') if wrap
            # Long listings in a slightly smaller font, like the ones from
            # _include_file (a table with line numbers, .rouge-table).
            pre['class'] = [pre['class'], 'long-code'].compact.join(' ') if !lineno && code.text.lines.size >= LONG_CODE_LINES
            pre['class'] = [pre['class'], 'copy-code'].compact.join(' ') if flags.include?('copy')

            formatter = if lineno
                Rouge::Formatters::HTMLTable.new(Rouge::Formatters::HTML.new)
                # Rouge::Formatters::HTMLLineTable.new(Rouge::Formatters::HTML.new)
            else
                Rouge::Formatters::HTML.new
            end
            lexer = nil
            case language
            when 'bash', 'sh'
                lexer = Rouge::Lexers::Shell.new
            when 'basic'
                lexer = Rouge::Lexers::VisualBasic.new
            when 'bibtex'
                lexer = Rouge::Lexers::BibTeX.new
            when 'c'
                lexer = Rouge::Lexers::C.new
            when 'clisp'
                lexer = Rouge::Lexers::CommonLisp.new
            when 'cobol'
                lexer = Rouge::Lexers::COBOL.new
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
            when 'uml'
                lexer = Rouge::Lexers::UML.new
                pre['class'] = [pre['class'], 'code-uml'].compact.join(' ')
            when 'ebnf'
                # Grammars (Rouge::Lexers::EBNF): nonterminals and terminals
                # apart, as in the syntax diagrams
                lexer = Rouge::Lexers::EBNF.new
                pre['class'] = [pre['class'], 'code-ebnf'].compact.join(' ')
            when 'erlang'
                lexer = Rouge::Lexers::Erlang.new
            when 'fortran'
                lexer = Rouge::Lexers::Fortran.new
            when 'glsl'
                lexer = Rouge::Lexers::GLSL.new
            when 'go'
                lexer = Rouge::Lexers::Go.new
            when 'html'
                lexer = Rouge::Lexers::HTML.new
            when 'ini'
                # Every ```ini on the site is an Anaglyph scene (anaglyph,
                # reference/farben).
                lexer = Rouge::Lexers::AnaglyphScene.new
            when 'java'
                lexer = Rouge::Lexers::Java.new
            when 'js', 'javascript'
                lexer = Rouge::Lexers::Javascript.new
            when 'json'
                lexer = Rouge::Lexers::JSON.new
            when 'lua'
                lexer = Rouge::Lexers::Lua.new
            when 'markdown'
                lexer = Rouge::Lexers::Markdown.new
            when 'minicpu'
                lexer = Rouge::Lexers::MiniCPU.new
                pre['class'] = [pre['class'], 'code-minicpu'].compact.join(' ')
            when 'nasm'
                lexer = Rouge::Lexers::Nasm.new
            when 'pascal'
                lexer = Rouge::Lexers::Pascal.new
            when 'perl'
                lexer = Rouge::Lexers::Perl.new
            when 'php'
                # A whole file starts with <?php and may hold HTML around the
                # PHP; a snippet is PHP from the first character.
                lexer = Rouge::Lexers::PHP.new(:start_inline => !code.text.include?('<?'))
            when 'prolog'
                lexer = Rouge::Lexers::Prolog.new
            when 'pseudocode'
                # The Handbuch's pseudocode (Rouge::Lexers::Pseudocode)
                lexer = Rouge::Lexers::Pseudocode.new
                pre['class'] = [pre['class'], 'code-pseudocode'].compact.join(' ')
            when 'struktogramm'
                # Pseudocode that struktogramm.js draws as a Struktogramm;
                # without JavaScript it stays highlighted pseudocode.
                lexer = Rouge::Lexers::Pseudocode.new
                pre['class'] = [pre['class'], 'code-pseudocode', 'code-struktogramm'].compact.join(' ')
            when 'python'
                lexer = Rouge::Lexers::Python.new
            when 'room'
                # Rooms and levels of the Backrooms: the floor plan as a grid
                lexer = Rouge::Lexers::BackroomsRoom.new
                pre['class'] = [pre['class'], 'code-room'].compact.join(' ')
            when 'ruby'
                lexer = Rouge::Lexers::Ruby.new
            when 'rust'
                lexer = Rouge::Lexers::Rust.new
            when 'smalltalk'
                lexer = Rouge::Lexers::Smalltalk.new
            when 'sql'
                lexer = Rouge::Lexers::SQL.new
            when 'syntaxdiagramm', 'syntaxbaum'
                # EBNF or a tree that syntax_diagram.js draws as a syntax
                # diagram or a syntax tree; without JavaScript the text stays,
                # a grammar highlighted like ```ebnf.
                lexer = language == 'syntaxdiagramm' ? Rouge::Lexers::EBNF.new : Rouge::Lexers::PlainText.new
                pre['class'] = [pre['class'], "code-#{language}", ('code-ebnf' if language == 'syntaxdiagramm')].compact.join(' ')
            when 'svelte'
                lexer = Rouge::Lexers::Svelte.new
            when 'tex'
                lexer = Rouge::Lexers::TeX.new
            when 'text'
                lexer = Rouge::Lexers::PlainText.new
            when 'yaml', 'yml'
                lexer = Rouge::Lexers::YAML.new
            end
            lexer ||= Rouge::Lexers::PlainText.new if language.nil? || language.empty?
            next if lexer.nil?
            text, context = split_context(code.text)
            pre.content = ''
            if context.empty? || lineno
                pre << formatter.format(tokens(lexer, text))
            else
                # One span per line, those with context marked
                pre['class'] = [pre['class'], 'has-code-context'].compact.join(' ')
                lines = Rouge::Formatters::HTMLLinewise.new(formatter, :class => 'code-line-%i').format(tokens(lexer, text))
                fragment = Nokogiri::HTML::DocumentFragment.parse(lines)
                fragment.css('div').each do |div|
                    div.name = 'span'
                    number = div['class'].to_s[/code-line-(\d+)/, 1].to_i
                    classes = ['code-line']
                    classes << 'code-context' if context.include?(number)
                    div['class'] = classes.join(' ')
                    # where context and new code meet, the block is cut (default.css)
                    div.add_previous_sibling("<span class='code-cut' aria-hidden='true'></span>") if number > 1 && context.include?(number - 1) != context.include?(number)
                    # Each line is a block of its own (default.css); its line break would add one more
                    last = div.children.last
                    last.content = last.content.sub(/\n\z/, '') if last&.text? && last.content.end_with?("\n")
                    last = div.children.last
                    if last && !last.text? && last.children.last&.text?
                        t = last.children.last
                        t.content = t.content.sub(/\n\z/, '')
                    end
                end
                pre << fragment.to_html
            end
        end
        root
    end
end
