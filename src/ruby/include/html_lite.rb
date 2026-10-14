require 'set'

# A small, forgiving HTML reader for checking students' pages
# (docs/aufgaben.md, check type html).
#
# Browsers repair almost any HTML silently, so »it shows up« says little.
# This reader builds a tree the way students think about their file and
# reports, with line numbers, where tags do not fit together: a closing tag
# without an opening one, an element still open at the end, a tag or comment
# that never ends. It knows void elements, raw text (script, style) and the
# end tags HTML allows to leave out (p, li, td, …).
#
# It is not a full HTML5 parser and does not try to be one.
module HtmlLite
    MAX_DEPTH = 256
    VOID = %w[area base br col embed hr img input link meta source track wbr].to_set.freeze
    RAW_TEXT = %w[script style textarea title].to_set.freeze
    # End tags that may be left out, and what closes them implicitly.
    OPTIONAL_END = %w[html head body p li dt dd tr td th thead tbody tfoot option colgroup].to_set.freeze
    CLOSES_P = %w[
        address article aside blockquote details div dl fieldset figcaption figure footer form
        h1 h2 h3 h4 h5 h6 header hr main nav ol p pre section table ul
    ].to_set.freeze
    IMPLIED_BY = {
        'li' => %w[li], 'dt' => %w[dt dd], 'dd' => %w[dt dd], 'tr' => %w[tr], 'td' => %w[td th],
        'th' => %w[td th], 'option' => %w[option], 'tbody' => %w[thead tbody], 'tfoot' => %w[thead tbody],
    }.freeze

    class Node
        attr_reader :name, :attrs, :children, :line
        attr_accessor :parent, :end_line

        def initialize(name, attrs = {}, line = 1)
            @name = name
            @attrs = attrs
            @children = []
            @line = line
        end

        def text
            out = +''
            stack = @children.reverse
            until stack.empty?
                c = stack.pop
                if c.is_a?(String)
                    out << c
                else
                    stack.concat(c.children.reverse)
                end
            end
            out
        end

        def elements
            @children.grep(Node)
        end

        def classes
            @attrs['class'].to_s.split(/\s+/)
        end

        # Depth first, in document order; iterative, so deep nesting is fine.
        def each_element
            stack = elements.reverse
            until stack.empty?
                e = stack.pop
                yield e
                stack.concat(e.elements.reverse)
            end
        end

        # Elements below this one that match a simple selector: tag, .class,
        # tag.class, #id, and descendants separated by spaces.
        def select(selector)
            steps = selector.to_s.strip.split(/\s+/).map { |s| HtmlLite.parse_step(s) }
            return [] if steps.empty?

            matches = [self]
            steps.each do |step|
                found = []
                matches.each do |m|
                    m.each_element { |e| found << e if HtmlLite.step_matches?(step, e) }
                end
                matches = found.uniq
            end
            matches
        end

        def at(selector)
            select(selector).first
        end
    end

    Problem = Struct.new(:line, :message)

    class Document
        attr_reader :root, :problems

        def initialize(root, problems)
            @root = root
            @problems = problems
        end

        def select(selector)
            @root.select(selector)
        end

        def at(selector)
            @root.at(selector)
        end
    end

    def self.parse_step(step)
        m = step.match(/\A([a-zA-Z][a-zA-Z0-9-]*)?((?:[.#][a-zA-Z0-9_-]+)*)\z/)
        raise ArgumentError, "selector not supported: #{step}" unless m

        { :tag => m[1]&.downcase, :classes => m[2].scan(/\.([a-zA-Z0-9_-]+)/).flatten,
          :id => m[2][/#([a-zA-Z0-9_-]+)/, 1] }
    end

    def self.step_matches?(step, e)
        (step[:tag].nil? || e.name == step[:tag]) &&
            step[:classes].all? { |c| e.classes.include?(c) } &&
            (step[:id].nil? || e.attrs['id'] == step[:id])
    end

    def self.parse(source)
        source = source.to_s
        root = Node.new('#document')
        stack = [root]
        problems = []
        pos = 0
        newlines = []
        source.scan("\n") { newlines << $~.begin(0) }
        line_at = lambda { |index| (newlines.bsearch_index { |n| n >= index } || newlines.size) + 1 }
        add_text = lambda do |text|
            stack.last.children << text unless text.empty?
        end

        while pos < source.size
            lt = source.index('<', pos)
            if lt.nil?
                add_text.(source[pos..])
                break
            end
            add_text.(source[pos...lt])
            line = line_at.(lt)

            if source[lt, 4] == '<!--'
                close = source.index('-->', lt + 4)
                if close.nil?
                    problems << Problem.new(line, 'Der Kommentar `<!--` wird nie mit `-->` geschlossen.')
                    break
                end
                pos = close + 3
                next
            end

            if source[lt + 1] == '!' || source[lt + 1] == '?'
                close = source.index('>', lt)
                if close.nil?
                    problems << Problem.new(line, 'Ein `<!` wird nie mit `>` geschlossen.')
                    break
                end
                pos = close + 1
                next
            end

            m = source.match(%r{\G<(/?)([a-zA-Z][a-zA-Z0-9-]*)}, lt)
            unless m
                # A lone < in text, as browsers treat it.
                add_text.('<')
                pos = lt + 1
                next
            end
            closing = m[1] == '/'
            name = m[2].downcase
            tag_end, attrs, attr_problem = scan_attributes(source, m.end(0))
            if tag_end.nil?
                problems << Problem.new(line, "Das Tag `<#{closing ? '/' : ''}#{name}` wird nie mit `>` geschlossen.")
                break
            end
            problems << Problem.new(line, attr_problem.sub('%s', name)) if attr_problem
            self_closing = source[tag_end - 1] == '/'
            pos = tag_end + 1

            if closing
                close_element(stack, name, line, problems)
                next
            end

            # Start tag: first close what this tag implicitly ends.
            if stack.last.name == 'p' && CLOSES_P.include?(name)
                stack.pop.end_line = line
            end
            if IMPLIED_BY[name]
                open = stack.rindex { |n| IMPLIED_BY[name].include?(n.name) }
                barrier = stack.rindex { |n| %w[ul ol dl table tbody thead tfoot select].include?(n.name) }
                if open && (barrier.nil? || open > barrier)
                    stack.pop while stack.size > open
                end
            end

            node = Node.new(name, attrs, line)
            node.parent = stack.last
            stack.last.children << node
            next if VOID.include?(name) || self_closing

            if RAW_TEXT.include?(name)
                close = source.index(%r{</#{name}\s*>}i, pos)
                if close.nil?
                    problems << Problem.new(line, "`<#{name}>` wird nie mit `</#{name}>` geschlossen.")
                    node.children << source[pos..]
                    break
                end
                node.children << source[pos...close]
                node.end_line = line_at.(close)
                pos = source.index('>', close) + 1
                next
            end
            stack << node
            if stack.size > MAX_DEPTH
                problems << Problem.new(line, "Hier sind mehr als #{MAX_DEPTH} Elemente ineinander geöffnet. Fehlen schließende Tags?")
                break
            end
        end

        unclosed = stack[1..].reject { |open| OPTIONAL_END.include?(open.name) }
        unclosed.first(5).each do |open|
            problems << Problem.new(open.line, "`<#{open.name}>` aus Zeile #{open.line} wird nie geschlossen.")
        end
        Document.new(root, problems.sort_by(&:line))
    end

    # Reads attributes up to the closing >. Returns [index of >, attrs,
    # problem or nil]; index is nil if the tag never ends.
    def self.scan_attributes(source, pos)
        attrs = {}
        problem = nil
        loop do
            pos += 1 while pos < source.size && source[pos] =~ /\s/
            return [nil, attrs, problem] if pos >= source.size

            ch = source[pos]
            return [pos, attrs, problem] if ch == '>'
            if ch == '/'
                pos += 1
                next
            end
            if ch == '<'
                problem ||= 'In `<%s …` fehlt das `>`, bevor das nächste Tag beginnt.'
                return [pos - 1, attrs, problem]
            end
            m = source.match(/\G([^\s"'>\/=]+)/, pos)
            unless m
                if ch == '"' || ch == "'"
                    problem ||= "In `<%s>` steht ein #{ch} an einer Stelle, an der es nicht hingehört."
                end
                pos += 1
                next
            end
            name = m[1].downcase
            pos = m.end(0)
            pos += 1 while pos < source.size && source[pos] =~ /\s/
            if source[pos] == '='
                pos += 1
                pos += 1 while pos < source.size && source[pos] =~ /\s/
                quote = source[pos]
                if quote == '"' || quote == "'"
                    close = source.index(quote, pos + 1)
                    return [nil, attrs, "Bei `#{name}=` in `<%s>` fehlt das schließende #{quote}."] if close.nil?
                    value = source[pos + 1...close]
                    if (value.include?('>') && value.include?("\n")) || value =~ /\s[a-zA-Z-]+=\z/
                        problem ||= "Bei `#{name}=` in `<%s>` fehlt vermutlich das schließende #{quote}."
                    end
                    attrs[name] ||= source[pos + 1...close]
                    pos = close + 1
                elsif quote == '{'
                    # A Svelte expression, disabled={state < 3}: up to the
                    # matching }, so its < and > are not taken for tags.
                    depth = 0
                    close = pos
                    while close < source.size
                        depth += 1 if source[close] == '{'
                        depth -= 1 if source[close] == '}'
                        break if depth.zero?

                        close += 1
                    end
                    return [nil, attrs, "Bei `#{name}=` in `<%s>` fehlt die schließende `}`."] if close >= source.size
                    attrs[name] ||= source[pos..close]
                    pos = close + 1
                else
                    m = source.match(/\G([^\s>]*)/, pos)
                    attrs[name] ||= m[1]
                    pos = m.end(0)
                end
            else
                attrs[name] ||= ''
            end
        end
    end

    def self.close_element(stack, name, line, problems)
        if VOID.include?(name)
            return if name == 'br' # </br> is read as <br>

            problems << Problem.new(line, "`</#{name}>` gibt es nicht: `<#{name}>` hat kein schließendes Tag.")
            return
        end
        index = stack.rindex { |n| n.name == name }
        if index.nil? || index.zero?
            problems << Problem.new(line, "`</#{name}>` hat kein passendes `<#{name}>` davor.")
            return
        end
        stack[(index + 1)..].each do |open|
            next if OPTIONAL_END.include?(open.name)

            problems << Problem.new(line, "`</#{name}>` kommt, aber `<#{open.name}>` aus Zeile #{open.line} ist noch offen.")
        end
        stack.pop while stack.size > index + 1
        stack.pop.end_line = line
    end
end
