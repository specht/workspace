# A small CSS reader for checking students' stylesheets (docs/aufgaben.md,
# check type css). Browsers skip broken CSS silently; this reader names the
# line where braces, colons or semicolons do not fit.
module CssLite
    Rule = Struct.new(:selector, :line, :offset, :declarations)
    Problem = Struct.new(:line, :message)
    Result = Struct.new(:rules, :problems, :urls, :imports)
    # At-rules whose block holds declarations, like a rule.
    DECLARATION_AT_RULES = %w[@font-face @page @counter-style @property @viewport].freeze

    # Rules in @media and similar blocks count like other rules. Returns
    # rules, problems (sorted by line), url(...) values and @import targets,
    # each url/import as [value, line].
    def self.parse(source)
        source = source.to_s
        newlines = []
        source.scan("\n") { newlines << $~.begin(0) }
        line_at = lambda { |index| (newlines.bsearch_index { |n| n >= index } || newlines.size) + 1 }

        problems = []
        # Blank out comments but keep offsets and lines.
        text = source.dup
        pos = 0
        while (start = text.index('/*', pos))
            close = text.index('*/', start + 2)
            if close.nil?
                problems << Problem.new(line_at.(start), 'Der Kommentar `/*` wird nie mit `*/` geschlossen.')
                text[start..] = text[start..].gsub(/[^\n]/, ' ')
                break
            end
            text[start..close + 1] = text[start..close + 1].gsub(/[^\n]/, ' ')
            pos = close + 2
        end

        rules = []
        urls = []
        imports = []
        text.scan(/url\(\s*(?:"([^"]*)"|'([^']*)'|([^)\s]*))\s*\)/) do
            urls << [($1 || $2 || $3).to_s, line_at.($~.begin(0))]
        end
        text.scan(/@import\s+(?:url\(\s*)?["']?([^"');\s]+)/) do
            imports << [$1.to_s, line_at.($~.begin(0))]
        end

        # stack entries: [:block | :rule, line of {, Rule or nil]. Rules may
        # be nested (CSS nesting); @font-face and friends hold declarations.
        stack = []
        buffer_start = 0
        i = 0
        while i < text.size
            ch = text[i]
            if ch == '"' || ch == "'"
                close = text.index(ch, i + 1)
                if close.nil? || text[i...close].include?("\n")
                    problems << Problem.new(line_at.(i), "Hier beginnt ein Text mit #{ch}, der nicht in derselben Zeile endet.")
                    close = text.index("\n", i) || text.size - 1
                end
                i = close + 1
                next
            end
            if ch == '{'
                segment_start = buffer_start
                if stack.last && stack.last[0] == :rule
                    # Declarations before a nested rule end at the last ;
                    cut = last_separator(text, buffer_start, i)
                    if cut
                        check_declarations(text, buffer_start, cut, stack.last[2], line_at, problems)
                        segment_start = cut + 1
                    end
                end
                prelude = text[segment_start...i].strip
                sel_offset = segment_start + (text[segment_start...i] =~ /\S/).to_i
                if prelude.empty?
                    problems << Problem.new(line_at.(i), 'Vor diesem `{` fehlt der Selektor.')
                    stack << [:rule, line_at.(i), nil]
                elsif prelude.start_with?('@') && !DECLARATION_AT_RULES.include?(prelude[/\A@[a-z-]+/])
                    stack << [:block, line_at.(i), nil]
                else
                    rule = Rule.new(prelude.gsub(/\s+/, ' '), line_at.(sel_offset), sel_offset, [])
                    rules << rule
                    stack << [:rule, line_at.(i), rule]
                end
                buffer_start = i + 1
            elsif ch == '}'
                if stack.empty?
                    problems << Problem.new(line_at.(i), 'Dieses `}` schließt nichts. Steht es doppelt?')
                else
                    kind, _, rule = stack.pop
                    if kind == :rule
                        check_declarations(text, buffer_start, i, rule, line_at, problems)
                    elsif !text[buffer_start...i].strip.empty?
                        problems << Problem.new(line_at.(buffer_start + (text[buffer_start...i] =~ /\S/).to_i),
                            'Hier steht etwas, das zu keiner Regel gehört. Fehlt ein `{`?')
                    end
                end
                buffer_start = i + 1
            elsif ch == ';' && (stack.empty? || stack.last[0] == :block)
                statement = text[buffer_start...i].strip
                unless statement.empty? || statement.start_with?('@')
                    problems << Problem.new(line_at.(buffer_start + (text[buffer_start...i] =~ /\S/).to_i),
                        "`#{statement[0, 40]}` steht außerhalb einer Regel. Fehlen Selektor und `{`?")
                end
                buffer_start = i + 1
            end
            i += 1
        end
        stack.each do |_, line, _|
            problems << Problem.new(line, "Das `{` aus Zeile #{line} wird nie mit `}` geschlossen.")
        end
        rest = text[buffer_start..].to_s.strip
        if stack.empty? && !rest.empty?
            problems << Problem.new(line_at.(buffer_start + (text[buffer_start..] =~ /\S/).to_i),
                'Am Ende der Datei steht etwas, das zu keiner Regel gehört.')
        end
        Result.new(rules, problems.sort_by(&:line), urls, imports)
    end

    # Index of the last ; between from and to outside strings and (), or nil.
    def self.last_separator(text, from, to)
        last = nil
        split_points(text, from, to) { |i| last = i }
        last
    end

    # Yields the index of every ; between from and to that separates
    # declarations (not inside quotes or parentheses).
    def self.split_points(text, from, to)
        quote = nil
        depth = 0
        (from...to).each do |i|
            ch = text[i]
            if quote
                quote = nil if ch == quote
            elsif ch == '"' || ch == "'"
                quote = ch
            elsif ch == '('
                depth += 1
            elsif ch == ')'
                depth -= 1 if depth > 0
            elsif ch == ';' && depth.zero?
                yield i
            end
        end
    end

    def self.check_declarations(text, from, to, rule, line_at, problems)
        cuts = []
        split_points(text, from, to) { |i| cuts << i }
        parts = []
        start = from
        (cuts + [to]).each do |cut|
            parts << text[start...cut]
            start = cut + 1
        end
        offset = from
        parts.each do |part|
            here = offset
            offset += part.size + 1
            next if part.strip.empty?

            start = here + (part =~ /\S/).to_i
            line = line_at.(start)
            name, value = part.split(':', 2)
            if value.nil?
                problems << Problem.new(line, "`#{part.strip[0, 40]}`: Hier fehlt der Doppelpunkt zwischen Eigenschaft und Wert.")
                next
            end
            unless name.strip =~ /\A-?-?[a-zA-Z][a-zA-Z0-9-]*\z/
                problems << Problem.new(line, "`#{name.strip[0, 40]}` ist kein Name einer Eigenschaft.")
                next
            end
            if value.strip.empty?
                problems << Problem.new(line, "Bei `#{name.strip}` fehlt der Wert.")
                next
            end
            if (m = value.match(/\n\s*(-?[a-zA-Z][a-zA-Z0-9-]*)\s*:/))
                problems << Problem.new(line, "Nach `#{name.strip}: #{value.strip.lines.first.strip}` fehlt vermutlich ein Semikolon, denn danach beginnt schon `#{m[1]}:`.")
                next
            end
            rule&.declarations&.push([name.strip, value.strip])
        end
    end
end
