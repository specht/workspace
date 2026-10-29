require 'prism'

# Whether a Kata's function is recursive (check type function, recursive:
# true) and free of loops (loops: false), read from the code, not run: a
# loop gives the same results as a recursion, so the cases cannot tell
# them apart. Asked only once the cases pass (KataCheck.check).
#
# Ruby is parsed with Prism; a loop is while, until, for, or a call with a
# block (each, times, map …). Python and JavaScript are read as text with
# their strings and comments blanked: the function's body is what is
# indented under its def (Python) or between the braces after its head
# (JavaScript, a function declaration or const name = … =>). A loop there
# is for or while, in JavaScript also do and the methods of Array that
# take a function (forEach, map …). Dart and Java are read like JavaScript:
# the body after the head »type name(…) {« (in Dart also »=> …;«); a loop is
# for, while, do, or a method that takes a function (forEach, map, where,
# fold, reduce; in Java also stream).
#
# Plain Ruby, tested on its own (test/recursion_check_test.rb).
module RecursionCheck
    NOT_RECURSIVE = 'Deine Funktion ruft sich nicht selbst auf. Hier soll sie es tun: Was ist der kleinste Fall, in dem du sofort fertig bist, und wie hilft dir sonst ein Aufruf mit etwas Kleinerem?'
    LOOP = 'Deine Funktion benutzt eine Schleife (%s). Hier soll sie ohne auskommen und sich stattdessen selbst aufrufen.'
    JS_LOOP_METHODS = %w[forEach map filter reduce reduceRight some every find findIndex flatMap].freeze
    TYPED_LOOP_METHODS = %w[forEach map where fold reduce any every expand firstWhere stream].freeze

    # The message for code whose function name does not do what check asks,
    # or nil. A function the code does not define is left to the cases.
    def self.problem(check, code, name)
        recursive = check['recursive'] == true
        loops_allowed = check['loops'] != false
        return nil if !recursive && loops_allowed

        found = case check['language']
            when 'ruby' then ruby(code.to_s, name)
            when 'python' then python(code.to_s, name)
            when 'javascript' then javascript(code.to_s, name)
            when 'dart', 'java' then typed(code.to_s, name)
            end
        return nil if found.nil?

        return format(LOOP, found[:loop]) if !loops_allowed && found[:loop]
        return NOT_RECURSIVE if recursive && !found[:calls_itself]

        nil
    end

    # The function's name, from the first case's call (»factorial(5)«).
    def self.function_name(check)
        first = (check['cases'] || []).first
        call = first.is_a?(Hash) ? (first['call'] || first.keys.first) : nil
        call.to_s[/\A\s*([A-Za-z_]\w*)\s*\(/, 1]
    end

    # --- Ruby ------------------------------------------------------------------

    def self.ruby(code, name)
        result = Prism.parse(code)
        return nil unless result.success?

        definition = find_def(result.value, name)
        return nil unless definition && definition.body

        found = { :calls_itself => false, :loop => nil }
        walk(definition.body) do |node|
            case node
            when Prism::WhileNode then found[:loop] ||= '`while`'
            when Prism::UntilNode then found[:loop] ||= '`until`'
            when Prism::ForNode then found[:loop] ||= '`for`'
            when Prism::CallNode
                found[:calls_itself] = true if node.receiver.nil? && node.name.to_s == name
                found[:loop] ||= "`#{node.name}` mit einem Block" if node.block.is_a?(Prism::BlockNode)
            end
        end
        found
    end

    def self.find_def(node, name)
        return node if node.is_a?(Prism::DefNode) && node.name.to_s == name && node.receiver.nil?

        node.compact_child_nodes.each do |child|
            hit = find_def(child, name)
            return hit if hit
        end
        nil
    end

    def self.walk(node, &block)
        yield node
        node.compact_child_nodes.each { |child| walk(child, &block) }
    end

    # --- Python and JavaScript, as text --------------------------------------

    # The code with every string and comment replaced by spaces, so that the
    # lines and columns stay where they were.
    def self.blank(code, language)
        pattern = if language == :python
            /"""[\s\S]*?"""|'''[\s\S]*?'''|"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'|#[^\n]*/
        else
            %r{`(?:\\.|[^`\\])*`|"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'|//[^\n]*|/\*[\s\S]*?\*/}
        end
        code.gsub(pattern) { |match| match.gsub(/[^\n]/, ' ') }
    end

    def self.python(code, name)
        lines = blank(code, :python).lines
        start = lines.index { |line| line =~ /\A(\s*)def\s+#{Regexp.escape(name)}\s*\(/ }
        return nil unless start

        indent = lines[start][/\A\s*/].size
        body = lines[(start + 1)..].take_while { |line| line.strip.empty? || line[/\A\s*/].size > indent }.join
        loop = body[/\b(for|while)\b/, 1]
        { :calls_itself => body.match?(/(?<![\w.])#{Regexp.escape(name)}\s*\(/), :loop => loop && "`#{loop}`" }
    end

    def self.javascript(code, name)
        text = blank(code, :javascript)
        n = Regexp.escape(name)
        head = text.match(/\bfunction\s+#{n}\s*\([^)]*\)\s*\{/) ||
               text.match(/\b(?:const|let|var)\s+#{n}\s*=\s*(?:function\b[^{]*|\([^)]*\)\s*=>\s*|\w+\s*=>\s*)\{/)
        return nil unless head

        depth = 1
        i = head.end(0)
        while i < text.size && depth > 0
            depth += 1 if text[i] == '{'
            depth -= 1 if text[i] == '}'
            i += 1
        end
        body = text[head.end(0)...(i - 1)]
        keyword = body[/\b(for|while|do)\b/, 1]
        method = body[/\.(#{JS_LOOP_METHODS.join('|')})\s*\(/, 1]
        loop = keyword ? "`#{keyword}`" : (method && "`#{method}`")
        # its own name called, also in a spread (...hanoi(…)), but not as
        # the method of something (list.flatten(…))
        { :calls_itself => body.match?(/(?:\A|[^\w$.]|\.\.\.)#{n}\s*\(/), :loop => loop }
    end

    # Dart and Java: »type name(…) {« with the body up to its brace, or in
    # Dart »type name(…) => …;«.
    def self.typed(code, name)
        text = blank(code, :javascript)
        n = Regexp.escape(name)
        head = text.match(/[\w>\]?]\s+#{n}\s*\([^)]*\)\s*(\{|=>)/) or return nil

        if head[1] == '=>'
            depth = 0
            i = head.end(0)
            while i < text.size && !(depth.zero? && text[i] == ';')
                depth += 1 if '([{'.include?(text[i])
                depth -= 1 if ')]}'.include?(text[i])
                i += 1
            end
            body = text[head.end(0)...i]
        else
            depth = 1
            i = head.end(0)
            while i < text.size && depth > 0
                depth += 1 if text[i] == '{'
                depth -= 1 if text[i] == '}'
                i += 1
            end
            body = text[head.end(0)...(i - 1)]
        end
        keyword = body[/\b(for|while|do)\b/, 1]
        method = body[/\.(#{TYPED_LOOP_METHODS.join('|')})\s*\(/, 1]
        { :calls_itself => body.match?(/(?:(?<![\w$.])|(?<=\.\.\.))#{n}\s*\(/), :loop => keyword ? "`#{keyword}`" : (method && "`#{method}`") }
    end
end
