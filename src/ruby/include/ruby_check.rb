require 'json'
require 'shellwords'
require_relative 'call_check'

# Check type ruby (docs/aufgaben.md): calls into a Ruby file the person
# wrote and compares what comes back, like PythonCheck.
#
# A fixed harness (HARNESS) loads the file with ruby in the person's
# Workspace, as the guest user that can read /workspace but not change it
# (the code is theirs, rule 1), evaluates each call at the top level of the
# file and prints one JSON line. Reading the result and the feedback are
# shared with PythonCheck (CallCheck).
#
# Only the definitions of the file are loaded: def, class, module, constants
# and global or instance variables, require and include, and statements that
# fill a constant (TABLE[key] = …, TABLE << …, also in a loop). Everything
# else at the top level, the program itself (a loop that reads input, a
# picture that is saved), is blanked out with spaces, so a plain script needs
# no »if __FILE__ == $0« and line numbers stay as they are. A heredoc counts
# with the statement it belongs to. A require after the first def, class or
# module may belong to the program: it is loaded, but a missing library is
# no error.
#
#   check:
#     type: ruby
#     file: lzw/lzw.rb
#     cases:
#       - compress('ABABABA'): '[65, 66, 256, 258]'   # call: expected value
#       - call: compress('')
#         expect: '[]'
#         say: Ein leerer Text ergibt keinen Code.
#       - call: decompress([300])
#         raises: true                                # must raise a StandardError
#
# Calls and expected values are Ruby expressions; true, false, ~ and
# numbers in YAML become true, false, nil and the number. Results are
# compared with ==.
module RubyCheck
    MAX_CASES = CallCheck::MAX_CASES
    RESULT_MARK = CallCheck::RESULT_MARK

    HARNESS = <<~RUBY
        require 'json'
        require 'stringio'
        require 'prism'
        path, cases = ARGV[0], JSON.parse(ARGV[1])
        full = File.expand_path(path)
        result = { 'cases' => [] }
        real_stdout = $stdout

        line_in_file = lambda do |error|
            location = (error.backtrace_locations || []).find { |l| l.path == full || l.absolute_path == full }
            location&.lineno
        end
        text = ->(error) { "\#{error.class}: \#{error.message.to_s.lines.first.to_s.strip}"[0, 300] }
        short = lambda do |value|
            shown = value.inspect
            shown.length <= 200 ? shown : shown[0, 200] + '…'
        end

        # The source with everything but the definitions blanked out
        definitions = lambda do |source|
            parsed = Prism.parse(source, filepath: full)
            return [nil, parsed.errors.first] unless parsed.errors.empty?

            kinds = [Prism::DefNode, Prism::ClassNode, Prism::ModuleNode, Prism::AliasMethodNode,
                     Prism::ConstantWriteNode, Prism::ConstantOrWriteNode, Prism::ConstantPathWriteNode,
                     Prism::ConstantPathOrWriteNode, Prism::GlobalVariableWriteNode, Prism::InstanceVariableWriteNode]
            loading = %i[require require_relative load]
            calls = loading + %i[include extend using private public]
            mutators = %i[[]= << push append store add update concat insert unshift merge! delete clear replace]
            constant = ->(node) { node.is_a?(Prism::ConstantReadNode) || node.is_a?(Prism::ConstantPathNode) }
            # does a statement fill a constant, anywhere inside?
            fills = lambda do |node|
                (node.is_a?(Prism::CallNode) && node.receiver && constant.call(node.receiver) &&
                    (mutators.include?(node.name) || node.name.to_s.end_with?('!'))) ||
                    ([Prism::IndexOperatorWriteNode, Prism::IndexOrWriteNode, Prism::IndexAndWriteNode].any? { |kind| node.is_a?(kind) } &&
                        node.receiver && constant.call(node.receiver)) ||
                    node.compact_child_nodes.any? { |child| fills.call(child) }
            end
            keep = lambda do |node|
                kinds.any? { |kind| node.is_a?(kind) } ||
                    (node.is_a?(Prism::CallNode) && node.receiver.nil? && calls.include?(node.name)) ||
                    (node.is_a?(Prism::MultiWriteNode) && node.lefts.all? { |target| target.is_a?(Prism::ConstantTargetNode) }) ||
                    fills.call(node)
            end
            reach = lambda do |node|
                ends = node.compact_child_nodes.map { |child| reach.call(child) } << node.location.end_offset
                ends << node.closing_loc.end_offset if node.respond_to?(:closing_loc) && node.closing_loc
                ends.max
            end
            statements = parsed.value.statements.body
            kept = statements.map { |node| keep.call(node) }
            begun = statements.index { |node| [Prism::DefNode, Prism::ClassNode, Prism::ModuleNode].any? { |kind| node.is_a?(kind) } } || statements.length
            blanked = source.b
            # from the end, so that the offsets before stay right
            statements.each_with_index.reverse_each do |node, i|
                from, to = node.location.start_offset, reach.call(node)
                if !kept[i]
                    blanked[from...to] = blanked[from...to].gsub(/[^\\n]/n, ' ')
                elsif i > begun && node.is_a?(Prism::CallNode) && loading.include?(node.name)
                    blanked[from...to] = "begin; \#{blanked[from...to]}; rescue LoadError; end".b
                end
            end
            [blanked.force_encoding(source.encoding), nil]
        end

        catch(:done) do
            unless File.file?(full)
                result['error'] = { 'kind' => 'missing' }
                throw :done
            end
            $LOAD_PATH.unshift(File.dirname(full))
            code, syntax = definitions.call(File.read(full))
            if syntax
                result['error'] = { 'kind' => 'syntax', 'line' => syntax.location.start_line, 'message' => syntax.message.to_s[0, 300] }
                throw :done
            end
            begin
                $stdout = StringIO.new
                TOPLEVEL_BINDING.eval(code, full, 1)
            rescue SyntaxError => error
                message = error.message.to_s
                line = message[/\#{Regexp.escape(full)}:(\\d+)/, 1]
                detail = message.lines.map(&:strip).map { |l| l[/\\^+\\s+(.+)/, 1] }.compact.first ||
                    message.lines.first.to_s.sub(/\\A.*?:\\d+:\\s*/, '').strip
                result['error'] = { 'kind' => 'syntax', 'line' => line&.to_i, 'message' => detail[0, 300] }
                throw :done
            rescue Exception => error
                result['error'] = { 'kind' => 'load', 'line' => line_in_file.call(error), 'message' => text.call(error) }
                throw :done
            ensure
                $stdout = real_stdout
            end
            cases.each do |c|
                entry = { 'call' => c['call'], 'expect' => c['expect'] }
                if c['raises']
                    begin
                        $stdout = StringIO.new
                        got = TOPLEVEL_BINDING.eval(c['call'])
                        $stdout = real_stdout
                        entry['got'] = short.call(got)
                        entry['ok'] = false
                    rescue StandardError => error
                        $stdout = real_stdout
                        entry['ok'] = true
                        entry['error'] = text.call(error)
                    end
                    result['cases'] << entry
                    next
                end
                begin
                    expected = TOPLEVEL_BINDING.eval(c['expect'])
                    $stdout = StringIO.new
                    got = TOPLEVEL_BINDING.eval(c['call'])
                    $stdout = real_stdout
                    entry['got'] = short.call(got)
                    entry['ok'] = (got == expected)
                rescue Exception => error
                    $stdout = real_stdout
                    entry['ok'] = false
                    entry['error'] = text.call(error)
                    entry['line'] = line_in_file.call(error)
                end
                result['cases'] << entry
            end
        end
        real_stdout.write("\\n#{RESULT_MARK}" + JSON.generate(result) + "\\n")
    RUBY

    # The cases of a check as [{ 'call' =>, 'expect' => (Ruby), 'say' => }].
    def self.cases(check)
        CallCheck.cases(check, method(:ruby_value))
    end

    def self.ruby_value(value)
        case value
        when true then 'true'
        when false then 'false'
        when nil then 'nil'
        else value.to_s
        end
    end

    def self.spec_errors(check)
        CallCheck.spec_errors(check, '.rb')
    end

    # The command that prints the result, run in /workspace as the guest.
    # The harness as a list of arguments, for a run outside the Workspace
    # too (KataCheck: the code of a card, in a container of its own).
    def self.argv(file, cases)
        payload = cases.map { |c| { 'call' => c['call'], 'expect' => c['expect'], 'raises' => c['raises'] } }
        ['ruby', '-W0', '-e', HARNESS, file, JSON.generate(payload)]
    end

    def self.command(file, cases)
        # With RubyGems: a program may use what the person installed with
        # gem install (ChunkyPNG for Steganographie), from GEM_HOME.
        Shellwords.join(argv(file, cases))
    end

    def self.parse(output)
        CallCheck.parse(output)
    end

    # [status, message, read] for the harness output.
    def self.evaluate(task, check, output, timed_out: false)
        CallCheck.evaluate(check, cases(check), output, language: 'Ruby', timed_out: timed_out)
    end
end
