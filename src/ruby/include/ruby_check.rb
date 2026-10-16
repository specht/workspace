require 'json'
require 'shellwords'
require_relative 'call_check'

# Check type ruby (docs/aufgaben.md): calls into a Ruby file the person
# wrote and compares what comes back, like PythonCheck.
#
# A fixed harness (HARNESS) loads the file with ruby in the person's
# Workspace, as the guest user that can read /workspace but not change it
# (the code is theirs, rule 1), evaluates each call at the top level of the
# file and prints one JSON line. What the program itself prints is
# swallowed, and a block »if __FILE__ == $0« does not run. Reading the
# result and the feedback are shared with PythonCheck (CallCheck).
#
#   check:
#     type: ruby
#     file: lzw/lzw.rb
#     cases:
#       - compress('ABABABA'): '[65, 66, 256, 258]'   # call: expected value
#       - call: compress('')
#         expect: '[]'
#         say: Ein leerer Text ergibt keinen Code.
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

        catch(:done) do
            unless File.file?(full)
                result['error'] = { 'kind' => 'missing' }
                throw :done
            end
            $LOAD_PATH.unshift(File.dirname(full))
            begin
                $stdout = StringIO.new
                TOPLEVEL_BINDING.eval(File.read(full), full, 1)
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
    def self.command(file, cases)
        payload = cases.map { |c| { 'call' => c['call'], 'expect' => c['expect'] } }
        # With RubyGems: a program may use what the person installed with
        # gem install (ChunkyPNG for Steganographie), from GEM_HOME.
        Shellwords.join(['ruby', '-W0', '-e', HARNESS, file, JSON.generate(payload)])
    end

    def self.parse(output)
        CallCheck.parse(output)
    end

    # [status, message, read] for the harness output.
    def self.evaluate(task, check, output, timed_out: false)
        CallCheck.evaluate(check, cases(check), output, language: 'Ruby', timed_out: timed_out)
    end
end
