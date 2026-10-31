# Check type interleaving (docs/aufgaben.md): threads that run side by side,
# one step at a time, in the order the person picked in the widget
# (interleaving.js). The card sends that order as text, »A A B B A B«; the
# server runs the threads in it with the same rules as the widget and
# compares the end with the task's goal. Nothing the person wrote is run.
#
#   check:
#     type: interleaving
#     shared: { counter: 0 }       # shared variables and their start values
#     locks: [m]                   # optional: locks, free at the start
#     threads:                     # each thread's lines, in the site's pseudocode
#       A: ["a ← counter", "a ← a + 1", "counter ← a"]
#       B: ["b ← counter", "b ← b + 1", "counter ← b"]
#     goal: counter == 1           # when every thread is done; or: deadlock
#
# A line is »name ← expression« (names and whole numbers with + and -),
# »lock m« or »unlock m«. A name that is not shared belongs to the thread
# that uses it. lock waits while another thread holds the lock: then that
# thread cannot take a step. Deadlock: not every thread is done, and none
# can take a step.
module InterleavingCheck
    NAME = /\A[a-z_][a-z0-9_]{0,15}\z/
    THREAD = /\A[A-Z]\z/
    MAX_THREADS = 4
    MAX_LINES = 12
    MAX_STEPS = 100
    SPEC_KEYS = %w[shared locks threads goal].freeze
    GOAL = /\A\s*([a-z_][a-z0-9_]*)\s*(==|!=|<=|>=|<|>)\s*(-?\d+)\s*\z/

    # A line as { kind: :assign, target:, terms: [[sign, name_or_int]] },
    # { kind: :lock, lock: } or { kind: :unlock, lock: }; nil if it is none
    def self.parse_line(line)
        text = line.to_s.strip
        if (m = text.match(/\A(lock|unlock)\s+([a-z_][a-z0-9_]*)\z/))
            return { :kind => m[1].to_sym, :lock => m[2] }
        end
        m = text.match(/\A([a-z_][a-z0-9_]*)\s*←\s*(.+)\z/)
        return nil unless m

        expr = m[2].strip
        terms = []
        sign = 1
        tokens = expr.scan(/[a-z_][a-z0-9_]*|\d+|[+-]|\S/)
        expect_term = true
        tokens.each do |t|
            if expect_term
                return nil unless t =~ /\A([a-z_][a-z0-9_]*|\d+)\z/
                terms << [sign, t =~ /\A\d+\z/ ? t.to_i : t]
                expect_term = false
            else
                return nil unless %w[+ -].include?(t)
                sign = t == '+' ? 1 : -1
                expect_term = true
            end
        end
        return nil if expect_term

        { :kind => :assign, :target => m[1], :terms => terms }
    end

    def self.spec_errors(check)
        errors = []
        extra = check.keys - %w[type] - SPEC_KEYS
        shared = check['shared']
        unless shared.is_a?(Hash) && shared.all? { |k, v| k.to_s =~ NAME && v.is_a?(Integer) }
            errors << 'shared must map names to whole numbers'
            shared = {}
        end
        locks = check['locks'] || []
        errors << 'locks must be a list of names' unless locks.is_a?(Array) && locks.all? { |l| l.to_s =~ NAME }
        threads = check['threads']
        if threads.is_a?(Hash) && threads.size.between?(2, MAX_THREADS) && threads.keys.all? { |k| k.to_s =~ THREAD }
            threads.each do |name, lines|
                unless lines.is_a?(Array) && lines.size.between?(1, MAX_LINES) && lines.all? { |l| l.is_a?(String) }
                    errors << "threads.#{name} must be a list of 1 to #{MAX_LINES} lines"
                    next
                end
                lines.each_with_index do |line, i|
                    parsed = parse_line(line)
                    if parsed.nil?
                        errors << "threads.#{name}[#{i}]: »#{line}« is not »name ← expression«, »lock m« or »unlock m«"
                    elsif parsed[:lock] && !(locks.is_a?(Array) && locks.include?(parsed[:lock]))
                        errors << "threads.#{name}[#{i}]: no lock #{parsed[:lock]} in locks"
                    end
                end
            end
        else
            errors << "threads must map 2 to #{MAX_THREADS} capital letters to their lines"
        end
        goal = check['goal']
        unless goal == 'deadlock' || (goal.is_a?(String) && (m = goal.match(GOAL)) && shared.key?(m[1]))
            errors << 'goal must be »deadlock« or a comparison of a shared variable with a number (counter == 1)'
        end
        errors << "unknown keys: #{extra.join(', ')}" unless extra.empty?
        errors
    end

    # The state while the order runs
    class Run
        attr_reader :shared, :locals, :at, :holder

        def initialize(check)
            @threads = check['threads'].to_h { |name, lines| [name, lines.map { |l| InterleavingCheck.parse_line(l) }] }
            @source = check['threads']
            @shared = check['shared'].dup
            @locals = @threads.keys.to_h { |t| [t, {}] }
            @at = @threads.keys.to_h { |t| [t, 0] }
            @holder = (check['locks'] || []).to_h { |l| [l, nil] }
        end

        def names = @threads.keys
        def done?(t) = @at[t] >= @threads[t].size
        def all_done? = names.all? { |t| done?(t) }
        def line(t) = @source[t][@at[t]]

        # Why t cannot take a step now, or nil
        def blocked(t)
            return :done if done?(t)

            step = @threads[t][@at[t]]
            return nil unless step[:kind] == :lock

            other = @holder[step[:lock]]
            other && other != t ? [:waits, step[:lock], other] : nil
        end

        def deadlock? = !all_done? && names.all? { |t| blocked(t) }

        def value(t, term)
            return term if term.is_a?(Integer)
            return @shared[term] if @shared.key?(term)

            @locals[t].fetch(term, 0)
        end

        def step(t)
            s = @threads[t][@at[t]]
            case s[:kind]
            when :lock then @holder[s[:lock]] = t
            when :unlock then @holder[s[:lock]] = nil if @holder[s[:lock]] == t
            when :assign
                v = s[:terms].sum { |sign, term| sign * value(t, term) }
                (@shared.key?(s[:target]) ? @shared : @locals[t])[s[:target]] = v
            end
            @at[t] += 1
        end
    end

    def self.shared_text(run)
        run.shared.map { |k, v| "#{k} = #{v}" }.join(', ')
    end

    # [status, message, read]
    def self.check(task, check, answer)
        order = answer.to_s.split(/[\s,]+/).reject(&:empty?)
        return [:invalid, 'Wähle zuerst mit den Knöpfen eine Reihenfolge der Schritte.', nil] if order.empty?
        return [:invalid, "Das sind mehr als #{MAX_STEPS} Schritte.", nil] if order.size > MAX_STEPS

        run = Run.new(check)
        order.each_with_index do |t, i|
            unless run.names.include?(t)
                return [:invalid, "Schritt #{i + 1}: Einen Thread #{t} gibt es hier nicht, nur #{run.names.join(' und ')}.", nil]
            end
            why = run.blocked(t)
            if why == :done
                return [:wrong, "Schritt #{i + 1}: #{t} ist schon fertig und hat keinen Schritt mehr.", nil]
            elsif why
                return [:wrong, "Schritt #{i + 1}: #{t} wartet auf den Lock #{why[1]}, den #{why[2]} hält, und kann keinen Schritt machen.", nil]
            end
            run.step(t)
        end
        read = "Deine Reihenfolge: #{order.join(' ')}. Am Ende: #{shared_text(run)}" +
            (run.all_done? ? '.' : "; #{run.names.reject { |t| run.done?(t) }.map { |t| "#{t} steht bei »#{run.line(t)}«" }.join(', ')}.")
        say = ->(fallback) { (task['feedback'] || []).find { |f| f['when'] && f['when']['problem'].to_s == 'goal' }&.dig('say') || fallback }

        if check['goal'] == 'deadlock'
            return [:correct, 'Passt: Keiner der Threads kann weiter.', read] if run.deadlock?
            return [:wrong, say.('Alle Threads sind fertig geworden; gesucht ist eine Reihenfolge, in der keiner mehr weiterkommt.'), read] if run.all_done?

            return [:wrong, "Es kann noch ein Thread weiter: #{run.names.reject { |t| run.blocked(t) }.join(' und ')}. Gesucht ist eine Reihenfolge, in der keiner mehr weiterkommt.", read]
        end

        if run.deadlock?
            return [:wrong, 'Hier kommt keiner mehr weiter. Gesucht ist eine Reihenfolge, in der alle Threads fertig werden.', read]
        end
        unless run.all_done?
            left = run.names.reject { |t| run.done?(t) }
            return [:wrong, "#{left.join(' und ')} #{left.size == 1 ? 'ist' : 'sind'} noch nicht fertig. Lass alle Threads bis zum Ende laufen.", read]
        end
        m = check['goal'].match(GOAL)
        have = run.shared[m[1]]
        ok = have.public_send(m[2].to_sym, m[3].to_i)
        return [:correct, "Passt: Am Ende ist #{m[1]} = #{have}.", read] if ok

        [:wrong, say.("Am Ende ist #{m[1]} = #{have}; gesucht ist eine Reihenfolge mit #{check['goal'].strip}."), read]
    end
end
