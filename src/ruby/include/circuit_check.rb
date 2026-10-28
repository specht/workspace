require 'set'

# A circuit built in the Logik-Werkstatt (check type circuit, docs/aufgaben.md):
# the editor on the page (circuit_editor.js) sends the circuit as a netlist,
# the server reads it, replaces every block by its gates and lets it run
# round by round, exactly as the page does (logic_circuit.js, Netlist and
# settle), and compares the lamps with what the task expects.
#
#   check:
#     type: circuit
#     inputs: [a, b]            # the switches, as they stand in the card, top to bottom
#     outputs: [y]              # the lamps
#     expect:                   # for each lamp (or number, below) what it shows,
#       y: a ^ b                # in every row of the truth table
#     gates: [nand]             # the gates that may be used, inside blocks too
#     max_gates: 4              # at most this many gates, inside blocks too
#     min_blocks: 2             # at least this many blocks in the circuit itself
#     blocks: false             # no blocks at all
#     numbers:                  # several switches or lamps read as one number,
#       a: [a3, a2, a1, a0]     # the highest bit first
#     sequence:                 # instead of expect, for a circuit with memory:
#       - { d: 1, takt: 1, q: 1 }    # steps one after another; inputs set,
#       - { takt: 0, d: 0, q: 1, say: … }   # lamps expected, the rest kept
#
# An expression of expect knows the inputs, the numbers of inputs, whole
# numbers and ! ~ - * + - << >> < > <= >= == != & ^ | and c ? a : b, with the
# precedence of C; a lamp shows the lowest bit of its value, a number of
# lamps as many bits as it has.
#
# Each row of the truth table starts from a circuit in which every gate is
# 0, as the truth table beside the editor does; a sequence keeps its state
# from step to step. The check runs nothing the person wrote: it reads the
# text, nothing in it is executed.
module CircuitCheck
    GATE_KINDS = %w[and or xor not nand nor].freeze
    GATE_NAMES = { 'and' => 'UND', 'or' => 'ODER', 'xor' => 'XOR', 'not' => 'NICHT', 'nand' => 'NAND', 'nor' => 'NOR' }.freeze
    NAME = /\A[A-Za-z_][A-Za-z0-9_]{0,15}\z/
    BLOCK_NAME = /\A[A-Za-z][A-Za-z0-9_]{0,11}\z/
    SOURCE = /\A[A-Za-z_][A-Za-z0-9_]{0,15}(\.[A-Za-z_][A-Za-z0-9_]{0,15})?\z/
    KEYWORDS = (%w[in out block end] + GATE_KINDS).freeze
    MAX_PARTS = 150
    MAX_BLOCKS = 16
    MAX_DEPTH = 8
    MAX_GATES = 1000
    MAX_INPUTS = 4
    MAX_ROUNDS = 64
    MAX_TEXT = 4000
    # a truth table with at most 2^10 rows, a sequence with at most 64 steps
    MAX_TASK_INPUTS = 10
    MAX_STEPS = 64
    SPEC_KEYS = %w[inputs outputs expect sequence numbers gates max_gates min_blocks blocks].freeze

    Part = Struct.new(:id, :kind, :block, :src, :x, :y, keyword_init: true)
    Def = Struct.new(:parts) do
        def ins = parts.select { |p| p.kind == 'in' }.map(&:id)
        def outs = parts.select { |p| p.kind == 'out' }.map(&:id)
    end
    Circuit = Struct.new(:parts, :blocks) do
        def ins = parts.select { |p| p.kind == 'in' }.map(&:id)
        def outs = parts.select { |p| p.kind == 'out' }.map(&:id)
    end

    # ------------------------------------------------------------------
    # The netlist (logic_circuit.js, Netlist.parse: the same text, the same
    # messages)

    # [circuit, errors]
    def self.parse(text)
        errors = []
        top = Def.new([])
        blocks = {}
        scope = top
        scope_name = nil
        text.to_s.split(/\r?\n|;/).each do |raw|
            line = raw.sub(/#.*\z/, '').strip
            next if line.empty?

            tokens = line.split(/\s+/)
            head = tokens[0]
            if head == 'block'
                if scope_name
                    errors << "Ein Baustein steht nicht in einem anderen: #{scope_name} hat noch kein end."
                    next
                end
                name = tokens[1].to_s
                if tokens.size != 2 || name !~ BLOCK_NAME || KEYWORDS.include?(name.downcase)
                    errors << 'Ein Baustein braucht einen Namen aus Buchstaben und Ziffern, etwa »block HA«.'
                    next
                end
                if blocks.key?(name)
                    errors << "Den Baustein #{name} gibt es zweimal."
                    next
                end
                if blocks.size >= MAX_BLOCKS
                    errors << "Mehr als #{MAX_BLOCKS} Bausteine gehen nicht."
                    next
                end
                scope = Def.new([])
                blocks[name] = scope
                scope_name = name
                next
            end
            if head == 'end'
                if scope_name.nil?
                    errors << 'Zu diesem end gibt es kein block.'
                    next
                end
                scope = top
                scope_name = nil
                next
            end
            x = y = nil
            if (m = tokens.last.match(/\A@(\d{1,4}),(\d{1,4})\z/))
                tokens.pop
                x = m[1].to_i
                y = m[2].to_i
            elsif tokens.last.start_with?('@')
                errors << "»#{tokens.last}« ist keine Stelle: so etwa @120,48."
                next
            end
            kind, id, *src = tokens
            unless id.to_s =~ NAME
                errors << "»#{id}« taugt nicht als Name: Buchstaben, Ziffern und _, vorn ein Buchstabe."
                next
            end
            if scope.parts.any? { |p| p.id == id }
                errors << "Den Namen #{id} gibt es hier zweimal."
                next
            end
            if scope.parts.size >= MAX_PARTS
                errors << "Mehr als #{MAX_PARTS} Teile gehen nicht."
                next
            end
            part = Part.new(:id => id, :kind => kind, :src => src.map { |s| s == '-' ? nil : s }, :x => x, :y => y)
            if kind == 'in'
                unless src.empty?
                    errors << "Ein Schalter (in #{id}) hat keine Eingänge."
                    next
                end
            elsif kind == 'out'
                unless src.size == 1
                    errors << "Eine Lampe (out #{id}) hat genau eine Leitung, etwa »out #{id} g1«."
                    next
                end
            elsif GATE_KINDS.include?(kind)
                unless kind == 'not' ? src.size == 1 : src.size.between?(2, MAX_INPUTS)
                    errors << (kind == 'not' ? "Ein NICHT (#{id}) hat genau einen Eingang." : "Ein #{GATE_NAMES[kind]} (#{id}) hat 2 bis #{MAX_INPUTS} Eingänge.")
                    next
                end
            elsif kind =~ BLOCK_NAME
                part.kind = 'block'
                part.block = kind
            else
                errors << "»#{kind}« kenne ich nicht: in, out, ein Gatter (#{GATE_KINDS.join(', ')}) oder der Name eines Bausteins."
                next
            end
            bad = part.src.compact.find { |s| s !~ SOURCE }
            if bad
                errors << "»#{bad}« ist keine Quelle: ein Name wie g1 oder h1.s, oder - ohne Leitung."
                next
            end
            scope.parts << part
        end
        errors << "Dem Baustein #{scope_name} fehlt sein end." if scope_name
        circuit = Circuit.new(top.parts, blocks)
        errors.concat(check_netlist(circuit)) if errors.empty?
        [circuit, errors]
    end

    def self.check_netlist(circuit)
        errors = []
        where = ->(name) { name ? " im Baustein #{name}" : '' }
        scopes = [[nil, circuit]] + circuit.blocks.to_a
        scopes.each do |name, scope|
            by_id = scope.parts.to_h { |p| [p.id, p] }
            scope.parts.each do |p|
                if p.kind == 'block'
                    inner = circuit.blocks[p.block]
                    if inner.nil?
                        errors << "Den Baustein #{p.block} (#{p.id}#{where.(name)}) gibt es nicht."
                        next
                    end
                    errors << "#{p.block} hat #{inner.ins.size} Eingänge, #{p.id}#{where.(name)} bekommt #{p.src.size}." if p.src.size != inner.ins.size
                end
                p.src.compact.each do |s|
                    id, port = s.split('.')
                    from = by_id[id]
                    bad = ->(msg) { errors << "#{p.id}#{where.(name)}: #{msg}" }
                    if from.nil?
                        bad.("Die Quelle #{s} gibt es nicht.")
                    elsif from.kind == 'out'
                        bad.("#{s} ist eine Lampe; eine Leitung kommt aus einem Schalter, einem Gatter oder einem Baustein.")
                    elsif from.kind == 'block'
                        inner = circuit.blocks[from.block]
                        if inner && port.nil?
                            bad.("Bei einem Baustein nennt die Quelle den Ausgang, etwa #{id}.#{inner.outs.first || 's'}.")
                        elsif inner && !inner.outs.include?(port)
                            bad.("#{from.block} hat keinen Ausgang #{port}.")
                        end
                    elsif port
                        bad.("#{id} hat nur einen Ausgang; die Quelle heißt #{id}.")
                    end
                end
            end
        end
        visiting = Set.new
        done = Set.new
        visit = lambda do |name, path|
            next if done.include?(name)
            if visiting.include?(name)
                errors << "Der Baustein #{name} enthält sich selbst (#{(path + [name]).join(' → ')})."
                next
            end
            visiting << name
            circuit.blocks[name].parts.each { |p| visit.(p.block, path + [name]) if p.kind == 'block' && circuit.blocks[p.block] }
            visiting.delete(name)
            done << name
        end
        circuit.blocks.each_key { |n| visit.(n, []) }
        errors
    end

    # The blocks a list of parts uses, directly or inside other blocks.
    def self.used_blocks(circuit, parts = circuit.parts, seen = Set.new)
        parts.each do |p|
            next unless p.kind == 'block' && !seen.include?(p.block) && circuit.blocks[p.block]

            seen << p.block
            used_blocks(circuit, circuit.blocks[p.block].parts, seen)
        end
        seen
    end

    # ------------------------------------------------------------------
    # Flattened: every block replaced by its gates (logic_circuit.js,
    # Netlist.flatten). Returns a Flat with the gates as arrays of indices.

    ZERO = '#0'

    class Flat
        attr_reader :inputs, :outputs, :gates, :kinds, :inner, :blocks

        # gates: [[kind, key, [input keys]]], outputs: { name => key }
        def initialize(inputs, outputs, gates, inner, blocks)
            @inputs = inputs
            @outputs = outputs
            @kinds = Hash.new(0)
            gates.each { |kind, _, _| @kinds[kind] += 1 }
            @inner = inner
            @blocks = blocks
            keys = { ZERO => 0 }
            inputs.each { |name| keys[name] ||= keys.size }
            gates.each { |_, key, _| keys[key] ||= keys.size }
            @size = keys.size
            @input_index = inputs.to_h { |name| [name, keys[name]] }
            @output_index = outputs.transform_values { |key| keys.fetch(key, 0) }
            # an input from a key that is no gate, input or 0 cannot happen
            # after chasing; it reads 0
            @gates = gates.map { |kind, key, ins| [kind, keys[key], ins.map { |k| keys.fetch(k, 0) }] }
            @fanout = Array.new(@size) { [] }
            @gates.each_with_index { |(_, _, ins), g| ins.uniq.each { |i| @fanout[i] << g } }
            reset
        end

        def gate_count = @gates.size

        # every gate 0, every input 0, as at the start of a row
        def reset
            @values = Array.new(@size, 0)
            @pending = (0...@gates.size).to_a
        end

        def set(name, bit)
            index = @input_index.fetch(name)
            return if @values[index] == bit

            @values[index] = bit
            @pending.concat(@fanout[index])
        end

        def output(name) = @values[@output_index.fetch(name)]

        # Rounds until nothing changes, at most MAX_ROUNDS: every gate whose
        # input changed computes from the values of the round before, all at
        # once (the same as round() in logic_circuit.js, which computes every
        # gate: one whose inputs stayed computes what it already shows).
        # true when it came to rest.
        def settle
            MAX_ROUNDS.times do
                dirty = @pending.uniq
                @pending = []
                return true if dirty.empty?

                changes = dirty.filter_map do |g|
                    kind, out, ins = @gates[g]
                    bit = CircuitCheck.compute(kind, ins.map { |i| @values[i] })
                    [out, bit] if bit != @values[out]
                end
                return true if changes.empty?

                changes.each do |out, bit|
                    @values[out] = bit
                    @pending.concat(@fanout[out])
                end
            end
            # still changing in the last round: as settle() in logic_circuit.js
            false
        end
    end

    def self.compute(kind, bits)
        case kind
        when 'and' then bits.all? { |b| b == 1 } ? 1 : 0
        when 'or' then bits.any? { |b| b == 1 } ? 1 : 0
        when 'xor' then bits.count(1).odd? ? 1 : 0
        when 'not' then 1 - bits[0]
        when 'nand' then bits.all? { |b| b == 1 } ? 0 : 1
        when 'nor' then bits.any? { |b| b == 1 } ? 0 : 1
        end
    end

    def self.flatten(circuit)
        gates = []
        aliases = {}
        outputs = {}
        inner = 0
        local = lambda do |prefix, s|
            next ZERO if s.nil?

            id, port = s.split('.')
            port ? "#{prefix}#{id}/#{port}" : "#{prefix}#{id}"
        end
        expand = lambda do |scope, prefix, depth|
            raise ArgumentError, "Bausteine stecken mehr als #{MAX_DEPTH}-mal ineinander." if depth > MAX_DEPTH

            scope.parts.each do |p|
                case p.kind
                when 'in'
                    nil
                when 'out'
                    if prefix.empty?
                        outputs[p.id] = local.(prefix, p.src[0])
                    else
                        aliases["#{prefix}#{p.id}"] = local.(prefix, p.src[0])
                    end
                when 'block'
                    block = circuit.blocks[p.block]
                    inside = "#{prefix}#{p.id}/"
                    block.ins.each_with_index { |pin, i| aliases["#{inside}#{pin}"] = local.(prefix, p.src[i]) }
                    expand.(block, inside, depth + 1)
                else
                    inner += 1 unless prefix.empty?
                    raise ArgumentError, "Mehr als #{MAX_GATES} Gatter gehen nicht." if gates.size >= MAX_GATES

                    gates << [p.kind, "#{prefix}#{p.id}", p.src.map { |s| local.(prefix, s) }]
                end
            end
        end
        expand.(circuit, '', 0)
        chase = lambda do |key|
            seen = Set.new
            while aliases.key?(key)
                return ZERO if seen.include?(key)

                seen << key
                key = aliases[key]
            end
            key
        end
        gates = gates.map { |kind, key, ins| [kind, key, ins.map { |k| chase.(k) }] }
        outputs = outputs.transform_values { |k| chase.(k) }
        Flat.new(circuit.ins, outputs, gates, inner, circuit.parts.count { |p| p.kind == 'block' })
    end

    # ------------------------------------------------------------------
    # The expressions of expect

    module Expr
        TOKEN = /\G\s*(?:(\d+)|([A-Za-z_][A-Za-z0-9_]*)|(==|!=|<=|>=|<<|>>|[-+*&|^~!()<>?:]))/
        LEVELS = [%w[|], %w[^], %w[&], %w[== !=], %w[< > <= >=], %w[<< >>], %w[+ -], %w[*]].freeze

        # an AST of nested arrays; raises ArgumentError
        def self.parse(text)
            tokens = []
            pos = 0
            source = text.to_s
            until source[pos..].to_s.strip.empty?
                m = TOKEN.match(source, pos)
                raise ArgumentError, "cannot read »#{source[pos..].strip[0, 10]}«" unless m

                tokens << (m[1] ? [:num, m[1].to_i] : m[2] ? [:var, m[2]] : [:op, m[3]])
                pos = m.end(0)
            end
            parser = Parser.new(tokens)
            ast = parser.ternary
            raise ArgumentError, 'unexpected text at the end' unless parser.done?

            ast
        end

        def self.names(ast)
            case ast[0]
            when :var then [ast[1]]
            when :num then []
            else ast.drop(1).select { |x| x.is_a?(Array) }.flat_map { |x| names(x) }
            end
        end

        def self.value(ast, env)
            case ast[0]
            when :num then ast[1]
            when :var then env.fetch(ast[1])
            when :if then value(ast[1], env).zero? ? value(ast[3], env) : value(ast[2], env)
            when :un
                x = value(ast[2], env)
                { '!' => x.zero? ? 1 : 0, '~' => ~x, '-' => -x }.fetch(ast[1])
            when :bin
                a = value(ast[2], env)
                b = value(ast[3], env)
                case ast[1]
                when '|' then a | b
                when '^' then a ^ b
                when '&' then a & b
                when '==' then a == b ? 1 : 0
                when '!=' then a == b ? 0 : 1
                when '<' then a < b ? 1 : 0
                when '>' then a > b ? 1 : 0
                when '<=' then a <= b ? 1 : 0
                when '>=' then a >= b ? 1 : 0
                when '<<' then a << b.clamp(0, 31)
                when '>>' then a >> b.clamp(0, 31)
                when '+' then a + b
                when '-' then a - b
                when '*' then a * b
                end
            end
        end

        class Parser
            def initialize(tokens)
                @tokens = tokens
                @i = 0
            end

            def done? = @i == @tokens.size
            def peek = @tokens[@i]

            def op?(name)
                t = peek
                t && t[0] == :op && t[1] == name
            end

            def take = (@i += 1; @tokens[@i - 1])

            def ternary
                cond = binary(0)
                return cond unless op?('?')

                take
                yes = ternary
                raise ArgumentError, 'missing : after ?' unless op?(':')

                take
                [:if, cond, yes, ternary]
            end

            def binary(level)
                return unary if level == LEVELS.size

                left = binary(level + 1)
                while peek && peek[0] == :op && LEVELS[level].include?(peek[1])
                    op = take[1]
                    left = [:bin, op, left, binary(level + 1)]
                end
                left
            end

            def unary
                if peek && peek[0] == :op && %w[! ~ -].include?(peek[1])
                    op = take[1]
                    return [:un, op, unary]
                end
                t = take
                raise ArgumentError, 'expression ends too early' if t.nil?
                return t if %i[num var].include?(t[0])
                raise ArgumentError, "unexpected #{t[1]}" unless t[1] == '('

                inside = ternary
                raise ArgumentError, 'missing )' unless op?(')')

                take
                inside
            end
        end
    end

    # ------------------------------------------------------------------
    # The task

    def self.names_list?(value) = value.is_a?(Array) && !value.empty? && value.all? { |v| v.is_a?(String) && v =~ NAME } && value.uniq.size == value.size

    def self.spec_errors(check)
        errors = []
        inputs = check['inputs']
        outputs = check['outputs']
        errors << 'inputs must list the names of the switches' unless names_list?(inputs)
        errors << 'outputs must list the names of the lamps' unless names_list?(outputs)
        return errors unless errors.empty?

        errors << 'inputs and outputs need different names' unless (inputs & outputs).empty?
        errors << "at most #{MAX_TASK_INPUTS} inputs" if inputs.size > MAX_TASK_INPUTS
        numbers = check['numbers'] || {}
        number_ok = lambda do |name, members|
            name.to_s =~ NAME && names_list?(members) && ((members - inputs).empty? || (members - outputs).empty?)
        end
        if !numbers.is_a?(Hash) || !numbers.all? { |k, v| number_ok.(k, v) }
            errors << 'numbers must name lists of inputs or of outputs, the highest bit first'
            numbers = {}
        end
        errors << 'a number needs a name of its own' unless (numbers.keys & (inputs + outputs)).empty?
        if check.key?('gates') && !(check['gates'].is_a?(Array) && !check['gates'].empty? && (check['gates'] - GATE_KINDS).empty?)
            errors << "gates must list gates from #{GATE_KINDS.join(', ')}"
        end
        %w[max_gates min_blocks].each do |key|
            errors << "#{key} must be a positive number" if check.key?(key) && !(check[key].is_a?(Integer) && check[key] > 0)
        end
        errors << 'blocks must be true or false' if check.key?('blocks') && ![true, false].include?(check['blocks'])
        errors << 'min_blocks needs blocks' if check['min_blocks'] && check['blocks'] == false
        if check.key?('expect') == check.key?('sequence')
            errors << 'a circuit check has either expect (a truth table) or sequence (steps)'
            return errors
        end
        input_numbers = numbers.select { |_, v| (v - inputs).empty? }
        output_numbers = numbers.select { |_, v| (v - outputs).empty? }
        if check.key?('expect')
            expect = check['expect']
            unless expect.is_a?(Hash) && !expect.empty?
                errors << 'expect must map lamps (or numbers of lamps) to expressions'
                return errors
            end
            covered = expect.keys.flat_map { |k| output_numbers[k] || [k] }
            errors << "expect: every lamp once (#{outputs.join(', ')})" unless covered.sort == outputs.sort
            known = inputs + input_numbers.keys
            expect.each do |key, text|
                ast = Expr.parse(text.to_s)
                unknown = Expr.names(ast) - known
                errors << "expect.#{key}: unknown names #{unknown.join(', ')}" unless unknown.empty?
            rescue ArgumentError => e
                errors << "expect.#{key}: #{e.message}"
            end
        else
            steps = check['sequence']
            unless steps.is_a?(Array) && !steps.empty? && steps.size <= MAX_STEPS
                errors << "sequence must be a list of 1 to #{MAX_STEPS} steps"
                return errors
            end
            steps.each_with_index do |step, i|
                unless step.is_a?(Hash) && (step.keys - inputs - outputs - ['say']).empty? &&
                       step.all? { |k, v| k == 'say' ? v.is_a?(String) : [0, 1].include?(v) }
                    errors << "sequence[#{i}]: inputs and lamps with 0 or 1, and say"
                end
            end
        end
        errors
    end

    # What the editor starts with: the task's switches and lamps, no wires.
    def self.starter(check)
        (check['inputs'].map { |n| "in #{n}" } + check['outputs'].map { |n| "out #{n} -" }).join("\n") + "\n"
    end

    # a list for a message: »a, b und c«
    def self.join(items)
        items.size <= 1 ? items.join : "#{items[0..-2].join(', ')} und #{items[-1]}"
    end

    def self.lamp(name, shown)
        shown == 1 ? "leuchtet die Lampe #{name}, sie sollte aus sein" : "ist die Lampe #{name} aus, sie sollte leuchten"
    end

    # [status, message, read]
    def self.check(task, check, answer)
        text = answer.to_s
        if text.strip.empty?
            return [:invalid, 'Deine Schaltung ist noch leer. Setz Gatter in den Editor und verbinde sie.', nil]
        end
        return [:invalid, "Deine Schaltung ist zu groß zum Prüfen (mehr als #{MAX_TEXT} Zeichen).", nil] if text.size > MAX_TEXT

        circuit, errors = parse(text)
        return [:invalid, "Deine Schaltung lässt sich nicht lesen: #{errors.first}", nil] unless errors.empty?

        inputs = check['inputs']
        outputs = check['outputs']
        missing = inputs - circuit.ins
        return [:wrong, "Der Schalter #{missing.first} fehlt. Die Aufgabe braucht die Schalter #{join(inputs)}.", nil] unless missing.empty?
        missing = outputs - circuit.outs
        return [:wrong, "Die Lampe #{missing.first} fehlt. Die Aufgabe braucht die Lampen #{join(outputs)}.", nil] unless missing.empty?
        extra = circuit.ins - inputs
        return [:wrong, "Den Schalter #{extra.first} gibt es in dieser Aufgabe nicht, nur #{join(inputs)}.", nil] unless extra.empty?
        extra = circuit.outs - outputs
        return [:wrong, "Die Lampe #{extra.first} gibt es in dieser Aufgabe nicht, nur #{join(outputs)}.", nil] unless extra.empty?

        used = used_blocks(circuit).to_a
        if check['blocks'] == false && !used.empty?
            return [:wrong, "In dieser Aufgabe baust du alles aus Gattern, ohne Bausteine; deine Schaltung benutzt #{join(used)}.", nil]
        end
        circuit.parts.each do |p|
            return [:wrong, "Die Lampe #{p.id} hat noch keine Leitung.", nil] if p.kind == 'out' && p.src[0].nil?
        end
        ([[nil, circuit.parts]] + used.map { |n| [n, circuit.blocks[n].parts] }).each do |name, parts|
            parts.each do |p|
                next if %w[in out].include?(p.kind) || p.src.none?(&:nil?)

                what = p.kind == 'block' ? "Baustein #{p.block} (#{p.id})" : "#{GATE_NAMES[p.kind]} #{p.id}"
                inside = name ? " im Baustein #{name}" : ''
                return [:wrong, "Am #{what}#{inside} ist ein Eingang ohne Leitung. Jeder Eingang eines Gatters braucht eine.", nil]
            end
            next unless name

            parts.each do |p|
                if p.kind == 'out' && p.src[0].nil?
                    return [:wrong, "Im Baustein #{name} hat die Lampe #{p.id} keine Leitung: Dieser Ausgang des Bausteins liefert nichts.", nil]
                end
            end
        end

        flat = begin
            flatten(circuit)
        rescue ArgumentError => e
            return [:invalid, e.message, nil]
        end
        read = read_line(circuit, flat, used)
        if check['gates']
            wrong = flat.kinds.keys - check['gates']
            unless wrong.empty?
                allowed = join(check['gates'].map { |g| GATE_NAMES[g] })
                found = join(wrong.map { |g| GATE_NAMES[g] })
                inside = flat.inner.positive? ? ', auch in deinen Bausteinen' : ''
                return [:wrong, "In dieser Aufgabe gibt es nur #{allowed}. Deine Schaltung hat auch #{found}#{inside}.", read]
            end
        end

        status, message = check['sequence'] ? run_sequence(check, flat) : run_table(check, flat)
        return [status, message, read] unless status == :correct

        count = flat.gate_count
        if check['max_gates'] && count > check['max_gates']
            inner = flat.inner.positive? ? ", #{flat.inner} davon in Bausteinen" : ''
            return [:wrong, "#{message} Aber deine Schaltung hat #{count} Gatter#{inner}; es geht mit #{check['max_gates']}.", read]
        end
        if check['min_blocks'] && flat.blocks < check['min_blocks']
            return [:wrong, "#{message} Bau sie aber aus Bausteinen: Gefragt sind mindestens #{check['min_blocks']}, in deiner Schaltung #{flat.blocks.zero? ? 'steht keiner' : "stehen #{flat.blocks}"}.", read]
        end
        [:correct, message, read]
    end

    # What the check found in the circuit, for »Was der Workspace gelesen hat«.
    def self.read_line(circuit, flat, used)
        lines = []
        lines << "- Schalter: #{circuit.ins.map { |n| "`#{n}`" }.join(', ')}"
        lines << "- Lampen: #{circuit.outs.map { |n| "`#{n}`" }.join(', ')}"
        kinds = GATE_KINDS.select { |k| flat.kinds[k].positive? }.map { |k| "#{flat.kinds[k]} #{GATE_NAMES[k]}" }
        inner = flat.inner.positive? ? ", #{flat.inner} davon in Bausteinen" : ''
        lines << "- Gatter: #{flat.gate_count}#{kinds.empty? ? '' : " (#{kinds.join(', ')})"}#{inner}"
        unless used.empty?
            here = circuit.parts.select { |p| p.kind == 'block' }.map(&:block).tally.map { |n, c| c > 1 ? "#{c} × #{n}" : n }
            lines << "- Bausteine: #{here.join(', ')}#{(used - circuit.parts.map(&:block)).empty? ? '' : "; darin #{join(used - circuit.parts.map(&:block))}"}"
        end
        lines.join("\n")
    end

    def self.bits_of(value, width) = Array.new(width) { |i| (value >> (width - 1 - i)) & 1 }.join

    # »a = 1 und b = 0«, numbers of inputs as their bits
    def self.describe_inputs(check, bits)
        numbers = (check['numbers'] || {}).select { |_, v| (v - check['inputs']).empty? }
        parts = []
        done = Set.new
        check['inputs'].each do |name|
            next if done.include?(name)

            number = numbers.find { |_, v| v.include?(name) }
            if number
                key, members = number
                value = members.reduce(0) { |n, m| 2 * n + bits[m] }
                parts << "#{key} = #{members.map { |m| bits[m] }.join} (#{value})"
                done.merge(members)
            else
                parts << "#{name} = #{bits[name]}"
                done << name
            end
        end
        join(parts)
    end

    # The expected lamps for one row: { lamp => bit }
    def self.expected(check, env, asts)
        numbers = check['numbers'] || {}
        want = {}
        asts.each do |key, ast|
            value = Expr.value(ast, env)
            if numbers.key?(key) && (numbers[key] - check['outputs']).empty?
                members = numbers[key]
                members.each_with_index { |m, i| want[m] = (value >> (members.size - 1 - i)) & 1 }
            else
                want[key] = value & 1
            end
        end
        want
    end

    # The first lamp that is wrong, said for a person; a lamp of a number
    # shows the whole number.
    def self.wrong_lamp(check, shown, want)
        name = check['outputs'].find { |o| shown[o] != want[o] }
        return nil unless name

        numbers = (check['numbers'] || {}).select { |_, v| (v - check['outputs']).empty? }
        number = numbers.find { |_, v| v.include?(name) }
        if number
            _, members = number
            got = members.map { |m| shown[m] }.join
            ok = members.map { |m| want[m] }.join
            return "zeigen die Lampen #{members.join(' ')} `#{got}`, richtig wäre `#{ok}` (#{ok.to_i(2)})"
        end
        lamp(name, shown[name])
    end

    def self.run_table(check, flat)
        inputs = check['inputs']
        numbers = (check['numbers'] || {}).select { |_, v| (v - inputs).empty? }
        asts = check['expect'].transform_values { |t| Expr.parse(t.to_s) }
        rows = 1 << inputs.size
        rows.times do |row|
            bits = inputs.each_with_index.to_h { |name, i| [name, (row >> (inputs.size - 1 - i)) & 1] }
            env = bits.merge(numbers.transform_values { |members| members.reduce(0) { |n, m| 2 * n + bits[m] } })
            flat.reset
            bits.each { |name, bit| flat.set(name, bit) }
            at = describe_inputs(check, bits)
            unless flat.settle
                return [:wrong, "Bei #{at} kommt deine Schaltung nicht zur Ruhe: Auch nach #{MAX_ROUNDS} Runden schalten noch Gatter um. Führt eine Leitung im Kreis zurück zu einem Gatter davor?"]
            end
            shown = check['outputs'].to_h { |o| [o, flat.output(o)] }
            wrong = wrong_lamp(check, shown, expected(check, env, asts))
            return [:wrong, "Bei #{at} #{wrong}."] if wrong
        end
        [:correct, "Alle #{rows} Zeilen der Wahrheitstabelle stimmen."]
    end

    def self.run_sequence(check, flat)
        inputs = check['inputs']
        bits = inputs.to_h { |name| [name, 0] }
        flat.reset
        steps = check['sequence']
        steps.each_with_index do |step, i|
            inputs.each { |name| bits[name] = step[name] if step.key?(name) }
            bits.each { |name, bit| flat.set(name, bit) }
            at = "Im #{i + 1}. Schritt (#{describe_inputs(check, bits)})"
            unless flat.settle
                return [:wrong, "#{at} kommt deine Schaltung nicht zur Ruhe: Auch nach #{MAX_ROUNDS} Runden schalten noch Gatter um."]
            end
            want = step.select { |k, _| check['outputs'].include?(k) }
            name = want.keys.find { |o| flat.output(o) != want[o] }
            next unless name

            say = step['say'] ? " #{step['say']}" : ''
            return [:wrong, "#{at} #{lamp(name, flat.output(name))}.#{say}"]
        end
        [:correct, "Alle #{steps.size} Schritte stimmen."]
    end
end
