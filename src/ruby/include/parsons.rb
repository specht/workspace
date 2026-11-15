# Parsons puzzles (docs/aufgaben.md, »function«, »Parsons«): a Kata card
# whose code is not typed but put together from given lines. parsons: the
# lines of a correct solution, indented as they must be; parsons_unused:
# lines that look right and do not belong (optional). The card shows all of
# them sorted and without indentation, so the page never contains the
# solution; tasks.js shuffles them per person, the student orders and
# indents them (and leaves out what does not belong), and the joined lines
# go to the function check as code.
module Parsons
    MAX_LINES = 20
    MAX_UNUSED = 3

    # The code of the solution, as the check gets it
    def self.code(lines)
        "#{Array(lines).join("\n")}\n"
    end

    # The lines as the card shows them: without indentation, sorted (the
    # same for everyone; tasks.js shuffles them per person)
    def self.shown(lines, unused = nil)
        (Array(lines) + Array(unused)).map(&:strip).sort
    end

    # Spaces per step of indentation in the solution (2 or 4, …); 4 if no
    # line is indented
    def self.indent(lines)
        steps = Array(lines).map { |l| l[/\A */].size }.reject(&:zero?)
        steps.empty? ? 4 : steps.min
    end

    # What is wrong with a task's parsons fields, as messages for TaskSpec
    def self.errors(task)
        errors = []
        lines = task['parsons']
        unused = task['parsons_unused']
        return errors if lines.nil? && unused.nil?

        type = (task['check'] || {})['type']
        errors << 'parsons only works with function' unless type == 'function'
        if !lines.is_a?(Array) || lines.size < 3 || lines.size > MAX_LINES || !lines.all? { |l| l.is_a?(String) && !l.strip.empty? }
            errors << "parsons must be 3 to #{MAX_LINES} lines of code, none empty"
        elsif lines.any? { |l| l.include?("\t") }
            errors << 'parsons lines are indented with spaces, not tabs'
        elsif (step = indent(lines)) && lines.any? { |l| l[/\A */].size % step != 0 }
            errors << "parsons lines must be indented in steps of #{step} spaces"
        end
        unless unused.nil?
            if !unused.is_a?(Array) || unused.empty? || unused.size > MAX_UNUSED || !unused.all? { |l| l.is_a?(String) && !l.strip.empty? }
                errors << "parsons_unused must be 1 to #{MAX_UNUSED} lines"
            elsif lines.is_a?(Array) && (unused.map(&:strip) & lines.map(&:strip)).any?
                errors << 'a line in parsons_unused is also a line of the solution'
            end
            errors << 'parsons_unused needs parsons' if lines.nil?
        end
        errors << 'a Parsons puzzle has no starter' if task['starter'] && lines
        errors
    end
end
