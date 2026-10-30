require 'json'
require_relative 'code_languages'

# A later task builds on functions a student wrote in earlier ones (Pip's
# turn_right and walk, the turtle's tree).
# Its starter names such a function on a line of its own instead of
# spelling it out:
#
#   {{own turn-right turn_right}}
#
# Loaded (TaskSpec.load_file), every such line becomes the function's empty
# frame: its first line from the earlier task's solution and a note where
# it comes from, so that the page, the checks and the examples see code and
# nobody sees a solution before solving it. For a person who solved the
# earlier task, /api/task/states fills in that person's own function from
# the code that last passed its check (TaskState.correct_answer; main.rb,
# tasks.js): solved once, it need not be written again, and solved anew,
# the new one is in every later card. Plain Ruby, tested on its own
# (test/own_functions_test.rb).
module OwnFunctions
    LINE = /^([ \t]*)\{\{own ([a-z0-9-]+) ([A-Za-z_][A-Za-z0-9_]*)\}\}[ \t]*$/

    # [[task id, function name], …] in a starter.
    def self.names(starter)
        starter.to_s.scan(LINE).map { |_, task, name| [task, name] }
    end

    # The lines [first, last] of the function name in code, or nil. Every
    # one of them stands at the left margin (Pip, the turtle): Ruby up to its
    # »end«, Python up to the last indented line after it, the others up to
    # the brace that closes the first one.
    def self.span(code, name, language)
        lines = code.to_s.lines.map(&:rstrip)
        n = Regexp.escape(name)
        case language
        when 'ruby'
            start = lines.index { |l| l =~ /\Adef #{n}\b/ }
            stop = start && (start + 1...lines.size).find { |i| lines[i] =~ /\Aend\b/ }
            stop && [start, stop]
        when 'python'
            start = lines.index { |l| l =~ /\Adef #{n}\s*\(/ }
            return nil unless start

            stop = start
            (start + 1...lines.size).each do |i|
                break unless lines[i].empty? || lines[i] =~ /\A\s/

                stop = i unless lines[i].empty?
            end
            [start, stop]
        else
            start = lines.index { |l| l =~ /\A(?:function\s+#{n}\s*\(|[A-Za-z_][\w<>\[\], ?]*\s+#{n}\s*\()/ && l.include?('{') }
            return nil unless start

            depth = 0
            (start...lines.size).each do |i|
                depth += lines[i].count('{') - lines[i].count('}')
                return [start, i] if depth <= 0
            end
            nil
        end
    end

    # The function name in code, from its first line to its end, or nil.
    def self.extract(code, name, language)
        first, last = span(code, name, language)
        first && code.to_s.lines.map(&:rstrip)[first..last].join("\n")
    end

    # The empty frame of the function for one who has not solved the task:
    # its first line from the solution, a note in it, nothing else.
    def self.stub(solution, name, language, title)
        whole = extract(solution, name, language)
        return nil unless whole

        first = whole.lines.first.rstrip
        note = "noch leer: deine Lösung aus »#{title}«"
        case language
        when 'ruby' then "#{first}\n  # #{note}\nend"
        when 'python' then "#{first}\n    # #{note}\n    pass"
        when 'java' then "#{first}\n    // #{note}\n}"
        else "#{first}\n  // #{note}\n}"
        end
    end

    # The code inside the first fence of a solution (```ruby … ```).
    def self.solution_code(solution)
        solution.to_s[/```[^\n]*\n(.*?)^```/m, 1].to_s
    end

    # The earlier task's function: [its frame, its whole solution]. Raises on
    # a task or function that is not there, so that tasks.yaml says so.
    def self.reference(task_id, name, language, tasks)
        task = tasks[task_id] or raise ArgumentError, "{{own #{task_id} #{name}}}: no task #{task_id}"
        variant = CodeLanguages.variant(task, language)
        code = solution_code(variant['solution'])
        frame = stub(code, name, language, variant['title'] || task['title'])
        raise ArgumentError, "{{own #{task_id} #{name}}}: no #{name} in the #{language} solution of #{task_id}" unless frame

        [frame, extract(code, name, language)]
    end

    # A starter with each line {{own task name}} replaced: by mine.(task,
    # name), the person's own function, where it gives one, else by the
    # frame from that task's solution (tasks: { id => task }).
    def self.resolve(starter, language, tasks, mine: ->(_task, _name) { nil })
        starter.to_s.gsub(LINE) do
            task_id, name = Regexp.last_match(2), Regexp.last_match(3)
            frame, = reference(task_id, name, language, tasks)
            mine.call(task_id, name) || frame
        end
    end

    # What a person sees in a card whose starter (starter_own) names own
    # functions: their saved code, or the starter without one. In saved code
    # an own function the person left as the card gave it (filled: { name =>
    # what was in the card when it was saved, see .filled }) becomes their
    # newest own again, or the frame without one; so a function solved anew
    # reaches every later card. One they changed in this card stays as they
    # wrote it. Saved before filled was kept: the frame and the earlier
    # task's solution (as starters spelled it out before) count as left.
    def self.personal(code, starter_own, language, tasks, mine: ->(_task, _name) { nil }, filled: {})
        return resolve(starter_own, language, tasks, :mine => mine) unless code.is_a?(String)

        lines = code.lines.map(&:chomp)
        replace = names(starter_own).filter_map do |task_id, name|
            first, last = span(code, name, language)
            next unless first

            frame, whole = reference(task_id, name, language, tasks)
            now = lines[first..last].join("\n")
            given = filled[name] ? [filled[name], frame] : [frame, whole]
            next unless given.any? { |g| same_code?(now, g) }

            [first, last, mine.call(task_id, name) || frame]
        end
        replace.sort_by { |first, _, _| -first }.each do |first, last, text|
            lines[first..last] = text.split("\n")
        end
        lines.join("\n") + (code.end_with?("\n") ? "\n" : '')
    end

    # What a card gives for each own function right now ({ name => code }):
    # the person's own, or the frame. Kept with the card's code when it is
    # saved (TaskState.own_filled), so that .personal tells it from a change.
    def self.filled(starter_own, language, tasks, mine: ->(_task, _name) { nil })
        names(starter_own).to_h do |task_id, name|
            frame, = reference(task_id, name, language, tasks)
            [name, mine.call(task_id, name) || frame]
        end
    end

    def self.same_code?(a, b)
        squeeze = ->(t) { t.to_s.lines.map(&:strip).reject(&:empty?) }
        squeeze.(a) == squeeze.(b)
    end

    # [language, part] of a task with own functions in its starter: the
    # task itself in its own language and each language variant.
    def self.parts(task)
        [task, *CodeLanguages::LANGUAGES.map { |l| task[l] }].filter_map do |part|
            next unless part.is_a?(Hash) && (part['starter_own'] || part['starter']).to_s.match?(LINE)

            [part.equal?(task) ? (task.dig('check', 'language') || 'ruby') : CodeLanguages::LANGUAGES.find { |l| task[l].equal?(part) }, part]
        end
    end

    # Every starter of the file (data['tasks'], in place): resolved to frames
    # in 'starter', the lines with {{own …}} kept in 'starter_own'.
    def self.prepare!(data)
        tasks = (data['tasks'] || []).to_h { |t| [t['id'], t] }
        (data['tasks'] || []).each do |task|
            parts(task).each do |language, part|
                part['starter_own'] = part['starter']
                part['starter'] = resolve(part['starter'], language, tasks)
            end
        end
        data
    end

    # For one person (tasks: { id => task } of an activity, correct: { id =>
    # { language => code that passed } }, answers: { id => stored answer,
    # JSON { language => code } }, given: { id => JSON { language => { name
    # => code } } }, see .filled): the starters of the cards with own
    # functions ({ id => { language => code } }) and those cards' answers
    # with the own functions filled in.
    def self.for_person(tasks, correct, answers, given = {})
        starters = {}
        result = {}
        tasks.each do |id, task|
            parts(task).each do |language, part|
                mine = mine(correct, language)
                (starters[id] ||= {})[language] = personal(nil, part['starter_own'], language, tasks, :mine => mine)
                next unless answers[id]

                codes = (result[id] ||= parse_codes(answers[id]))
                next unless codes[language].is_a?(String)

                filled = parse_codes(given[id])[language]
                codes[language] = personal(codes[language], part['starter_own'], language, tasks,
                    :mine => mine, :filled => filled.is_a?(Hash) ? filled : {})
            end
        end
        [starters, result.transform_values { |codes| JSON.generate(codes) }]
    end

    # The person's own function name of the task task_id, from the code
    # that passed (correct: { id => { language => code } }), or nil.
    def self.mine(correct, language)
        lambda do |task_id, name|
            code = (correct[task_id] || {})[language]
            code.is_a?(String) ? extract(code, name, language) : nil
        end
    end

    def self.parse_codes(json)
        value = JSON.parse(json.to_s)
        value.is_a?(Hash) ? value : {}
    rescue JSON::ParserError
        {}
    end
end
