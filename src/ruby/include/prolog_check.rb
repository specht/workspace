require 'json'
require 'shellwords'
require_relative 'call_check'

# Check type prolog (docs/aufgaben.md): asks queries of a Prolog file the
# person wrote and compares the answers, like RubyCheck calls into a Ruby
# file. A fixed harness (HARNESS) loads the file with SWI-Prolog in the
# person's Workspace, as the guest that can read /workspace but not change
# it, runs each query with a time limit and prints the answers as JSON.
#
#   check:
#     type: prolog
#     file: prolog/familie.pl
#     cases:
#       - query: "grandparent(X, wilhelm_friedemann)"
#         answers: ["X = johann_ambrosius", "X = maria_elisabeth"]
#       - query: "ancestor(johann_ambrosius, carl_philipp_emanuel)"
#         answers: [true]                      # no variables: true or false
#       - query: "member(X, [a, b])"
#         answers: ["X = a", "X = b"]
#         ordered: true                        # in this order, duplicates count
#         say: …                               # the message when it differs
#         seconds: 8                           # for this query (default 2)
#
# An answer names the variables of the query that got a value, in their
# order, »X = a, Y = b«, a value as Prolog writes it (writeq); with none, it
# is true. Without ordered, the answers count as
# a set: in any order, an answer found twice once. A query that runs into
# the time limit (a left recursion) gets its own message.
module PrologCheck
    MAX_CASES = CallCheck::MAX_CASES
    RESULT_MARK = CallCheck::RESULT_MARK
    # seconds for one query; at most so many answers are collected
    QUERY_SECONDS = 2
    MAX_ANSWERS = 500
    MAX_SHOWN = 6

    HARNESS = <<~'PROLOG'
        % in a module of its own, so that its predicates never meet the
        % student's (a main/0 or answer/2 of theirs); queries run in user
        :- module(check_harness, []).
        :- use_module(library(http/json)).
        :- use_module(library(time)).
        :- use_module(library(apply)).
        :- use_module(library(lists)).
        :- dynamic load_error/2.

        % the first error while loading, with its line
        :- multifile user:message_hook/3.
        user:message_hook(Term, error, Lines) :-
            \+ load_error(_, _),
            ( Term = error(_, file(_, Line, _, _)) -> true ; Line = 0 ),
            with_output_to(string(Text), print_message_lines(current_output, '', Lines)),
            assertz(load_error(Line, Text)),
            fail.

        binding_text(Name = Value, Text) :- format(string(Text), "~w = ~q", [Name, Value]).

        % as the top level of Prolog: only the variables that got a value
        bound(_ = Value) :- nonvar(Value).

        answer_text(All, Text) :-
            include(bound, All, Bindings),
            Bindings \== [], !,
            maplist(binding_text, Bindings, Parts),
            atomic_list_concat(Parts, ', ', Atom),
            atom_string(Atom, Text).
        answer_text(_, "true").

        run_case(Case, Out) :-
            get_dict(query, Case, Query),
            (   catch(term_string(Goal, Query, [variable_names(Bindings), module(user)]), _, fail)
            ->  run_goal(Case, Query, Goal, Bindings, Out)
            ;   Out = _{query: Query, error: "syntax"}
            ).

        run_goal(Case, Query, Goal, Bindings, Out) :-
            get_dict(seconds, Case, Seconds),
            get_dict(max, Case, Max),
            catch(call_with_time_limit(Seconds,
                      findnsols(Max, Text, (call(user:Goal), answer_text(Bindings, Text)), Found)),
                  E, true),
            (   var(E)
            ->  ( Found == [] -> Answers = ["false"] ; Answers = Found ),
                Out = _{query: Query, answers: Answers}
            ;   E = time_limit_exceeded
            ->  Out = _{query: Query, error: "time"}
            ;   E = error(resource_error(_), _)
            ->  Out = _{query: Query, error: "stack"}
            ;   E = error(existence_error(procedure, PI), _)
            ->  format(string(P), "~q", [PI]),
                Out = _{query: Query, error: "unknown", message: P}
            ;   format(string(M), "~q", [E]),
                Out = _{query: Query, error: "other", message: M}
            ).

        main :-
            current_prolog_flag(argv, Argv),
            append(_, [File, Json], Argv),
            open_string(Json, In),
            json_read_dict(In, Cases, [value_string_as(string)]),
            (   exists_file(File)
            ->  catch(load_files(user:File, [silent(true)]), E, print_message(error, E)),
                (   load_error(Line, Text)
                ->  Result = _{error: _{kind: "syntax", line: Line, message: Text}, cases: []}
                ;   maplist(run_case, Cases, Outs),
                    Result = _{cases: Outs}
                )
            ;   Result = _{error: _{kind: "missing"}, cases: []}
            ),
            format("~n~w", ['@@MARK@@']),
            json_write_dict(current_output, Result, [width(0)]),
            nl,
            % before a main of the file's own (initialization(main, main))
            % would run at the end, reading input or printing
            halt(0).
    PROLOG

    def self.harness
        HARNESS.sub('@@MARK@@', RESULT_MARK)
    end

    def self.spec_errors(check)
        errors = []
        file = check['file']
        errors << 'file must end in .pl' unless file.is_a?(String) && file.end_with?('.pl')
        errors << 'timeout must be 1 to 30 seconds' if check.key?('timeout') && !(check['timeout'].is_a?(Integer) && check['timeout'].between?(1, 30))
        list = check['cases']
        return errors << "cases must be a list of 1 to #{MAX_CASES} queries" unless list.is_a?(Array) && !list.empty? && list.size <= MAX_CASES

        list.each_with_index do |item, i|
            unless item.is_a?(Hash) && item['query'].is_a?(String) && !item['query'].strip.empty?
                errors << "cases[#{i}]: needs a query"
                next
            end
            extra = item.keys - %w[query answers ordered say seconds]
            errors << "cases[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
            if item.key?('seconds') && !(item['seconds'].is_a?(Integer) && item['seconds'].between?(1, 20))
                errors << "cases[#{i}]: seconds must be 1 to 20"
            end
            answers = item['answers']
            errors << "cases[#{i}]: answers must be a list of texts, or [true] or [false]" unless answers.is_a?(Array) &&
                                                                                           answers.all? { |a| a.is_a?(String) || a == true || a == false }
        end
        errors
    end

    # the cases as the harness gets them
    def self.cases(check)
        (check['cases'] || []).map do |c|
            { 'query' => c['query'].to_s.strip.sub(/\.\z/, ''), 'answers' => (c['answers'] || []).map { |a| a.to_s },
              'ordered' => c['ordered'] == true, 'say' => c['say'], 'seconds' => c['seconds'] || QUERY_SECONDS }
        end
    end

    def self.argv(file, cases)
        payload = cases.map { |c| { 'query' => c['query'], 'seconds' => c['seconds'] || QUERY_SECONDS, 'max' => MAX_ANSWERS } }
        # the harness comes as an argument: the guest cannot write a file
        ['swipl', '--no-packs', '-q', '-g',
         'current_prolog_flag(argv, A), append(_, [H, _, _], A), open_string(H, S), load_files(check_harness, [stream(S)]), check_harness:main',
         '-t', 'halt', '--', harness, file, JSON.generate(payload)]
    end

    def self.command(file, cases)
        Shellwords.join(argv(file, cases))
    end

    def self.parse(output)
        CallCheck.parse(output)
    end

    def self.normal(answers, ordered)
        list = answers.map { |a| a.to_s.gsub(/\s+/, ' ').strip }
        ordered ? list : list.uniq.sort
    end

    def self.shown(answers)
        list = answers.first(MAX_SHOWN).map { |a| "`#{a}`" }
        list << "und #{answers.size - MAX_SHOWN} weitere" if answers.size > MAX_SHOWN
        list.empty? ? 'keine Antwort' : list.join(', ')
    end

    # [status, message, read] for the harness output.
    def self.evaluate(task, check, output, timed_out: false)
        file = check['file']
        if timed_out
            return [:wrong, check['slow'] || "Das Prüfen von `#{file}` lief zu lange und wurde abgebrochen. Ruft sich eine Regel endlos selbst auf?", nil]
        end
        data = parse(output)
        if data.nil?
            shown = output.to_s.strip.lines.last(8).join
            return [:wrong, "`#{file}` ließ sich nicht prüfen.", shown.empty? ? nil : "```\n#{shown.gsub('```', "'''")}\n```"]
        end
        if (error = data['error'])
            return [:wrong, "`#{file}` gibt es nicht.", nil] if error['kind'] == 'missing'

            line = error['line'].to_i.positive? ? ", Zeile #{error['line']}," : ''
            text = error['message'].to_s.strip.lines.first.to_s.strip
            return [:wrong, "In `#{file}`#{line} kann Prolog etwas nicht lesen: #{text}", nil]
        end

        cases = cases(check)
        results = data['cases'] || []
        read = []
        cases.each_with_index do |c, i|
            r = results[i] || {}
            if r['error']
                read << "- <i class='bi bi-x-lg'></i> `?- #{c['query']}.` → #{error_text(r, c['seconds'])}"
                message = r['error'] == 'time' && check['slow'] ? check['slow'] : "`?- #{c['query']}.` #{error_text(r, c['seconds'])}"
                return [:wrong, message, read.join("\n")]
            end
            got = normal(r['answers'] || [], c['ordered'])
            expected = normal(c['answers'], c['ordered'])
            ok = got == expected
            read << "- <i class='bi #{ok ? 'bi-check-lg' : 'bi-x-lg'}'></i> `?- #{c['query']}.` → #{shown(got)}"
            next if ok

            missing = expected - got
            extra = got - expected - ['false']
            said = got == ['false'] ? 'keine Antwort, Prolog sagt `false`' : shown(got)
            detail = if c['ordered'] && missing.empty? && extra.empty?
                         'Die Antworten stimmen, aber nicht in dieser Reihenfolge.'
                     else
                         parts = []
                         parts << "Es fehlt #{shown(missing)}." unless missing.empty? || missing == ['false']
                         parts << "Zu viel ist #{shown(extra)}." unless extra.empty?
                         parts.join(' ')
                     end
            # a long query (a whole check in one) speaks through its say alone
            message = if c['say'] && c['query'].length > 80
                          c['say']
                      else
                          ["`?- #{c['query']}.` ergibt #{said}.", detail, c['say']].compact.reject(&:empty?).join(' ')
                      end
            return [:wrong, message, read.join("\n")]
        end
        n = cases.size
        [:correct, n == 1 ? 'Die Anfrage liefert, was sie soll.' : "Alle #{n} Anfragen liefern, was sie sollen.", read.join("\n")]
    end

    def self.error_text(result, seconds = QUERY_SECONDS)
        case result['error']
        when 'time' then "hört nicht auf: Nach #{seconds} Sekunden war noch keine Antwort fertig. Ruft sich eine Regel selbst auf, bevor sie etwas festlegt?"
        # a left recursion that fills the stack before the time is up
        when 'stack' then "hört nicht auf: Sie ruft sich immer tiefer selbst auf, bis kein Speicher mehr frei ist. Ruft sich eine Regel selbst auf, bevor sie etwas festlegt?"
        when 'unknown' then "fragt nach `#{result['message']}`, und das gibt es in deiner Datei nicht."
        when 'syntax' then "lässt sich nicht lesen."
        else "bricht ab: `#{result['message'].to_s.lines.first.to_s.strip[0, 200]}`"
        end
    end
end
