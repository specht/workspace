require 'json'
require 'shellwords'
require_relative 'call_check'

# Check type dart (docs/aufgaben.md): calls into the Dart file the person
# wrote and compares what comes back, like RubyCheck and JavaCheck.
#
# Dart has no eval either: a shell script, run in the person's Workspace as
# the guest user that can read /workspace but not change it (rule 1), writes
# harness.dart into a fresh folder in /tmp, which imports the file (and so
# what it imports) and runs the calls of the check, and runs it with the
# Dart SDK itself (DART: the dart on the path is Flutter's, which wants to
# write into its own folder first). A main of the file never runs.
#
#   check:
#     type: dart
#     file: bibliothek/library.dart
#     setup: |                         # optional: Dart before the calls, helpers
#       Book faust() => Book('Faust', 'Goethe');
#     cases:
#       - Book('Faust', 'Goethe').title: "'Faust'"
#       - call: final b = faust(); b.lend(); b.isAvailable
#         expect: false
#       - call: Book('', 'Goethe')
#         raises: true
#
# Calls and expected values are Dart; several steps divided by »;«
# (CallCheck.steps), each case in a function of its own. Compared by value
# as in JavaCheck (numbers as numbers, doubles within 1e-9; lists, sets and
# maps by their elements; the rest with ==), shown like a literal ('Faust',
# [1, 2]), an object of the person's class with its toString. print in their
# code goes nowhere. Dart finds errors in the code only when it runs it: they
# come back as errors of the compiler, said like JavaCheck's.
module DartCheck
    MAX_CASES = CallCheck::MAX_CASES
    RESULT_MARK = CallCheck::RESULT_MARK
    COMPILE_MARK = '__HS_COMPILE__'
    DART = '/opt/flutter/bin/cache/dart-sdk/bin/dart'
    MAX_ERROR_LINES = 6

    def self.cases(check)
        CallCheck.cases(check, method(:dart_value))
    end

    def self.dart_value(value)
        case value
        when true then 'true'
        when false then 'false'
        when nil then 'null'
        else value.to_s
        end
    end

    def self.spec_errors(check)
        errors = CallCheck.spec_errors(check, '.dart')
        errors << 'setup must be Dart code (text)' if check.key?('setup') && !(check['setup'].is_a?(String) && !check['setup'].strip.empty?)
        errors
    end

    # A Dart string literal for text: »\«, »$«, »\"« and line breaks escaped.
    def self.literal(text)
        '"' + text.to_s.gsub('\\') { '\\\\' }.gsub('$') { '\\$' }.gsub('"') { '\\"' }.gsub("\n", '\\n').gsub("\r", '\\r') + '"'
    end

    # The harness; [source, ranges, setup_lines] as JavaCheck.harness. The
    # file to import is __HS_FILE__, filled in by the script.
    def self.harness(cases, setup = nil)
        source = +<<~DART
            import 'dart:async';
            import 'dart:convert';
            import '__HS_FILE__';

            final _results = <Map<String, Object?>>[];

            String _repr(Object? v) {
              if (v == null) return 'null';
              if (v is String) return "'${v.replaceAll(r'\\', r'\\\\').replaceAll("'", r"\\'").replaceAll('\\n', r'\\n')}'";
              if (v is List) return '[${v.map(_repr).join(', ')}]';
              if (v is Set) return '{${v.map(_repr).join(', ')}}';
              if (v is Map) return '{${v.entries.map((e) => '${_repr(e.key)}: ${_repr(e.value)}').join(', ')}}';
              return v.toString();
            }

            bool _same(Object? a, Object? b) {
              if (a == null || b == null) return a == b;
              if (a is num && b is num) {
                if (a is double || b is double) return a == b || (a - b).abs() <= 1e-9;
                return a == b;
              }
              if (a is List && b is List) {
                if (a.length != b.length) return false;
                for (var i = 0; i < a.length; i++) {
                  if (!_same(a[i], b[i])) return false;
                }
                return true;
              }
              if (a is Set && b is Set) return a.length == b.length && a.every((x) => b.any((y) => _same(x, y)));
              if (a is Map && b is Map) {
                if (a.length != b.length) return false;
                return a.keys.every((k) => b.containsKey(k) && _same(a[k], b[k]));
              }
              return a == b;
            }

            // where in the person's code an error came from
            String _error(Object e, StackTrace s) {
              var text = '$e';
              final at = RegExp(r'file://\\S*/([^/\\s:]+\\.dart):(\\d+)').allMatches('$s').where((m) => !m.group(1)!.startsWith('harness'));
              if (at.isNotEmpty) text += ' (${at.first.group(1)}, Zeile ${at.first.group(2)})';
              return text.length > 300 ? text.substring(0, 300) : text;
            }

            T _quiet<T>(T Function() f) =>
                runZoned(f, zoneSpecification: ZoneSpecification(print: (self, parent, zone, line) {}));

            void _add(String call, String expect, String? got, bool ok, String? error) {
              _results.add({
                'call': call,
                'expect': expect,
                'ok': ok,
                if (got != null) 'got': got.length > 300 ? '${got.substring(0, 300)} …' : got,
                if (error != null) 'error': error,
              });
            }

        DART
        setup_lines = nil
        if setup
            first = source.count("\n") + 1
            source << setup.chomp << "\n\n"
            setup_lines = first..(source.count("\n"))
        end
        ranges = []
        cases.each_with_index do |c, i|
            first = source.count("\n") + 1
            *prepare, last = CallCheck.steps(c['call'])
            body = prepare.map { |step| "    #{step};\n" }.join
            call = literal(c['call'])
            expect = literal(c['expect'])
            source << "void _case#{i}() {\n  try {\n"
            if c['raises']
                source << "    final Object? got = _quiet<Object?>(() {\n#{body}    return (#{last});\n    });\n"
                source << "    _add(#{call}, #{expect}, _repr(got), false, null);\n"
                source << "  } catch (e, s) {\n    _add(#{call}, #{expect}, null, true, _error(e, s));\n  }\n}\n\n"
            else
                source << "    final Object? expected = (#{c['expect']});\n"
                source << "    final Object? got = _quiet<Object?>(() {\n#{body}    return (#{last});\n    });\n"
                source << "    _add(#{call}, #{expect}, _repr(got), _same(got, expected), null);\n"
                source << "  } catch (e, s) {\n    _add(#{call}, #{expect}, null, false, _error(e, s));\n  }\n}\n\n"
            end
            ranges << (first..source.count("\n"))
        end
        source << <<~DART
            void main() {
            #{cases.each_index.map { |i| "  _case#{i}();" }.join("\n")}
              print('');
              print('#{RESULT_MARK}${jsonEncode({'cases': _results})}');
            }
        DART
        [source, ranges, setup_lines]
    end

    def self.command_for_check(check)
        command(check['file'], cases(check), check['setup'])
    end

    # The script that prints the result, run in /workspace as the guest.
    def self.command(file, cases, setup = nil)
        source, = harness(cases, setup)
        quoted = Shellwords.escape(file)
        <<~SH
            f="$PWD/"#{quoted}
            if [ ! -f "$f" ]; then printf '\\n%s\\n' '#{RESULT_MARK}{"error":{"kind":"missing"}}'; exit 0; fi
            t=$(mktemp -d) || exit 1
            trap 'rm -rf "$t"' EXIT
            cat > "$t/harness.dart" <<'__HS_HARNESS__'
            #{source.chomp}
            __HS_HARNESS__
            e=$(printf '%s' "$f" | sed 's/[&|\\\\]/\\\\&/g')
            sed -i "s|__HS_FILE__|file://$e|" "$t/harness.dart"
            dart=#{Shellwords.escape(DART)}
            [ -x "$dart" ] || dart=dart
            HOME="$t" "$dart" "$t/harness.dart" > "$t/out" 2> "$t/errors"
            if grep -q '#{RESULT_MARK}' "$t/out"; then cat "$t/out"; exit 0; fi
            echo '#{COMPILE_MARK}'
            d=$(dirname "$f")
            sed "s|file://||g; s|$t/||g; s|$d/||g" "$t/errors"
        SH
    end

    def self.parse(output)
        CallCheck.parse(output)
    end

    def self.evaluate(task, check, output, timed_out: false)
        output = output.to_s
        list = cases(check)
        if !timed_out && !CallCheck.parse(output) && (at = output.index(COMPILE_MARK))
            return compile_problem(check['file'], list, output[(at + COMPILE_MARK.size)..], check['setup'])
        end

        CallCheck.evaluate(check, list, output, language: 'Dart', timed_out: timed_out)
    end

    # The first error Dart reports, said for the person, as JavaCheck does.
    def self.compile_problem(file, cases, errors, setup = nil)
        lines = errors.to_s.lines.map(&:rstrip)
        at = lines.index { |l| l =~ /\A\S+\.dart:\d+:\d+: Error: / }
        read = lines.reject { |l| l.strip.empty? }.first(MAX_ERROR_LINES)
        read = read.empty? ? nil : "```\n#{read.join("\n").gsub('```', "'''")}\n```"
        return [:wrong, "`#{file}` ließ sich nicht ausführen.", read] if at.nil?

        where, number, message = lines[at].match(/\A(\S+\.dart):(\d+):\d+: Error: (.*)\z/).captures
        if File.basename(where) == 'harness.dart'
            _, ranges, setup_lines = harness(cases, setup)
            index = ranges.index { |r| r.include?(number.to_i) }
            call = index ? cases[index]['call'] : nil
            if (name = message[/\A(?:Method not found|Undefined name|Type not found): '(\w+)'/, 1])
                return [:wrong, "In `#{file}` gibt es `#{name}` noch nicht. Heißt es genau so?", read]
            end
            if (m = message.match(/\AThe (method|getter|setter) '(\w+)' isn't defined for the (?:class|type) '(\w+)'/))
                said = m[1] == 'method' ? "keine Methode `#{m[2]}`. Heißt sie genau so?" : "kein `#{m[2]}`. Heißt es genau so?"
                return [:wrong, "`#{m[3]}` hat noch #{said}", read]
            end
            return [:wrong, "Der Aufruf `#{call}` passt noch nicht zu deinem Programm. Dart sagt: `#{message}`", read] if call
            return [:wrong, "Die Prüfung passt noch nicht zu deinem Programm. Dart sagt: `#{message}`", read] if setup_lines&.include?(number.to_i)

            return [:wrong, "`#{file}` ließ sich nicht ausführen: `#{message}`", read]
        end
        [:wrong, "`#{File.basename(where)}` enthält einen Fehler (Zeile #{number}): `#{message}`", read]
    end
end
