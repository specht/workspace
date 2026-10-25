require 'json'
require 'shellwords'
require_relative 'call_check'

# Check type c (docs/aufgaben.md): calls functions in a C file the person
# wrote and compares what comes back, like RubyCheck and PythonCheck.
#
# A shell script, run in the person's Workspace as the guest user that can
# read /workspace but not change it (the code is theirs, rule 1), writes a
# harness into a fresh folder in /tmp and compiles it with cc. The harness
# includes the person's file with its main renamed (#define main …), so the
# program itself never runs, only the calls of the check. Functions of a
# library the file uses but the calls do not need (PixelRAM's screen_open,
# set_pixel …) are left out by the linker (--gc-sections), and whatever is
# still missing links as an empty address (--unresolved-symbols=ignore-all):
# a call that reaches it crashes and is reported like any other crash.
# pixelram.h comes from the .pixelram folder that make fills next to the
# file; without it the check says to run make once. raylib.h, raymath.h and
# rlgl.h come from the Emscripten sysroot of the Workspace (RAYLIB_INCLUDE),
# searched after the system's own headers, so stdio.h stays the native one;
# raymath's functions become static inline there (RAYMATH_STATIC_INLINE),
# since a plain inline function in C has no definition to link against.
#
# Every case runs in a child process of its own (fork) with a time limit of
# CASE_SECONDS, so a crash, an endless loop or an exit() in one call costs
# only that case. Output of the person's code goes to /dev/null.
#
#   check:
#     type: c
#     file: raytracer/raytracer.c
#     tolerance: 0.001                 # for numbers with a decimal point; default 1e-6
#     setup: |                         # optional: C code before the calls, helpers
#       double floor_color(double x, double z)
#       {
#           hit h;
#           nearest_hit((vec){x, 1, z}, (vec){0, -1, 0}, &h);
#           return h.mat.color.x;
#       }
#     cases:
#       - reflect((vec){0.6, -0.8, 0}, (vec){0, 1, 0}).y: 0.8
#       - call: hit_floor((vec){0, 1, 0}, (vec){0, 1, 0})
#         expect: INFINITY
#         say: Ein Strahl nach oben trifft den Boden nie.
#       - floor_color(0.5, 4.5) != floor_color(1.5, 4.5): true
#
# Calls and expected values are C expressions. A call must give a number
# (int, double, bool, char …) or a string (char *): for a struct, the case
# names a field (.y). true, false, ~ and numbers in YAML become true,
# false, NULL and the number; .inf becomes INFINITY. Numbers are compared
# as double, equal within the tolerance (infinities of the same sign are
# equal); strings with strcmp, bools as true or false.
module CCheck
    MAX_CASES = CallCheck::MAX_CASES
    RESULT_MARK = CallCheck::RESULT_MARK
    CASE_MARK = '__HS_CASE__'
    COMPILE_MARK = '__HS_COMPILE__'
    CASE_SECONDS = 2
    # where the Workspace image installs raylib (docker/code/Dockerfile)
    RAYLIB_INCLUDE = '/opt/emsdk/upstream/emscripten/cache/sysroot/include'
    DEFAULT_TOLERANCE = 1e-6
    MAX_ERROR_LINES = 6

    SIGNALS = {
        6 => 'bricht mit abort ab, etwa an einem assert',
        8 => 'bricht mit einem Rechenfehler ab, etwa einer Division durch null bei ganzen Zahlen',
        11 => 'greift auf Speicher zu, der ihm nicht gehört (Segmentation fault)',
        7 => 'greift auf Speicher zu, der ihm nicht gehört (Bus error)',
        14 => "läuft länger als #{CASE_SECONDS} Sekunden. Steckt irgendwo eine Endlosschleife?",
    }.freeze

    # The cases of a check as [{ 'call' =>, 'expect' => (C), 'say' => }].
    def self.cases(check)
        CallCheck.cases(check, method(:c_value))
    end

    def self.c_value(value)
        case value
        when true then 'true'
        when false then 'false'
        when nil then 'NULL'
        when Float
            if value.infinite?
                value.positive? ? 'INFINITY' : '-INFINITY'
            else
                value.to_s
            end
        else value.to_s
        end
    end

    def self.spec_errors(check)
        errors = CallCheck.spec_errors(check, '.c')
        (check['cases'] || []).each_with_index do |item, i|
            errors << "cases[#{i}]: raises is not possible in C" if item.is_a?(Hash) && item.key?('raises')
        end
        if check.key?('tolerance') && !(check['tolerance'].is_a?(Numeric) && check['tolerance'] >= 0)
            errors << 'tolerance must be a number of at least 0'
        end
        errors << 'setup must be C code (text)' if check.key?('setup') && !(check['setup'].is_a?(String) && !check['setup'].strip.empty?)
        errors
    end

    # The harness: the person's file, the setup of the check, a function per
    # case and a main that runs each case in a child process. Returns
    # [source, lines, setup_lines], lines giving for each case the line of its
    # call in the source, setup_lines the range of the setup.
    def self.harness(cases, tolerance, setup = nil)
        lines = []
        source = +<<~C
            #define main hs_student_main
            #include HS_FILE
            #undef main
            #include <math.h>
            #include <signal.h>
            #include <stdio.h>
            #include <stdlib.h>
            #include <string.h>
            #include <sys/wait.h>
            #include <unistd.h>

            static int hs_out = 1;
            static const double hs_tolerance = #{format('%.17g', tolerance)};

            static void hs_text(const char *text, char *buffer, size_t size)
            {
                size_t n = 0;
                for (const char *p = text; *p && n + 4 < size; p++)
                    buffer[n++] = (*p == '\\t' || *p == '\\n' || *p == '\\r') ? ' ' : *p;
                buffer[n] = 0;
            }

            static void hs_num(int i, double got, double expect)
            {
                int ok = got == expect || fabs(got - expect) <= hs_tolerance;
                dprintf(hs_out, "#{CASE_MARK}\\t%d\\tok\\t%d\\t%.10g\\n", i, ok, got);
            }

            static void hs_bool(int i, _Bool got, double expect)
            {
                dprintf(hs_out, "#{CASE_MARK}\\t%d\\tok\\t%d\\t%s\\n", i, got == (expect != 0), got ? "true" : "false");
            }

            static void hs_str(int i, const char *got, const char *expect)
            {
                char shown[220];
                hs_text(got ? got : "NULL", shown, sizeof shown);
                int ok = got && expect ? strcmp(got, expect) == 0 : got == expect;
                dprintf(hs_out, "#{CASE_MARK}\\t%d\\tok\\t%d\\t%s\\n", i, ok, shown);
            }

            #define HS_CHECK(i, got, expect) _Generic((got), \\
                char *: hs_str, const char *: hs_str, _Bool: hs_bool, default: hs_num)(i, got, expect)

        C
        setup_lines = nil
        if setup
            first = source.count("\n") + 1
            source << setup.chomp << "\n\n"
            setup_lines = first..(source.count("\n"))
        end
        cases.each_with_index do |c, i|
            source << "static void hs_case_#{i}(void)\n{\n"
            lines << source.count("\n") + 1
            source << "    HS_CHECK(#{i}, (#{c['call']}), (#{c['expect']}));\n}\n\n"
        end
        source << <<~C
            int main(void)
            {
                void (*cases[])(void) = { #{cases.each_index.map { |i| "hs_case_#{i}" }.join(', ')} };
                hs_out = dup(1);
                if (!freopen("/dev/null", "w", stdout))
                    return 1;
                for (int i = 0; i < (int)(sizeof cases / sizeof cases[0]); i++)
                {
                    pid_t pid = fork();
                    if (pid == 0)
                    {
                        alarm(#{CASE_SECONDS});
                        cases[i]();
                        _exit(42);
                    }
                    int status = 0;
                    waitpid(pid, &status, 0);
                    if (WIFSIGNALED(status))
                        dprintf(hs_out, "#{CASE_MARK}\\t%d\\tsignal\\t%d\\n", i, WTERMSIG(status));
                    else if (WIFEXITED(status) && WEXITSTATUS(status) != 42)
                        dprintf(hs_out, "#{CASE_MARK}\\t%d\\texit\\t%d\\n", i, WEXITSTATUS(status));
                }
                return 0;
            }
        C
        [source, lines, setup_lines]
    end

    # The command that prints the result, run in /workspace as the guest.
    def self.command_for_check(check)
        command(check['file'], cases(check), check['tolerance'] || DEFAULT_TOLERANCE, check['setup'])
    end

    def self.command(file, cases, tolerance = DEFAULT_TOLERANCE, setup = nil)
        source, = harness(cases, tolerance, setup)
        quoted = Shellwords.escape(file)
        <<~SH
            f="$PWD/"#{quoted}
            if [ ! -f "$f" ]; then printf '\\n%s\\n' '#{RESULT_MARK}{"error":{"kind":"missing"}}'; exit 0; fi
            t=$(mktemp -d) || exit 1
            trap 'rm -rf "$t"' EXIT
            cat > "$t/check.c" <<'__HS_HARNESS__'
            #{source.chomp}
            __HS_HARNESS__
            d=$(dirname "$f")
            set -- -I"$d"
            for p in "$d"/.pixelram/*/; do [ -f "$p/pixelram.h" ] && set -- "$@" -I"$p"; done
            r=#{Shellwords.escape(RAYLIB_INCLUDE)}
            [ -f "$r/raylib.h" ] && set -- "$@" -idirafter "$r" -DRAYMATH_STATIC_INLINE
            if ! cc -std=gnu11 -O0 -DHS_FILE="\\"$f\\"" "$@" -Werror=implicit-function-declaration \\
                -ffunction-sections -fdata-sections -Wl,--gc-sections -Wl,--unresolved-symbols=ignore-all \\
                -o "$t/check" "$t/check.c" -lm 2> "$t/errors"; then
                echo '#{COMPILE_MARK}'
                sed "s|$t/check.c|check.c|g" "$t/errors"
                exit 0
            fi
            "$t/check"
        SH
    end

    # [status, message, read] for the output of the command.
    def self.evaluate(task, check, output, timed_out: false)
        file = check['file']
        list = cases(check)
        output = output.to_s
        if timed_out || CallCheck.parse(output)
            return CallCheck.evaluate(check, list, output, language: 'C', timed_out: timed_out)
        end

        if (at = output.index(COMPILE_MARK))
            return compile_problem(file, list, output[(at + COMPILE_MARK.size)..], check['setup'])
        end

        results = list.each_with_index.map do |c, i|
            { 'call' => c['call'], 'expect' => c['expect'], 'ok' => false,
              'error' => 'liefert kein Ergebnis' }
        end
        output.each_line do |line|
            next unless line.start_with?(CASE_MARK)

            _, index, kind, value, shown = line.chomp.split("\t", 5)
            entry = results[index.to_i]
            next unless entry

            case kind
            when 'ok'
                entry.delete('error')
                entry['ok'] = value == '1'
                entry['got'] = shown.to_s
                # a comparison in C is an int: said as true or false when that is expected
                if %w[true false].include?(entry['expect']) && %w[0 1].include?(entry['got'])
                    entry['got'] = entry['got'] == '1' ? 'true' : 'false'
                end
            when 'signal'
                entry['error'] = SIGNALS[value.to_i] || "bricht mit dem Signal #{value} ab"
            when 'exit'
                entry['error'] = "beendet das Programm mit exit(#{value})"
            end
        end
        verdict(list, results)
    end

    # Like CallCheck.evaluate, with a crash said in words: »`f(0)` greift auf
    # Speicher zu, der ihm nicht gehört«.
    def self.verdict(cases, results)
        read = results.first(CallCheck::MAX_SHOWN).map do |r|
            icon = r['ok'] ? "<i class='bi bi-check-lg'></i>" : "<i class='bi bi-x-lg'></i>"
            outcome = r['error'] ? r['error'] : "`#{r['got']}`"
            "- #{icon} `#{r['call']}` → #{outcome}"
        end
        read << "- … und #{results.size - CallCheck::MAX_SHOWN} weitere" if results.size > CallCheck::MAX_SHOWN
        read = read.join("\n")

        failed_at = results.index { |r| !r['ok'] }
        if failed_at.nil?
            n = results.size
            return [:correct, n == 1 ? 'Der Aufruf liefert, was er soll.' : "Alle #{n} Aufrufe liefern, was sie sollen.", read]
        end

        r = results[failed_at]
        message = if r['error']
            "`#{r['call']}` #{r['error']}#{r['error'].end_with?('?') ? '' : '.'}"
        else
            "`#{r['call']}` ergibt `#{r['got']}`, richtig wäre `#{r['expect']}`."
        end
        say = cases[failed_at] && cases[failed_at]['say']
        message += " #{say}" if say
        [:wrong, message, read]
    end

    # The first error of the compiler, said for the person: in their file,
    # a function the check calls that is not there, or a call that does not
    # fit their function.
    def self.compile_problem(file, cases, errors, setup = nil)
        name = File.basename(file)
        lines = errors.lines.map(&:rstrip)
        first = lines.find { |l| l =~ /\berror:/ } || lines.find { |l| !l.strip.empty? }
        read = lines.reject { |l| l.strip.empty? }.first(MAX_ERROR_LINES)
        read = read.empty? ? nil : "```\n#{read.join("\n").gsub('```', "'''")}\n```"
        return [:wrong, "`#{file}` ließ sich nicht übersetzen.", read] if first.nil?

        if first =~ /pixelram\.h/ && first =~ /No such file|not found/i
            return [:wrong, "`#{file}` braucht `pixelram.h`, und das holt erst `make`. Übersetze dein Programm einmal im Terminal mit `make`, dann prüf noch einmal.", nil]
        end

        _, harness_lines, setup_lines = harness(cases, DEFAULT_TOLERANCE, setup)
        message = first.sub(/\A.*?\berror:\s*/, '').strip
        if (m = first.match(/\A(?:.*\/)?check\.c:(\d+):/))
            index = harness_lines.index(m[1].to_i)
            call = index ? cases[index]['call'] : nil
            missing = message[/implicit declaration of function [‘'"`]?([A-Za-z_]\w*)/, 1] ||
                      message[/[‘'"`]([A-Za-z_]\w*)[’'"`] undeclared/, 1]
            if missing
                return [:wrong, "In `#{name}` gibt es `#{missing}` noch nicht. Heißt deine Funktion genau so?", read]
            end
            if call
                return [:wrong, "Der Aufruf `#{call}` passt noch nicht zu deinem Programm. Der Compiler sagt: `#{message}`", read]
            end
            if setup_lines&.include?(m[1].to_i)
                return [:wrong, "Die Prüfung passt noch nicht zu deinem Programm. Der Compiler sagt: `#{message}`", read]
            end
        elsif (m = first.match(/#{Regexp.escape(name)}:(\d+):/))
            return [:wrong, "`#{name}` lässt sich nicht übersetzen (Zeile #{m[1]}): `#{message}`", read]
        end
        [:wrong, "`#{file}` ließ sich nicht übersetzen: `#{message}`", read]
    end
end
