#!/usr/bin/env ruby
# bin/run_task
require "json"
require "yaml"

ROOT = File.expand_path("..", __dir__)

# ---- args ----
args = ARGV.dup
stream = args.delete("--stream")
stream_json = args.delete("--stream-json")

stream_format_arg = args.find { |a| a.start_with?("--stream-format=") }
summary_arg       = args.find { |a| a.start_with?("--summary=") }

args.delete(stream_format_arg) if stream_format_arg
args.delete(summary_arg) if summary_arg

stream_format = (stream_format_arg&.split("=", 2)&.last || "compact") # compact|failures|full
summary_mode  = (summary_arg&.split("=", 2)&.last || "failures")      # compact|full|failures

task_id  = args[0]
language = args[1]

if task_id.nil? || language.nil?
    warn "Usage: bin/run_task <task_id> <language> [--stream] [--stream-json] [--stream-format=compact|failures|full] [--summary=compact|full|failures] < submission_file"
    exit 2
end

image = "hackschule-exec-#{language}"

# ---- read submission ----
submission_code = STDIN.read

# ---- require judge core ----
require File.join(ROOT, "include", "docker_executor")
require File.join(ROOT, "include", "function_task")
require File.join(ROOT, "include", "core")

executor = Judge::DockerExecutor.new(image: image)

# ---- load task ----
task_file = File.join(ROOT, "tasks", task_id, "task.rb")
unless File.exist?(task_file)
    puts JSON.generate({
    status: "error",
    tests: [],
    error: { type: "UnknownTask", message: "Unknown task: #{task_id}", location: nil }
})
exit 1
end

require task_file

unless Object.private_instance_methods.include?(:build_task) || Object.instance_methods.include?(:build_task)
    puts JSON.generate({
    status: "error",
    tests: [],
    error: { type: "TaskContractError", message: "Task #{task_id} must define build_task(executor:)", location: nil }
})
exit 1
end

task = Object.new.send(:build_task, executor: executor)

# ---- helpers ----
MAX_CALL = 140

def truncate_call(call)
    return call if call.nil? || call.length <= MAX_CALL
    call[0, MAX_CALL - 1] + "…"
end

def compact_test_event(ev)
    # ev: {"event"=>"test","index"=>i,"total"=>n,"test"=>{...}}
    t = ev["test"] || ev[:test] || {}
    idx = ev["index"] || ev[:index]
    total = ev["total"] || ev[:total]
    status = t["status"] || t[:status]
    ok = (status == "pass")
    msg = t["message"] || t[:message]
    loc = t["location"] || t[:location]
    call = t["call"] || t[:call]

    out = { event: "t", i: idx, total: total, ok: ok }
    # include call only when useful (failure), and truncate
    if !ok
        out[:call] = truncate_call(call) if call
        out[:msg] = msg if msg
        out[:loc] = loc if loc
    end
    out
end

def full_test_event(ev)
    # Keep your existing schema, but truncate call to avoid massive NDJSON lines.
    ev = ev.dup
    test = ev["test"] || ev[:test]
    if test && (test["call"] || test[:call])
        c = test["call"] || test[:call]
        if test.is_a?(Hash)
            if test.key?("call")
                test["call"] = truncate_call(c)
            else
                test[:call] = truncate_call(c)
            end
        end
    end
    ev
end

def summarize_result_compact(out, total:, failed_count:)
    {
    event: "summary",
    status: out[:status],
    total: total,
    failed: failed_count,
    error: out[:error],
    task_id: out[:task_id],
    language: out[:language]
}
end

def summarize_failures_only(out)
    tests = out[:tests] || []
    failed_tests = tests.select { |t| t[:status] == "fail" }

    # Truncate call to avoid giant payloads
    failed_tests.each do |t|
        t[:call] = truncate_call(t[:call]) if t[:call]
    end

    {
    event: "summary",
    status: out[:status],
    failed_tests: failed_tests,
    error: out[:error],
    task_id: out[:task_id],
    language: out[:language]
}
end

# ---- run ----
if stream
    if stream_json
        # Legacy, fail-fast NDJSON stream for programmatic consumers.
        res = task.run_stream(submission_code: submission_code, fail_fast: true) do |ev|
            ev = JSON.parse(JSON.generate(ev))

            case ev["event"]
            when "output"
                # Keep it simple for the frontend terminal: one event type.
                puts JSON.generate({
                    event: "o",
                    stream: ev["stream"],
                    text: ev["text"],
                    i: ev["index"],
                    task_id: task_id,
                    language: language
                })
                STDOUT.flush

            when "test"
                t = ev["test"] || {}
                to_print =
                    case stream_format
                    when "full"
                        full_test_event(ev)
                    when "failures"
                        (t["status"] == "fail") ? compact_test_event(ev) : nil
                    else
                        compact_test_event(ev)
                    end

                if to_print
                    to_print = JSON.parse(JSON.generate(to_print))
                    to_print["task_id"] = task_id
                    to_print["language"] = language
                    puts JSON.generate(to_print)
                    STDOUT.flush
                end

            when "error"
                puts JSON.generate({ event: "error", error: ev["error"], task_id: task_id, language: language })
                STDOUT.flush
            end
        end

        out = res.to_h
        exit(out[:status] == "pass" ? 0 : 1)
    end

    # Human-friendly ANSI stream:
    # - Forward output immediately (stdout/stderr from student code)
    # - Run all tests (no fail-fast)
    # - Print a colored progress line per test
    # - Print nice errors (with locations)

    module ANSI
        RESET = "\e[0m"
        BOLD  = "\e[1m"
        DIM   = "\e[2m"

        RED   = "\e[31m"
        GREEN = "\e[32m"
        YELLOW= "\e[33m"
        CYAN  = "\e[36m"
        GRAY  = "\e[90m"

        BG_RED   = "\e[41m"
        BG_GREEN = "\e[42m"

        WHITE = "\e[97m"

        def self.wrap(*codes, s)
            "#{codes.join}#{s}#{RESET}"
        end
    end

    def fmt_loc(loc)
        return nil if loc.nil?
        file = loc["file"] || loc[:file]
        line = loc["line"] || loc[:line]
        return nil unless file || line
        [file, (line ? "#{line}" : nil)].compact.join(":")
    end

    # Abbreviate values (results / expected) so the terminal stays readable.
    MAX_VAL_STR = 120
    MAX_ELEMS   = 10
    MAX_DEPTH   = 3

    def fmt_value(v, depth: 0)
        return "…" if depth >= MAX_DEPTH

        case v
        when Array
            if v.length <= MAX_ELEMS
                "[#{v.map { |x| fmt_value(x, depth: depth + 1) }.join(', ')}]"
            else
                head = v.first(3).map { |x| fmt_value(x, depth: depth + 1) }
                tail = v.last(2).map { |x| fmt_value(x, depth: depth + 1) }
                "[#{(head + ['…'] + tail).join(', ')}]"
            end
        when Hash
            pairs = v.to_a
            shown = pairs.first(MAX_ELEMS).map do |k, val|
                "#{fmt_value(k, depth: depth + 1)}=>#{fmt_value(val, depth: depth + 1)}"
            end
            shown << "…" if pairs.length > MAX_ELEMS
            "{#{shown.join(', ')}}"
        when String
            s = v
            if s.length > MAX_VAL_STR
                (s[0, MAX_VAL_STR - 1] + "…").inspect
            else
                s.inspect
            end
        else
            s = v.inspect
            s = s[0, MAX_VAL_STR - 1] + "…" if s.length > MAX_VAL_STR
            s
        end
    end

    def print_error_block(title:, type:, message:, location: nil)
        loc = fmt_loc(location)
        header = [title, (type && !type.empty? ? type : nil)].compact.join(": ")
        puts ANSI.wrap(ANSI::RED, "\n#{ANSI::BOLD}#{header}#{ANSI::RESET}")
        puts "  #{message}" if message && !message.empty?
        puts ANSI.wrap(ANSI::GRAY, "  at #{loc}") if loc
        STDOUT.flush
    end

    def unwrap_expected(exp)
        return exp unless exp.is_a?(Hash)

        kind = exp["kind"] || exp[:kind]
        case kind
        when "equals"
            # print the actual expected value, not the matcher wrapper
            return exp["value"] if exp.key?("value")
            return exp[:value]  if exp.key?(:value)
        end

        exp
    end

    started = false
    total = nil
    passed = 0
    failed = 0
    failures = []
    fatal_error = nil

    puts ANSI.wrap(ANSI::CYAN, "#{ANSI::BOLD}Running #{task_id} (#{language})#{ANSI::RESET}\n")
    STDOUT.flush
    first_task = true

    res = task.run_stream(submission_code: submission_code, fail_fast: true) do |ev|
        ev = JSON.parse(JSON.generate(ev))

        case ev["event"]
        when "output"
            text = ev["text"] || ""
            stream_name = ev["stream"]

            # Print raw student output, but visually separate stderr.
            if stream_name == "stderr"
                $stdout.print(ANSI.wrap(ANSI::YELLOW, text))
            else
                $stdout.print(text)
            end
            $stdout.flush

        when "test"
            idx = (ev["index"] || 0).to_i
            total = (ev["total"] || total).to_i if ev["total"]

            t = ev["test"] || {}
            status = t["status"] || t[:status]
            call = t["call"] || t[:call]
            msg  = t["message"] || t[:message]
            loc  = t["location"] || t[:location]
            expected = t["expected"] || t[:expected]
            actual   = t["actual"] || t[:actual]

            started ||= true
            total ||= (ev["total"] || 0)

            label = "[#{idx + 1}/#{total}]"
            pass_label = ANSI.wrap(ANSI::BOLD, ANSI::WHITE, ANSI::BG_GREEN, " ✓ PASS  ")
            fail_label = ANSI.wrap(ANSI::BOLD, ANSI::WHITE, ANSI::BG_RED,   " ✗ FAIL  ")

            # puts unless first_task

            if status == "pass"
                passed += 1
                shown_actual = (t.key?("actual") || t.key?(:actual)) ? fmt_value(actual) : nil
                suffix = shown_actual ? " #{ANSI.wrap(ANSI::GRAY, "=>")} #{shown_actual} #{ANSI.wrap(ANSI::BOLD, ANSI::GREEN, '✓')}" : ""
                puts "#{pass_label} #{ANSI.wrap(ANSI::GRAY, label)} #{call}#{suffix}"
                # puts "#{ANSI.wrap(ANSI::GREEN, "✓ PASS")} #{ANSI.wrap(ANSI::GRAY, label)} #{call}#{suffix}"
            else
                failed += 1
                puts "#{fail_label} #{ANSI.wrap(ANSI::GRAY, label)} #{call}"
                # puts "#{ANSI.wrap(ANSI::RED, "✗ FAIL")} #{ANSI.wrap(ANSI::GRAY, label)} #{call}"

                # Show expectation mismatch, if present.
                if (t.key?("expected") || t.key?(:expected) || t.key?("actual") || t.key?(:actual))
                    exp2 = unwrap_expected(expected)
                    puts "  #{ANSI.wrap(ANSI::GRAY, "expected:")} #{fmt_value(exp2)}"
                    puts "  #{ANSI.wrap(ANSI::GRAY, "got:     ")} #{fmt_value(actual)} #{ANSI.wrap(ANSI::BOLD, ANSI::RED, "✗")}"
                end

                # If we already printed expected/got, suppress the common redundant "Expected X, got Y" message.
                if msg && !msg.empty?
                    has_exp_got = (t.key?("expected") || t.key?(:expected) || t.key?("actual") || t.key?(:actual))
                    redundant = false

                    if has_exp_got
                        # Very small heuristic: most of these messages start with "Expected "
                        # or contain ", got " / "got " patterns.
                        m = msg.strip
                        redundant = (m.start_with?("Expected ") || m.include?(", got ") || m.include?(" got "))
                    end

                    puts "  #{msg}" unless redundant
                end
                loc_s = fmt_loc(loc)
                puts ANSI.wrap(ANSI::GRAY, "  at #{loc_s}") if loc_s
                # puts

                failures << t
            end
            STDOUT.flush
            first_task = false

        when "error"
            fatal_error = ev["error"]
            break
        end
    end

    out = res.to_h

    if fatal_error || out[:status] == "error"
        e = fatal_error || out[:error] || {}
        print_error_block(
            title: "Execution error",
            type: (e["type"] || e[:type]),
            message: (e["message"] || e[:message]),
            location: (e["location"] || e[:location])
        )
        exit 1
    end

    # Final summary
    total ||= out[:total]
    if out[:status] == "pass"
        puts ANSI.wrap(ANSI::GREEN, "\n#{ANSI::BOLD}All tests passed (#{passed}/#{total}).#{ANSI::RESET}")
        exit 0
    elsif failed == total
        puts ANSI.wrap(ANSI::RED, "\n#{ANSI::BOLD}All tests failed (#{failed}/#{total}).#{ANSI::RESET}")
        exit 1
    else
        puts ANSI.wrap(ANSI::RED, "\n#{ANSI::BOLD}Some tests failed (#{failed}/#{total}).#{ANSI::RESET}")
        exit 1
    end
else
    res = task.run(submission_code: submission_code)
    out = res.to_h.merge(task_id: task_id, language: language)
    puts JSON.generate(out)
    exit(out[:status] == "pass" ? 0 : 1)
end