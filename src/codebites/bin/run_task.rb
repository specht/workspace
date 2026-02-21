#!/usr/bin/env ruby
# bin/run_task
require "json"
require "yaml"

ROOT = File.expand_path("..", __dir__)

# ---- args ----
args = ARGV.dup
stream = args.delete("--stream")

stream_format_arg = args.find { |a| a.start_with?("--stream-format=") }
summary_arg       = args.find { |a| a.start_with?("--summary=") }

args.delete(stream_format_arg) if stream_format_arg
args.delete(summary_arg) if summary_arg

stream_format = (stream_format_arg&.split("=", 2)&.last || "compact") # compact|failures|full
summary_mode  = (summary_arg&.split("=", 2)&.last || "failures")      # compact|full|failures

task_id  = args[0]
language = args[1]

if task_id.nil? || language.nil?
    warn "Usage: bin/run_task <task_id> <language> [--stream] [--stream-format=compact|failures|full] [--summary=compact|full|failures] < submission_file"
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
    # Fail-fast, truly-live stream:
    # - Forward output events immediately (stdout/stderr from student code)
    # - Emit test result events as each case finishes
    # - Stop after the first failing test

    res = task.run_stream(submission_code: submission_code) do |ev|
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

    # No summary in stream mode.
    out = res.to_h
    exit(out[:status] == "pass" ? 0 : 1)
else
    res = task.run(submission_code: submission_code)
    out = res.to_h.merge(task_id: task_id, language: language)
    puts JSON.generate(out)
    exit(out[:status] == "pass" ? 0 : 1)
end