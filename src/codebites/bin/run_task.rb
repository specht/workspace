#!/usr/bin/env ruby
# bin/run_task
require "json"
require "base64"
require "securerandom"

ROOT = File.expand_path("..", __dir__)

# ---- args ----
task_id  = ARGV[0]
language = ARGV[1]

if task_id.nil? || language.nil?
  warn "Usage: bin/run_task <task_id> <language> < submission_file"
  exit 2
end

canonical_lang =
  case language
  when "rb" then "ruby"
  when "py" then "python"
  when "js", "node" then "javascript"
  else language
  end

$CODEBITES_LANG = canonical_lang
image = "hackschule-exec-#{canonical_lang}"

# ---- read submission ----
submission_code = STDIN.read

def language_ext(language)
  case language
  when "ruby", "rb" then "rb"
  when "python", "py" then "py"
  when "javascript", "js", "node" then "js"
  else language
  end
end

def wrap_with_patch(language:, patch_code:, submission_code:)
  return submission_code if patch_code.nil? || patch_code.strip.empty?

  if ["ruby", "rb"].include?(language)
    patch_tag = "__CODEBITES_PATCH_#{SecureRandom.hex(8)}__"
    sub_tag   = "__CODEBITES_SUB_#{SecureRandom.hex(8)}__"

    return <<~RUBY
      # --- injected by runner: task patch + submission wrapper ---
      __cb_patch__ = <<'#{patch_tag}'
      #{patch_code}
      #{patch_tag}
      eval(__cb_patch__, TOPLEVEL_BINDING, "patch.rb", 1)

      __cb_submission__ = <<'#{sub_tag}'
      #{submission_code}
      #{sub_tag}
      eval(__cb_submission__, TOPLEVEL_BINDING, "submission.rb", 1)
    RUBY
  end

  if ["python", "py"].include?(language)
    patch_b64 = Base64.strict_encode64(patch_code)
    sub_b64   = Base64.strict_encode64(submission_code)

    return <<~PY
      # --- injected by runner: task patch + submission wrapper ---
      import base64

      __cb_patch__ = base64.b64decode("#{patch_b64}").decode("utf-8")
      exec(compile(__cb_patch__, "patch.py", "exec"), globals(), globals())

      __cb_submission__ = base64.b64decode("#{sub_b64}").decode("utf-8")
      exec(compile(__cb_submission__, "submission.py", "exec"), globals(), globals())
    PY
  end

  if ["javascript", "js", "node"].include?(language)
    patch_b64 = Base64.strict_encode64(patch_code)
    sub_b64   = Base64.strict_encode64(submission_code)

    return <<~JS
      // --- injected by runner: task patch + submission wrapper ---
      const __cb_b64__ = (s) => Buffer.from(s, "base64").toString("utf-8");

      function __cb_escape_re(s) {
        return s.replace(/[.*+?^${}()|[\\]\\\\]/g, "\\\\$&");
      }

      function __cb_code_frame(src, line, col) {
        const lines = String(src).split(/\\r?\\n/);
        const i = Math.max(0, Math.min(lines.length - 1, (line || 1) - 1));
        const start = Math.max(0, i - 2);
        const end = Math.min(lines.length, i + 3);
        const width = String(end).length;

        let out = "";
        for (let n = start; n < end; n++) {
          const ln = String(n + 1).padStart(width, " ");
          out += `${ln} | ${lines[n]}\\n`;
          if (n === i) {
            const caretPos = Math.max(0, (col || 1) - 1);
            out += `${" ".repeat(width)} | ${" ".repeat(caretPos)}^\\n`;
          }
        }
        return out;
      }

      function __cb_run(code, filename) {
        // Function() gives better filename/loc than eval() in many sandboxes,
        // and doesn't depend on require/vm.
        const wrapped = code + "\\n//# sourceURL=" + filename;

        try {
          (new Function(wrapped))();
        } catch (e) {
          // Improve SyntaxError visibility (line/col + snippet)
          const stack = String((e && e.stack) || "");
          const re = new RegExp(__cb_escape_re(filename) + ":(\\\\d+):(\\\\d+)");
          const m = stack.match(re);
          const line = m ? parseInt(m[1], 10) : null;
          const col  = m ? parseInt(m[2], 10) : null;

          if (e && e.name === "SyntaxError") {
            // Print to stderr; your runner shows stderr in yellow.
            console.error("\\n--- JS SyntaxError in " + filename + (line ? `:${line}:${col}` : "") + " ---");
            if (line) console.error(__cb_code_frame(code, line, col));
          }

          throw e;
        }
      }

      const __cb_patch__ = __cb_b64__("#{patch_b64}");
      __cb_run(__cb_patch__, "patch.js");

      const __cb_submission__ = __cb_b64__("#{sub_b64}");
      __cb_run(__cb_submission__, "submission.js");
    JS
  end

  "#{patch_code}\n\n#{submission_code}"
end

# ---- require judge core ----
require File.join(ROOT, "include", "docker_executor")
require File.join(ROOT, "include", "single_file_task")

executor = Judge::DockerExecutor.new(image: image)

# ---- load task (single-file format only) ----
task_path = File.join(ROOT, "tasks", "#{task_id}.rb")
unless File.exist?(task_path)
  warn "Unknown task: #{task_id}"
  exit 1
end

begin
  spec = CodeBites::SingleFileTask.load(task_path)
rescue => e
  warn "Task load error: #{e.class}: #{e.message}"
  exit 1
end

sections = spec[:sections]
patch_code = sections["patch.#{language_ext(canonical_lang)}"]

# Apply optional patch before submission (keeping line numbers)
# For JS we send patch separately; for others we keep wrapping.
if canonical_lang == "javascript"
  # leave submission_code as-is
else
  submission_code = wrap_with_patch(language: canonical_lang, patch_code: patch_code, submission_code: submission_code)
end

# ---- build task ----
seed = Random.new_seed
rng  = Random.new(seed)

begin
  task = spec[:build_judge].call(executor: executor, rng: rng)
rescue => e
  warn "Task build error: #{e.class}: #{e.message}"
  exit 1
end

# ---- ANSI output ----
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
    s = s[0, MAX_VAL_STR - 1] + "…" if s.length > MAX_VAL_STR
    s
  when FalseClass, TrueClass
    $CODEBITES_LANG == "python" ? (v ? "True" : "False") : (v ? "true" : "false")
  when NilClass
    case $CODEBITES_LANG
    when "javascript" then "null"
    when "python"     then "None"
    else "nil"
    end
  else
    s = v.inspect
    s = s[0, MAX_VAL_STR - 1] + "…" if s.length > MAX_VAL_STR
    s
  end
end

def unwrap_expected(exp)
  return exp unless exp.is_a?(Hash)
  kind = exp["kind"] || exp[:kind]
  if kind == "equals"
    return exp["value"] if exp.key?("value")
    return exp[:value]  if exp.key?(:value)
  end
  exp
end

puts ANSI.wrap(ANSI::CYAN, "#{ANSI::BOLD}Running #{task_id} (#{canonical_lang})#{ANSI::RESET}\n")
STDOUT.flush

passed = 0
failed = 0
total = nil
fatal_error = nil

res = task.run_stream(submission_code: submission_code, patch_code: patch_code, fail_fast: true) do |ev|
  ev = JSON.parse(JSON.generate(ev)) # normalize keys

  case ev["event"]
  when "output"
    text = ev["text"] || ""
    stream_name = ev["stream"]

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
    expected =
        if t.key?("expected") then t["expected"]
        elsif t.key?(:expected) then t[:expected]
        end

    actual =
        if t.key?("actual") then t["actual"]
        elsif t.key?(:actual) then t[:actual]
        end

    total ||= (ev["total"] || 0)

    label = "[#{idx + 1}/#{total}]"
    pass_label = ANSI.wrap(ANSI::BOLD, ANSI::WHITE, ANSI::BG_GREEN, " ✓ PASS  ")
    fail_label = ANSI.wrap(ANSI::BOLD, ANSI::WHITE, ANSI::BG_RED,   " ✗ FAIL  ")

    if status == "pass"
      passed += 1
      shown_actual = (t.key?("actual") || t.key?(:actual)) ? fmt_value(actual) : nil
      suffix = shown_actual ? " #{ANSI.wrap(ANSI::GRAY, "=>")} #{shown_actual} #{ANSI.wrap(ANSI::BOLD, ANSI::GREEN, '✓')}" : ""
      puts "#{pass_label} #{ANSI.wrap(ANSI::GRAY, label)} #{call}#{suffix}"
    else
      failed += 1
      puts "#{fail_label} #{ANSI.wrap(ANSI::GRAY, label)} #{call}"

      if (t.key?("expected") || t.key?(:expected) || t.key?("actual") || t.key?(:actual))
        exp2 = unwrap_expected(expected)
        puts "  #{ANSI.wrap(ANSI::GRAY, "expected:")} #{fmt_value(exp2)}"
        puts "  #{ANSI.wrap(ANSI::GRAY, "got:     ")} #{fmt_value(actual)} #{ANSI.wrap(ANSI::BOLD, ANSI::RED, "✗")}"
      end

      if msg && !msg.empty?
        has_exp_got = (t.key?("expected") || t.key?(:expected) || t.key?("actual") || t.key?(:actual))
        redundant = false
        if has_exp_got
          m = msg.strip
          redundant = (m.start_with?("Expected ") || m.include?(", got ") || m.include?(" got "))
        end
        puts "  #{msg}" unless redundant
      end

      loc_s = fmt_loc(loc)
      puts ANSI.wrap(ANSI::GRAY, "  at #{loc_s}") if loc_s
    end

    STDOUT.flush

  when "error"
    fatal_error = ev["error"]
    break
  end
end

out = res.to_h

if fatal_error || out[:status] == "error"
  e = fatal_error || out[:error] || {}
  type = (e["type"] || e[:type])
  message = (e["message"] || e[:message])
  location = (e["location"] || e[:location])
  loc = fmt_loc(location)

  header = ["Execution error", (type && !type.empty? ? type : nil)].compact.join(": ")
  puts ANSI.wrap(ANSI::RED, "\n#{ANSI::BOLD}#{header}#{ANSI::RESET}")
  puts "  #{message}" if message && !message.empty?
  puts ANSI.wrap(ANSI::GRAY, "  at #{loc}") if loc
#   puts ANSI.wrap(ANSI::GRAY, "  Seed: #{seed}") if seed
  exit 1
end

total ||= out[:total]
if out[:status] == "pass"
  puts ANSI.wrap(ANSI::GREEN, "\n#{ANSI::BOLD}All tests passed (#{passed}/#{total}).#{ANSI::RESET}")
  exit 0
elsif failed == total
  puts ANSI.wrap(ANSI::RED, "\n#{ANSI::BOLD}All tests failed (#{failed}/#{total}).#{ANSI::RESET}")
#   puts ANSI.wrap(ANSI::GRAY, "Seed: #{seed}")
  exit 1
else
  puts ANSI.wrap(ANSI::RED, "\n#{ANSI::BOLD}Some tests failed (#{failed}/#{total}).#{ANSI::RESET}")
#   puts ANSI.wrap(ANSI::GRAY, "Seed: #{seed}")
  exit 1
end
