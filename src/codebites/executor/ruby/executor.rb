#!/usr/bin/env ruby
require "json"

# Interactive NDJSON protocol executor.
#
# Reads commands from STDIN (one JSON object per line):
#   {"cmd":"init","mode":"function","entry":"two_sum","files":{"submission":"..."}}
#   {"cmd":"case","index":0,"args":[...]} 
#
# Emits events on STDOUT (one JSON object per line):
#   {"event":"ready"}
#   {"event":"output","stream":"stdout|stderr","text":"...","index":0}
#   {"event":"case","index":0,"ok":true,"result":...,"error":null}
#   {"event":"fatal","error":{...}}

SUB_FILE = "submission.rb"
SUB_BASENAME = SUB_FILE

# IMPORTANT: keep stable handles to the *real* stdout/stderr for protocol output.
# We'll temporarily redirect STDOUT/STDERR to capture student output.
REAL_STDOUT = IO.new(1).dup
REAL_STDERR = IO.new(2).dup

def emit(obj)
  REAL_STDOUT.puts(JSON.generate(obj))
  REAL_STDOUT.flush
end

def extract_location(backtrace)
  return nil unless backtrace && !backtrace.empty?
  line = backtrace.find { |l| l.include?(SUB_BASENAME) } || backtrace.first
  if line =~ /(.+?):(\d+)/
    { "file" => Regexp.last_match(1).split("/").last, "line" => Regexp.last_match(2).to_i }
  end
end

def safe_json_value(x)
  JSON.generate({ "v" => x })
  x
rescue StandardError
  { "__inspect__" => x.inspect }
end

def start_stream_reader(io, stream_name, index)
  Thread.new do
    begin
      loop do
        chunk = io.readpartial(4096)
        break if chunk.nil? || chunk.empty?
        emit({ event: "output", stream: stream_name, text: chunk, index: index })
      end
    rescue EOFError
      # normal
    rescue StandardError => e
      REAL_STDERR.puts("[executor] output reader error: #{e.class}: #{e.message}")
    ensure
      begin
        io.close
      rescue StandardError
        nil
      end
    end
  end
end

def with_live_stdio(index)
  out_r, out_w = IO.pipe
  err_r, err_w = IO.pipe

  # Make sure writes flush immediately for a "truly live" experience.
  out_w.sync = true
  err_w.sync = true

  # Redirect both globals and constants. Many programs write to STDERR directly.
  old_stdout = STDOUT.dup
  old_stderr = STDERR.dup

  STDOUT.reopen(out_w)
  STDERR.reopen(err_w)

  $stdout = STDOUT
  $stderr = STDERR

  t_out = start_stream_reader(out_r, "stdout", index)
  t_err = start_stream_reader(err_r, "stderr", index)

  yield
ensure
  # Restore stdio for subsequent protocol output.
  $stdout = REAL_STDOUT
  $stderr = REAL_STDERR

  begin
    STDOUT.reopen(old_stdout)
  rescue StandardError
    nil
  end
  begin
    STDERR.reopen(old_stderr)
  rescue StandardError
    nil
  end

  begin
    old_stdout.close
  rescue StandardError
    nil
  end
  begin
    old_stderr.close
  rescue StandardError
    nil
  end

  begin
    out_w.close
  rescue StandardError
    nil
  end
  begin
    err_w.close
  rescue StandardError
    nil
  end

  begin
    t_out.join
  rescue StandardError
    nil
  end
  begin
    t_err.join
  rescue StandardError
    nil
  end
end

function_name = nil
receiver = nil
initialized = false

STDIN.each_line do |line|
  line = line.strip
  next if line.empty?

  msg = JSON.parse(line)
  cmd = msg["cmd"]

  case cmd
  when "init"
    begin
      function_name = msg["entry"]
      submission_code = msg.dig("files", "submission") || ""

      File.write("/workspace/#{SUB_FILE}", submission_code)
      load "/workspace/#{SUB_FILE}", false

      unless function_name && (Object.private_instance_methods.include?(function_name.to_sym) || Object.instance_methods.include?(function_name.to_sym))
        err = {
          "type" => "MissingFunction",
          "message" => "Function #{function_name} is not defined in #{SUB_FILE}",
          "location" => { "file" => SUB_FILE, "line" => 1 }
        }
        emit({ event: "fatal", error: err })
        next
      end

      receiver = Object.new
      initialized = true
      emit({ event: "ready" })
    rescue SyntaxError, StandardError => e
      err = {
        "type" => e.class.to_s,
        "message" => e.message,
        "location" => extract_location(e.backtrace)
      }
      emit({ event: "fatal", error: err })
    end

  when "case"
    unless initialized
      emit({ event: "fatal", error: { "type" => "NotInitialized", "message" => "Executor not initialized", "location" => nil } })
      next
    end

    idx = msg["index"]
    args = msg["args"] || []

    begin
      result = nil
      with_live_stdio(idx) do
        result = receiver.send(function_name, *args)
      end

      emit({ event: "case", index: idx, ok: true, result: safe_json_value(result), error: nil })
    rescue StandardError => e
      err = {
        "type" => e.class.to_s,
        "message" => e.message,
        "location" => extract_location(e.backtrace)
      }
      emit({ event: "case", index: idx, ok: false, result: nil, error: err })
    end
  when "stop"
    break
  else
    # Ignore unknown commands
  end
end
