#!/usr/bin/env ruby
require "json"

SUB_FILE = "submission.rb"

def extract_location(backtrace)
  return nil unless backtrace && !backtrace.empty?
  line = backtrace.find { |l| l.include?(SUB_FILE) } || backtrace.first
  if line =~ /(.+?):(\d+)/
    { "file" => Regexp.last_match(1).split("/").last, "line" => Regexp.last_match(2).to_i }
  end
end

request = JSON.parse(STDIN.read)

submission_code = request.dig("files", "submission") || ""
function_name   = request.dig("entry", "name")
cases           = request["cases"] || []

response = { "runs" => [], "error" => nil }

begin
  # Write into writable tmpfs (/workspace)
  File.write("/workspace/#{SUB_FILE}", submission_code)
  load "/workspace/#{SUB_FILE}", false
rescue SyntaxError, StandardError => e
  response["error"] = {
    "type" => e.class.to_s,
    "message" => e.message,
    "location" => extract_location(e.backtrace)
  }
  puts JSON.generate(response)
  exit 0
end

# Ensure function exists (defined at top-level => private method on Object)
unless Object.private_instance_methods.include?(function_name.to_sym) || Object.instance_methods.include?(function_name.to_sym)
  response["error"] = {
    "type" => "MissingFunction",
    "message" => "Function #{function_name} is not defined in #{SUB_FILE}",
    "location" => { "file" => SUB_FILE, "line" => 1 }
  }
  puts JSON.generate(response)
  exit 0
end

receiver = Object.new

cases.each do |c|
  args = c["args"] || []
  begin
    result = receiver.send(function_name, *args)
    response["runs"] << { "ok" => true, "result" => result }
  rescue StandardError => e
    response["runs"] << {
      "ok" => false,
      "error" => {
        "type" => e.class.to_s,
        "message" => e.message,
        "location" => extract_location(e.backtrace)
      }
    }
  end
end

puts JSON.generate(response)