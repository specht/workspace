#!/usr/bin/env ruby
require "json"

request = JSON.parse(STDIN.read)

submission_code = request.dig("files", "submission")
function_name   = request.dig("entry", "name")
cases           = request["cases"] || []

response = { "runs" => [], "error" => nil }

begin
  File.write("submission.rb", submission_code)
  load "./submission.rb", false
rescue SyntaxError, StandardError => e
  response["error"] = {
    "type" => e.class.to_s,
    "message" => e.message,
    "location" => extract_location(e.backtrace)
  }
  puts JSON.generate(response)
  exit 0
end

def extract_location(backtrace)
  return nil unless backtrace && !backtrace.empty?
  line = backtrace.find { |l| l.include?("submission.rb") } || backtrace.first
  if line =~ /(.+?):(\d+)/
    { "file" => Regexp.last_match(1), "line" => Regexp.last_match(2).to_i }
  end
end

cases.each do |c|
  args = c["args"]
  begin
    result = Object.new.send(function_name, *args)
    response["runs"] << { "ok" => true, "result" => result }
  rescue => e
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