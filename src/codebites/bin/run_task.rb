#!/usr/bin/env ruby
# bin/run_task
require "json"
require "yaml"

ROOT = File.expand_path("..", __dir__)

# ---- args ----
task_id  = ARGV[0]
language = ARGV[1]

if task_id.nil? || language.nil?
  warn "Usage: bin/run_task <task_id> <language> < submission_file"
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

# The task file must define: build_task(executor:) -> Judge task instance
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

# ---- run ----
res = task.run(submission_code: submission_code)

# Include metadata so UI/backend can show what ran
out = res.to_h.merge(task_id: task_id, language: language)

puts JSON.generate(out)
exit(out[:status] == "pass" ? 0 : 1)