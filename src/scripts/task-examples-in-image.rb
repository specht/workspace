#!/usr/bin/env ruby
# The task examples (task-examples.yaml) in the Workspace image, which has
# the versions the checks need: Java 25, Node 24, Dart. On the host they are
# skipped when its java or node is older (TaskExamples::JAVA_MAJOR,
# NODE_MAJOR). Run from the repository, in a memory scope:
#
#   systemd-run --user --scope -q -p MemoryMax=6G \
#     docker run --rm -v "$PWD":/repo:ro -e LANG=C.UTF-8 --entrypoint ruby \
#     hs_code_server:latest /repo/src/scripts/task-examples-in-image.rb kata-lists laufzeit
#
# Without names it takes every activity with a tasks.yaml (long: an hour and
# more). Prints a line per activity and every example that does not get its
# verdict; exits with 1 if one does not.
require_relative '../ruby/include/task_examples'

$stdout.sync = true
content = File.expand_path('../content', __dir__)
names = ARGV.empty? ? Dir[File.join(content, '*', 'tasks.yaml')].map { |f| File.basename(File.dirname(f)) }.sort : ARGV
bad = 0
names.each do |name|
    dir = File.join(content, name)
    outcomes, problems = TaskExamples.run(dir)
    problems.each { |p| puts "PROBLEM #{name}: #{p}"; bad += 1 }
    outcomes.reject(&:ok).each { |o| puts "FAIL #{o.label}: #{o.detail.to_s[0, 300]}"; bad += 1 }
    missing = TaskExamples.missing(dir)
    missing.each { |m| puts "MISSING #{m}"; bad += 1 }
    puts "#{name}: #{outcomes.size} examples, #{outcomes.count { |o| !o.ok }} not as expected"
end
exit(bad.zero? ? 0 : 1)
