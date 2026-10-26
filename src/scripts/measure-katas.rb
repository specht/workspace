#!/usr/bin/env ruby
# How many Kata runs the server should run at a time (KATA_PARALLEL_RUNS in
# credentials.rb, docs/plan-katas.md): a class presses »Prüfen« at once, so
# for each number of parallel runs this sends RUNS runs through the queue
# (KataQueue, KataRunner, the same containers and limits as for students):
# half a check that takes a moment (counting primes), half one that is quick,
# and reports how long the last one waited and how many finish per second.
# More parallel runs help until the CPUs are busy; after that they only make
# each run slower. Run it on the server outside lessons, inside the Ruby
# container (it goes through the Docker guard like the backend):
#
#   ./config.rb exec ruby ruby /src/scripts/measure-katas.rb        # 30 runs
#   ./config.rb exec ruby ruby /src/scripts/measure-katas.rb 60
#
# The report is printed and kept in /internal/katas-<time>.txt.
require 'etc'
require 'fileutils'
require_relative '../ruby/include/kata_runner'
require_relative '../ruby/include/kata_queue'

RUNS = (ARGV[0] || 30).to_i
SLOW = "n = 0\nfor k in range(2, 60000):\n    if all(k % d for d in range(2, int(k ** 0.5) + 1)):\n        n += 1\nprint(n)\n"
QUICK = "print(sum(range(100)))\n"

def measure(parallel)
    queue = KataQueue.new(parallel: parallel)
    started = Time.now
    tickets = RUNS.times.map do |i|
        code = i.even? ? SLOW : QUICK
        queue.submit("measure-#{i}") do
            begun = Time.now
            result = KataRunner.run(files: { 'kata.py' => code }, argv: %w[python3 -I kata.py], timeout: 60)
            { :ok => result.ok?, :waited => begun - started, :ran => Time.now - begun }
        end
    end
    tickets.each { |t| t.wait(600) }
    total = Time.now - started
    results = tickets.map(&:result).compact
    failed = RUNS - results.count { |r| r[:ok] }
    { :parallel => parallel, :total => total, :per_second => RUNS / total, :failed => failed,
      :longest_wait => results.map { |r| r[:waited] }.max.to_f, :slowest_run => results.map { |r| r[:ran] }.max.to_f }
end

cpus = Etc.nprocessors
lines = ["Kata runs: #{RUNS} at once (half counting primes, half quick), #{cpus} CPUs, #{Time.now}"]
lines << format('%-9s %-9s %-11s %-13s %-12s %s', 'parallel', 'total s', 'runs per s', 'longest wait', 'slowest run', 'failed')
best = nil
[1, 2, 4, 8, 12, 16].select { |p| p <= [cpus * 2, 16].min }.each do |parallel|
    row = measure(parallel)
    best = row if best.nil? || row[:per_second] > best[:per_second] * 1.1
    lines << format('%-9d %-9.1f %-11.2f %-13.1f %-12.1f %d', parallel, row[:total], row[:per_second],
                    row[:longest_wait], row[:slowest_run], row[:failed])
    puts lines.last
end
lines << ''
lines << "Suggestion: KATA_PARALLEL_RUNS = #{best[:parallel]} (more gained less than a tenth)."
puts lines.last
dir = File.directory?('/internal') ? '/internal' : Dir.pwd
path = File.join(dir, "katas-#{Time.now.strftime('%Y%m%d-%H%M%S')}.txt")
File.write(path, lines.join("\n") + "\n")
puts "Report: #{path}"
