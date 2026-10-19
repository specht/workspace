#!/usr/bin/env ruby
# How the server copes when a whole class starts at once: starts N test
# Workspaces at the same time (the same image and limits as for students,
# each with an empty folder of its own), measures how long each takes until
# it answers, and samples load and memory meanwhile. Afterwards everything
# is stopped and removed. Run it on the server outside lessons, inside the
# Ruby container (it goes through the Docker guard like the backend):
#
#   ./config.rb exec ruby ruby /src/scripts/measure-capacity.rb        # 30
#   ./config.rb exec ruby ruby /src/scripts/measure-capacity.rb 10
#
# The report is printed and kept in /internal/capacity-<time>.txt.
require 'json'
require 'open3'
require 'fileutils'
require 'net/http'
require 'shellwords'
require_relative '../ruby/credentials'
require_relative '../ruby/include/workspace_runtime'

COUNT = (ARGV[0] || 30).to_i
IMAGE = 'hs_code_server'
NETWORK = 'workspace_user'
READY_TIMEOUT = 300
PREFIX = "hs_code_loadtest_#{Time.now.strftime('%H%M%S')}"
HOST_USER = File.join(PATH_TO_HOST_DATA, 'user')
# As workspace_runtime.rb starts a Workspace.
HARDENING = WorkspaceRuntime::DirectDocker::HARDENING.split

def sh(*args)
    out, status = Open3.capture2e(*args)
    [out, status.success?]
end

def load_and_memory
    load = File.read('/proc/loadavg').split.first.to_f
    info = File.read('/proc/meminfo').lines.to_h { |l| k, v = l.split(':'); [k, v.to_i] }
    { :load => load, :mem_used_gb => ((info['MemTotal'] - info['MemAvailable']) / 1024.0 / 1024.0).round(1),
      :mem_total_gb => (info['MemTotal'] / 1024.0 / 1024.0).round(1) }
end

def ip_of(name)
    out, ok = sh('docker', 'inspect', '-f', "{{(index .NetworkSettings.Networks \"#{NETWORK}\").IPAddress}}", name)
    ok ? out.strip : nil
end

def ready?(ip)
    Net::HTTP.start(ip, 8443, :open_timeout => 2, :read_timeout => 2) { |http| http.get('/healthz').code.to_i < 500 }
rescue StandardError
    false
end

puts "Starting #{COUNT} test Workspaces at once (#{PREFIX}_*) …"
cpus = Integer(`nproc`.strip) rescue nil
before = load_and_memory
samples = []
sampler = Thread.new do
    loop do
        samples << load_and_memory.merge(:t => Time.now)
        sleep 5
    end
end

started_at = Time.now
results = Array.new(COUNT)
threads = (0...COUNT).map do |i|
    Thread.new do
        name = "#{PREFIX}_#{i}"
        dir = File.join('/user', "#{PREFIX}_#{i}")
        FileUtils.mkdir_p(%w[config workspace].map { |d| File.join(dir, d) })
        FileUtils.chown_R(1000, 1000, dir)
        t0 = Time.now
        _, ok = sh('docker', 'run', '--cpus=4', '--memory=4g', '--memory-swap=4g', '--pids-limit=256', *HARDENING, '-d', '--rm',
            '--hostname', 'workspace', '-e', 'PUID=1000', '-e', 'PGID=1000', '-e', 'TZ=Europe/Berlin',
            '-v', "#{HOST_USER}/#{PREFIX}_#{i}/config:/config", '-v', "#{HOST_USER}/#{PREFIX}_#{i}/workspace:/workspace",
            '--network', NETWORK, '--name', name, IMAGE)
        unless ok
            results[i] = { :name => name, :error => 'docker run failed' }
            next
        end
        started = Time.now - t0
        ip = nil
        until (ip = ip_of(name)) && !ip.empty? && ready?(ip)
            break if Time.now - t0 > READY_TIMEOUT

            sleep 1
        end
        results[i] = ready?(ip.to_s) ? { :name => name, :run => started, :ready => Time.now - t0 } : { :name => name, :error => 'not ready in time' }
    end
end
threads.each(&:join)
all_ready = Time.now - started_at
sampler.kill
peak = samples.max_by { |s| s[:load] } || before

stats, = sh('docker', 'stats', '--no-stream', '--format', '{{.Name}} {{.CPUPerc}} {{.MemUsage}}')
ours = stats.lines.select { |l| l.start_with?(PREFIX) }

ok = results.compact.reject { |r| r[:error] }
times = ok.map { |r| r[:ready] }.sort
median = times.empty? ? nil : times[times.size / 2]
report = []
report << "Capacity test #{Time.now.strftime('%Y-%m-%d %H:%M')}: #{COUNT} Workspaces started at once"
report << "CPUs: #{cpus || '?'}; memory: #{before[:mem_total_gb]} GB; load before: #{before[:load]}, memory used before: #{before[:mem_used_gb]} GB"
report << "Ready: #{ok.size} of #{COUNT}#{results.compact.size < COUNT ? '' : ''}"
report << format('Time until ready: median %.1f s, slowest %.1f s; all done after %.1f s', median.to_f, times.last.to_f, all_ready) unless times.empty?
report << "Peak load: #{peak[:load]} (#{cpus ? (peak[:load] / cpus).round(2) : '?'} per CPU); memory used at peak: #{peak[:mem_used_gb]} GB"
results.compact.select { |r| r[:error] }.each { |r| report << "Failed: #{r[:name]}: #{r[:error]}" }
report << ''
report << 'Per Workspace after the start (CPU, memory):'
report.concat(ours.map { |l| "  #{l.strip}" })
report << ''
report << 'Samples every 5 s (load, memory used GB):'
report.concat(samples.map { |s| "  #{(s[:t] - started_at).round}s #{s[:load]} #{s[:mem_used_gb]}" })

puts report
File.write("/internal/capacity-#{Time.now.strftime('%Y%m%d-%H%M%S')}.txt", report.join("\n") + "\n")

puts "Cleaning up …"
(0...COUNT).each { |i| sh('docker', 'kill', "#{PREFIX}_#{i}") }
sleep 3
(0...COUNT).each { |i| FileUtils.rm_rf(File.join('/user', "#{PREFIX}_#{i}")) }
puts 'Done.'
