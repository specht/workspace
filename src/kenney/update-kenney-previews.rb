#!/usr/bin/env ruby

_kits = {}
File.open('kits.txt', 'r') do |f|
    f.each_line do |line|
        line.strip!
        next if line.empty? || line.start_with?('#')
        parts = line.split(' ')
        url = parts.pop
        name = parts.join(' ')
        tag = name.downcase.gsub(/\s+/, '-').gsub(/[\(\)]/, '')
        _kits[tag.to_sym] = url
    end
end

KITS = _kits

command = "python3 -m http.server 9247 2> /dev/null"
pid = Process.spawn(command)
begin
    sleep 2

    puts "Server started on http://localhost:9247"

    kits = ARGV
    if ARGV.include?('--all')
        kits = KITS.keys
    end

    kits.each do |kit|
        system("./download.rb #{kit}")
        system("node screenshot.mjs #{kit}")
        system("rm -rf .dl #{kit}")
    end

ensure
    # Stop the server
    Process.kill("TERM", pid)
    Process.wait(pid)
    puts "Server stopped"
end