#!/usr/bin/env ruby

hostname = `hostname`.strip

path = case hostname
when 'vaka'
    "/home/michael/Screenshots/"
when 'vakadell'
    "/home/michael/Pictures/Screenshots/"
end
latest_path = Dir.glob(File.join(path, '*')).sort_by { |f| File.mtime(f) }.last

STDERR.puts latest_path

print "Please enter image title: "
title = gets.strip

slug = title.downcase.gsub(/[^a-z0-9]+/, '-').chomp('-')

latest_content_path = File.dirname(Dir["src/content/*/*.md"].sort_by { |f| File.mtime(f) }.last)

target_path = "#{File.join(latest_content_path, slug)}.webp"

# raise 'file already exists!' if File.exist?(target_path)

system("cwebp -q 80 \"#{latest_path}\" -o \"#{target_path}\"")

system("rm \"#{latest_path}\"")