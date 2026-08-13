#!/usr/bin/env ruby

hostname = `hostname`.strip

paths = case hostname
when 'vaka'
    [
        "/home/michael/Screenshots/",
        "/share/"
    ]
when 'vakadell'
    [
        "/home/michael/Pictures/Screenshots/"
    ]
else
    abort "Unknown hostname: #{hostname}"
end

latest_path = paths
    .flat_map { |path| Dir.glob(File.join(path, '*')) }
    .select { |f| File.file?(f) }
    .max_by { |f| File.mtime(f) }

abort "No screenshot found in: #{paths.join(', ')}" unless latest_path

fuzz = 50

if ARGV.size > 0
    fuzz = ARGV.first.to_i
end

STDERR.puts latest_path

print "Please enter image title: "
title = $stdin.gets.strip
nocrop = false
if title[0] == '*'
    title = title[1..-1]
    nocrop = true
end

slug = title.downcase.gsub(/[^a-z0-9]+/, '-').chomp('-')

latest_content_path = File.dirname(Dir["src/content/*/*.md"].sort_by { |f| File.mtime(f) }.last)

target_path = "#{File.join(latest_content_path, slug)}.webp"

if File.exist?(target_path)
    print "File already exists! Replace it? (y/n) "
    answer = $stdin.gets.strip
    if answer != 'y'
        exit
    end
end

if fuzz > 0
    system("mogrify -fuzz #{fuzz}% -trim +repage \"#{latest_path}\"") unless nocrop
end
system("cwebp -lossless \"#{latest_path}\" -o \"#{target_path}\"")
system("rm \"#{latest_path}\"")