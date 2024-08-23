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

if File.exist?(target_path)
    print "File already exists! Replace it? (y/n) "
    answer = gets.strip
    if answer != 'y'
        exit
    end
end

system("mogrify -fuzz 50% -trim +repage \"#{latest_path}\"")
system("cwebp -lossless \"#{latest_path}\" -o \"#{target_path}\"")
system("rm \"#{latest_path}\"")