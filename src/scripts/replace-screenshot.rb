#!/usr/bin/env ruby

require 'digest'

template_path = File.expand_path(ARGV.first)
basename = File.basename(template_path)

find_path = File.expand_path(File.join(File.dirname(template_path), '..', '*', basename))
STDERR.puts "Finding all files with path: #{find_path}"

Dir[find_path].each do |path|
    sha1sum = Digest::SHA1.hexdigest(File.read(path))
    is_source = path == template_path
    STDERR.puts "#{is_source ? ' ' : '*'} #{sha1sum[0, 8]} #{path}"
    if ARGV.include?('--srsly')
        unless is_source
            File.open(path, 'w') do |file|
                file.write(File.read(template_path))
            end
        end
    end
end

unless ARGV.include?('--srsly')
    STDERR.puts "Use --srsly to actually replace the files"
end
