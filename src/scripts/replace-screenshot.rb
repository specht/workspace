#!/usr/bin/env ruby

require 'digest'

template_path = File.expand_path(ARGV.first)
basename = File.basename(template_path)

target_sha1 = nil
if ARGV.include?('--sha1')
    target_sha1 = ARGV[ARGV.index('--sha1') + 1].strip
    STDERR.puts "Target SHA1: #{target_sha1}"
end

find_path = File.expand_path(File.join(File.dirname(template_path), '..', '*', basename))
STDERR.puts "Finding all files with path: #{find_path}"

Dir[find_path].each do |path|
    sha1sum = Digest::SHA1.hexdigest(File.read(path))
    if target_sha1 && sha1sum[0, 8] != target_sha1
        next
    end
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
