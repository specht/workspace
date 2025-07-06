#!/usr/bin/env ruby

require 'fileutils'
require 'set'

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

kit = (ARGV.first || '').to_sym
if !KITS.has_key?(kit)
    puts "Usage: download.rb <kit name>"
    puts "Available kits: #{KITS.keys.join(', ')}"
    puts "Example: download.rb industrial"
    exit 1
end

system("mkdir -p .dl")
system("rm -rf _temp; rm -rf #{kit}; mkdir -p _temp; mkdir -p _temp/unpacked; mkdir -p #{kit}/textures")
STDERR.puts "Fetching #{kit}..."
system("wget -q -N --no-if-modified-since -P .dl #{KITS[kit]}")
STDERR.puts "Unpacking #{kit}..."
system("unzip -q -o .dl/#{File.basename(KITS[kit])} -d _temp/unpacked")

FileUtils.cp("_temp/unpacked/License.txt", "#{kit}/")

Dir["_temp/unpacked/Models/OBJ format/*.obj"].each do |path|
    s = File.read(path)
    mtl_set = Set.new()
    s.scan(/^usemtl\s+(\S+)/).each do |match|
        mtl_set.add(match.first)
    end
    if mtl_set.size < 2
        FileUtils.cp(path, "#{kit}/#{File.basename(path)}")
        mtl_set.each do |mtl|
            begin
                FileUtils.cp("_temp/unpacked/Models/OBJ format/Textures/#{mtl}.png", "#{kit}/textures/#{mtl}.png")
                File.link("#{kit}/textures/#{mtl}.png", "#{kit}/textures/#{File.basename(path).sub('.obj', '.png')}")
            rescue
            end
        end
    end
end
FileUtils.mkpath("previews")
FileUtils.cp("_temp/unpacked/Preview.png", "previews/#{kit}.png")
begin
    FileUtils.cp("_temp/unpacked/Sample.png", "previews/#{kit}-sample.png")
rescue
end


system("rm -rf _temp") unless ARGV.include?('--keep-temp')
