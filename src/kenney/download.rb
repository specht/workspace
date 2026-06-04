#!/usr/bin/env ruby

require 'fileutils'
require 'set'
require 'uri'

def run!(*cmd)
    ok = system(*cmd)
    abort "Command failed: #{cmd.join(' ')}" unless ok
end

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

unless KITS.key?(kit)
    puts "Usage: download.rb <kit name>"
    puts "Available kits: #{KITS.keys.join(', ')}"
    puts "Example: download.rb #{KITS.keys.first}"
    exit 1
end

url = KITS[kit]
zip_name = File.basename(URI.parse(url).path)
zip_path = File.join('.dl', zip_name)

FileUtils.mkdir_p('.dl')

STDERR.puts "Fetching #{kit}..."

run!(
    'wget',
    '-q',
    '-N',
    '--no-if-modified-since',
    '-P',
    '.dl',
    url
)

abort "Download did not produce expected file: #{zip_path}" unless File.exist?(zip_path)

# Only touch output directories after the download succeeded.
FileUtils.rm_rf('_temp')
FileUtils.rm_rf(kit.to_s)

FileUtils.mkdir_p('_temp/unpacked')
FileUtils.mkdir_p("#{kit}/textures")

STDERR.puts "Unpacking #{kit}..."

run!(
    'unzip',
    '-q',
    '-o',
    zip_path,
    '-d',
    '_temp/unpacked'
)

license_path = '_temp/unpacked/License.txt'
abort "Missing License.txt in downloaded archive" unless File.exist?(license_path)

FileUtils.cp(license_path, "#{kit}/")

Dir['_temp/unpacked/Models/OBJ format/*.obj'].each do |path|
    s = File.read(path)

    mtl_set = Set.new

    s.scan(/^usemtl\s+(\S+)/).each do |match|
        mtl_set.add(match.first)
    end

    next unless mtl_set.size < 2

    FileUtils.cp(path, "#{kit}/#{File.basename(path)}")

    mtl_set.each do |mtl|
        texture_source = "_temp/unpacked/Models/OBJ format/Textures/#{mtl}.png"
        texture_target = "#{kit}/textures/#{mtl}.png"
        obj_texture_name = File.basename(path).sub(/\.obj\z/, '.png')
        obj_texture_target = "#{kit}/textures/#{obj_texture_name}"

        next unless File.exist?(texture_source)

        FileUtils.cp(texture_source, texture_target)

        begin
            File.link(texture_target, obj_texture_target)
        rescue SystemCallError
            FileUtils.cp(texture_target, obj_texture_target)
        end
    end
end

FileUtils.rm_rf('_temp') unless ARGV.include?('--keep-temp')