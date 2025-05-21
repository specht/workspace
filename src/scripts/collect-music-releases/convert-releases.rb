#!/usr/bin/env ruby

require 'nokogiri'
require 'yaml'
require 'fileutils'
require 'open3'
require 'set'

$wanted_artists = Set.new()

Dir['cache/artists/*.xml'].each do |file|
    $wanted_artists << File.basename(file, '.xml')
end

STDERR.puts "Wanted artists: #{$wanted_artists.size}"

$wanted_releases = Set.new()

Dir['cache/masters/*.xml'].each do |path|
    release_id = File.read(path).match(/<main_release>(\d+)<\/main_release>/)[1]
    $wanted_releases << release_id
end

def handle_xml(xml)
    id = xml.match(/<release id="(\d+)"/)[1]
    return unless $wanted_releases.include?(id)
    path = "cache/releases/#{id}.xml"
    FileUtils.mkpath(File.dirname(path))
    File.open(path, 'w') { |f| f.write(xml) }
end

Open3.popen2("pigz -cd releases.xml.gz") do |stdin, stdout, wait_thr|
    xml = ''
    stdout.readline
    stdout.each_line do |line|
        xml += line
        if line[line.size - 11, 10] == '</release>'
            handle_xml(xml)
            xml = ''
        end
    end
end
