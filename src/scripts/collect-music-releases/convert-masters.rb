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

def handle_xml(xml)
    id = xml.match(/<master id="(\d+)">/)[1]
    artist_id = xml.match(/<artist><id>(\d+)<\/id>/)[1]
    return unless $wanted_artists.include?(artist_id)
    path = "cache/masters/#{id}.xml"
    FileUtils.mkpath(File.dirname(path))
    File.open(path, 'w') { |f| f.write(xml) }
end

Open3.popen2("pigz -cd masters.xml.gz") do |stdin, stdout, wait_thr|
    xml = ''
    stdout.readline
    stdout.each_line do |line|
        xml += line
        if line[line.size - 10, 9] == '</master>'
            handle_xml(xml)
            xml = ''
        end
    end
end
