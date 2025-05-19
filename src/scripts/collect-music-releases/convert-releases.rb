#!/usr/bin/env ruby

require 'nokogiri'
require 'yaml'
require 'fileutils'
require 'open3'
require 'set'

$wanted_artists = Set.new()
$country_for_artist = {}

File.open('wanted-artists.txt') do |f|
    current_country = nil
    f.each_line do |line|
        line.strip!
        next if line.empty?
        break if line == '__END__'
        if line[0] == '#'
            line = line[1, line.size - 1].strip
            current_country = line
        else
            artist_id = line.sub('https://www.discogs.com/artist/', '').split('-')[0]
            $wanted_artists << artist_id
            $country_for_artist[artist_id] = current_country
        end
    end
end

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
