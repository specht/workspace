#!/usr/bin/env ruby

require 'nokogiri'
require 'yaml'
require 'fileutils'
require 'set'
require 'open3'

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

$round = 1
$transitive_wanted_artist_ids = Set.new()

def handle_xml(xml)
    id = xml.match(/<id>(\d+)<\/id>/)[1]
    return unless $wanted_artists.include?(id) || $transitive_wanted_artist_ids.include?(id)
    path = "cache/artists/#{id}.xml"
    FileUtils.mkpath(File.dirname(path))
    File.open(path, 'w') { |f| f.write(xml) }
    if $round == 1 || $round == 3
        doc = Nokogiri::XML(xml)
        doc.css('members *').each do |member|
            $transitive_wanted_artist_ids << member.attr('id')
        end
    elsif $round == 2
        doc = Nokogiri::XML(xml)
        doc.css('groups *').each do |member|
            $transitive_wanted_artist_ids << member.attr('id')
        end
    end
end

$round = 0
4.times do
    $round += 1
    Open3.popen2("pigz -cd artists.xml.gz") do |stdin, stdout, wait_thr|
        xml = ''
        stdout.readline
        stdout.each_line do |line|
            xml += line
            if line[line.size - 10, 9] == '</artist>'
                handle_xml(xml)
                xml = ''
            end
        end
    end

    STDERR.puts "Got #{($transitive_wanted_artist_ids | $wanted_artists).size} artists after round #{$round}"
end
