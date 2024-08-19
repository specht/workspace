#!/usr/bin/env ruby

require 'nokogiri'
require 'yaml'
require 'fileutils'
require 'set'
require 'stringio'
require "unicode/display_width/string_ext"

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

$artist_for_id = {}

Dir['cache/artists/*.xml'].each do |path|
    contents = File.read(path)
    doc = Nokogiri::XML(contents)
    artist = doc.at('artist')
    id = artist.at('id').text
    name = artist.at('name').text
    name.gsub!(/\s\(\d+\)$/, '')
    $artist_for_id[id] = name
end

$seen_bios = Set.new()

def cleanup_markup(s)
    s.gsub!(/\[url=[^\]]+\]/, '')
    s.gsub!('[/url]', '')
    s.gsub!(/\[[aA]=([^\]]+)\]/) do |match|
        $1.sub(/\(\d+\)\s*$/, '').strip
    end
    s.gsub!(/\[[aA]([^\]]+)\]/) do |match|
        value = $1
        `grep '^#{value}\s' index-artists.txt`.sub(/^\d+/, '').strip.sub(/\(\d+\)\s*$/, '').strip
    end
    s.gsub!(/\[[lL]=([^\]]+)\]/, '\1')
    s.gsub!(/\[[lL]([^\]]+)\]/) do |match|
        value = $1
        `grep '^#{value}\s' index-labels.txt`.sub(/^\d+/, '').strip
    end
    s.gsub!(/\[[rR]=([^\]]+)\]/, '\1')
    s.gsub!(/\[[rR]([^\]]+)\]/) do |match|
        value = $1
        `grep '^#{value}\s' index-releases.txt`.sub(/^\d+/, '').strip
    end
    s.gsub!(/\[[mM]=([^\]]+)\]/) do |match|
        value = $1
        `grep '^#{value}\s' index-masters.txt`.sub(/^\d+/, '').strip
    end
    s.gsub!('[b]', '')
    s.gsub!('[/b]', '')
    s.gsub!('[i]', '')
    s.gsub!('[/i]', '')
    s.gsub!('[u]', '')
    s.gsub!('[/u]', '')
    s.gsub!('&amp;', '&')
    s
end

def store(album)
    return if (album[:released] || '').empty?
    type = nil
    if album[:description].include?('Album')
        type = 'Albums'
    elsif album[:description].include?('Single') || album[:description].include?('Maxi-Single') || album[:description].include?('EP')
        type = 'Singles - EPs'
    end
    return if type.nil?
    ['Unofficial Release', 'Single', 'Maxi-Single', 'Reissue', 'Promo', 'Compilation'].each do |word|
        return if album[:description].include?(word)
    end
    path = "music-releases/#{$country_for_artist[album[:artist_id]]}/#{album[:artist].gsub('/', '-')}/#{album[:artist].gsub('/', '-')}.txt"
    unless $seen_bios.include?(path)
        $seen_bios << path
        STDERR.puts "Storing #{path}"
        contents = StringIO.open do |io|
            io.puts "-" * (album[:artist].size + 3)
            io.puts "> #{album[:artist]}"
            io.puts "-" * (album[:artist].size + 3)
            io.puts
            doc = Nokogiri::XML(File.read('cache/artists/' + album[:artist_id] + '.xml'))
            unless doc.css('members name').empty?
                io.puts "Members: #{doc.css('members name').map { |x| x.text.sub(/\(\d+\)\s*$/, '').strip }.join(', ')}"
                io.puts
            end
            unless doc.css('aliases name').empty?
                io.puts "Aliases: #{doc.css('aliases name').map { |x| x.text.sub(/\(\d+\)\s*$/, '').strip }.join(', ')}"
                io.puts
            end
            profile = doc.at('profile').text
            profile = cleanup_markup(profile)
            io.puts profile
            io.string
        end
        FileUtils.mkpath(File.dirname(path))
        File.open(path, 'w') { |f| f.write(contents) }
        # STDERR.puts contents
    end
    path = "music-releases/#{$country_for_artist[album[:artist_id]]}/#{album[:artist].gsub('/', '-')}/#{type}/#{album[:released][0, 4]} - #{album[:title].gsub('/', '-')}.txt"
    unless File.exist?(path)
        STDERR.puts "Storing #{path}"
        contents = StringIO.open do |io|
            io.puts "-" * (album[:title].size + 3)
            io.puts "> #{album[:title]}"
            io.puts "-" * (album[:title].size + 3)
            io.puts
            io.puts "Artist  : #{album[:artist]}"
            io.puts "Released: #{album[:released]}, #{album[:country]}"
            io.puts "Genres  : #{album[:genres].join(', ')}" unless album[:genres].empty?
            io.puts "Styles  : #{album[:styles].join(', ')}" unless album[:styles].empty?
            io.puts
            longest_position = album[:tracklist].map { |x| x[:position].size }.max
            longest_title = album[:tracklist].map { |x| x[:title].display_width }.max
            album[:tracklist].each do |track|
                if track[:position].empty?
                    io.puts
                    io.puts "#{track[:title]}"
                    io.puts
                else
                    io.print "#{track[:position].rjust(longest_position)}. "
                    io.print "#{track[:title]}"
                    io.print " " * (longest_title - track[:title].display_width)
                    unless track[:duration].empty?
                        io.print " (#{track[:duration]})"
                    end
                    io.puts
                end
            end
            io.puts
            io.puts cleanup_markup(album[:notes])
            io.string
        end
        FileUtils.mkpath(File.dirname(path))
        File.open(path, 'w') { |f| f.write(contents) }
    end
end

Dir['cache/releases/*.xml'].each do |path|
    contents = File.read(path)
    # next unless contents.include?('<artist><id>3840</id>')
    doc = Nokogiri::XML(contents)
    release = doc.at('release')
    album = {}
    artist_id = doc.xpath('//artists/artist').map { |x| x.at('id').text }
    next if artist_id.size > 1
    artist_id = artist_id.first
    next unless $wanted_artists.include?(artist_id)

    album[:id] = release.at('id').text
    album[:artist_id] = artist_id
    album[:artist] = $artist_for_id[artist_id]
    album[:title] = release.at('title').text
    album[:description] = release.xpath('formats/format/descriptions/description').map { |x| x.text }
    album[:genres] = release.xpath('genres/genre').map { |x| x.text }
    album[:styles] = release.xpath('styles/style').map { |x| x.text }
    album[:country] = release.xpath('country').text
    album[:released] = release.xpath('released').text
    album[:notes] = release.xpath('notes').text
    album[:tracklist] = []
    release.xpath('tracklist/track').each do |track|
        album[:tracklist] << {
            :position => track.xpath('position').text,
            :title => track.xpath('title').text,
            :duration => track.xpath('duration').text,
        }
    end

    store(album)
end