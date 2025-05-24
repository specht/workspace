#!/usr/bin/env ruby

require 'nokogiri'
require 'yaml'
require 'fileutils'
require 'set'
require 'stringio'

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

Dir['cache/artists/*.xml'].each do |file|
    $wanted_artists << File.basename(file, '.xml')
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

$all_artists_ids = Set.new()
$artist_part_of = {}
$all_descriptions = {}
$all_genres = {}
$all_styles = {}
$all_releases = {}

def store(album)
    artist_id = album[:artist_id]
    return unless File.exist?('cache/artists/' + artist_id + '.xml')
    country = $country_for_artist[artist_id]
    # STDERR.puts country
    # STDERR.puts album.to_yaml
    return unless album[:description].include?('Album')
    return if album[:description].include?('Compilation')
    return if album[:description].include?('Unofficial Release')
    return if album[:description].include?('Reissue')
    return if album[:description].include?('Remastered')
    return if album[:description].include?('Tour Recording')
    return if (album[:released] || '').empty?
    album[:description].each do |description|
        $all_descriptions[description] ||= $all_descriptions.size + 1
    end
    album[:genres].each do |genre|
        $all_genres[genre] ||= $all_genres.size + 1
    end
    album[:styles].each do |style|
        $all_styles[style] ||= $all_styles.size + 1
    end
    unless $all_artists_ids.include?(artist_id)
        $all_artists_ids << artist_id
        doc = Nokogiri::XML(File.read('cache/artists/' + artist_id + '.xml'))
        doc.css('members *').each do |member|
            member_artist_id = member.attr('id')
            next unless File.exist?('cache/artists/' + member_artist_id + '.xml')
            $artist_part_of[member_artist_id] ||= Set.new()
            $artist_part_of[member_artist_id] << artist_id
            $all_artists_ids << member_artist_id
        end
        doc.css('aliases name').each do |alias_name|
            # STDERR.puts alias_name.text
        end
    end
    $all_releases[album[:id]] ||= album
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
    next unless File.exist?('cache/artists/' + artist_id + '.xml')

    album[:id] = release.attr('id')
    album[:artist_id] = artist_id
    album[:artist] = $artist_for_id[artist_id]
    album[:title] = release.at('title').text
    album[:description] = release.xpath('formats/format/descriptions/description').map { |x| x.text }
    album[:genres] = release.xpath('genres/genre').map { |x| x.text }
    album[:styles] = release.xpath('styles/style').map { |x| x.text }
    album[:country] = release.xpath('country').text.strip
    album[:country] = null if album[:country].empty?
    album[:released] = release.xpath('released').text.strip
    album[:notes] = release.xpath('notes').text.strip
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

File.open('music-archive-dump.sql', 'w') do |f|
    f.puts File.read('music-archive-schema.sql')

    f.puts "SET FOREIGN_KEY_CHECKS = 0;"
    f.puts "SET UNIQUE_CHECKS = 0;"
    f.puts "SET AUTOCOMMIT = 0;"

    $all_genres.each do |genre, genre_id|
        f.puts "INSERT INTO genre (id, genre) VALUES (#{genre_id}, '#{genre.gsub('\'', '\'\'')}');"
    end

    # $all_styles.each do |style, style_id|
    #     f.puts "INSERT INTO style (id, style) VALUES (#{style_id}, '#{style.gsub('\'', '\'\'')}');"
    # end

    # $all_descriptions.each do |description, description_id|
    #     f.puts "INSERT INTO description (id, description) VALUES (#{description_id}, '#{description}');"
    # end

    $all_artists_ids.to_a.sort.each do |artist_id|
        doc = Nokogiri::XML(File.read('cache/artists/' + artist_id + '.xml'))
        name = doc.at('artist/name').text
        # remove the (n) at the end of the name
        name.gsub!(/\s\(\d+\)$/, '')
        born = nil
        died = nil
        if doc.at('artist/members').nil?
            begin
                # description = cleanup_markup(doc.at('artist/profile').text).downcase
                description = doc.at('artist/profile').text.downcase
                if description.include?('born')
                    index_born = description.index('born')
                    born = description[index_born, 40].scan(/[^\d](\d{4})[^\d]/).map { |x| x[0] }.first
                end
                if description.include?('died')
                    index_died = description.index('died')
                    died = description[index_died, 40].scan(/[^\d](\d{4})[^\d]/).map { |x| x[0] }.first
                end
            rescue
            end
        end
        if born && died && died.to_i <= born.to_i
            born = nil
            died = nil
        end
        born ||= 'NULL'
        died ||= 'NULL'

        f.puts "INSERT INTO artist (id, name, born, died) VALUES (#{artist_id}, '#{name.gsub('\'', '\'\'')}', #{born}, #{died});"
    end
    $artist_part_of.each do |artist_id, part_of|
        part_of.each do |part_of_id|
            f.puts "INSERT INTO artist_part_of (member_id, band_id) VALUES (#{artist_id}, #{part_of_id});"
        end
    end
    $all_releases.each do |release_id, release|
        artist_id = release[:artist_id]
        title = release[:title]
        year = release[:released][0, 4].strip
        year = 'NULL' if year.empty?
        country = release[:country]
        f.puts "INSERT INTO album (id, artist_id, title, year, country) VALUES (#{release_id}, #{artist_id}, '#{title.gsub('\'', '\'\'')}', #{year}, '#{country}');"
    end
    f.puts "COMMIT;"

    $all_releases.each do |release_id, release|
        # release[:styles].uniq.each do |style|
        #     style_id = $all_styles[style]
        #     f.puts "INSERT INTO album_style (album_id, style_id) VALUES (#{release_id}, #{style_id});"
        # end
        release[:genres].uniq.each do |genre|
            genre_id = $all_genres[genre]
            f.puts "INSERT INTO album_genre (album_id, genre_id) VALUES (#{release_id}, #{genre_id});"
        end
    end
    f.puts "COMMIT;"

    $all_releases.each do |release_id, release|
        skip_tracks_without_position = release[:tracklist].any? { |x| !(x[:position] || '').empty? }
        i = 0
        release[:tracklist].each.with_index do |track|
            next if skip_tracks_without_position && (track[:position] || '').empty?
            title = track[:title].strip
            if title.size > 80
                title = title[0, 80] + '…'
            end
            duration = track[:duration].strip.split(':').map { |x| x.to_i }
            begin
                duration = duration[0] * 60 + duration[1]
            rescue
                duration = 'NULL'
            end
            album_id = release_id
            number = i + 1
            f.puts "INSERT INTO track (album_id, number, title, duration) VALUES (#{album_id}, #{number}, '#{title.gsub('\'', '\'\'')}', #{duration});"
            i += 1
        end
    end
    f.puts "COMMIT;"

    f.puts "SET FOREIGN_KEY_CHECKS = 1;"
    f.puts "SET UNIQUE_CHECKS = 1;"

end