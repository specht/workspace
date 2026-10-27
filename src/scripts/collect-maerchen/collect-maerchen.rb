#!/usr/bin/env ruby
# frozen_string_literal: true

# Builds the text of »Ein winziges Sprachmodell« (src/content/sprachmodell),
# served as src/static/sprachmodell/maerchen.txt:
# the fairy tales of the Brothers Grimm in German, from Project Gutenberg's
# eBook #77905, »Deutsche Märchen gesammelt durch die Brüder Grimm« (edited
# by M. Thilo-Luyken, Langewiesche-Brandt 1921; transcribed by Distributed
# Proofreaders, public domain). What comes out is the tales' text only, a
# paragraph a line, the tales separated by an empty line: no titles, no
# notes on the transcription, no table of contents, none of Project
# Gutenberg's header and licence (its trademark goes with them). Spelling
# and punctuation stay those of the book (»daß«). The three tales in Low
# German, and the one told mostly in it (»Der Hase und der Igel«), are left
# out, so that the model learns one language.
#
#   ruby collect-maerchen.rb            # download and write the text
#   ruby collect-maerchen.rb FILE.txt   # from a downloaded copy

require 'net/http'
require 'uri'

SOURCE = 'https://www.gutenberg.org/cache/epub/77905/pg77905.txt'
TARGET = File.expand_path('../../static/sprachmodell/maerchen.txt', __dir__)
LOW_GERMAN = ['Von dem Fischer un syner Fru', 'Von dem Machandelboom', 'Die drei Vügelkens', 'Der Hase und der Igel'].freeze
# a centered line of the book: a title, an illustration, »Erstes Märchen.«
CENTERED = /\A {12,}\S/

def download(url, limit = 5)
    raise 'too many redirects' if limit.zero?

    response = Net::HTTP.get_response(URI(url))
    return download(response['location'], limit - 1) if response.is_a?(Net::HTTPRedirection)
    raise "#{url}: #{response.code}" unless response.is_a?(Net::HTTPSuccess)

    response.body.force_encoding('UTF-8')
end

# [[title, [paragraph, …]], …]: a tale begins at a centered [Illustration]
# followed by its centered title
def tales(book)
    lines = book.lines.map(&:chomp)
    from = lines.index { |l| l =~ /\A {12,}Marienkind\s*\z/ } or raise 'no first tale'
    to = lines.index { |l| l =~ /\A\s+Inhalt\s*\z/ } or raise 'no table of contents'
    out = []
    title = []
    paragraph = []
    in_title = true
    finish_paragraph = lambda do
        out.last[1] << paragraph.join(' ') unless paragraph.empty? || out.empty?
        paragraph = []
    end
    out << [nil, []]
    lines[from...to].each do |line|
        if line =~ CENTERED
            finish_paragraph.call
            if line.strip == '[Illustration]'
                out << [nil, []]
                in_title = true
                title = []
            elsif in_title && !line.strip.start_with?('[')
                title << line.strip.tr('·', ' ').squeeze(' ')
                out.last[0] = title.join(' ')
            end
            next
        end
        # an illustration set less far in than the others
        next if line.strip.start_with?('[Illustration')

        if line.strip.empty?
            finish_paragraph.call
        else
            # the first line of text after the title
            in_title = false
            paragraph << line.strip.delete('~<>')
        end
    end
    finish_paragraph.call
    out.reject { |name, paragraphs| paragraphs.empty? || LOW_GERMAN.include?(name) }
end

if $PROGRAM_NAME == __FILE__
    book = ARGV[0] ? File.read(ARGV[0], :encoding => 'UTF-8') : download(SOURCE)
    book = book.delete_prefix("﻿")
    found = tales(book)
    text = found.map { |_, paragraphs| paragraphs.join("\n") }.join("\n\n") + "\n"
    File.write(TARGET, text)
    puts "#{found.size} tales, #{text.split.size} words, #{text.bytesize} bytes -> #{TARGET}"
end
