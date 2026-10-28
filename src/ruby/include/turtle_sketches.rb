require 'json'
require_relative 'turtle_world'

# Several drawings on a free turtle card (docs/plan-katas.md, »Turtle«, the
# cards with free:): each with a name, per language, one of them open in
# the card's editor. Any of them may be handed in; what the class sees is a
# picture the server drew from the commands of its last run (svg), never
# code that would run in someone else's browser.
#
# Stored on the card's TaskState (sketches), as JSON:
#
#   { "ruby" => { "current" => "Wald", "list" => [{ "name" => "Wald", "code" => "…" }] } }
#
# The hand-ins of a card are one file in the submission (FILE), a list of
# { "name", "language", "code", "svg" }.
#
# Plain Ruby, tested on its own (test/turtle_sketches_test.rb).
module TurtleSketches
    MAX = 20
    MAX_NAME = 40
    MAX_HANDED = 10
    # a picture with more lines is drawn with the first of them
    MAX_SEGMENTS = 40_000
    FILE = 'bilder.json'
    PAPER = '#fffdf8'

    def self.parse(text)
        value = JSON.parse(text.to_s)
        return {} unless value.is_a?(Hash)

        value.each_with_object({}) do |(language, book), out|
            next unless book.is_a?(Hash) && book['list'].is_a?(Array)

            list = book['list'].select { |s| s.is_a?(Hash) && s['name'].is_a?(String) && s['code'].is_a?(String) }
            out[language] = { 'current' => book['current'].to_s, 'list' => list }
        end
    rescue JSON::ParserError
        {}
    end

    # a name as given, trimmed, or nil when it is empty, too long or has
    # characters that are not text
    def self.name(text)
        name = text.to_s.strip.gsub(/\s+/, ' ')
        return nil if name.empty? || name.length > MAX_NAME || name.match?(/[[:cntrl:]]/)

        name
    end

    # »Bild 1«, »Bild 2«, …: the first that is free
    def self.free_name(list)
        used = list.map { |s| s['name'] }
        (1..).lazy.map { |i| "Bild #{i}" }.find { |n| !used.include?(n) }
    end

    def self.find(list, name)
        list.find { |s| s['name'] == name }
    end

    # The picture as SVG on its paper, from the lines TurtleWorld drew:
    # y up as on the card, one path per colour and width.
    def self.svg(segments, background = nil)
        segments = segments.first(MAX_SEGMENTS)
        return nil if segments.empty?

        widest = segments.map(&:width).max.to_f
        xs = segments.flat_map { |s| [s.x1, s.x2] }
        ys = segments.flat_map { |s| [s.y1, s.y2] }
        pad = 12 + widest / 2
        x0 = xs.min - pad
        y0 = -ys.max - pad
        w = [xs.max - xs.min, 1].max + 2 * pad
        h = [ys.max - ys.min, 1].max + 2 * pad
        f = ->(v) { (v.round(1).zero? ? 0.0 : v.round(1)).to_s.sub(/\.0\z/, '') }
        paths = segments.group_by { |s| [s.color, s.width] }.map do |(color, width), list|
            d = list.map { |s| "M#{f.(s.x1)} #{f.(-s.y1)}L#{f.(s.x2)} #{f.(-s.y2)}" }.join
            "<path d='#{d}' stroke='#{color}' stroke-width='#{f.(width.to_f)}'/>"
        end
        "<svg xmlns='http://www.w3.org/2000/svg' viewBox='#{f.(x0)} #{f.(y0)} #{f.(w)} #{f.(h)}' fill='none' stroke-linecap='round' stroke-linejoin='round'>" \
            "<rect x='#{f.(x0)}' y='#{f.(y0)}' width='#{f.(w)}' height='#{f.(h)}' fill='#{background || PAPER}'/>#{paths.join}</svg>"
    end

    # The commands a card reports, replayed (TurtleWorld, with its
    # segments and paper); nil when they are none.
    def self.world(commands)
        TurtleWorld.replay(commands)
    rescue TurtleWorld::Error
        nil
    end

    def self.picture(commands)
        world(commands)&.segments
    end
end
