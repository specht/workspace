#!/usr/bin/env ruby
# frozen_string_literal: true

# Prepares a drawing (an SVG) for the pages: ends the line of every arrow
# inside its head, so that it does not show beside the tip (svg_arrows.py),
# embeds the fonts its texts use (svg_fonts.py), so that it looks the same in
# every browser, and writes
# NAME-dark.svg beside it, the same drawing in the dark palette below. A page
# shows the SVG itself, <img src='NAME.svg'>: sharp at every size and in the
# enlarged view; in dark mode the site swaps in NAME-dark.svg
# (content_pipeline.rb, add_media_variants), so a drawing needs no
# invert-dark.
#
#   src/scripts/render-svg.rb src/content/dns/dns-tree.svg
#   src/scripts/render-svg.rb --webp src/content/dns/dns-tree.svg [WIDTH]
#
# --webp also renders NAME.webp and NAME-dark.webp (WIDTH pixels wide,
# 1600 if not given): only for a place that needs a picture made of pixels,
# the card of an activity (image: in its meta) or a social preview. A page
# never shows a drawing as WebP (docs/checkliste.md).
#
# The palette maps the colours the diagrams use (Tailwind greys, the site's
# blue). A colour that is not in it stops the script: add it here first, so
# that nothing stays light by accident.
#
# A shape that needs another opacity in the dark picture carries it as
# data-dark-fill-opacity="0.3" right after its fill-opacity; the dark picture
# takes that value instead.

require 'open3'
require 'tmpdir'

DARK = {
    '#e5185d' => '#e5185d', # code, as on the page (diagram_labels.py)
    '#f384af' => '#f384af', # the pale pink behind code (with fill-opacity)
    '#ffffff' => '#151514', # background and white boxes: the page in dark mode
    '#eef3fb' => '#1e2836', # pale blue boxes
    '#1f2937' => '#e3e6ea', # text, borders, edges
    '#2b5fb4' => '#6ea2f2', # the blue of the way, pointers, headings
    '#6b7280' => '#9aa1ac', # grey notes and dashed arrows
    '#d1d5db' => '#3a3f47', # separating lines
    '#f3f4f6' => '#202226', # a second pale area beside the pale blue (a ceiling over a floor)
    '#dc2626' => '#f87171', # red: the x axis
    '#16a34a' => '#4ade80', # green: the y axis
    '#d97706' => '#fbbf24', # amber: the sun and rays towards it
    '#0b0f19' => '#000000', # shadow on a floor: dark in both modes
    '#fdfdfd' => '#fdfdfd', # a printed label (a barcode): white in both modes
    '#fde68a' => '#fde68a', # a yellow sticker on a printed label (a QR code): the same in both modes
    '#7c3aed' => '#a78bfa', # purple: a fourth colour beside blue, green and amber (branches in a commit graph)
    '#eab308' => '#facc15', # yellow: a colour by name beside red, green and blue (a map in four colours)
}.freeze

# in docker/drawings, the same fonts and tools on every machine: started
# again there through src/scripts/drawings
unless ENV['DRAWINGS_CONTAINER'] == '1'
    repo = File.expand_path('../..', __dir__)
    inside = ARGV.map { |a| a.start_with?("#{repo}/") ? "/repo/#{a.delete_prefix("#{repo}/")}" : a }
    exec(File.join(__dir__, 'drawings'), 'ruby', '/repo/src/scripts/render-svg.rb', *inside)
end

webp = !ARGV.delete('--webp').nil?
ARGV.delete('--svg')          # the default now; accepted for old notes
svg_path = ARGV[0] or abort "usage: #{$PROGRAM_NAME} [--webp] FILE.svg [WIDTH]"
width = Integer(ARGV[1] || 1600)
# rsvg-convert forgives it, a browser shows nothing: two hyphens inside a
# comment end the XML
if File.read(svg_path).scan(/<!--(.*?)-->/m).any? { |(text)| text.include?('--') }
    abort "#{svg_path}: a comment contains »--«, which browsers refuse in an SVG"
end
# the lines of the arrows end inside their heads, not at the tips
# (svg_arrows.py), then the fonts go in (svg_fonts.py)
%w[svg_arrows.py svg_fonts.py].each do |step|
    output, status = Open3.capture2e('python3', File.join(__dir__, step), svg_path)
    abort "#{step} failed: #{output}" unless status.success?
end
light = File.read(svg_path)

unknown = light.scan(/#[0-9a-fA-F]{6}\b/).map(&:downcase).uniq - DARK.keys
abort "#{svg_path}: no dark colour for #{unknown.join(', ')}; add it to DARK in #{__FILE__}" unless unknown.empty?
dark = light.gsub(/#[0-9a-fA-F]{6}\b/) { |colour| DARK.fetch(colour.downcase) }
dark = dark.gsub(/fill-opacity="[^"]*" data-dark-fill-opacity="([^"]*)"/) { "fill-opacity=\"#{Regexp.last_match(1)}\"" }

def render(svg, target, width)
    Dir.mktmpdir do |dir|
        source = File.join(dir, 'picture.svg')
        png = File.join(dir, 'picture.png')
        File.write(source, svg)
        [['rsvg-convert', '-w', width.to_s, source, '-o', png],
         ['cwebp', '-quiet', '-lossless', png, '-o', target]].each do |command|
            output, status = Open3.capture2e(*command)
            abort "#{command.first} failed: #{output}" unless status.success?
        end
    end
    puts target
end

base = svg_path.delete_suffix('.svg')
File.write("#{base}-dark.svg", dark)
puts "#{base}-dark.svg"
if webp
    render(light, "#{base}.webp", width)
    render(dark, "#{base}-dark.webp", width)
end
