#!/usr/bin/env ruby
# frozen_string_literal: true

# Renders a hand-drawn SVG diagram as NAME.webp and, with the dark palette
# below, as NAME-dark.webp beside it. The site shows the -dark picture in
# dark mode, on the page and on the card of the activity (add_media_variants
# and image_dark in main.rb), so a diagram needs no invert-dark.
#
#   src/scripts/render-svg.rb src/content/dns/dns-tree.svg [WIDTH]
#
# The palette maps the colours the diagrams use (Tailwind greys, the site's
# blue). A colour that is not in it stops the script: add it here first, so
# that nothing stays light by accident.

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

svg_path = ARGV[0] or abort "usage: #{$PROGRAM_NAME} FILE.svg [WIDTH]"
width = Integer(ARGV[1] || 1600)
light = File.read(svg_path)

unknown = light.scan(/#[0-9a-fA-F]{6}\b/).map(&:downcase).uniq - DARK.keys
abort "#{svg_path}: no dark colour for #{unknown.join(', ')}; add it to DARK in #{__FILE__}" unless unknown.empty?
dark = light.gsub(/#[0-9a-fA-F]{6}\b/) { |colour| DARK.fetch(colour.downcase) }

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
render(light, "#{base}.webp", width)
render(dark, "#{base}-dark.webp", width)
