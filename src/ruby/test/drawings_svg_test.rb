require 'minitest/autorun'

# A drawing stands on its page as an SVG, never as a WebP made from it
# (docs/checkliste.md, »Bilder«): sharp at every size and in the enlarged
# view. render-svg.rb embeds its fonts and writes x-dark.svg. A WebP of a
# drawing is only for a place that needs pixels: the card (image: in the
# meta, and the hidden <img class='teaser'> that gives it its alt text).
# A picture a recipe renders (.tutorial-screenshots.json, also a PDF page)
# is made of pixels anyway and not a drawing.
class DrawingsSvgTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def rendered_by_recipe(folder, src)
        Dir[File.join(folder, '.tutorial-screenshots.json')].any? { |list| File.read(list).include?("\"#{src}.webp\"") }
    end

    def test_no_page_shows_a_drawing_as_webp
        found = Dir[File.join(CONTENT, '**', '*.md')].flat_map do |file|
            folder = File.dirname(file)
            File.read(file, encoding: 'UTF-8').scan(/<img\b[^>]*>/).filter_map do |tag|
                next if tag =~ /class=['"][^'"]*\bteaser\b/
                src = tag[/src=['"]([^'"]+)\.webp['"]/, 1]
                next unless src && !src.start_with?('/', 'http')
                next if rendered_by_recipe(folder, src)
                "#{file.sub(CONTENT + '/', '')}: #{src}.webp (#{src}.svg is beside it)" if File.exist?(File.join(folder, "#{src}.svg"))
            end
        end
        assert_empty found, "Show the drawing as SVG (src='x.svg', render-svg.rb for fonts and x-dark.svg):\n#{found.join("\n")}"
    end

    def test_every_dark_svg_has_its_drawing_and_parses
        require 'nokogiri'
        bad = Dir[File.join(CONTENT, '**', '*-dark.svg')].filter_map do |dark|
            light = dark.sub(/-dark\.svg\z/, '.svg')
            next "#{dark}: no #{File.basename(light)} beside it" unless File.exist?(light)
            [light, dark].filter_map do |svg|
                text = File.read(svg, encoding: 'UTF-8')
                # browsers refuse an SVG whose comment contains two hyphens
                next "#{svg}: »--« in a comment" if text.scan(/<!--(.*?)-->/m).any? { |(c)| c.include?('--') }
                Nokogiri::XML(text, &:strict) && nil
            rescue Nokogiri::XML::SyntaxError => e
                "#{svg}: #{e.message.lines.first}"
            end
        end.flatten
        assert_empty bad, bad.join("\n")
    end
end
