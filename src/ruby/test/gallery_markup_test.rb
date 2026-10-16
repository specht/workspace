require 'minitest/autorun'

# Pictures side by side are a gallery, one way on the whole site
# (docs/aktivitaet-schreiben.md): <div class='gallery'> with one <figure>
# per picture, each with an alt text and a <figcaption>. Not a Bootstrap row
# of pictures, not the old class screen-gallery.
class GalleryMarkupTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def pages
        Dir[File.join(CONTENT, '**', '*.md')].map { |file| [file.sub(CONTENT + '/', ''), File.read(file, encoding: 'UTF-8')] }
    end

    # The blocks <div class='gallery …'> … </div> of a page
    def galleries(text)
        text.scan(%r{<div class=['"]gallery(?: [a-z0-9 -]+)?['"]>(.*?)\n</div>}m).map(&:first)
    end

    def test_every_picture_in_a_gallery_has_an_alt_text_and_a_caption
        found = pages.flat_map do |name, text|
            galleries(text).flat_map do |gallery|
                gallery.scan(%r{<figure>(.*?)</figure>}m).map(&:first).reject do |figure|
                    figure =~ /<img\s[^>]*alt=['"][^'"]+['"]/ && figure.include?('<figcaption>')
                end.map { |figure| "#{name}: #{figure.strip[0, 100]}" }
            end
        end
        assert_empty found, "A picture in a gallery needs alt and <figcaption>:\n#{found.join("\n")}"
    end

    def test_pictures_side_by_side_are_a_gallery
        found = pages.flat_map do |name, text|
            hits = []
            hits << "#{name}: screen-gallery" if text.include?('screen-gallery')
            text.scan(%r{<div class=['"][^'"]*\brow\b[^'"]*['"]>(.*?)\n</div>}m).each do |(row)|
                hits << "#{name}: a row of pictures" if row.scan('<img').length > 1
            end
            hits
        end
        assert_empty found, "Pictures side by side go in <div class='gallery'>:\n#{found.join("\n")}"
    end
end
