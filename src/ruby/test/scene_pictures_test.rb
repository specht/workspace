require 'minitest/autorun'

# A picture of a 3D scene is as wide as the phone's screen and does not scroll
# sideways (docs/checkliste.md, »Bilder«): `full fit-width`, also with labels
# as large as the room names in the Automap of the Backrooms. Only a picture
# with small text scrolls (`full` alone): the editor, the terminal, a page
# with body text, a program's interface.
#
# Checked where it can be told from the page: in the tutorials whose preview
# runs on the graphics card (gpu: true in tutorial-screenshot-hooks.mjs), every
# picture a recipe takes in the preview (tab: preview) is a scene, except the
# few that show small text, named below with their reason. In a gallery that
# does not scroll on a phone, `inspect` opens a picture large instead.
class ScenePicturesTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    SMALL_TEXT = {
        'glsl/fehler.webp' => 'the compiler\'s error message, in lines of small text',
        'raylib/studio-kacheln.webp' => 'the interface of the 2D Game Studio',
        'raylib/studio-speichern.webp' => 'the interface of the 2D Game Studio, with its menu',
    }.freeze

    def scene_tutorials
        Dir[File.join(CONTENT, '**', 'tutorial-screenshot-hooks.mjs')]
            .select { |hooks| File.read(hooks).match?(/^\s*gpu:\s*true/) }
            .map { |hooks| File.dirname(hooks) }
    end

    def test_scene_pictures_fit_the_phone
        tutorials = scene_tutorials
        refute_empty tutorials, 'no tutorial with gpu: true found'
        found = tutorials.flat_map do |folder|
            Dir[File.join(folder, '*.md')].flat_map do |file|
                File.read(file, encoding: 'UTF-8').scan(/<!-- tutorial-screenshot\n(.*?)-->\s*(<img\b[^>]*>)/m).filter_map do |recipe, tag|
                    next unless recipe.match?(/^tab:\s*preview\s*$/)
                    classes = tag[/class=['"]([^'"]*)['"]/, 1].to_s.split
                    next unless classes.include?('full')
                    next if (classes & %w[fit-width inspect teaser]).any?
                    name = "#{File.basename(folder)}/#{tag[/src=['"]([^'"]+)['"]/, 1]}"
                    next if SMALL_TEXT.key?(name)
                    name
                end
            end
        end
        assert_empty found, "A 3D scene fits the phone: class='full fit-width' (or list it in SMALL_TEXT with the reason):\n#{found.join("\n")}"
    end

    def test_the_exceptions_still_exist
        SMALL_TEXT.each_key do |name|
            assert File.exist?(File.join(CONTENT, name)), "#{name} is gone: take it out of SMALL_TEXT"
        end
    end
end
