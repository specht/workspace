require 'minitest/autorun'
require_relative '../include/scene_color'
require_relative '../include/task_spec'

class SceneColorTest < Minitest::Test
    def family(value)
        SceneColor.family(SceneColor.css(value))
    end

    def test_names_codes_and_numbers
        assert_equal [255, 215, 0], SceneColor.css('Gold')
        assert_equal [255, 170, 0], SceneColor.css('#fa0')
        assert_equal [58, 110, 232], SceneColor.css('#3a6ee8')
        assert_equal [10, 20, 30], SceneColor.css('rgb(10, 20, 30)')
        assert_nil SceneColor.css('sonnengelb')
        assert_equal [255, 204, 0], SceneColor.rgb({ 'rgb' => [1, 0.8, 0] })
        assert_nil SceneColor.rgb('off')
    end

    def test_families_people_would_agree_with
        { 'yellow' => %w[yellow gold #f9c80e], 'orange' => %w[orange darkorange #f9b935],
          'blue' => %w[blue navy skyblue #3a6ee8 royalblue], 'cyan' => %w[cyan teal turquoise],
          'white' => %w[white snow ivory], 'grey' => %w[grey silver lightgray], 'black' => %w[black #111],
          'red' => %w[red crimson darkred], 'brown' => %w[brown saddlebrown #865c39],
          'green' => %w[green lime #2b8f45], 'purple' => %w[purple violet], 'pink' => %w[pink hotpink] }.each do |expected, colours|
            colours.each { |c| assert_equal expected, family(c), c }
        end
    end

    def test_description_shows_what_was_written
        assert_equal 'gold (#ffd700, Gelb)', SceneColor.describe({ 'css' => 'gold' })
        assert_equal '#3a6ee8 (Blau)', SceneColor.describe({ 'css' => '#3a6ee8' })
        assert_equal '#000000 (Schwarz)', SceneColor.describe({ 'rgb' => [0, 0, 0] })
        assert_equal 'sonnengelb (unbekannte Farbe)', SceneColor.describe({ 'css' => 'sonnengelb' })
        assert_equal 'keine Füllung', SceneColor.describe('off')
        assert_equal 'Gelb oder Orange', SceneColor.german_list(%w[yellow orange])
    end

    def test_task_files_name_known_families
        assert_empty TaskSpec.color_errors(%w[yellow orange], 'color')
        assert_empty TaskSpec.color_errors('black', 'background')
        refute_empty TaskSpec.color_errors('gelb', 'color')
        refute_empty TaskSpec.color_errors([], 'color')
    end
end
