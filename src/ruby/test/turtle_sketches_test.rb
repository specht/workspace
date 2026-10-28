require 'minitest/autorun'
require_relative '../include/turtle_sketches'
require_relative '../include/kata_hand_ins'

# Several drawings on a free turtle card (TurtleSketches): names, the
# stored book, and the picture the class sees when one is handed in.
class TurtleSketchesTest < Minitest::Test
    def test_names_are_text_of_at_most_forty_characters
        assert_equal 'Mein Wald', TurtleSketches.name("  Mein   Wald ")
        assert_nil TurtleSketches.name('')
        assert_nil TurtleSketches.name('x' * 41)
        assert_nil TurtleSketches.name("Wa\u0007ld")
        assert_equal 'Bild 2', TurtleSketches.free_name([{ 'name' => 'Bild 1' }, { 'name' => 'Wald' }])
    end

    def test_a_stored_book_keeps_only_what_it_should
        text = JSON.generate({ 'ruby' => { 'current' => 'Wald', 'list' => [{ 'name' => 'Wald', 'code' => 'forward 10' }, 'nonsense', { 'name' => 3 }] },
                               'python' => 'nonsense' })
        books = TurtleSketches.parse(text)
        assert_equal ['ruby'], books.keys
        assert_equal [{ 'name' => 'Wald', 'code' => 'forward 10' }], books['ruby']['list']
        assert_equal({}, TurtleSketches.parse('not json'))
        assert_equal({}, TurtleSketches.parse(nil))
    end

    def test_the_picture_is_drawn_from_the_commands_with_y_up
        segments = TurtleSketches.picture([['color', '#ff0000'], ['forward', 100], ['left', 90], ['width', 4], ['forward', 50]])
        svg = TurtleSketches.svg(segments)
        assert svg.start_with?("<svg xmlns='http://www.w3.org/2000/svg'")
        assert_includes svg, "<path d='M0 0L100 0' stroke='#ff0000' stroke-width='2'/>"
        # up on the card is up in the picture: y turns around
        assert_includes svg, "<path d='M100 0L100 -50' stroke='#ff0000' stroke-width='4'/>"
        assert_nil TurtleSketches.picture([['jump', 1]])
        assert_nil TurtleSketches.svg([])
    end

    def test_handed_in_drawings_show_as_pictures_with_their_names
        svg = TurtleSketches.svg(TurtleSketches.picture([['forward', 10]]))
        entries = [{ :email => 'ada@x', :name => 'Ada', :title => 'Wald <b>', :svg => svg },
                   { :email => 'ada@x', :name => 'Ada', :title => 'Meer', :svg => svg }]
        html = KataHandIns.html('turtle-3/finale', 12, 'Dein Bild', entries)
        assert_includes html, '12. Dein Bild: 2 Bilder eingereicht'
        assert_includes html, "data-label='Bild 1: "
        assert_includes html, 'Wald &lt;b&gt;'
        assert_includes html, "<div class='kata-hand-in-picture'><svg"
        refute_includes html, '<pre'
    end
end
