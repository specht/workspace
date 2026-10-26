require 'minitest/autorun'
require_relative '../include/kata_map'

class KataMapTest < Minitest::Test
    def content(levels, title:, idea: nil, curriculum: nil)
        by_id = levels.each_with_index.to_h { |level, i| ["k#{i}", level == :extra ? { 'extra' => true } : { 'level' => level }] }
        { :title => title, :idea => idea, :curriculum => curriculum, :tasks => { :on_page => by_id.keys, :by_id => by_id } }
    end

    def stops
        [KataMap.stop('kata-functions', content([1, 1, 2, 3, :extra], :title => 'Katas: Erste Funktionen', :idea => 'Werte bekommen, rechnen, zurückgeben',
                                                :curriculum => 'Sek I: Variablen, Ausdrücke')),
         KataMap.stop('kata-loops', content([1, 2, 2], :title => 'Katas: Wiederholen'))]
    end

    def test_a_stop_counts_its_katas_by_level
        stop = stops.first
        assert_equal 'Erste Funktionen', stop.title
        assert_equal '2 Einstieg · 1 Übung · 1 Knobelei · 1 Extra', KataMap.counts_text(stop.counts)
    end

    def test_a_visitor_sees_the_sets_and_how_many_katas_there_are
        html = KataMap.carousel_html(stops)
        # the hues equally spaced from Pip's amber: two sets, half the wheel apart
        assert_includes html, "<div class='kata-set' style='--accent: 38'>"
        assert_includes html, "<div class='kata-set' style='--accent: 218'>"
        assert_equal [38, 66, 93], (0..2).map { |i| KataMap.hue(i, 13) }
        assert_includes html, "href='/kata-functions' aria-label='Ansehen: Erste Funktionen'>"
        assert_includes html, 'Werte bekommen, rechnen, zurückgeben'
        # no total beside the strip
        refute_includes html, 'kata-total'
        # the levels of a set on hover, in its line how many
        assert_includes html, "title='2 Einstieg · 1 Übung · 1 Knobelei · 1 Extra'>4 Katas, 1 Extra</span>"
        refute_includes html, 'gelöst'
        refute_includes html, 'Sek I'
    end

    def test_a_student_sees_how_far_they_are_in_each_set_and_in_all
        html = KataMap.carousel_html(stops, :progress => { 'kata-functions' => { :done => 4, :total => 4, :extras_done => 1 } })
        assert_includes html, '4 von 4 gelöst, dazu 1 Extra'
        assert_includes html, "<div class='kata-set kata-set-done' style='--accent: 38'>"
        assert_includes html, "aria-label='Ansehen: Erste Funktionen'"
        assert_includes html, "aria-label='Loslegen: Wiederholen'"
        assert_includes html, '0 von 3 gelöst'
    end

    def test_a_pinned_set_is_marked_and_the_latest_centred
        html = KataMap.carousel_html(stops, :progress => {}, :pinned => %w[kata-loops kata-functions])
        assert_equal 1, html.scan("<div class='kata-set kata-set-pinned' style='--accent: 218' data-centre=''>").size
        assert_equal 2, html.scan("kata-set-pinned").size
        assert html.index("data-centre") < html.index("href='/kata-loops'"), 'the latest pin is centred'
        assert_equal 2, html.scan('Angepinnt').size
    end

    # an icon of Bootstrap, or a figure such as Pip, tinted in the band's hue
    def test_a_set_has_its_icon
        stop = KataMap.stop('pip-1', content([1], :title => 'Pip').merge(:icon => '/images/pip/icon-pip.png'))
        html = KataMap.carousel_html([stop] + stops)
        assert_includes html, "<span class='kata-set-sprite kata-sprite-icon-pip' style='--sprite: url(/images/pip/icon-pip.png)'></span>"
        assert_includes html, "<i class='bi bi-code-slash'></i>"
    end

    def test_a_teacher_sees_the_curriculum
        assert_includes KataMap.carousel_html(stops, :teacher => true), ">Sek I: Variablen, Ausdrücke</span>"
    end
end
