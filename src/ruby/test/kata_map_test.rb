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

    def test_a_visitor_sees_the_path_without_progress_or_curriculum
        html = KataMap.html(stops)
        assert_includes html, "<a class='kata-stop-link' href='/kata-functions'>"
        assert_includes html, 'Werte bekommen, rechnen, zurückgeben'
        refute_includes html, 'gelöst'
        refute_includes html, 'Sek I'
    end

    def test_a_student_sees_how_far_they_are
        html = KataMap.html(stops, :progress => { 'kata-functions' => { :done => 4, :total => 4, :extras_done => 1 } })
        assert_includes html, '4 von 4 gelöst, dazu 1 Extra'
        assert_includes html, "<li class='kata-stop kata-stop-done'>"
        assert_includes html, '0 von 3 gelöst'
        assert_includes html, "<i class='bi bi-check-lg'></i>"
    end

    def test_a_teacher_sees_the_curriculum
        assert_includes KataMap.html(stops, :teacher => true), "<span class='kata-stop-curriculum'>Sek I: Variablen, Ausdrücke</span>"
    end
end
