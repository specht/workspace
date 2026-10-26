require 'minitest/autorun'
require_relative '../include/curriculum_overview'

# /lehrplan: the activities under the names of the curricula.
class CurriculumOverviewTest < Minitest::Test
    CONTENT = {
        'kata-loops' => { :title => 'Katas: Schleifen', :kind => 'uebung',
                          :curriculum => 'Sek I: Algorithmisches Problemlösen (algorithmische Grundstrukturen: Wiederholung); Oberstufe: Algorithmen und Datenstrukturen' },
        'raytracing' => { :title => 'Ray&shy;tracing', :kind => 'projekt', :language => 'Ruby', :curriculum => 'Oberstufe: Vertiefung Computergrafik' },
        'broken' => { :title => 'Kaputt', :kind => 'projekt', :curriculum => 'Sek I: Kontrollstrukturen' },
        'secret' => { :title => 'Verborgen', :kind => 'projekt', :curriculum => 'Oberstufe: Vertiefung Computergrafik (<b>x</b>)' },
    }.freeze

    def html
        CurriculumOverview.html(CONTENT, %w[raytracing kata-loops broken])
    end

    def test_every_name_of_the_plans_has_its_place
        (Curriculum::SEK_I.map { |n| ['Sek I', n] } + Curriculum::OBERSTUFE.map { |n| ['Oberstufe', n] }).each do |level, name|
            assert_includes html, "id='#{CurriculumOverview.anchor(level, name)}'"
        end
        assert_includes html, 'Noch keine Aktivität im Workspace.'
    end

    def test_an_activity_under_each_of_its_names_with_its_details
        assert_includes html, "<a href='/kata-loops'>Katas: Schleifen</a> <span class='lp-kind'>Übung</span><span class='lp-details'>algorithmische Grundstrukturen: Wiederholung</span>"
        assert_includes html, "<a href='/raytracing'>Raytracing</a> <span class='lp-kind'>Projekt (Ruby)</span></li>"
        assert_equal 2, html.scan("href='/kata-loops'").size, 'once for Sek I, once for the Oberstufe'
    end

    def test_only_the_given_activities_and_only_right_lines
        refute_includes html, 'secret', 'not among the slugs, as a hidden section'
        refute_includes html, 'Kaputt', 'a line with no name of the plans'
    end

    def test_anchor
        assert_equal 'sek-i-algorithmisches-problemlosen', CurriculumOverview.anchor('Sek I', 'Algorithmisches Problemlösen')
        assert_equal 'oberstufe-vertiefung-computer-netze', CurriculumOverview.anchor('Oberstufe', 'Vertiefung Computer-Netze')
    end
end
