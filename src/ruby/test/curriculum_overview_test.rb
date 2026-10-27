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
        (Curriculum.sek_i.map { |n| ['Sek I', n] } + Curriculum.oberstufe.map { |n| ['Oberstufe', n] }).each do |level, name|
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

    # Another state's plan: its levels and names, its source; Berlin's
    # source for Brandenburg's teachers says what is shared.
    def test_another_plan
        plan = Curriculum.plan_for('baden-wuerttemberg')
        content = { 'x' => { :title => 'X', :kind => 'projekt', :curriculum => 'Oberstufe: Sprachen und Automaten', :topics => %w[automaten] } }
        html = CurriculumOverview.html(content, %w[x], :plan => plan)
        assert_includes html, "<h2 id='kursstufe'>"
        assert_includes html, "id='#{CurriculumOverview.anchor('Kursstufe', 'Automaten und formale Sprachen')}'"
        assert_includes html, "<a href='/x'>X</a> <span class='lp-kind'>Projekt</span><span class='lp-details'>endliche Automaten</span>"
        refute_includes html, "id='sek-i'"
        brandenburg = CurriculumOverview.html(content, %w[x], :state => 'brandenburg')
        assert_includes brandenburg, 'gemeinsam mit Brandenburg'
        refute_includes html(), 'gemeinsam mit Brandenburg'
    end

    # Every level links its plan as a PDF and the state's page; Brandenburg's
    # Oberstufe has a plan of its own, Berlin's teachers see Berlin's
    def test_the_plans_to_read
        berlin = html()
        assert_includes berlin, 'rahmenlehrplan-informatik-go-teil-c.pdf'
        assert_includes berlin, 'Teil_C_Informatik_2015_11_10.pdf'
        refute_includes berlin, 'RLP_GOST_2022_Informatik.pdf'
        brandenburg = CurriculumOverview.html({}, [], :state => 'brandenburg')
        assert_includes brandenburg, 'Teil_C_RLP_GOST_2022_Informatik.pdf'
        assert_includes brandenburg, 'curricula-gost-bb'
        refute_includes brandenburg, 'rahmenlehrplan-informatik-go-teil-c.pdf'
        Curriculum.plans.each do |plan|
            plan[:levels].each { |l| assert l[:pdf], "#{plan[:key]} #{l[:id]}: no pdf:" }
        end
    end
end
