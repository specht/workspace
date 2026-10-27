require 'minitest/autorun'
require_relative '../include/curriculum_graph'

class CurriculumGraphTest < Minitest::Test
    def node(slug, section = 'katas', topics = [])
        { :slug => slug, :title => "Katas: #{slug}", :section => section, :topics => topics }
    end

    NODES = %w[a b c d e alone].freeze
    EDGES = [%w[a b], %w[b c], %w[a c], %w[d e], %w[c c], %w[x a]].freeze

    def graph
        CurriculumGraph.layout(NODES.map { |s| node(s) }, EDGES)
    end

    def test_a_box_stands_right_of_the_longest_chain_before_it
        depth = graph[:nodes].to_h { |n| [n[:slug], n[:depth]] }
        assert_equal({ 'a' => 0, 'b' => 1, 'c' => 2, 'd' => 0, 'e' => 1 }, depth)
        x = graph[:nodes].to_h { |n| [n[:slug], n[:x]] }
        assert x['a'] < x['b'] && x['b'] < x['c']
    end

    def test_every_box_once_and_every_edge_between_boxes
        slugs = graph[:nodes].map { |n| n[:slug] }
        assert_equal slugs.uniq, slugs
        graph[:edges].each { |a, b| assert_includes slugs, a; assert_includes slugs, b; refute_equal a, b }
        # an edge to a slug that is no activity, and a loop, are left out
        refute_includes graph[:edges], %w[x a]
        refute_includes graph[:edges], %w[c c]
    end

    def test_what_nothing_is_joined_to_is_listed_apart
        assert_equal ['alone'], graph[:alone]
        refute_includes graph[:nodes].map { |n| n[:slug] }, 'alone'
    end

    def test_boxes_of_a_column_do_not_overlap
        graph[:nodes].group_by { |n| n[:depth] }.each_value do |column|
            ys = column.map { |n| n[:y] }.sort
            ys.each_cons(2) { |top, below| assert below - top >= CurriculumGraph::ROW - 0.001 }
        end
        assert graph[:nodes].all? { |n| n[:y] >= CurriculumGraph::PAD - 0.001 && n[:y] + CurriculumGraph::BOX_H <= graph[:height] + 0.001 }
    end

    def test_a_cycle_does_not_hang
        result = CurriculumGraph.layout(%w[p q].map { |s| node(s) }, [%w[p q], %w[q p]])
        assert_equal 2, result[:nodes].size
    end

    def test_series_keep_their_second_part
        assert_equal 'Kata: Listen', CurriculumGraph.short('Katas: Listen')
        assert_equal 'Pip: Der Mampfer', CurriculumGraph.short('Pip unter Funkelbach: Der Mampfer')
        assert_equal 'DNS', CurriculumGraph.short('DNS: Wie aus einem Namen eine Adresse wird')
    end

    def test_the_html_has_a_box_and_an_arrow_for_each
        sections = { 'katas' => { :label => 'Katas' } }
        html = CurriculumGraph.html(graph, sections, ['katas'], { 'alone' => { :title => 'Allein' } })
        assert_equal graph[:nodes].size, html.scan("class='cg-node").size
        assert_equal graph[:edges].size, html.scan("class='cg-edge'").size
        assert_includes html, "href='/a' target='_blank'"
        assert_includes html, 'Nicht im Bild: eine Aktivität, die auf keiner aufbaut'
    end
end
