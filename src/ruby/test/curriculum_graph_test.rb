require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/curriculum_graph'

class CurriculumGraphTest < Minitest::Test
    def node(slug, section = 'katas', topics = [], levels = [])
        { :slug => slug, :title => "Katas: #{slug}", :section => section, :topics => topics, :levels => levels }
    end

    # a larger graph: long edges, three ages, several arrows into one box
    def big_inputs
        levels = { 's1' => ['Sek I'], 's2' => ['Sek I'], 's3' => ['Sek I'], 'b1' => ['Sek I', 'Oberstufe'], 'b2' => ['Sek I', 'Oberstufe'],
                   'o1' => ['Oberstufe'], 'o2' => ['Oberstufe'], 'o3' => ['Oberstufe'], 'x' => [] }
        nodes = levels.map { |slug, l| node(slug, 'katas', [], l) }
        edges = [%w[s1 s2], %w[s2 s3], %w[s3 o3], %w[s1 o3], %w[s1 b1], %w[b1 b2], %w[b2 o1], %w[s1 o1], %w[o1 o2], %w[s2 o2], %w[x o2], %w[b1 o3]]
        [nodes, edges]
    end

    def big
        CurriculumGraph.layout(*big_inputs)
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

    def test_a_box_is_coloured_by_age
        g = big
        age = g[:nodes].to_h { |n| [n[:slug], n[:age]] }
        assert_equal 0, age['s1']
        assert_equal 1, age['b1']
        assert_equal 1, age['x']
        assert_equal 2, age['o1']
        html = CurriculumGraph.html(g, {}, {})
        assert_includes html, "class='cg-node cg-a2' style='left: #{g[:nodes].find { |n| n[:slug] == 'o1' }[:x]}px;"
        assert_equal ['Sek I', 'Sek I und Oberstufe', 'Oberstufe'], html.scan(%r{<span class='cg-swatch cg-a\d'></span>([^<]+)</li>}).flatten
    end

    def test_a_long_edge_passes_each_column_between
        g = big
        depth = g[:nodes].to_h { |n| [n[:slug], n[:depth]] }
        g[:edges].each do |a, b|
            assert_equal depth[b] - depth[a] - 1, g[:routes][[a, b]].size, "#{a} → #{b}"
        end
    end

    def test_no_two_boxes_overlap
        boxes = big[:nodes]
        boxes.combination(2).each do |p, q|
            apart = p[:x] + CurriculumGraph::BOX_W <= q[:x] || q[:x] + CurriculumGraph::BOX_W <= p[:x] ||
                    p[:y] + CurriculumGraph::BOX_H <= q[:y] || q[:y] + CurriculumGraph::BOX_H <= p[:y]
            assert apart, "#{p[:slug]} and #{q[:slug]} overlap"
        end
    end

    def test_no_arrow_runs_through_a_box
        g = big
        at = g[:nodes].to_h { |n| [n[:slug], n] }
        g[:edges].each do |a, b|
            arrow = CurriculumGraph.edge_path(at[a], at[b], g[:routes][[a, b]], g[:nodes])
            points = CurriculumGraph.points(arrow[:d]) + arrow[:head]
            # (its own ends: near a corner they lie inside the plain rectangle)
            g[:nodes].reject { |n| [a, b].include?(n[:slug]) }.each do |n|
                inside = points.select do |px, py|
                    px > n[:x] + 0.5 && px < n[:x] + CurriculumGraph::BOX_W - 0.5 && py > n[:y] + 0.5 && py < n[:y] + CurriculumGraph::BOX_H - 0.5
                end
                assert_empty inside, "#{a} → #{b} runs through #{n[:slug]}"
            end
        end
    end

    # how far a point lies outside a box's rounded outline (negative: inside)
    def outside(box, pt)
        w = CurriculumGraph::BOX_W / 2.0
        h = CurriculumGraph::BOX_H / 2.0
        r = CurriculumGraph::RADIUS
        dx = (pt[0] - (box[:x] + w)).abs - (w - r)
        dy = (pt[1] - (box[:y] + h)).abs - (h - r)
        Math.hypot([dx, 0].max, [dy, 0].max) + [[dx, dy].max, 0].min - r
    end

    def test_arrows_into_one_box_end_at_different_points_on_its_border_aimed_at_its_centre
        g = big
        at = g[:nodes].to_h { |n| [n[:slug], n] }
        box = at['o3']
        centre = [box[:x] + CurriculumGraph::BOX_W / 2.0, box[:y] + CurriculumGraph::BOX_H / 2.0]
        arrows = g[:edges].select { |_, b| b == 'o3' }.map { |a, b| CurriculumGraph.edge_path(at[a], at[b], g[:routes][[a, b]], g[:nodes]) }
        assert arrows.size >= 2
        assert_equal arrows.size, arrows.map { |arrow| arrow[:head][0].map(&:round) }.uniq.size
        arrows.each do |arrow|
            tip, wing, other = arrow[:head]
            base = [(wing[0] + other[0]) / 2, (wing[1] + other[1]) / 2]
            # the tip on the rounded outline
            assert_in_delta 0, outside(box, tip), 0.05, "tip #{tip} not on the border"
            # the head points at the centre, and the path ends at its base
            # heading the same way
            assert_in_delta Math.atan2(centre[1] - tip[1], centre[0] - tip[0]), Math.atan2(tip[1] - base[1], tip[0] - base[0]), 0.01
            points = CurriculumGraph.points(arrow[:d])
            assert_in_delta 0, Math.hypot(points[-1][0] - base[0], points[-1][1] - base[1]), 0.15
            assert_in_delta Math.atan2(tip[1] - base[1], tip[0] - base[0]), Math.atan2(points[-1][1] - points[-3][1], points[-1][0] - points[-3][0]), 0.05
        end
    end

    # near a corner the rim is the quarter circle, at the radius from its centre
    def test_the_rim_follows_the_rounded_corner
        w = CurriculumGraph::BOX_W / 2.0
        h = CurriculumGraph::BOX_H / 2.0
        r = CurriculumGraph::RADIUS
        u = [w, h].then { |x, y| l = Math.hypot(x, y); [x / l, y / l] }
        pt = CurriculumGraph.rim([0.0, 0.0], u)
        assert_in_delta r, Math.hypot(pt[0] - (w - r), pt[1] - (h - r)), 1e-9
        assert pt[0] < w && pt[1] < h, 'inside the plain rectangle'
        # on a straight side as before
        assert_equal [w, 0.0], CurriculumGraph.rim([0.0, 0.0], [1.0, 0.0])
        x, y = CurriculumGraph.rim([10.0, 20.0], [-0.2, 0.98].then { |a, b| l = Math.hypot(a, b); [a / l, b / l] })
        assert_in_delta 20 + h, y, 1e-9
        assert x < 10
    end

    def test_the_search_is_no_worse_than_the_quick_order_and_repeats_itself
        nodes, edges = big_inputs
        found = CurriculumGraph.search(nodes, edges, :restarts => 3, :steps => 300)
        assert found[:cost] <= found[:quick]
        frame = CurriculumGraph.frame(nodes, edges)
        assert CurriculumGraph.fits?(frame, found[:orders])
        assert_equal found[:orders], CurriculumGraph.search(nodes, edges, :restarts => 3, :steps => 300)[:orders]
        # stored and read back, it gives the picture of that order
        Dir.mktmpdir do |dir|
            path = File.join(dir, 'graph.json')
            key = CurriculumGraph.key(nodes, edges)
            File.write(path, JSON.generate({ 'layouts' => [{ 'key' => key, 'orders' => CurriculumGraph.dump_orders(found[:orders]) }] }))
            assert_equal found[:orders], CurriculumGraph.stored(key, path)
            assert_nil CurriculumGraph.stored(CurriculumGraph.key(nodes, edges + [%w[s3 o2]]), path)
        end
        assert_equal CurriculumGraph.picture(frame, found[:orders]), CurriculumGraph.layout(nodes, edges, :orders => found[:orders])
        # an order that does not fit gives the quick one
        refute CurriculumGraph.fits?(frame, found[:orders].merge(0 => ['nope']))
        assert_equal CurriculumGraph.layout(nodes, edges), CurriculumGraph.layout(nodes, edges, :orders => found[:orders].merge(0 => ['nope']))
    end

    def test_the_key_changes_with_what_the_layout_is_made_of
        nodes = %w[a b c].map { |s| node(s, 'katas', [], ['Sek I']) }
        k = CurriculumGraph.key(nodes, [%w[a b]])
        assert_equal k, CurriculumGraph.key(nodes.reverse, [%w[a b], %w[a b]])
        refute_equal k, CurriculumGraph.key(nodes, [%w[a c]])
        refute_equal k, CurriculumGraph.key(nodes[0, 2], [%w[a b]])
        # a title, the lines of the plans (the colour) change nothing in it
        assert_equal k, CurriculumGraph.key(nodes.map { |n| n.merge(:title => 'X', :topics => ['y'], :levels => ['Oberstufe']) }, [%w[a b]])
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
        html = CurriculumGraph.html(graph, sections, { 'alone' => { :title => 'Allein' } })
        assert_equal graph[:nodes].size, html.scan("class='cg-node").size
        assert_equal graph[:edges].size, html.scan("class='cg-edge'").size
        assert_includes html, "href='/a' target='_blank'"
        assert_includes html, 'Nicht im Bild: eine Aktivität, die auf keiner aufbaut'
    end

    # The stored order of the picture on /lehrplan (curriculum-graph.json)
    # fits the activities of the catalog and their requires:; the server
    # would otherwise fall back to the quick order
    def test_the_stored_picture_fits_the_activities
        content, slugs = CurriculumGraph.read_content(File.expand_path('../../..', __dir__))
        nodes, edges = CurriculumGraph.inputs(content, slugs)
        orders = CurriculumGraph.stored(CurriculumGraph.key(nodes, edges))
        assert orders, "The picture on /lehrplan is stale (an activity entered or left the catalog, or its requires: changed): run #{CurriculumGraph::COMMAND} and commit src/ruby/include/curriculum-graph.json"
        assert CurriculumGraph.fits?(CurriculumGraph.frame(nodes, edges), orders), "run #{CurriculumGraph::COMMAND}"
    end
end
