require 'cgi'
require 'set'
require_relative 'curriculum'
require_relative 'page_kinds'

# The graph of »Baut auf … auf« on /lehrplan, for teachers: every activity
# that builds on another or that another builds on (meta requires:), as a
# box coloured by its catalog section, with an arrow from what comes first.
# Left to right by depth: a box stands one column right of the longest chain
# of activities it builds on. Within a column the boxes are ordered by the
# mean height of their neighbours (a few passes back and forth), so that the
# arrows stay short and cross rarely. Activities that neither build on one
# nor are built on are not in the picture; the page lists them below it.
# The boxes are HTML over an SVG of the arrows (buttons, Bootstrap icons);
# lehrplan.html highlights a name of the plans or a box and the way to it.
module CurriculumGraph
    BOX_W = 118
    BOX_H = 36
    COL = 156
    ROW = 46
    PAD = 14
    SWEEPS = 6

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # nodes: [{ slug:, title:, section:, topics: [anchor] }] in catalog
    # order; edges: [[from, to]] between them. Returns
    # { nodes: [{ …, depth:, x:, y: }], edges:, width:, height:, alone: [slug] }
    def self.layout(nodes, edges)
        by_slug = nodes.to_h { |n| [n[:slug], n] }
        edges = edges.select { |a, b| by_slug.key?(a) && by_slug.key?(b) && a != b }.uniq
        linked = edges.flatten.uniq
        alone = nodes.map { |n| n[:slug] } - linked
        before = Hash.new { |hash, key| hash[key] = [] }
        after = Hash.new { |hash, key| hash[key] = [] }
        edges.each { |a, b| before[b] << a; after[a] << b }
        # the longest chain before each one; an edge back into the chain
        # being followed (a cycle) does not count
        depth = {}
        visit = lambda do |slug, seen|
            return depth[slug] if depth.key?(slug)
            return 0 if seen.include?(slug)

            d = before[slug].map { |p| visit.call(p, seen + [slug]) + 1 }.max || 0
            depth[slug] = d
        end
        linked.each { |slug| visit.call(slug, []) }
        order = nodes.map { |n| n[:slug] }
        columns = linked.group_by { |slug| depth[slug] }.transform_values { |list| list.sort_by { |slug| order.index(slug) } }
        tallest = columns.values.map(&:size).max || 0
        place = lambda do
            columns.to_h do |d, list|
                top = (tallest - list.size) * ROW / 2.0
                [d, list.each_with_index.to_h { |slug, i| [slug, top + i * ROW] }]
            end.values.reduce({}, :merge)
        end
        y = place.call
        SWEEPS.times do |sweep|
            neighbours = sweep.even? ? before : after
            keys = columns.keys.sort
            keys = keys.reverse if sweep.odd?
            keys.each do |d|
                columns[d] = columns[d].sort_by.with_index do |slug, i|
                    near = neighbours[slug]
                    [near.empty? ? y[slug] : near.sum { |n| y[n] } / near.size.to_f, i]
                end
                y = place.call
            end
        end
        # then each box as near the mean height of the boxes it is joined to
        # as the boxes above it in its column allow, the column shifted to
        # stand where it wants on the whole
        3.times do
            columns.keys.sort.each do |d|
                list = columns[d]
                want = list.to_h do |slug|
                    near = before[slug] + after[slug]
                    [slug, near.empty? ? y[slug] : near.sum { |n| y[n] } / near.size.to_f]
                end
                list = list.sort_by.with_index { |slug, i| [want[slug], i] }
                spots = []
                list.each { |slug| spots << (spots.empty? ? want[slug] : [want[slug], spots[-1] + ROW].max) }
                shift = (list.sum { |slug| want[slug] } - spots.sum) / list.size
                list.each_with_index { |slug, i| y[slug] = spots[i] + shift }
                columns[d] = list
            end
        end
        top = y.values.min || 0
        y.transform_values! { |v| v - top }
        placed = linked.map do |slug|
            by_slug[slug].merge(:depth => depth[slug], :x => PAD + depth[slug] * COL, :y => PAD + y[slug])
        end.sort_by { |n| [n[:depth], n[:y]] }
        width = PAD * 2 + ((depth.values.max || 0) * COL) + BOX_W
        { :nodes => placed, :edges => edges, :width => width, :height => PAD * 2 + (y.values.max || 0).round + BOX_H, :alone => alone }
    end

    # The nodes and edges of the activities students see in the catalog
    # (slugs, in its order), with the anchors of their names of the plans.
    def self.from_content(content, slugs, development: false)
        nodes = slugs.map do |slug|
            entry = content[slug] || {}
            { :slug => slug, :title => CGI.unescapeHTML(PageKinds.plain_title(entry)), :section => entry[:section],
              :topics => Curriculum.entries(entry[:curriculum]).map { |level, name, _| Curriculum.anchor(level, name) }.uniq }
        end
        edges = slugs.flat_map do |slug|
            PageKinds.requires_of(slug, content, :development => development).map { |name, _| [name, slug] }
        end
        layout(nodes, edges)
    end

    # Series whose pages share the first part of their titles: on a box their
    # short name and the second part (»Kata: Listen«)
    SERIES = { 'Katas' => 'Kata', 'Pip unter Funkelbach' => 'Pip' }.freeze

    # The title on a box, in two lines at most: up to a colon (a series'
    # pages: its short name and what follows), at most 40 characters
    def self.short(title)
        first, rest = title.split(/:\s/, 2)
        text = SERIES.key?(first) && rest ? "#{SERIES[first]}: #{rest}" : first.to_s.strip
        text.length > 40 ? "#{text[0, 39].rstrip}…" : text
    end

    # An arrow from the right edge of a box to the left edge of the next,
    # level at both ends: it aims along the boxes' middle line, its head
    # follows the curve, its tip on the border.
    def self.edge_path(from, to)
        x1 = from[:x] + BOX_W
        y1 = from[:y] + BOX_H / 2.0
        x2 = to[:x] - 1.5
        y2 = to[:y] + BOX_H / 2.0
        # the handles half the gap: longer ones overshoot each other in a
        # narrow gap, and a rising arrow kinks just before its tip
        bend = (x2 - x1) / 2.0
        format('M%.1f %.1f C%.1f %.1f %.1f %.1f %.1f %.1f', x1, y1, x1 + bend, y1, x2 - bend, y2, x2, y2)
    end

    # The section's place in the catalog, for its colour (lehrplan.html)
    def self.html(graph, sections, section_order, content)
        visible = section_order.reject { |key| (sections[key] || {})[:hidden] }
        colour = ->(section) { visible.index(section) || visible.size }
        at = graph[:nodes].to_h { |n| [n[:slug], n] }
        io = +"<div class='cg-scroll'><div class='cg-graph' style='width: #{graph[:width]}px; height: #{graph[:height]}px;'>"
        io << "<svg class='cg-edges' width='#{graph[:width]}' height='#{graph[:height]}' viewBox='0 0 #{graph[:width]} #{graph[:height]}' aria-hidden='true'>"
        io << "<defs><marker id='cg-head' viewBox='0 0 10 10' refX='10' refY='5' markerWidth='7' markerHeight='7' markerUnits='userSpaceOnUse' orient='auto-start-reverse'><path d='M0 0L10 5L0 10z' fill='context-stroke'/></marker></defs>"
        graph[:edges].each do |a, b|
            io << "<path class='cg-edge' data-from='#{h(a)}' data-to='#{h(b)}' d='#{edge_path(at[a], at[b])}' marker-end='url(#cg-head)'/>"
        end
        io << '</svg>'
        graph[:nodes].each do |n|
            label = section_label(sections, n[:section])
            io << "<div class='cg-node cg-c#{colour.(n[:section])}' style='left: #{n[:x]}px; top: #{n[:y]}px;' data-slug='#{h(n[:slug])}' data-topics='#{h(n[:topics].join(' '))}'>"
            io << "<button type='button' class='cg-pick' title='#{h(n[:title])}#{label ? " · #{h(label)}" : ''}: der Weg dorthin'>#{h(short(n[:title]))}</button>"
            io << "<a class='cg-open' href='/#{h(n[:slug])}' target='_blank' rel='noopener' aria-label='#{h(n[:title])} öffnen' title='Öffnen'><i class='bi bi-box-arrow-up-right' aria-hidden='true'></i></a>"
            io << '</div>'
        end
        io << '</div></div>'
        # the sections in the picture, with their colours
        shown = graph[:nodes].map { |n| n[:section] }.uniq.sort_by { |s| colour.(s) }
        io << "<ul class='cg-legend'>"
        shown.each { |s| io << "<li><span class='cg-swatch cg-c#{colour.(s)}'></span>#{h(section_label(sections, s) || s)}</li>" }
        io << '</ul>'
        unless graph[:alone].empty?
            links = graph[:alone].map { |slug| "<a href='/#{h(slug)}'>#{h(PageKinds.plain_title(content[slug]))}</a>" }
            what = graph[:alone].size == 1 ? 'eine Aktivität, die auf keiner aufbaut und auf der keine aufbaut' :
                "#{graph[:alone].size} Aktivitäten, die auf keiner aufbauen und auf denen keine aufbaut"
            io << "<details class='cg-alone'><summary>Nicht im Bild: #{what}</summary><p>#{links.join(', ')}</p></details>"
        end
        io
    end

    # The names of the plans that an activity in the picture carries, for
    # choosing one: [[level, [[anchor, name]]]]
    def self.topics(graph)
        present = graph[:nodes].flat_map { |n| n[:topics] }.to_set
        Curriculum::LEVELS.map do |level, names|
            [level, names.map { |name| [Curriculum.anchor(level, name), name] }.select { |anchor, _| present.include?(anchor) }]
        end.reject { |_, list| list.empty? }
    end

    def self.topic_select(graph)
        groups = topics(graph).map do |level, list|
            "<optgroup label='#{h(level)}'>#{list.map { |anchor, name| "<option value='#{h(anchor)}'>#{h(name)}</option>" }.join}</optgroup>"
        end
        "<label class='cg-choose'>Name der Pläne <select class='form-select form-select-sm cg-topic'><option value=''>keiner</option>#{groups.join}</select></label>"
    end

    def self.section_label(sections, key)
        label = (sections[key] || {})[:label]
        label && CGI.unescapeHTML(label.to_s.gsub('&shy;', ''))
    end
end
