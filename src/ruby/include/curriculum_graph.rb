require 'cgi'
require 'digest'
require 'json'
require 'set'
require 'yaml'
require_relative 'content_parts'
require_relative 'curriculum'
require_relative 'page_kinds'

# The graph of »Baut auf … auf« on /lehrplan, for teachers: every activity
# that builds on another or that another builds on (meta requires:), as a
# box coloured by age (what only the plans of Sek I name, what both name,
# what only the Oberstufe's name), with an arrow from what comes first.
# Left to right by depth: a box stands one column right of the longest chain
# of activities it builds on. An arrow that spans columns takes room of its
# own in each column it passes (a pass), so it never runs through a box.
# (Bands by age, top to bottom, pulled the boxes away from their neighbours:
# the arrows ran long and steep, the search scored them three times worse.)
#
# The order of the boxes and passes within a column is searched
# once in development (search: restarts and simulated annealing, scoring
# crossings, the arrows' lengths and bends, arrows that would need a detour)
# and stored in STORE, with a key of what it was made from (key); the server
# only reads it. src/scripts/curriculum-graph.rb searches and stores it, and
# test/curriculum_graph_test.rb fails while the stored order no longer
# fits the activities. Without a fitting one the server orders them quickly
# (by the mean height of their neighbours, a few passes back and forth); in
# development it searches itself, briefly, and keeps the result in memory.
#
# Activities that neither build on one nor are built on are not in the
# picture; the page lists them below it. The boxes are HTML over an SVG of
# the arrows (buttons, Bootstrap icons); lehrplan.html highlights a name of
# the plans or a box and the way to it.
module CurriculumGraph
    BOX_W = 118
    BOX_H = 36
    # the corner of a box (lehrplan.html, .cg-node: border-radius, the outer
    # edge of its border, as the box is sized with its border)
    RADIUS = 7.0
    COL = 156
    ROW = 46
    PAD = 14
    SWEEPS = 6
    PLACING = 12
    # an arrow's head, drawn as a triangle at the end of its path
    HEAD_LEN = 7.0
    HEAD_W = 6.0

    STORE = File.expand_path('curriculum-graph.json', __dir__)
    COMMAND = 'src/scripts/curriculum-graph.rb'
    # goes up when what the layout is made of changes, so that a stored
    # order made the old way no longer fits
    VERSION = 2
    # the search: how often it starts afresh, how many moves each time; and
    # how much each thing counts (an arrow's length and bends in rows)
    RESTARTS = 24
    STEPS = 2500
    QUICK_RESTARTS = 3
    CROSSING = 4.0
    STEEP = 0.5
    BEND = 1.0
    THROUGH = 50.0
    DETOUR = 1.5

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # The ages, a colour each: what the plans of Sek I name only, what both
    # name, what only the Oberstufe's names (curriculum:, docs/lehrplan.md);
    # an activity without a line counts as both
    AGES = ['Sek I', 'Sek I und Oberstufe', 'Oberstufe'].freeze
    # the room an arrow takes where it passes a column (a dummy, as in a
    # Sugiyama layout): no box stands there, so no arrow runs through one
    PASS = 20

    def self.age_of(levels)
        sek = levels.include?('Sek I')
        ober = levels.include?('Oberstufe')
        return 0 if sek && !ober
        return 2 if ober && !sek

        1
    end

    # What a layout is made from, for telling whether a stored one fits:
    # the activities and the arrows (not titles, sections or the lines of
    # the plans, which change nothing in it but colours and names)
    def self.key(nodes, edges)
        data = [VERSION, nodes.map { |n| n[:slug] }.sort, edges.map { |a, b| [a, b] }.uniq.sort]
        Digest::SHA256.hexdigest(JSON.generate(data))[0, 16]
    end

    # Everything about a layout but the order within each column: depths,
    # the items of each column (boxes, and passes of arrows
    # over several columns) and how they are chained.
    def self.frame(nodes, edges)
        by_slug = nodes.to_h { |n| [n[:slug], n] }
        edges = edges.select { |a, b| by_slug.key?(a) && by_slug.key?(b) && a != b }.uniq
        linked = edges.flatten.uniq
        alone = nodes.map { |n| n[:slug] } - linked
        before = Hash.new { |hash, key| hash[key] = [] }
        edges.each { |a, b| before[b] << a }
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
        # every item in a column: the boxes and, for an arrow that spans
        # several columns, a pass in each column between
        item = {}
        linked.each { |slug| item[slug] = { :depth => depth[slug], :room => ROW, :rank => order.index(slug) } }
        chains = {}
        left = Hash.new { |hash, key| hash[key] = [] }
        right = Hash.new { |hash, key| hash[key] = [] }
        edges.each do |a, b|
            span = depth[b] - depth[a]
            next unless span >= 1

            chain = [a]
            (1...span).each do |k|
                id = "#{a}>#{b}##{k}"
                item[id] = { :depth => depth[a] + k, :room => PASS, :rank => order.index(a) + k * 0.01, :pass => true }
                chain << id
            end
            chain << b
            chains[[a, b]] = chain
            chain.each_cons(2) { |p, q| right[p] << q; left[q] << p }
        end
        item.each_key { |id| left[id]; right[id] }
        groups = item.keys.group_by { |id| item[id][:depth] }.sort.to_h.transform_values { |ids| ids.sort_by { |id| item[id][:rank] } }
        # as high as the tallest column
        size = groups.values.map { |ids| ids.sum { |id| item[id][:room] } }.max || 0
        segments = chains.values.flat_map { |chain| chain.each_cons(2).to_a }
        { :by_slug => by_slug, :edges => edges, :linked => linked, :alone => alone, :depth => depth,
          :item => item, :chains => chains, :left => left, :right => right, :groups => groups,
          :size => size, :height => size + PAD * 2, :segments => segments,
          :columns => segments.group_by { |a, _| item[a][:depth] }.values,
          :passes => item.keys.select { |id| item[id][:pass] } }
    end

    # each column stacked in the middle of the picture, in its order
    def self.stack(frame, groups)
        item = frame[:item]
        centre = {}
        groups.each_value do |ids|
            total = ids.sum { |id| item[id][:room] }
            at = PAD + (frame[:size] - total) / 2.0
            ids.each { |id| centre[id] = at + item[id][:room] / 2.0; at += item[id][:room] }
        end
        centre
    end

    def self.mean(list, centre, own)
        list.empty? ? own : list.sum { |n| centre[n] } / list.size.to_f
    end

    # The quick order: within each column by the mean height of the
    # neighbours on one side, a few passes back and forth
    def self.sweep(frame, groups)
        groups = groups.transform_values(&:dup)
        centre = stack(frame, groups)
        SWEEPS.times do |sweep|
            near = sweep.even? ? frame[:left] : frame[:right]
            (sweep.even? ? groups.keys : groups.keys.reverse).each do |d|
                groups[d] = groups[d].sort_by.with_index { |id, i| [mean(near[id], centre, centre[id]), i] }
                centre = stack(frame, groups)
            end
        end
        groups
    end

    # The height of each item, given the order: each as near the mean
    # height of what it is joined to as the items around it and the
    # picture allow, again and again until nothing moves (a few passes
    # left a parent high above its children)
    def self.place(frame, groups)
        centre = stack(frame, groups)
        item = frame[:item]
        size = frame[:size]
        PLACING.times do
            moved = 0.0
            groups.each_value do |ids|
                want = ids.map { |id| mean(frame[:left][id] + frame[:right][id], centre, centre[id]) }
                room = ids.map { |id| item[id][:room] }
                # down the column: where it wants, below the one above and
                # inside the picture; then up: above the one below and inside
                # the picture (as high as the tallest column, so all fit)
                spots = []
                ids.each_index do |i|
                    low = i.zero? ? PAD + room[i] / 2.0 : spots[i - 1] + (room[i - 1] + room[i]) / 2.0
                    spots << [want[i], low].max
                end
                (ids.size - 1).downto(0) do |i|
                    high = i == ids.size - 1 ? PAD + size - room[i] / 2.0 : spots[i + 1] - (room[i] + room[i + 1]) / 2.0
                    spots[i] = [spots[i], high].min
                end
                ids.each_with_index do |id, i|
                    moved = [moved, (centre[id] - spots[i]).abs].max
                    centre[id] = spots[i]
                end
            end
            break if moved < 0.5
        end
        centre
    end

    # The picture for an order: { nodes: [{ …, depth:, age:, x:, y: }],
    # edges:, routes: { [from, to] => [[x, y]] }, width:, height:,
    # alone: [slug] }
    def self.picture(frame, groups)
        centre = place(frame, groups)
        item = frame[:item]
        x = ->(d) { PAD + d * COL }
        placed = frame[:linked].map do |slug|
            n = frame[:by_slug][slug]
            n.merge(:depth => frame[:depth][slug], :age => age_of(n[:levels] || []), :x => x.(frame[:depth][slug]), :y => centre[slug] - BOX_H / 2.0)
        end.sort_by { |n| [n[:depth], n[:y]] }
        routes = frame[:chains].transform_values { |chain| chain[1...-1].map { |id| [x.(item[id][:depth]), centre[id]] } }
        width = PAD * 2 + ((frame[:depth].values.max || 0) * COL) + BOX_W
        { :nodes => placed, :edges => frame[:edges], :routes => routes,
          :width => width, :height => frame[:height].round, :alone => frame[:alone] }
    end

    # How bad an order is, from the heights alone: crossings between two
    # columns, the arrows' rise (and more so a steep one, which runs past
    # the boxes beside its ends), and the bends where an arrow passes a column
    def self.cost(frame, centre)
        crossings = 0
        frame[:columns].each do |segments|
            segments.combination(2) do |(a, b), (c, d)|
                next if a == c || b == d

                crossings += 1 if (centre[a] - centre[c]) * (centre[b] - centre[d]) < 0
            end
        end
        rise = 0.0
        steep = 0.0
        frame[:segments].each do |a, b|
            dy = (centre[b] - centre[a]).abs / ROW
            rise += dy
            steep += dy * dy
        end
        bends = frame[:passes].sum do |id|
            ((centre[id] - centre[frame[:left][id][0]]) - (centre[frame[:right][id][0]] - centre[id])).abs / ROW
        end
        CROSSING * crossings + rise + STEEP * steep + BEND * bends
    end

    # The cost with the arrows drawn: an arrow through a box counts much, one
    # that cannot aim straight and leaves or arrives through the side facing
    # the gap a little
    def self.drawn_cost(frame, groups)
        g = picture(frame, groups)
        at = g[:nodes].to_h { |n| [n[:slug], n] }
        through = 0
        detours = 0
        g[:edges].each do |a, b|
            arrow = edge_path(at[a], at[b], g[:routes][[a, b]] || [], g[:nodes])
            detours += 1 if arrow[:kind] == :lean
            points = points(arrow[:d])
            through += g[:nodes].count do |n|
                next false if n[:slug] == a || n[:slug] == b

                points.any? { |px, py| px > n[:x] + 0.5 && px < n[:x] + BOX_W - 0.5 && py > n[:y] + 0.5 && py < n[:y] + BOX_H - 0.5 }
            end
        end
        cost(frame, place(frame, groups)) + THROUGH * through + DETOUR * detours
    end

    # The search for a good order: the quick one, and restarts from orders
    # shuffled and then sorted the quick way, each improved by simulated
    # annealing (swapping two items of a column or moving one within it);
    # the best by the cost with the arrows drawn. The same inputs give the
    # same order (the random numbers start from the key).
    # { orders: { depth => [id] }, cost:, quick: }
    def self.search(nodes, edges, restarts: RESTARTS, steps: STEPS)
        frame = frame(nodes, edges)
        rng = Random.new(key(nodes, edges).to_i(16))
        quick = sweep(frame, frame[:groups])
        best = quick
        best_cost = quick_cost = drawn_cost(frame, quick)
        pool = frame[:groups].keys.flat_map { |k| [k] * (frame[:groups][k].size - 1) }
        return { :orders => best, :cost => best_cost, :quick => quick_cost } if pool.empty?

        restarts.times do |r|
            groups = r.zero? ? quick : sweep(frame, frame[:groups].transform_values { |ids| ids.shuffle(:random => rng) })
            current = cost(frame, place(frame, groups))
            kept = groups
            kept_cost = current
            steps.times do |s|
                temperature = 2.0 * (0.01**(s / steps.to_f))
                k = pool.sample(:random => rng)
                ids = groups[k].dup
                i = rng.rand(ids.size)
                j = rng.rand(ids.size - 1)
                j += 1 if j >= i
                if rng.rand < 0.5
                    ids[i], ids[j] = ids[j], ids[i]
                else
                    ids.insert(j, ids.delete_at(i))
                end
                trial = groups.merge(k => ids)
                c = cost(frame, place(frame, trial))
                next unless c <= current || rng.rand < Math.exp((current - c) / temperature)

                groups = trial
                current = c
                if c < kept_cost
                    kept = groups
                    kept_cost = c
                end
            end
            total = drawn_cost(frame, kept)
            if total < best_cost
                best = kept
                best_cost = total
            end
        end
        { :orders => best, :cost => best_cost, :quick => quick_cost }
    end

    # The stored orders for a key, or nil: { depth => [id] }
    def self.stored(key, path = STORE)
        data = JSON.parse(File.read(path))
        entry = (data['layouts'] || []).find { |layout| layout['key'] == key }
        entry && entry['orders'].to_h { |d, ids| [d, ids] }
    rescue Errno::ENOENT, JSON::ParserError
        nil
    end

    # orders as stored: [[depth, [id]]]
    def self.dump_orders(orders)
        orders.keys.sort.map { |d| [d, orders[d]] }
    end

    # whether orders hold exactly the items of each column
    def self.fits?(frame, orders)
        orders.keys.sort == frame[:groups].keys &&
            frame[:groups].all? { |k, ids| orders[k].sort == ids.sort }
    end

    # nodes: [{ slug:, title:, section:, topics: [anchor], levels: [level] }]
    # in catalog order; edges: [[from, to]] between them; orders: what the
    # search found (or nil: the quick order)
    def self.layout(nodes, edges, orders: nil)
        frame = frame(nodes, edges)
        groups = orders && fits?(frame, orders) ? orders : sweep(frame, frame[:groups])
        picture(frame, groups)
    end

    # The nodes and edges of the activities students see in the catalog
    # (slugs, in its order), with the anchors of their names of the plans.
    def self.inputs(content, slugs, development: false)
        nodes = slugs.map do |slug|
            entry = content[slug] || {}
            { :slug => slug, :title => CGI.unescapeHTML(PageKinds.plain_title(entry)), :section => entry[:section],
              :topics => Curriculum.entries(entry[:curriculum]).map { |level, name, _| Curriculum.anchor(level, name) }.uniq,
              :levels => Curriculum.entries(entry[:curriculum]).map(&:first).uniq }
        end
        edges = slugs.flat_map do |slug|
            PageKinds.requires_of(slug, content, :development => development).map { |name, _| [name, slug] }
        end
        [nodes, edges]
    end

    @searched = {}
    @searching = Mutex.new

    # The picture for the page: the stored order if it fits; otherwise in
    # development one searched briefly (kept in memory until the inputs
    # change), in production the quick one. stale: no stored order fits.
    def self.from_content(content, slugs, development: false)
        nodes, edges = inputs(content, slugs, :development => development)
        k = key(nodes, edges)
        orders = stored(k)
        stale = orders.nil?
        if stale && development
            orders = @searching.synchronize do
                @searched[k] ||= search(nodes, edges, :restarts => QUICK_RESTARTS)[:orders]
            end
        end
        layout(nodes, edges, :orders => orders).merge(:stale => stale)
    end

    # The catalog and the meta lines the picture is made of, read from the
    # files (as main.rb reads them, without rendering the pages), for the
    # command that stores the layout and the test that it is current:
    # [content, slugs] as from_content takes them. root: the repository's.
    def self.read_content(root, development: false)
        dir = File.join(root, 'src/content')
        content = {}
        slugs = []
        YAML.load_file(File.join(dir, 'sections.yaml')).each do |section|
            next if section['catalog']

            (section['entries'] || []).each do |path|
                dev_only = path.start_with?('.')
                slug = path.delete_prefix('.')
                file = ContentParts.page_file(File.join(dir, slug), slug)
                next unless file && File.file?(file)

                text = File.read(file)
                meta = YAML.safe_load(text[/<div class=.meta.>\s*\n(.*?)\n\s*<\/div>/m, 1].to_s) || {}
                title = text[/^#\s+(.+)$/, 1].to_s.strip
                content[slug] = { :title => title, :section => section['key'], :dev_only => dev_only,
                                  :teacher_only => section['teacher_only'] == true, :hidden_section => section['hidden'] == true,
                                  :requires => Array(meta['requires']).map(&:to_s).reject(&:empty?), :curriculum => meta['curriculum'] }
                slugs << slug unless section['hidden'] || (dev_only && !development)
            end
        end
        [content, slugs]
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

    # Where a ray from a box's centre c along the unit vector u leaves the
    # box: on its straight sides, or near a corner on the quarter circle
    def self.rim(c, u)
        half_w = BOX_W / 2.0
        half_h = BOX_H / 2.0
        t = [u[0].zero? ? Float::INFINITY : half_w / u[0].abs, u[1].zero? ? Float::INFINITY : half_h / u[1].abs].min
        dx = u[0] * t
        dy = u[1] * t
        if dx.abs > half_w - RADIUS && dy.abs > half_h - RADIUS
            # the corner's circle: its centre k, the farther of the two
            # points where the ray meets it
            k = [(half_w - RADIUS) * (dx <=> 0), (half_h - RADIUS) * (dy <=> 0)]
            b = u[0] * k[0] + u[1] * k[1]
            t = b + Math.sqrt([b * b - k[0]**2 - k[1]**2 + RADIUS**2, 0].max)
        end
        [c[0] + u[0] * t, c[1] + u[1] * t]
    end

    # The points along a path's segments (M, C, L), sampled
    def self.points(d)
        tokens = d.scan(/[MCL]|-?\d+(?:\.\d+)?/)
        points = []
        at = nil
        until tokens.empty?
            case tokens.shift
            when 'M' then at = [tokens.shift.to_f, tokens.shift.to_f]
            when 'L'
                q = [tokens.shift.to_f, tokens.shift.to_f]
                (0..20).each { |i| t = i / 20.0; points << [at[0] + (q[0] - at[0]) * t, at[1] + (q[1] - at[1]) * t] }
                at = q
            when 'C'
                c1 = [tokens.shift.to_f, tokens.shift.to_f]
                c2 = [tokens.shift.to_f, tokens.shift.to_f]
                q = [tokens.shift.to_f, tokens.shift.to_f]
                (0..40).each do |i|
                    t = i / 40.0
                    u = 1 - t
                    points << (0..1).map { |n| u**3 * at[n] + 3 * u * u * t * c1[n] + 3 * u * t * t * c2[n] + t**3 * q[n] }
                end
                at = q
            end
        end
        points
    end

    # An arrow from a box to another (the site's rule for diagrams: it aims
    # at the centres, its head follows it, the tip on the border): { d: the
    # path, head: [tip, wing, wing], kind: }. The path starts on the source's
    # rim and stops at the head's base; the head lies along the path's last
    # direction, which points at the target's centre, its tip on the rim.
    # To the next column a straight line between the two centres (kind
    # :line), so that the arrows of a box with many children fan out. Where
    # that line would run through another box (boxes: all of them), the
    # arrow leaves on the line to the target's centre and curves into the
    # target's side facing the gap, aimed at its centre (:fan); only where
    # that curve too would cross a box does it leave through the side facing
    # the gap (:lean). Over several columns it runs level through the passes
    # it takes in the columns between (:long), its first stretch leaving
    # along the line from the source's centre and its last one arriving on
    # the line to the target's centre where they stay clear.
    def self.edge_path(from, to, through = [], boxes = [])
        half_w = BOX_W / 2.0
        half_h = BOX_H / 2.0
        cs = [from[:x] + half_w, from[:y] + half_h]
        ct = [to[:x] + half_w, to[:y] + half_h]
        others = boxes.reject { |n| n.equal?(from) || n.equal?(to) || (n[:slug] && (n[:slug] == from[:slug] || n[:slug] == to[:slug])) }
        unit = ->(v) { l = Math.hypot(*v); l.zero? ? [1.0, 0.0] : [v[0] / l, v[1] / l] }
        inside = ->(pt) { others.any? { |n| pt[0] > n[:x] + 0.5 && pt[0] < n[:x] + BOX_W - 0.5 && pt[1] > n[:y] + 0.5 && pt[1] < n[:y] + BOX_H - 0.5 } }
        clear = ->(pts) { pts.none? { |pt| inside.(pt) } }
        line_pts = ->(p, q) { (0..30).map { |i| t = i / 30.0; [p[0] + (q[0] - p[0]) * t, p[1] + (q[1] - p[1]) * t] } }
        curve_pts = lambda do |p, c1, c2, q|
            (0..40).map do |i|
                t = i / 40.0
                u = 1 - t
                (0..1).map { |k| u**3 * p[k] + 3 * u * u * t * c1[k] + 3 * u * t * t * c2[k] + t**3 * q[k] }
            end
        end
        # arriving along u: the tip on the target's rim, the head's base
        # behind it, and the two wings
        arrive = lambda do |u|
            tip = rim(ct, [-u[0], -u[1]])
            base = [tip[0] - u[0] * HEAD_LEN, tip[1] - u[1] * HEAD_LEN]
            n = [-u[1] * HEAD_W / 2, u[0] * HEAD_W / 2]
            [tip, base, [tip, [base[0] + n[0], base[1] + n[1]], [base[0] - n[0], base[1] - n[1]]], u]
        end
        arrow = ->(d, head, kind) { { :d => d, :head => head, :kind => kind } }
        if through.empty?
            u = unit.([ct[0] - cs[0], ct[1] - cs[1]])
            p0 = rim(cs, u)
            tip, base, head = arrive.(u)
            return arrow.(format('M%.1f %.1f L%.1f %.1f', *p0, *base), head, :line) if clear.(line_pts.(p0, tip))
        end
        # the ends through the sides facing the gap, aimed at the centres,
        # leaning towards the other end no further than that side allows
        gap = COL - BOX_W
        reach = half_w + gap / 2.0
        lean = (half_h - 5) / half_w * reach
        first_y = through.empty? ? ct[1] : through[0][1]
        last_y = through.empty? ? cs[1] : through[-1][1]
        level = [1.0, 0.0]
        # a curve from p heading ua to q, arriving heading ub; its handles
        # a share of the distance, so that it bends along the whole way and
        # not just before the head
        curve = lambda do |p, ua, q, ub|
            k = Math.hypot(q[0] - p[0], q[1] - p[1]) * 0.4
            c1 = [p[0] + ua[0] * k, p[1] + ua[1] * k]
            c2 = [q[0] - ub[0] * k, q[1] - ub[1] * k]
            [format('C%.1f %.1f %.1f %.1f %.1f %.1f', *c1, *c2, *q), curve_pts.(p, c1, c2, q)]
        end
        # the start: on the line to aim if that stays clear
        start = lambda do |aim|
            u = unit.([aim[0] - cs[0], aim[1] - cs[1]])
            p0 = rim(cs, u)
            return [p0, u, true] if u[0] > 0 && clear.(line_pts.(p0, aim))

            u = unit.([reach, ((first_y - cs[1]) * 0.3).clamp(-lean, lean)])
            [rim(cs, u), u, false]
        end
        # the end: on the line from from_pt to the centre if that stays
        # clear, else through the side facing the gap
        finish = lambda do |from_pt|
            u = unit.([ct[0] - from_pt[0], ct[1] - from_pt[1]])
            ends = arrive.(u)
            return ends + [true] if u[0] > 0 && clear.(line_pts.(from_pt, ends[0]))

            arrive.(unit.([reach, ((ct[1] - last_y) * 0.3).clamp(-lean, lean)])) + [false]
        end
        if through.empty?
            tip, base, head, u1, = finish.(cs)
            # leaving on the line to the target's centre, so that a box's
            # arrows fan out, then into the target's side facing the gap;
            # where that curve would run through a box, leaving that side too
            u = unit.([ct[0] - cs[0], ct[1] - cs[1]])
            p0 = rim(cs, u)
            seg, pts = curve.(p0, u, base, u1)
            return arrow.(format('M%.1f %.1f ', *p0) + seg, head, :fan) if u[0] > 0 && clear.(pts + line_pts.(base, tip))

            p0, u0, = start.(ct)
            seg, = curve.(p0, u0, base, u1)
            return arrow.(format('M%.1f %.1f ', *p0) + seg, head, :lean)
        end
        p0, u0, = start.([through[0][0], through[0][1]])
        d = +format('M%.1f %.1f ', *p0)
        at = p0
        heading = u0
        through.each do |px, py|
            seg, = curve.(at, heading, [px, py], level)
            d << seg << format(' L%.1f %.1f ', px + BOX_W, py)
            at = [px + BOX_W, py]
            heading = level
        end
        _, base, head, u1, = finish.(at)
        seg, = curve.(at, heading, base, u1)
        arrow.(d << seg, head, :long)
    end

    # The picture as HTML; the boxes coloured by age (lehrplan.html,
    # .cg-a0 … .cg-a2), their sections named in their titles
    def self.html(graph, sections, content)
        at = graph[:nodes].to_h { |n| [n[:slug], n] }
        io = +"<div class='cg-scroll'><div class='cg-graph' style='width: #{graph[:width]}px; height: #{graph[:height]}px;' data-stored='#{graph[:stale] ? 'no' : 'yes'}'>"
        io << "<svg class='cg-edges' width='#{graph[:width]}' height='#{graph[:height]}' viewBox='0 0 #{graph[:width]} #{graph[:height]}' aria-hidden='true'>"
        graph[:edges].each do |a, b|
            arrow = edge_path(at[a], at[b], (graph[:routes] || {})[[a, b]] || [], graph[:nodes])
            head = arrow[:head].map { |x, y| format('%.1f,%.1f', x, y) }.join(' ')
            io << "<g class='cg-edge' data-from='#{h(a)}' data-to='#{h(b)}'><path d='#{arrow[:d]}'/><polygon points='#{head}'/></g>"
        end
        io << '</svg>'
        graph[:nodes].each do |n|
            label = section_label(sections, n[:section])
            io << "<div class='cg-node cg-a#{n[:age]}' style='left: #{n[:x]}px; top: #{n[:y]}px;' data-slug='#{h(n[:slug])}' data-topics='#{h(n[:topics].join(' '))}'>"
            io << "<button type='button' class='cg-pick' title='#{h(n[:title])}#{label ? " · #{h(label)}" : ''}: der Weg dorthin'>#{h(short(n[:title]))}</button>"
            io << "<a class='cg-open' href='/#{h(n[:slug])}' target='_blank' rel='noopener' aria-label='#{h(n[:title])} öffnen' title='Öffnen'><i class='bi bi-box-arrow-up-right' aria-hidden='true'></i></a>"
            io << '</div>'
        end
        io << '</div></div>'
        # the ages, with their colours
        io << "<ul class='cg-legend'>"
        AGES.each_with_index { |age, i| io << "<li><span class='cg-swatch cg-a#{i}'></span>#{h(age)}</li>" }
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
