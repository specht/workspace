require 'cgi'

# The Katas on the front page (docs/plan-katas.md): every set of the catalog
# section »Katas« (carousel: true in sections.yaml), in its order, with its
# idea in a few words (meta idea:) and how many Katas it has on each level;
# for a logged-in person how far they are (solved of the core Katas, and
# Extras besides) and, in a ring, how far in all; for teachers the line of
# the curriculum the set belongs to (meta curriculum:). A set a teacher
# pinned for the person's group is marked and scrolled into view.
#
# Plain Ruby, tested on its own (test/kata_map_test.rb).
module KataMap
    LEVELS = { 1 => 'Einstieg', 2 => 'Übung', 3 => 'Knobelei' }.freeze

    Stop = Struct.new(:slug, :title, :idea, :curriculum, :icon, :counts, keyword_init: true)

    # A stop from a page's content entry: counts are { 1 => n, 2 => n,
    # 3 => n, :extra => n } over the tasks on the page.
    def self.stop(slug, content)
        tasks = content[:tasks] ? content[:tasks][:on_page].map { |id| content[:tasks][:by_id][id] } : []
        counts = Hash.new(0)
        tasks.each { |t| t['extra'] ? counts[:extra] += 1 : counts[t['level'] || 1] += 1 }
        Stop.new(:slug => slug, :title => title(content[:title]), :idea => content[:idea], :curriculum => content[:curriculum], :icon => content[:icon], :counts => counts)
    end

    # The picture of a set in its low band (16:6) is its card's picture,
    # ZOOM times the band's width (default.css, .kata-set-picture img), its
    # point of focus (meta focus: x:y in percent of the picture, where Pip or
    # Bello stands) in the middle of the band. The point keeps far enough
    # from the edges that the picture covers the band (a picture at least
    # half as high as wide). The custom properties for the <img>.
    ZOOM = 1.9
    EDGE_X = (50.0 / ZOOM).ceil
    EDGE_Y = 20

    def self.picture_style(focus)
        fx, fy = focus || [50, 50]
        "--fx: #{fx.clamp(EDGE_X, 100 - EDGE_X)}%; --fy: #{fy.clamp(EDGE_Y, 100 - EDGE_Y)}%;"
    end

    # »Katas: Erste Funktionen« -> »Erste Funktionen«
    def self.title(text)
        CGI.unescapeHTML(text.to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '')).sub(/\AKatas?:\s*/, '')
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # »4 Einstieg · 4 Übung · 2 Knobelei · 2 Extras«
    def self.counts_text(counts)
        parts = LEVELS.filter_map { |level, name| "#{counts[level]} #{name}" if counts[level].to_i > 0 }
        parts << "#{counts[:extra]} #{counts[:extra] == 1 ? 'Extra' : 'Extras'}" if counts[:extra].to_i > 0
        parts.join(' · ')
    end

    # The Katas on the front page instead of their cards: a ring with how
    # many are solved in all (or how many there are, for a visitor), then
    # every set in a strip that scrolls sideways. progress: { slug => {
    # done:, total:, extras_done: } }, nil for a visitor; pinned: the slugs
    # a teacher pinned for the person's groups (the first is scrolled into
    # the middle); images: { slug => the <img> of its card }.
    def self.carousel_html(stops, progress: nil, teacher: false, pinned: [], images: {})
        core = ->(stop) { stop.counts.reject { |k, _| k == :extra }.values.sum }
        io = +"<div class='kata-carousel'>"
        io << "<div class='kata-total'>"
        if progress
            total = stops.sum { |stop| progress[stop.slug] ? progress[stop.slug][:total].to_i : core.(stop) }
            solved = stops.sum { |stop| progress[stop.slug].to_h[:done].to_i }
            extras = stops.sum { |stop| progress[stop.slug].to_h[:extras_done].to_i }
            share = total > 0 ? (100.0 * solved / total).round : 0
            io << "<div class='kata-ring' style='--share: #{share}%' role='img' aria-label='#{solved} von #{total} Katas gelöst'>"
            io << "<span class='kata-ring-text' aria-hidden='true'><strong>#{solved}</strong><small>von #{total}</small></span></div>"
            io << "<span class='kata-total-label'>Katas gelöst#{extras > 0 ? ", dazu #{extras} #{extras == 1 ? 'Extra' : 'Extras'}" : ''}</span>"
        else
            total = stops.sum { |stop| core.(stop) }
            io << "<div class='kata-ring'><span class='kata-ring-text'><strong>#{total}</strong><small>Katas</small></span></div>"
            io << "<span class='kata-total-label'>in #{stops.size} Übungen</span>"
        end
        io << '</div>'
        io << "<div class='kata-strip'>"
        io << "<button type='button' class='kata-scroll' data-dir='-1' aria-label='Zurück'><i class='bi bi-chevron-left' aria-hidden='true'></i></button>"
        io << "<div class='kata-track'>"
        stops.each_with_index do |stop, i|
            mine = progress && progress[stop.slug]
            done = mine && mine[:total].to_i > 0 && mine[:done] == mine[:total]
            pin = pinned.include?(stop.slug)
            classes = ['kata-set', done ? 'kata-set-done' : nil, pin ? 'kata-set-pinned' : nil].compact.join(' ')
            # the strip is dragged with a mouse, so the set is no link: its
            # button opens it
            io << "<div class='#{classes}'#{pin && pinned.first == stop.slug ? " data-centre=''" : ''}>"
            # the card's picture, or for a set without one (no picture would
            # not give a solution away) its icon on a band (meta icon:)
            io << (images[stop.slug] || "<span class='kata-set-art' aria-hidden='true'><i class='bi #{h(stop.icon || 'bi-code-slash')}'></i></span>")
            io << "<span class='kata-set-marks'><span class='kata-set-number'>#{done ? "<i class='bi bi-check-lg' aria-hidden='true'></i><span class='visually-hidden'>Erledigt</span>" : i + 1}</span>"
            io << "<span class='kata-set-pin'><i class='bi bi-pin-angle-fill' aria-hidden='true'></i>Angepinnt</span>" if pin
            io << '</span>'
            io << "<span class='kata-set-body'>"
            io << "<span class='kata-set-title'>#{h(stop.title)}</span>"
            io << "<span class='kata-set-idea'>#{h(stop.idea)}</span>" if stop.idea
            # in one line, the levels on hover
            extras = stop.counts[:extra].to_i
            io << "<span class='kata-set-counts' title='#{h(counts_text(stop.counts))}'>#{core.(stop)} Katas#{extras > 0 ? ", #{extras} #{extras == 1 ? 'Extra' : 'Extras'}" : ''}</span>"
            io << "<span class='kata-set-curriculum' title='#{h(stop.curriculum)}'>#{h(stop.curriculum)}</span>" if teacher && stop.curriculum
            solved = 0
            if progress
                total = mine ? mine[:total].to_i : core.(stop)
                solved = mine ? mine[:done].to_i : 0
                extras = mine ? mine[:extras_done].to_i : 0
                share = total > 0 ? (100.0 * solved / total).round : 0
                io << "<span class='kata-set-progress'><span class='kata-set-bar' aria-hidden='true'><span style='width: #{share}%'></span></span>"
                io << "#{solved} von #{total} gelöst#{extras > 0 ? ", dazu #{extras} #{extras == 1 ? 'Extra' : 'Extras'}" : ''}</span>"
            end
            label = if !progress || done then 'Ansehen'
                    elsif solved > 0 then 'Weitermachen'
                    else 'Loslegen'
                    end
            io << "<a class='btn btn-sm btn-success kata-set-open' href='/#{h(stop.slug)}' aria-label='#{label}: #{h(stop.title)}'>" \
                  "#{label}<i class='bi bi-arrow-right' aria-hidden='true'></i></a>"
            io << '</span></div>'
        end
        io << '</div>'
        io << "<button type='button' class='kata-scroll' data-dir='1' aria-label='Weiter'><i class='bi bi-chevron-right' aria-hidden='true'></i></button>"
        io << '</div></div>'
        io << SCRIPT
    end

    # The arrows scroll by about one set and hide at the ends; a mouse drags
    # the strip; a pinned set (data-centre) is scrolled into the middle,
    # without moving the page.
    SCRIPT = <<~JS.freeze
        <script>
        document.querySelectorAll('.kata-carousel').forEach(function (carousel) {
            var track = carousel.querySelector('.kata-track');
            var buttons = carousel.querySelectorAll('.kata-scroll');
            // at an end its arrow fades out; where more sets follow, the
            // strip fades into the page (default.css, kata-more-left/right)
            function ends() {
                var start = track.scrollLeft <= 8;
                var end = track.scrollLeft + track.clientWidth >= track.scrollWidth - 8;
                buttons[0].disabled = start;
                buttons[1].disabled = end;
                carousel.classList.toggle('kata-more-left', !start);
                carousel.classList.toggle('kata-more-right', !end);
            }
            buttons.forEach(function (button) {
                button.addEventListener('click', function () {
                    var set = track.querySelector('.kata-set');
                    var step = set ? set.getBoundingClientRect().width + 16 : track.clientWidth * 0.8;
                    track.scrollBy({ left: Number(button.dataset.dir) * step });
                });
            });
            track.addEventListener('scroll', ends, { passive: true });
            // with a mouse the strip is dragged sideways; after a drag, the
            // click that ends it opens nothing
            var drag = null;
            track.addEventListener('pointerdown', function (event) {
                if (event.pointerType !== 'mouse' || event.button !== 0) return;
                drag = { x: event.clientX, left: track.scrollLeft, moved: false };
            });
            window.addEventListener('pointermove', function (event) {
                if (!drag) return;
                var dx = event.clientX - drag.x;
                if (!drag.moved && Math.abs(dx) < 6) return;
                if (!drag.moved) {
                    drag.moved = true;
                    carousel.classList.add('kata-dragging');
                }
                track.scrollLeft = drag.left - dx;
            });
            window.addEventListener('pointerup', function () {
                if (!drag) return;
                var moved = drag.moved;
                drag = null;
                carousel.classList.remove('kata-dragging');
                if (moved) {
                    track.addEventListener('click', function (event) { event.preventDefault(); event.stopPropagation(); }, { capture: true, once: true });
                }
            });
            track.addEventListener('dragstart', function (event) { event.preventDefault(); });
            var centre = track.querySelector('[data-centre]');
            if (centre) {
                track.style.scrollBehavior = 'auto';
                track.style.scrollSnapType = 'none';
                track.scrollLeft = centre.offsetLeft - track.offsetLeft - (track.clientWidth - centre.offsetWidth) / 2;
                requestAnimationFrame(function () { track.style.scrollBehavior = ''; track.style.scrollSnapType = ''; });
            }
            ends();
            window.addEventListener('resize', ends);
        });
        </script>
    JS
end
