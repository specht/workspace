require 'cgi'

# The overview of the Katas (/katas, docs/plan-katas.md): every set of the
# catalog section »Katas« as a stop on a path, in the order of sections.yaml,
# with its idea in a few words (meta idea:) and how many Katas it has on
# each level; for a logged-in person how far they are (solved of the core
# Katas, and Extras besides), for teachers the line of the curriculum the
# set belongs to (meta curriculum:). The page holds an empty
# <div class='kata-map'></div> (MARKER matches it after rendering, with
# either quotes); page_html puts the map there for the person viewing it.
#
# Plain Ruby, tested on its own (test/kata_map_test.rb).
module KataMap
    MARKER = %r{<div class=["']kata-map["']>\s*</div>}
    LEVELS = { 1 => 'Einstieg', 2 => 'Übung', 3 => 'Knobelei' }.freeze

    Stop = Struct.new(:slug, :title, :idea, :curriculum, :counts, keyword_init: true)

    # A stop from a page's content entry: counts are { 1 => n, 2 => n,
    # 3 => n, :extra => n } over the tasks on the page.
    def self.stop(slug, content)
        tasks = content[:tasks] ? content[:tasks][:on_page].map { |id| content[:tasks][:by_id][id] } : []
        counts = Hash.new(0)
        tasks.each { |t| t['extra'] ? counts[:extra] += 1 : counts[t['level'] || 1] += 1 }
        Stop.new(:slug => slug, :title => title(content[:title]), :idea => content[:idea], :curriculum => content[:curriculum], :counts => counts)
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

    # progress: { slug => { done:, total:, extras_done: } } of the person,
    # nil for a visitor; teacher: shows the curriculum lines.
    def self.html(stops, progress: nil, teacher: false)
        io = +"<ol class='kata-map-path'>"
        stops.each_with_index do |stop, i|
            mine = progress && progress[stop.slug]
            done = mine && mine[:total].to_i > 0 && mine[:done] == mine[:total]
            io << "<li class='kata-stop#{done ? ' kata-stop-done' : ''}'>"
            io << "<a class='kata-stop-link' href='/#{h(stop.slug)}'>"
            io << "<span class='kata-stop-number' aria-hidden='true'>#{done ? "<i class='bi bi-check-lg'></i>" : i + 1}</span>"
            io << "<span class='kata-stop-title'>#{h(stop.title)}</span>"
            io << "<span class='kata-stop-idea'>#{h(stop.idea)}</span>" if stop.idea
            io << "<span class='kata-stop-counts'>#{h(counts_text(stop.counts))}</span>"
            if progress
                total = mine ? mine[:total].to_i : stop.counts.select { |k, _| k != :extra }.values.sum
                solved = mine ? mine[:done].to_i : 0
                extras = mine ? mine[:extras_done].to_i : 0
                share = total > 0 ? (100.0 * solved / total).round : 0
                io << "<span class='kata-stop-progress'><span class='kata-stop-bar' aria-hidden='true'><span style='width: #{share}%'></span></span>"
                io << "#{solved} von #{total} gelöst#{extras > 0 ? ", dazu #{extras} #{extras == 1 ? 'Extra' : 'Extras'}" : ''}</span>"
            end
            io << "<span class='kata-stop-curriculum'>#{h(stop.curriculum)}</span>" if teacher && stop.curriculum
            io << '</a></li>'
        end
        io << '</ol>'
        "<div class='kata-map'>#{io}</div>"
    end
end
