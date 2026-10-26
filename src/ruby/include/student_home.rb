require 'cgi'
require_relative 'icons'
require_relative 'page_kinds'

# The student's part of the start page (M5), built from the stored
# TaskStates: activities a teacher pinned (first, with their progress),
# »Weiter mit« (the few activities the person worked on last, not finished,
# not pinned), »So geht's« for someone who has not checked anything yet, and
# on the catalog cards a small »3/7« once a task of the activity is solved.
# Plain Ruby, tested on its own.
module StudentHome
    DONE = %w[correct sent].freeze
    CONTINUE_MAX = 3

    # What the kinds on the cards mean, as parts of one sentence: the legend
    # in »So geht's«, and (capitalised, without tags) the tooltip of a
    # card's label.
    KIND_HINTS = {
        'grundlagen' => '<strong>Grundlagen</strong> führen dich in ein Werkzeug oder Thema ein',
        'uebung' => 'eine <strong>Übung</strong> trainiert eine Fertigkeit',
        'labor' => 'in einem <strong>Labor</strong> untersuchst du, wie etwas funktioniert',
        'projekt' => 'in einem <strong>Projekt</strong> baust du Schritt für Schritt etwas Eigenes',
    }.freeze

    def self.kind_hint(kind)
        hint = KIND_HINTS[kind.to_s]
        return nil unless hint

        text = hint.gsub(/<[^>]*>/, '')
        text[0].upcase + text[1..]
    end

    # activities: [{ slug:, activity:, title:, tasks: [{ id:, title:, version: }] }]
    #             tasks in page order
    # states:     { 'activity/id' => stored TaskState (symbol keys) }
    # Returns the activities the person has started, most recent first:
    # [{ slug:, title:, done:, total:, next: task or nil, updated:, marks: [..] }]
    # Tasks with extra: true (for the fast and the curious) count for
    # nobody's »x von y«: total, done, marks and next are the other tasks';
    # extras_done says how many extras are done besides.
    def self.progress(activities, states)
        activities.map do |a|
            mark = lambda do |t|
                s = states["#{a[:activity]}/#{t[:id]}"]
                if s.nil? then :open
                elsif s[:version] != t[:version] then :stale
                elsif DONE.include?(s[:status].to_s) then :done
                else :wrong
                end
            end
            core, extras = a[:tasks].partition { |t| !t[:extra] }
            marks = core.map(&mark)
            touched = a[:tasks].map { |t| states["#{a[:activity]}/#{t[:id]}"] }.compact
            next nil if touched.empty?

            next_index = marks.index { |m| m != :done }
            {
                :slug => a[:slug], :title => a[:title], :total => core.size,
                :done => marks.count(:done), :marks => marks,
                :extras_done => extras.count { |t| mark.call(t) == :done },
                :next => next_index ? core[next_index] : nil,
                :updated => touched.map { |s| s[:updated].to_i }.max,
            }
        end.compact.sort_by { |p| [p[:done] == p[:total] ? 1 : 0, -p[:updated]] }
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # », dazu 2 Extras«, when the person did some.
    def self.extras_text(entry)
        n = entry[:extras_done].to_i
        n > 0 ? ", dazu #{n} #{n == 1 ? 'Extra' : 'Extras'}" : ''
    end

    # The top of the start page for a student, in this order:
    # - activities a teacher pinned for one of the person's groups, with how
    #   far the person got; they keep the first place;
    # - »Weiter mit«: up to CONTINUE_MAX activities the person checked
    #   something in, most recent first, not finished and not pinned (those
    #   show their progress above), each linked to its first open task;
    # - »So geht's« while the person has not checked anything yet.
    # pinned: [{ slug:, title:, abstract:, group:, template_folder: }]; with a
    # template, home_templates.js fills in whether the person's clone is current
    # progress: as from progress()
    # listed: slugs a student can find in the catalog; others (hidden
    #         sections) never show up here
    # guide: false shows only the pinned activities (teacher accounts)
    def self.html(progress:, pinned: [], listed: nil, guide: true)
        io = +''
        unless pinned.empty?
            by_slug = progress.to_h { |p| [p[:slug], p] }
            io << "<section class='student-home' aria-labelledby='student-home-title'>"
            io << "<h2 id='student-home-title' class='student-home-label'>#{Icons.tabler('pin')} Von deiner Lehrkraft</h2><div class='student-home-cards'>"
            pinned.each do |p|
                io << card(p[:slug], p[:title], by_slug[p[:slug]], :abstract => p[:abstract], :pinned => p[:group],
                           :template_folder => p[:template_folder], :kind => p[:kind], :image => p[:image],
                           :image_dark => p[:image_dark], :position => p[:position])
            end
            io << '</div></section>'
        end
        return io unless guide

        io << continue_html(progress, :pinned => pinned.map { |p| p[:slug] }, :listed => listed)
        io << howto_html(:pinned => !pinned.empty?) if progress.empty?
        io
    end

    # Activities for »Weiter mit«: started, not finished, not pinned, most
    # recently checked first.
    def self.continue_entries(progress, pinned: [], listed: nil)
        progress.reject { |p| p[:done] == p[:total] || pinned.include?(p[:slug]) }
            .select { |p| listed.nil? || listed.include?(p[:slug]) }
            .sort_by { |p| -p[:updated].to_i }
            .first(CONTINUE_MAX)
    end

    def self.continue_html(progress, pinned: [], listed: nil)
        entries = continue_entries(progress, :pinned => pinned, :listed => listed)
        return '' if entries.empty?

        io = +"<section class='student-continue' aria-labelledby='student-continue-title'>"
        io << "<h2 id='student-continue-title' class='student-home-label'>Weiter mit</h2><ul class='student-continue-list'>"
        entries.each do |e|
            link = e[:next] ? "/#{h(e[:slug])}#aufgabe-#{h(e[:next][:id])}" : "/#{h(e[:slug])}"
            io << "<li><a href='#{link}'>"
            io << "<span class='sc-title'>#{h(e[:title])}</span>"
            io << "<span class='sc-meta'><span class='shc-bar' aria-hidden='true'>"
            e[:marks].each { |m| io << "<span class='shc-mark shc-#{m}'></span>" }
            io << '</span>'
            io << "<span class='sc-count'>#{e[:done]} von #{e[:total]} gelöst#{extras_text(e)}</span></span>"
            io << "<i class='bi bi-arrow-right-short' aria-hidden='true'></i>"
            io << '</a></li>'
        end
        io << '</ul></section>'
        io
    end

    # How the site works, for someone who has not checked a task yet. It
    # names no activity to start with: what a class works on is the
    # teacher's choice (pins), or the person's own.
    def self.howto_html(pinned: false)
        start = if pinned
            'Fang mit den Aktivitäten an, die deine Lehrkraft oben für deine Klasse ausgewählt hat.'
        else
            'Such dir unten eine Aktivität aus oder frag deine Lehrkraft.'
        end
        steps = [
            start,
            '<strong>Workspace öffnen</strong> (oben) startet dein eigenes VS Code im Browser.',
            'Auf den Seiten stehen Aufgaben: <strong>Prüfen</strong> sieht nach, ob deine Antwort oder dein Programm stimmt, ' \
                'und merkt sich das Ergebnis. Deine Lehrkraft sieht, wie weit du bist.',
            "Befehle, Tastenkürzel und Syntax schlägst du im <a href='/reference'>Handbuch</a> nach.",
        ]
        io = +"<section class='student-howto' aria-labelledby='student-howto-title'>"
        io << "<h2 id='student-howto-title' class='student-home-label'>So geht’s</h2><ol>"
        steps.each { |s| io << "<li>#{s}</li>" }
        io << '</ol>'
        io << "<p class='student-howto-kinds'>Auf den Karten: #{PageKinds.join_and(KIND_HINTS.values)}.</p>"
        io << '</section>'
        io
    end

    # A pinned activity as a wide card: its picture from the catalog on the
    # left (image: the <img> attributes, image_dark: those of the dark
    # variant, position: its focus), on the right its kind, title, the start
    # of its description, the progress once begun and a button to go on;
    # which group it was pinned for, small. On a phone the picture is on top.
    def self.card(slug, title, entry, abstract: nil, pinned: nil, template_folder: nil, kind: nil, image: nil, image_dark: nil, position: nil)
        link = entry && entry[:next] ? "/#{h(slug)}#aufgabe-#{h(entry[:next][:id])}" : "/#{h(slug)}"
        io = +"<article class='student-home-card#{pinned ? ' student-home-pinned' : ''}#{image ? ' shc-with-picture' : ''}' data-slug='#{h(slug)}'>"
        if image
            style = position ? " style='object-position: #{h(position)};'" : ''
            io << "<a class='shc-picture' href='#{link}' tabindex='-1' aria-hidden='true'>"
            io << "<img alt='' class='#{image_dark ? 'theme-image-light' : ''}' #{image} loading='lazy' decoding='async'#{style}>"
            io << "<img alt='' class='theme-image-dark' #{image_dark} loading='lazy' decoding='async'#{style}>" if image_dark
            io << '</a>'
        end
        io << "<div class='shc-body'>"
        io << "<span class='shc-kind'>#{h(kind)}</span>" if kind
        io << "<h3 class='shc-title'><a class='shc-link' href='#{link}'>#{h(title)}</a></h3>"
        text = abstract.to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '').strip
        io << "<p class='shc-abstract'>#{h(text)}</p>" unless text.empty?
        if entry
            io << "<div class='shc-progress'><span class='shc-bar' aria-hidden='true'>"
            entry[:marks].each { |m| io << "<span class='shc-mark shc-#{m}'></span>" }
            io << '</span>'
            io << "<span class='shc-count'>#{entry[:done]} von #{entry[:total]} gelöst#{extras_text(entry)}</span></div>"
            io << "<span class='shc-next'>Weiter mit: #{h(entry[:next][:title])}</span>" if entry[:next]
        end
        label = if entry.nil? || entry[:done].to_i.zero? then 'Loslegen'
                elsif entry[:done] >= entry[:total] then 'Ansehen'
                else 'Weitermachen'
                end
        io << "<div class='shc-actions'><a class='btn btn-success btn-sm shc-go' href='#{link}' aria-label='#{label}: #{h(title)}'>#{label}<i class='bi bi-arrow-right' aria-hidden='true'></i></a>"
        io << "<span class='shc-pinned-by'><i class='bi bi-pin-angle' aria-hidden='true'></i>Angeheftet für #{h(pinned)}</span>" if pinned
        io << '</div>'
        if template_folder
            io << "<span class='shc-template' data-slug='#{h(slug)}' data-folder='#{h(template_folder)}' aria-live='polite'></span>"
        end
        io << '</div></article>'
        io
    end

    # Small label for a catalog card: its kind, as in the strip above the
    # page. Cards show no progress (the catalog stays calm; an activity shows
    # it on its own page, and »Weiter mit« on the start page).
    def self.card_badges(kind, language = nil)
        label = PageKinds.kind_label(kind, language)
        parts = []
        if label
            hint = kind_hint(kind)
            title = hint ? " title='#{h(hint)}'" : ''
            parts << "<span class='card-kind'#{title}>#{h(label)}</span>"
        end
        parts.empty? ? '' : "<div class='card-badges'>#{parts.join}</div>"
    end

    # The line at the foot of a catalog card: what the activity builds on.
    # titles: plain titles of the required activities (PageKinds.requires_of)
    def self.card_requires(titles)
        return '' if titles.empty?

        "<div class='card-requires'><i class='bi bi-layers' aria-hidden='true'></i>" \
            "Baut auf #{PageKinds.join_and(titles.map { |t| h(CGI.unescapeHTML(t.to_s)) })} auf</div>"
    end
end
