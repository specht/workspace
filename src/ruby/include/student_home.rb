require 'cgi'
require_relative 'icons'

# The student's part of the start page (M5): where to continue, built from
# the stored TaskStates. Plain Ruby, tested on its own.
module StudentHome
    DONE = %w[correct sent].freeze
    MAX_SHOWN = 4

    # activities: [{ slug:, activity:, title:, tasks: [{ id:, title:, version: }] }]
    #             tasks in page order
    # states:     { 'activity/id' => stored TaskState (symbol keys) }
    # Returns the activities the person has started, most recent first:
    # [{ slug:, title:, done:, total:, next: task or nil, updated:, marks: [..] }]
    def self.progress(activities, states)
        activities.map do |a|
            marks = a[:tasks].map do |t|
                s = states["#{a[:activity]}/#{t[:id]}"]
                if s.nil? then :open
                elsif s[:version] != t[:version] then :stale
                elsif DONE.include?(s[:status].to_s) then :done
                else :wrong
                end
            end
            touched = a[:tasks].map { |t| states["#{a[:activity]}/#{t[:id]}"] }.compact
            next nil if touched.empty?

            next_index = marks.index { |m| m != :done }
            {
                :slug => a[:slug], :title => a[:title], :total => a[:tasks].size,
                :done => marks.count(:done), :marks => marks,
                :next => next_index ? a[:tasks][next_index] : nil,
                :updated => touched.map { |s| s[:updated].to_i }.max,
            }
        end.compact.sort_by { |p| [p[:done] == p[:total] ? 1 : 0, -p[:updated]] }
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # Words before the first name that are titles, not names.
    TITLES = %w[dr dr. prof prof. herr frau hr. fr. dipl. dipl.-ing. ing. mag. stud. str str. ostr ostr. studienrat studienrätin].freeze

    # »Dr. Michael Specht« -> »Michael«. An e-mail address gives nothing.
    def self.first_name(name)
        words = name.to_s.strip.split(/\s+/)
        words.shift while words.size > 1 && (TITLES.include?(words.first.downcase) || words.first.end_with?('.'))
        first = words.first.to_s
        first.include?('@') ? '' : first
    end

    # What to call the person: the first name if there is a name, otherwise
    # the whole e-mail address (a guessed name from it reads oddly).
    def self.greeting_name(name, email = nil)
        first = first_name(name)
        return first unless first.empty?

        [email, name].map { |a| a.to_s.strip }.find { |a| a.include?('@') } || ''
    end

    # Is an activity hidden from »Weitermachen«? hidden: { slug => time hidden }.
    # It comes back as soon as the person works on it again.
    def self.hidden?(entry, hidden)
        ts = hidden[entry[:slug]]
        !ts.nil? && ts.to_i >= entry[:updated].to_i
    end

    # pinned: activities a teacher pinned for one of the person's groups,
    #         [{ slug:, title:, abstract:, group: }], shown first, cannot be hidden
    # hidden: { slug => time } the person has hidden from »Weitermachen«
    def self.html(name:, progress:, pinned: [], hidden: {}, email: nil)
        io = +"<section class='student-home' aria-labelledby='student-home-title'>"
        first = greeting_name(name, email)
        io << "<h2 id='student-home-title'>#{first.empty? ? 'Hallo' : "Hallo #{h(first)}"}</h2>"
        by_slug = progress.to_h { |p| [p[:slug], p] }
        pinned_slugs = pinned.map { |p| p[:slug] }

        unless pinned.empty?
            io << "<h3 class='student-home-label'>#{Icons.tabler('pin')} Von deiner Lehrkraft</h3><div class='student-home-cards'>"
            pinned.each { |p| io << card(p[:slug], p[:title], by_slug[p[:slug]], :abstract => p[:abstract], :pinned => p[:group]) }
            io << '</div>'
        end

        rest = progress.reject { |p| pinned_slugs.include?(p[:slug]) }
        open = rest.reject { |p| p[:done] == p[:total] }
        visible = open.reject { |p| hidden?(p, hidden) }
        hidden_count = open.size - visible.size
        if !visible.empty?
            io << "<h3 class='student-home-label'>Weitermachen</h3><div class='student-home-cards'>"
            visible.first(MAX_SHOWN).each { |p| io << card(p[:slug], p[:title], p, :hideable => true) }
            io << '</div>'
        elsif pinned.empty?
            finished = rest.any? { |p| p[:done] == p[:total] }
            text = if hidden_count > 0 then 'Alles, was du angefangen hast, ist ausgeblendet oder fertig. Such dir unten etwas Neues aus.'
                elsif finished then 'Alles, was du angefangen hast, ist fertig. Such dir unten etwas Neues aus.'
                else 'Du hast noch keine Aufgabe geprüft. Such dir unten eine Aktivität aus, zum Beispiel eine Übung unter »Programmieren üben«.'
            end
            io << "<p class='student-home-empty'>#{text}</p>"
        end
        if hidden_count > 0
            io << "<p class='student-home-hidden'>#{hidden_count == 1 ? '1 angefangene Aktivität ist' : "#{hidden_count} angefangene Aktivitäten sind"} ausgeblendet. " \
                  "<button type='button' class='btn btn-link btn-sm student-home-unhide'>#{Icons.tabler('eye')}Wieder zeigen</button></p>"
        end
        finished = rest.select { |p| p[:done] == p[:total] }
        unless finished.empty?
            links = finished.first(8).map { |p| "<a href='/#{h(p[:slug])}'>#{h(p[:title])}</a>" }
            io << "<p class='student-home-finished'>Fertig: #{links.join(' · ')}</p>"
        end
        io << '</section>'
        io
    end

    def self.card(slug, title, entry, abstract: nil, pinned: nil, hideable: false)
        link = entry && entry[:next] ? "/#{h(slug)}#aufgabe-#{h(entry[:next][:id])}" : "/#{h(slug)}"
        io = +"<div class='student-home-card#{pinned ? ' student-home-pinned' : ''}' data-slug='#{h(slug)}'>"
        io << "<a class='shc-link' href='#{link}'>"
        io << "<span class='shc-title'>#{h(title)}</span>"
        if entry
            io << "<span class='shc-bar' aria-hidden='true'>"
            entry[:marks].each { |m| io << "<span class='shc-mark shc-#{m}'></span>" }
            io << '</span>'
            io << "<span class='shc-count'>#{entry[:done]} von #{entry[:total]} erledigt</span>"
            io << "<span class='shc-next'>Weiter mit: #{h(entry[:next][:title])}</span>" if entry[:next]
        elsif abstract && !abstract.to_s.strip.empty?
            io << "<span class='shc-next'>#{h(abstract.to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '').strip)}</span>"
        end
        io << "<span class='shc-pinned-by'>Angeheftet für #{h(pinned)}</span>" if pinned
        io << '</a>'
        if hideable
            io << "<button type='button' class='shc-hide' title='Ausblenden, bis du hier weiterarbeitest' aria-label='#{h(title)} ausblenden'>#{Icons.tabler('eye-off')}</button>"
        end
        io << '</div>'
        io
    end

    # Small label for a catalog card: kind and, if started, progress.
    KIND_LABELS = {
        'projekt' => 'Projekt', 'labor' => 'Labor', 'grundlagen' => 'Grundlagen',
        'galerie' => 'Galerie', 'uebung' => 'Übung',
    }.freeze

    def self.card_badges(kind, progress_entry)
        parts = []
        parts << "<span class='card-kind'>#{h(KIND_LABELS[kind.to_s])}</span>" if KIND_LABELS[kind.to_s]
        if progress_entry
            klass = progress_entry[:done] == progress_entry[:total] ? 'card-progress card-progress-done' : 'card-progress'
            parts << "<span class='#{klass}'>#{progress_entry[:done]}/#{progress_entry[:total]}</span>"
        end
        parts.empty? ? '' : "<div class='card-badges'>#{parts.join}</div>"
    end
end
