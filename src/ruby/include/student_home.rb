require 'cgi'

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

    def self.first_name(name)
        name.to_s.strip.split(/\s+/).first.to_s
    end

    def self.html(name:, progress:)
        io = +"<section class='student-home' aria-labelledby='student-home-title'>"
        greeting = first_name(name).empty? || first_name(name).include?('@') ? 'Hallo' : "Hallo #{h(first_name(name))}"
        io << "<h2 id='student-home-title'>#{greeting}</h2>"
        open = progress.reject { |p| p[:done] == p[:total] }.first(MAX_SHOWN)
        finished = progress.select { |p| p[:done] == p[:total] }
        if open.empty?
            io << "<p class='student-home-empty'>#{finished.empty? ? 'Du hast noch keine Aufgabe geprüft. Such dir unten eine Aktivität aus, zum Beispiel eine Übung unter »Programmieren üben«.' : 'Alles, was du angefangen hast, ist fertig. Such dir unten etwas Neues aus.'}</p>"
        else
            io << "<h3 class='student-home-label'>Weitermachen</h3><div class='student-home-cards'>"
            open.each do |p|
                link = p[:next] ? "/#{h(p[:slug])}#aufgabe-#{h(p[:next][:id])}" : "/#{h(p[:slug])}"
                io << "<a class='student-home-card' href='#{link}'>"
                io << "<span class='shc-title'>#{h(p[:title])}</span>"
                io << "<span class='shc-bar' aria-hidden='true'>"
                p[:marks].each { |m| io << "<span class='shc-mark shc-#{m}'></span>" }
                io << '</span>'
                io << "<span class='shc-count'>#{p[:done]} von #{p[:total]} erledigt</span>"
                io << "<span class='shc-next'>Weiter mit: #{h(p[:next][:title])}</span>" if p[:next]
                io << '</a>'
            end
            io << '</div>'
        end
        unless finished.empty?
            links = finished.first(8).map { |p| "<a href='/#{h(p[:slug])}'>#{h(p[:title])}</a>" }
            io << "<p class='student-home-finished'>Fertig: #{links.join(' · ')}</p>"
        end
        io << '</section>'
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
