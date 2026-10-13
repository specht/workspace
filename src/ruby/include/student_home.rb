require 'cgi'
require_relative 'icons'

# The student's part of the start page (M5): activities a teacher pinned,
# with progress built from the stored TaskStates. The catalog cards show no
# progress; an activity shows it on its own page. Plain Ruby, tested on its own.
module StudentHome
    DONE = %w[correct sent].freeze

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

    # Activities a teacher pinned for one of the person's groups, at the top
    # of the start page. Nothing else: progress shows on the catalog cards.
    # pinned: [{ slug:, title:, abstract:, group: }]
    # progress: as from progress(), to show how far the person got
    def self.html(progress:, pinned: [])
        return '' if pinned.empty?
        by_slug = progress.to_h { |p| [p[:slug], p] }
        io = +"<section class='student-home' aria-labelledby='student-home-title'>"
        io << "<h2 id='student-home-title' class='student-home-label'>#{Icons.tabler('pin')} Von deiner Lehrkraft</h2><div class='student-home-cards'>"
        pinned.each { |p| io << card(p[:slug], p[:title], by_slug[p[:slug]], :abstract => p[:abstract], :pinned => p[:group]) }
        io << '</div></section>'
        io
    end

    def self.card(slug, title, entry, abstract: nil, pinned: nil)
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
        io << '</div>'
        io
    end

    # Small label for a catalog card: its kind.
    KIND_LABELS = {
        'projekt' => 'Projekt', 'labor' => 'Labor', 'grundlagen' => 'Grundlagen',
        'galerie' => 'Galerie', 'uebung' => 'Übung',
    }.freeze

    def self.card_badges(kind)
        label = KIND_LABELS[kind.to_s]
        label ? "<div class='card-badges'><span class='card-kind'>#{h(label)}</span></div>" : ''
    end
end
