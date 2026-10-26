require_relative 'code_languages'
require 'cgi'

# The strip above a content page that says what kind of page it is
# (docs/aufgaben.md): an activity shows its kind, its duration (teachers
# only), the activities it builds on and the reference pages it uses; a reference page shows where it sits in
# Handbuch and which activities use it.
#
# Works on the plain hashes in @@content, so it can be tested on its own.
module PageKinds
    KIND_LABELS = {
        'projekt' => 'Projekt',
        'labor' => 'Labor',
        'grundlagen' => 'Grundlagen',
        'galerie' => 'Galerie',
        'uebung' => 'Übung',
        'steckbrief' => 'Steckbrief',
    }.freeze
    REFERENCE_PREFIX = 'reference/'

    # Label of an activity's kind, with the language from language: if the
    # page has one: »Projekt (C / raylib)«. nil for an unknown kind.
    def self.kind_label(kind, language = nil)
        label = KIND_LABELS[kind.to_s]
        return nil if label.nil?
        language = language.to_s.strip
        language.empty? ? label : "#{label} (#{language})"
    end

    def self.reference?(slug)
        slug.to_s.start_with?(REFERENCE_PREFIX)
    end

    # Title of a page without markup ('' if unknown).
    def self.plain_title(entry)
        (entry || {})[:title].to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '').strip
    end

    # Reference names a page uses: meta reference: plus every _include(name#part).
    # catalog: the reference slugs in the order of the Handbuch catalog
    # (reference/sections.yaml), which begins with the basics (Workspace,
    # Shortcuts, Terminal): the strip lists them in that order, basics
    # first; pages not in the catalog come last, as named.
    def self.references_of(page_meta, markdown, include_pattern, catalog: [])
        names = Array(page_meta['reference']).map(&:to_s)
        names += markdown.scan(include_pattern).map { |m| m.first.to_s }
        names = names.reject(&:empty?).uniq
        names.each_with_index.sort_by { |name, i| [catalog.index(name) || catalog.size, i] }.map(&:first)
    end

    # Activities a page builds on (requires: in its meta block), as
    # [slug, title] in the order named. Information, not a rule: nothing is
    # blocked or reordered by it. Only activities a student can find in the
    # catalog; hidden, teacher-only and (outside development) dev-only pages
    # are left out, as in used_by.
    def self.requires_of(slug, content, development: false)
        (content.dig(slug, :requires) || []).uniq.filter_map do |name|
            entry = content[name]
            next nil if entry.nil? || reference?(name)
            next nil if entry[:teacher_only] || entry[:hidden_section]
            next nil if entry[:dev_only] && !development
            title = plain_title(entry)
            [name, title.empty? ? name : title]
        end
    end

    # »A«, »A und B«, »A, B und C«
    def self.join_and(items)
        items.size <= 1 ? items.join : "#{items[0..-2].join(', ')} und #{items[-1]}"
    end

    # Pages that use a reference page, as [slug, title], sorted by title.
    # Hidden pages (teacher-only, in a hidden section such as the one only a
    # secret on the error page leads to, or dev-only outside development)
    # are left out: a Handbuch page must not give them away.
    def self.used_by(reference_slug, content, development: false)
        name = reference_slug.to_s.delete_prefix(REFERENCE_PREFIX)
        content.select do |slug, entry|
            next false if reference?(slug)
            next false if entry[:teacher_only] || entry[:hidden_section]
            next false if entry[:dev_only] && !development
            (entry[:references] || []).include?(name)
        end.map { |slug, entry| [slug, plain_title(entry).then { |t| t.empty? ? slug : t }] }
            .sort_by { |_, title| title.downcase }
    end

    # HTML of the strip for one page ('' for pages that get none).
    # extra: HTML at the right end of the strip (the teacher note's button).
    # show_duration: only for teachers; how long an activity takes is for
    # planning lessons, not something students need to read.
    # code_language: the programming language shown on a page that offers
    # more than one (CodeLanguages); the switch beside the label shows it,
    # the label itself does not name it (»Übung«, not »Übung (Ruby)«, which
    # would change width when switching).
    def self.header(slug, content, sections, development: false, extra: '', show_duration: false, code_language: nil)
        entry = content[slug]
        return '' if entry.nil? || entry[:teacher_only]

        h = ->(s) { CGI.escapeHTML(s.to_s) }
        if reference?(slug)
            section = sections[entry[:section]] || {}
            # the catalog's label is HTML already (»HTML &amp; CSS«): unescaped
            # first, so that it is escaped once
            label = CGI.unescapeHTML(section[:label].to_s.gsub('&shy;', ''))
            used = used_by(slug, content, development: development)
            html = +"<nav class='page-kind page-kind-reference' aria-label='Handbuch'>"
            html << "<span class='page-kind-label'><a href='/reference'><i class='bi bi-book'></i>Handbuch</a>"
            html << "<i class='bi bi-chevron-right page-kind-sep' aria-hidden='true'></i>#{h.(label)}" unless label.empty?
            html << '</span>'
            unless used.empty?
                # titles are HTML already (»HTML &amp; CSS«): unescaped first
                links = used.map { |s, title| "<a href='/#{h.(s)}'>#{h.(CGI.unescapeHTML(title.to_s))}</a>" }
                            .join(" <span class='page-kind-sep' aria-hidden='true'>·</span> ")
                html << "<span class='page-kind-links'>Wird verwendet in: #{links}</span>"
            end
            html << extra.to_s
            html << '</nav>'
        else
            languages = entry[:languages] || []
            switch = languages.include?(code_language)
            kind = h.(kind_label(entry[:kind], switch ? nil : entry[:language]) || 'Aktivität')
            html = +"<div class='page-kind page-kind-activity'>"
            html << "<span class='page-kind-label'><i class='bi bi-signpost-2'></i>#{kind}</span>"
            html << CodeLanguages.switch_html(languages, code_language) if switch
            if show_duration && entry[:duration].is_a?(String) && !entry[:duration].strip.empty?
                html << "<span class='page-kind-duration'><i class='bi bi-clock'></i>#{h.(entry[:duration].strip)}</span>"
            end
            required = requires_of(slug, content, development: development)
            unless required.empty?
                # titles are HTML already (»HTML &amp; CSS«): unescaped first
                links = required.map { |s, title| "<a href='/#{h.(s)}'>#{h.(CGI.unescapeHTML(title.to_s))}</a>" }
                html << "<span class='page-kind-requires'><i class='bi bi-layers'></i>Baut auf #{join_and(links)} auf</span>"
            end
            refs = (entry[:references] || []).map do |name|
                ref_slug = "#{REFERENCE_PREFIX}#{name}"
                ref = content[ref_slug]
                next nil if ref.nil? || (ref[:dev_only] && !development)
                title = plain_title(ref)
                "<a href='/#{h.(ref_slug)}'><i class='bi bi-book'></i>#{h.(title.empty? ? name : title)}</a>"
            end.compact
            html << "<span class='page-kind-links'>Im Handbuch: #{refs.join(' ')}</span>" unless refs.empty?
            html << extra.to_s
            html << '</div>'
        end
        html
    end

    # CSS class for the page container.
    def self.container_class(slug)
        reference?(slug) ? 'page-reference' : 'page-activity'
    end
end
