require 'cgi'
require_relative 'curriculum'
require_relative 'page_kinds'

# The page /lehrplan, for logged-in teachers: for every name of the plans
# (Curriculum, docs/lehrplan.md) the activities whose curriculum line names
# it, in the order of the catalog, with the details of their line. A name
# without an activity stays on the page: a gap is worth knowing too. Only
# what students see in the catalog (Main#pinnable_slugs): no hidden section.
module CurriculumOverview
    # The plans' own groups, with the source of each level.
    LEVELS = [
        {
            :level => 'Sek I', :title => 'Sekundarstufe I', :id => 'sek-i',
            :source => 'Rahmenlehrplan Teil C Informatik, Wahlpflichtfach, Jahrgangsstufen 7–10, Berlin und Brandenburg (2015)',
            :groups => [['Pflichtthemenfelder', Curriculum::SEK_I[0, 5]], ['Wahlthemenfelder', Curriculum::SEK_I[5..]]],
        },
        {
            :level => 'Oberstufe', :title => 'Oberstufe', :id => 'oberstufe',
            :source => 'Rahmenlehrplan für die gymnasiale Oberstufe, Teil C Informatik, Berlin',
            :groups => [['Einführungsphase', Curriculum::OBERSTUFE[0, 4]], ['Kurshalbjahre', Curriculum::OBERSTUFE[4, 8]],
                        ['Vertiefungsgebiete im 3. Kurshalbjahr', Curriculum::OBERSTUFE[12..]]],
        },
    ].freeze

    # Said beside a name where the plan says more about it.
    NOTES = { ['Sek I', 'Datenbanken'] => 'In Berlin verpflichtend.' }.freeze

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.anchor(level, name)
        Curriculum.anchor(level, name)
    end

    # [level, name] => [[slug, details], …] for the given activities, in
    # their order.
    def self.index(content, slugs)
        index = Hash.new { |hash, key| hash[key] = [] }
        slugs.each do |slug|
            Curriculum.entries((content[slug] || {})[:curriculum]).each do |level, name, details|
                index[[level, name]] << [slug, details]
            end
        end
        index
    end

    def self.html(content, slugs)
        index = index(content, slugs)
        html = +''
        LEVELS.each do |l|
            html << "<h2 id='#{l[:id]}'>#{h(l[:title])}</h2>\n"
            html << "<p class='lp-source'>#{h(l[:source])}</p>\n"
            l[:groups].each do |group, names|
                html << "<p class='lp-group'>#{h(group)}</p>\n"
                names.each do |name|
                    found = index[[l[:level], name]]
                    note = NOTES[[l[:level], name]]
                    html << "<section class='lp-topic'>\n"
                    # the count beside the heading, not in the side navigation
                    html << "<h3 id='#{anchor(l[:level], name)}'>#{h(name)} <span class='lp-count autotoc-skip'>#{found.size}</span></h3>\n"
                    html << "<p class='lp-note'>#{h(note)}</p>\n" if note
                    if found.empty?
                        html << "<p class='lp-none'>Noch keine Aktivität im Workspace.</p>\n"
                    else
                        html << "<ul class='lp-activities'>\n"
                        found.each do |slug, details|
                            entry = content[slug]
                            kind = PageKinds.kind_label(entry[:kind], entry[:language])
                            html << "<li><a href='/#{h(slug)}'>#{h(PageKinds.plain_title(entry))}</a>"
                            html << " <span class='lp-kind'>#{h(kind)}</span>" if kind
                            html << "<span class='lp-details'>#{h(details)}</span>" if details
                            html << "</li>\n"
                        end
                        html << "</ul>\n"
                    end
                    html << "</section>\n"
                end
            end
        end
        html
    end
end
