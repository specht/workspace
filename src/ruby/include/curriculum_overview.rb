require 'cgi'
require_relative 'curriculum'
require_relative 'page_kinds'
require 'set'

# The page /lehrplan, for logged-in teachers: for every name of the plan
# (Curriculum, the teacher's state; docs/lehrplan.md) the activities whose
# line names it, in the order of the catalog, with the details of their
# line. A name without an activity stays on the page: a gap is worth
# knowing too. Only what students see in the catalog (Main#pinnable_slugs):
# no hidden section.
module CurriculumOverview
    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.anchor(level, name)
        Curriculum.anchor(level, name)
    end

    # [level, name] => [[slug, details], …] for the given activities, in
    # their order.
    def self.index(content, slugs, plan = nil)
        index = Hash.new { |hash, key| hash[key] = [] }
        slugs.each do |slug|
            Curriculum.entries(Curriculum.line(plan, slug, content[slug]), plan).each do |level, name, details|
                index[[level, name]] << [slug, details]
            end
        end
        index
    end

    # in_graph: the anchors of the names the graph above can show
    # (CurriculumGraph); those get a button that shows them there. state:
    # the teacher's, for a source a plan gives for one of its states.
    def self.html(content, slugs, in_graph: Set.new, plan: nil, state: nil)
        plan ||= Curriculum.default_plan
        index = index(content, slugs, plan)
        html = +''
        plan[:levels].each do |l|
            source = l[:source_for][state.to_s] || l[:source]
            pdf = l[:pdf_for][state.to_s] || l[:pdf]
            url = l[:url_for][state.to_s] || l[:url]
            # the plan to read: its PDF, and the page it comes from
            links = []
            links << "<a href='#{h(pdf)}' target='_blank' rel='noopener'><i class='bi bi-file-earmark-pdf' aria-hidden='true'></i> Lehrplan als PDF</a>" if pdf
            links << "<a href='#{h(url)}' target='_blank' rel='noopener'><i class='bi bi-box-arrow-up-right' aria-hidden='true'></i> Seite des Landes</a>" if url
            html << "<h2 id='#{l[:id]}'>#{h(l[:title])}</h2>\n"
            html << "<p class='lp-source'>#{h(source)}#{links.empty? ? '' : " · #{links.join(' · ')}"}</p>\n"
            l[:groups].each do |group, names|
                html << "<p class='lp-group'>#{h(group)}</p>\n"
                names.each do |name|
                    found = index[[l[:level], name]]
                    note = plan[:notes][[l[:level], name]]
                    html << "<section class='lp-topic'>\n"
                    # the count beside the heading, not in the side navigation
                    show = in_graph.include?(anchor(l[:level], name)) ?
                        "<button type='button' class='lp-graph-show autotoc-skip' data-topic='#{anchor(l[:level], name)}' title='Im Bild oben zeigen, welcher Weg dorthin führt' aria-label='#{h(name)} im Bild zeigen'><i class='bi bi-diagram-3' aria-hidden='true'></i></button>" : ''
                    html << "<h3 id='#{anchor(l[:level], name)}'>#{h(name)} <span class='lp-count autotoc-skip'>#{found.size}</span>#{show}</h3>\n"
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
