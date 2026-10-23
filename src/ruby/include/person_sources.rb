require 'cgi'
require 'nokogiri'
require 'uri'
require 'yaml'

# The development page /person-sources: every sentence of the person boxes
# (reference/personen) with the sources that back it, from sources.yaml next
# to the page, so the boxes can be fact-checked. A sentence is covered when
# it contains the text of a claim (sources.yaml says how claims look); the
# page shows what is still open, what the sources contradict, and claims
# whose text the box no longer has.
module PersonSources
    DIR = '/src/content/reference/personen'
    # A period after these ends no sentence: »bis ins 20. Jahrhundert«, »z. B.«
    NO_END = /(?:\A|[\s(])(?:\d+|[[:alpha:]]|ca|bzw|Dr|St|Nr|usw|vgl|etc|Jh)\.\z/
    STATES = {
        'open' => ['Offen', 'bi-question-circle'],
        'partly' => ['Teilweise', 'bi-exclamation-triangle'],
        'wrong' => ['Falsch', 'bi-x-octagon'],
        'sourced' => ['Belegt', 'bi-check-lg'],
        'none' => ['Ohne Beleg nötig', 'bi-dash-circle'],
    }.freeze

    # The sentences of a paragraph's plain text.
    def self.sentences(text)
        result = []
        start = 0
        text.scan(/[.!?][»«"”)]*\s+(?=[[:upper:]»„(\d])/) do
            match = Regexp.last_match
            next if text[start...match.begin(0) + 1] =~ NO_END

            result << text[start...match.end(0)].strip
            start = match.end(0)
        end
        result << text[start..].strip
        result.reject(&:empty?)
    end

    # What a sentence is, from the claims it contains.
    def self.state(claims)
        return 'open' if claims.empty?
        return 'wrong' if claims.any? { |c| c['verdict'] == 'wrong' }
        return 'partly' if claims.any? { |c| c['verdict'] == 'partly' }
        return 'none' if claims.all? { |c| c['none'] }
        return 'sourced' if claims.all? { |c| c['none'] || !c['sources'].to_a.empty? }

        'open'
    end

    # [{part:, name:, paragraphs: [[{text:, state:, claims:}]], credit:, missing:}]
    # for every part of markdown with a person box.
    def self.overview(markdown, sources)
        ContentParts.parts(markdown).filter_map do |part, text|
            box = Nokogiri::HTML::DocumentFragment.parse(text).at_css('div.person')
            next unless box

            claims = (sources[part] || []).map { |c| c.merge('found' => false) }
            paragraphs = box.css('p').reject { |p| p['class'].to_s.include?('person-credit') }.map do |p|
                sentences(p.text.gsub(/\s+/, ' ').strip).map do |sentence|
                    matched = claims.select { |c| sentence.include?(c['text'].to_s.gsub(/\s+/, ' ').strip) }
                    matched.each { |c| c['found'] = true }
                    { :text => sentence, :state => state(matched), :claims => matched }
                end
            end
            {
                :part => part, :name => ContentParts.heading_text(text) || part,
                :paragraphs => paragraphs,
                :credit => box.at_css('p.person-credit')&.inner_html,
                :missing => claims.reject { |c| c['found'] },
            }
        end
    end

    def self.load
        markdown = File.read(File.join(DIR, 'personen.md'))
        file = File.join(DIR, 'sources.yaml')
        sources = File.file?(file) ? (YAML.safe_load(File.read(file), aliases: false) || {}) : {}
        overview(markdown, sources)
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.icon(state)
        label, icon = STATES[state]
        "<i class='bi #{icon}' title='#{h(label)}' aria-label='#{h(label)}'></i>"
    end

    def self.source_html(source)
        url = source['url'].to_s
        host = (URI.parse(url).host.to_s.delete_prefix('www.') rescue url)
        quote = source['quote'] ? "<blockquote>#{h(source['quote'])}</blockquote>" : ''
        "<li><a href='#{h(url)}' target='_blank' rel='noopener'>#{h(host)}</a> <span class='ps-url'>#{h(url)}</span>#{quote}</li>"
    end

    def self.claim_html(claim)
        parts = []
        parts << "<p class='ps-none'>Ohne Beleg: #{h(claim['none'])}</p>" if claim['none']
        parts << "<p class='ps-note'>#{h(claim['note'])}</p>" if claim['note']
        sources = claim['sources'].to_a
        parts << "<ul class='ps-sources'>#{sources.map { |s| source_html(s) }.join}</ul>" unless sources.empty?
        parts.join
    end

    # The page's content (static/person-sources.html), trusted: everything
    # from the files is escaped, except the credit line, which is the box's
    # own HTML.
    def self.page_html
        persons = load
        counts = Hash.new(0)
        missing = 0
        sections = persons.map do |person|
            states = []
            body = person[:paragraphs].map do |sentences|
                rows = sentences.map do |s|
                    counts[s[:state]] += 1
                    states << s[:state]
                    "<div class='ps-sentence ps-#{s[:state]}' data-state='#{s[:state]}'>" \
                        "<div class='ps-text'>#{icon(s[:state])}#{h(s[:text])}</div>" \
                        "#{s[:claims].map { |c| claim_html(c) }.join}</div>"
                end
                "<div class='ps-paragraph'>#{rows.join}</div>"
            end.join
            unless person[:missing].empty?
                missing += person[:missing].size
                states << 'missing'
                body += person[:missing].map do |c|
                    "<div class='ps-sentence ps-missing' data-state='missing'><div class='ps-text'>" \
                        "<i class='bi bi-link-45deg'></i>Nicht mehr im Kasten: »#{h(c['text'])}«</div>#{claim_html(c)}</div>"
                end.join
            end
            credit = person[:credit] ? "<p class='ps-credit'>#{person[:credit]}</p>" : ''
            "<section class='ps-person' data-states='#{states.uniq.join(' ')}'>" \
                "<h2><a href='/reference/personen##{h(person[:part])}'>#{h(person[:name])}</a></h2>#{body}#{credit}</section>"
        end
        filters = [['all', 'Alle', counts.values.sum]] + STATES.map { |key, (label, _)| [key, label, counts[key]] } +
                  [['missing', 'Nicht mehr im Kasten', missing]]
        buttons = filters.map do |key, label, count|
            "<button type='button' class='btn btn-sm btn-outline-secondary#{key == 'all' ? ' active' : ''}' data-filter='#{key}'>" \
                "#{h(label)} <span class='badge text-bg-secondary'>#{count}</span></button>"
        end.join
        "<div class='ps-filters'>#{buttons}</div>" \
            "<p class='ps-summary'>#{persons.size} Personen, #{counts.values.sum} Sätze, davon #{counts['sourced']} belegt, " \
            "#{counts['open']} offen.</p>#{sections.join}"
    end
end
