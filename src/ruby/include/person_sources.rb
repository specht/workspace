require 'cgi'
require 'nokogiri'
require 'uri'
require 'yaml'

# The development page /person-sources: every sentence of the person boxes
# (reference/personen) with the sources that back it, from sources.yaml next
# to the page, so the boxes can be fact-checked; below them the history
# boxes of the activities (<div class='history' id='history-…'>), with
# their sources in content/history-sources.yaml, keyed by the id without
# »history-«. A sentence is covered when
# it contains the text of a claim (sources.yaml says how claims look); the
# page shows what is still open, what the sources contradict, and claims
# whose text the box no longer has.
module PersonSources
    DIR = '/src/content/reference/personen'
    CONTENT = '/src/content'
    HISTORY_SOURCES = File.join(CONTENT, 'history-sources.yaml')
    # A period after these ends no sentence: »bis ins 20. Jahrhundert«, »z. B.«
    # A year ends one: »1970. Ein Band …«; an ellipsis none: »Let It Be... Naked«
    NO_END = /(?:(?:\A|[\s(])(?:\d{1,3}|[[:alpha:]]|ca|bzw|Dr|St|Nr|usw|vgl|etc|Jh)\.|\.\.\.)\z/
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

            entry(box, sources[part], part, ContentParts.heading_text(text) || part, 'person-credit')
        end
    end

    # One box: its sentences with their claims, the credit line, the claims
    # whose text the box no longer has.
    def self.entry(box, claims, part, name, credit_class)
        claims = (claims || []).map { |c| c.merge('found' => false) }
        paragraphs = box.css('p').reject { |p| p['class'].to_s.include?(credit_class) }.map do |p|
            sentences(p.text.gsub(/\s+/, ' ').strip).map do |sentence|
                matched = claims.select { |c| sentence.include?(c['text'].to_s.gsub(/\s+/, ' ').strip) }
                matched.each { |c| c['found'] = true }
                { :text => sentence, :state => state(matched), :claims => matched }
            end
        end
        {
            :part => part, :name => name,
            :paragraphs => paragraphs,
            :credit => box.at_css("p.#{credit_class}")&.inner_html,
            :missing => claims.reject { |c| c['found'] },
        }
    end

    # The history boxes of { slug => markdown }: like overview, with the
    # box's id (without »history-«) as its part and :link to it. A box
    # without an id gets the part nil.
    def self.history_overview(pages, sources)
        pages.flat_map do |slug, markdown|
            Nokogiri::HTML::DocumentFragment.parse(markdown).css('div.history').map do |box|
                id = box['id'].to_s[/\Ahistory-(.+)\z/, 1]
                name = box.at_css('p strong')&.text.to_s.strip
                entry(box, id && sources[id], id, name.empty? ? slug : name, 'history-credit')
                    .merge(:link => "/#{slug}##{box['id']}", :slug => slug)
            end
        end
    end

    # The activities' pages, { slug => markdown }
    def self.activity_pages(content = CONTENT)
        Dir[File.join(content, '*', '*.md')].sort.filter_map do |path|
            slug = File.basename(File.dirname(path))
            [slug, File.read(path)] if File.basename(path, '.md') == slug
        end.to_h
    end

    def self.history_load
        sources = File.file?(HISTORY_SOURCES) ? (YAML.safe_load(File.read(HISTORY_SOURCES), aliases: false) || {}) : {}
        history_overview(activity_pages, sources)
    end

    def self.load
        markdown = File.read(File.join(DIR, 'personen.md'))
        overview(markdown, read_sources(DIR))
    end

    def self.read_sources(dir)
        file = File.join(dir, 'sources.yaml')
        File.file?(file) ? (YAML.safe_load(File.read(file), aliases: false) || {}) : {}
    end

    # --- For readers (PersonBoxes): »Mehr dazu« behind the info icon -------

    # Names a reader recognises, by host
    HOSTS = {
        'mathshistory.st-andrews.ac.uk' => 'MacTutor',
        'computerhistory.org' => 'Computer History Museum',
        'amturing.acm.org' => 'ACM Turing Award',
        'plato.stanford.edu' => 'Stanford Encyclopedia of Philosophy',
        'britannica.com' => 'Britannica',
        'technologyreview.com' => 'MIT Technology Review',
        'spectrum.ieee.org' => 'IEEE Spectrum',
        'archive.org' => 'Internet Archive',
    }.freeze

    # Good for checking, not for reading on: answers of an API, raw data,
    # a text dump of a scanned book, readers' notes, a logo collection.
    NOT_FOR_READING = [%r{//api\.}, %r{/api/}, /\.json(\?|\z)/, /djvu\.txt\z/, /goodreads\.com/, /1000logos\.net/].freeze

    # The distinct sources of a box (the same page with another #anchor is
    # one), the most cited first: [{ url:, label: }]
    def self.reading(claims)
        urls = Array(claims).flat_map { |c| Array(c['sources']).map { |s| s['url'].to_s } }.reject(&:empty?)
        urls = urls.reject { |url| NOT_FOR_READING.any? { |pattern| url.match?(pattern) } }
        key = ->(url) { url.sub(/#.*/, '') }
        counts = urls.map(&key).tally
        first = urls.uniq(&key)
        order = first.sort_by { |url| [-counts[key.(url)], first.index(url)] }
        same_host = order.map { |url| host(url) }.tally
        order.map { |url| { :url => url, :label => label(url, same_host[host(url)] > 1) } }
    end

    # Addresses as written in sources.yaml may hold »ß« or [ ]
    def self.parse(url)
        URI.parse(url)
    rescue URI::InvalidURIError
        URI.parse(URI::DEFAULT_PARSER.escape(url).gsub('[', '%5B').gsub(']', '%5D')) rescue nil
    end

    def self.host(url)
        parse(url)&.host.to_s.sub(/\Awww\./, '')
    end

    # »Wikipedia: Snell's law«, »MacTutor: Snell«, »curl.se«; the page's
    # name too when one host has several pages for the box; »(PDF)«.
    def self.label(url, several)
        path = parse(url)&.path.to_s
        name = HOSTS.find { |h, _| host(url) == h || host(url).end_with?(".#{h}") }&.last
        text = if (m = host(url).match(/\A([a-z]+)\.wikipedia\.org\z/))
            title = URI.decode_www_form_component(path.sub(%r{\A/wiki/}, '')).tr('_', ' ')
            "Wikipedia#{m[1] == 'en' ? '' : " (#{m[1]})"}: #{title}"
        elsif name || several
            segment = URI.decode_www_form_component(path.split('/').reject(&:empty?).last.to_s).sub(/\.(html?|php|pdf)\z/i, '').tr('_-', '  ')
            [name || host(url), segment.empty? || segment.match?(/\A[\d.]+\z/) ? nil : segment].compact.join(': ')
        else
            host(url)
        end
        path.downcase.end_with?('.pdf') ? "#{text} (PDF)" : text
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
        persons = load.map { |p| p.merge(:link => "/reference/personen##{p[:part]}") }
        histories = history_load
        counts = Hash.new(0)
        missing = 0
        sections = (persons + histories).map do |person|
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
                "<h2><a href='#{h(person[:link])}'>#{h(person[:name])}</a></h2>#{body}#{credit}</section>"
        end
        filters = [['all', 'Alle', counts.values.sum]] + STATES.map { |key, (label, _)| [key, label, counts[key]] } +
                  [['missing', 'Nicht mehr im Kasten', missing]]
        buttons = filters.map do |key, label, count|
            "<button type='button' class='btn btn-sm btn-outline-secondary#{key == 'all' ? ' active' : ''}' data-filter='#{key}'>" \
                "#{h(label)} <span class='badge text-bg-secondary'>#{count}</span></button>"
        end.join
        "<div class='ps-filters'>#{buttons}</div>" \
            "<p class='ps-summary'>#{persons.size} Personen und #{histories.size} Kästen zur Geschichte, #{counts.values.sum} Sätze, davon #{counts['sourced']} belegt, " \
            "#{counts['open']} offen.</p>#{sections.join}"
    end
end
