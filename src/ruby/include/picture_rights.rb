require 'cgi'
require 'erb'
require 'set'
require 'yaml'

# The page /picture-rights: every picture on the site that is someone else's
# and not under a free licence, with a letter asking the rights holder for
# permission, ready to send. Which pictures: those whose credit line names
# no free licence, and photos in person boxes without a credit line (their
# source is not even known). What was asked and answered is in
# picture-rights.yaml at the top of the content (the file's head explains
# it); the page only reads it. Until a picture's entry says granted, it
# stays off the pages (withhold).
module PictureRights
    ROOT = '/src/content'
    FILE = 'picture-rights.yaml'
    # What makes a credit line free: a licence anyone may use the picture
    # under (the maintainer accepts NC too: the Workspace is non-commercial)
    FREE = /\b(?:CC0|CC[ -]BY|Creative Commons|gemeinfrei|public domain|GFDL|frei mit Namensnennung)\b/i
    # A caption is a credit when it says who made the picture
    CREDIT = /\b(?:Foto|Fotos|Zeichnung|Bild|Gemälde|Grafik|Illustration)\s*:/
    STATES = {
        'open' => ['Offen', 'bi-envelope'],
        'asked' => ['Angefragt', 'bi-hourglass-split'],
        'granted' => ['Erlaubt', 'bi-check-lg'],
        'refused' => ['Abgelehnt', 'bi-x-octagon'],
    }.freeze

    def self.text(html)
        CGI.unescapeHTML(html.to_s.gsub(/<[^>]+>/, '')).gsub(/\s+/, ' ').strip
    end

    # The address of the page a Markdown file becomes
    def self.url(md, root)
        relative = md.delete_prefix("#{root}/")
        dir = File.dirname(relative)
        dir == '.' ? '/' : "/#{dir}"
    end

    def self.title(markdown)
        line = markdown.lines.find { |l| l.start_with?('# ') }
        line ? text(line.sub(/^#\s*/, '')) : nil
    end

    # The part a line belongs to, for a link into the Personen page
    def self.part_at(markdown, offset)
        markdown[0, offset].scan(/<!--\s*part:\s*([a-z0-9-]+)\s*-->/).flatten.last
    end

    # [{ file:, pages: [{ url:, title: }], credit:, credit_html:, box: }] for
    # every picture that needs permission, found in the Markdown files below
    # root. box is :person or :figure.
    def self.scan(root = ROOT)
        found = {}
        Dir.glob(File.join(root, '**', '*.md')).sort.each do |md|
            markdown = File.read(md, encoding: 'UTF-8')
            page = { :url => url(md, root), :title => title(markdown) || url(md, root) }
            blocks = []
            # a box ends with </div> on a line of its own (a pair box holds
            # its portraits in a div of their own)
            markdown.scan(%r{<div class='person(?: [a-z ]+)?'>.*?^</div>}m) { blocks << [:person, $~] }
            markdown.scan(%r{<figure>.*?</figure>}m) { blocks << [:figure, $~] }
            blocks.each do |box, match|
                block = match[0]
                credit_html = if box == :person
                    block[%r{<p class='person-credit'>(.*?)</p>}m, 1]
                else
                    caption = block[%r{<figcaption>(.*?)</figcaption>}m, 1]
                    caption if caption && text(caption) =~ CREDIT
                end
                # from the caption only the credit (»Foto: …«), not what it says
                credit = credit_html && text(credit_html)
                credit = credit[credit.index(CREDIT)..] if credit && box == :figure
                next if credit && credit =~ FREE
                next if credit.nil? && box == :figure

                block.scan(/<img src='([^']+)'/).flatten.each do |src|
                    next if src.start_with?('/', 'http')

                    file = File.join(File.dirname(md.delete_prefix("#{root}/")), src)
                    part = part_at(markdown, match.begin(0))
                    link = page[:url] == '/reference/personen' && part ? "#{page[:url]}##{part}" : page[:url]
                    entry = found[file] ||= { :file => file, :pages => [], :credit => credit, :credit_html => credit_html, :box => box }
                    entry[:pages] << page.merge(:url => link) unless entry[:pages].any? { |p| p[:url] == link }
                end
            end
        end
        found.values
    end

    # The pictures (absolute paths) that need permission and do not have it
    # yet: they stay off the pages (withhold). Scanned again when a page or
    # the records changed.
    @@withheld = nil
    def self.withheld(root = ROOT)
        files = Dir.glob(File.join(root, '**', '*.md')) + [File.join(root, FILE)]
        key = files.sum { |f| File.file?(f) ? File.mtime(f).to_f : 0 }
        return @@withheld[1] if @@withheld && @@withheld[0] == [root, key]

        records = records(root)
        paths = scan(root).reject { |p| state(records[p[:file]] || {}) == 'granted' }.map { |p| File.join(root, p[:file]) }
        @@withheld = [[root, key], paths.to_set]
        paths.to_set
    end

    # Takes the pictures without permission out of a rendered page (doc,
    # from the Markdown file at md_path), with their credit line; in
    # development they stay, marked, so the maintainer sees what is missing.
    def self.withhold(doc, md_path, development: false, root: ROOT)
        withheld = withheld(root)
        return if withheld.empty?

        doc.css('img').each do |img|
            src = img['src'].to_s
            next if src.empty? || src.start_with?('/', 'http', 'data:')
            next unless withheld.include?(File.expand_path(src, File.dirname(md_path)))

            if development
                img.add_class('picture-withheld')
                img['title'] = 'Nur in der Entwicklung zu sehen: wartet auf Erlaubnis (/picture-rights)'
                next
            end
            figure = img.ancestors('figure').first
            box = img.ancestors('div.person').first
            if box
                img.remove
                box.css('div.portraits').each { |d| d.remove if d.css('img').empty? }
                box.css('p.person-credit').each(&:remove) if box.css('img').empty?
            elsif figure
                gallery = figure.parent
                figure.remove
                gallery.remove if gallery && gallery['class'].to_s.include?('gallery') && gallery.css('img').empty?
            else
                img.remove
            end
        end
    end

    def self.records(root = ROOT)
        path = File.join(root, FILE)
        File.file?(path) ? (YAML.safe_load(File.read(path), permitted_classes: [Date], aliases: false) || {}) : {}
    end

    def self.state(record)
        STATES.key?(record['status'].to_s) ? record['status'].to_s : 'open'
    end

    # The operator, from credentials.rb: name, role, school, town, address
    def self.sender
        value = ->(name) { Object.const_defined?(name) ? Object.const_get(name).to_s.strip : '' }
        {
            :name => value.('OPERATOR_NAME'), :role => value.('OPERATOR_ROLE'),
            :school => value.('OPERATOR_SCHOOL'), :email => value.('CONTACT_EMAIL'),
            # the town from the address's last line (»12345 Ort«)
            :city => value.('OPERATOR_ADDRESS').lines.last.to_s.sub(/\A\s*\d+\s*/, '').strip,
        }
    end

    # The public address of the site, for the links in a letter: in
    # development the one of the installation the letter is about
    def self.web_root
        if Object.const_defined?('PUBLIC_WEB_ROOT')
            PUBLIC_WEB_ROOT
        elsif Object.const_defined?('WEB_ROOT')
            WEB_ROOT
        else
            ''
        end
    end

    LETTERS = {
        'en' => {
            :subject => 'May we use <%= r["picture_en"] || "your picture" %> on a school learning platform?',
            :body => <<~'TEXT',
                <%= r['greeting'] || 'Dear Sir or Madam' %>,

                I teach computer science<%= s[:school].empty? ? '' : " at #{s[:school]}" %><%= s[:city].empty? ? '' : " in #{s[:city]}, Germany" %> and run the Hackschule Workspace, a free, non-commercial learning platform for school students (<%= root %>). There are no ads and no fees; students write programs in their browser and read short portraits of the people behind the ideas they learn.

                On <%= pages.size == 1 ? 'this page' : 'these pages' %> we would like to show <%= r['picture_en'] || 'your picture' %><%= r['context_en'] ? ", #{r['context_en']}" : '' %>:
                <%= pages.join("\n") %>

                The picture comes from <%= r['source'] %>. We would credit it as »<%= credit %>« with a link to the source, or however you prefer.

                May we use it there? Until we hear from you, it is not shown on the site. If you would rather we didn't use it, a short reply is all it takes.

                Thank you very much and kind regards,
                <%= [s[:name], [s[:role], s[:school]].reject(&:empty?).join(', '), s[:email]].reject(&:empty?).join("\n") %>
            TEXT
        },
        'de' => {
            :subject => 'Dürfen wir <%= r["picture_de"] || "Ihr Bild" %> auf einer Lernplattform für Schulen zeigen?',
            :body => <<~'TEXT',
                <%= r['greeting'] || 'Sehr geehrte Damen und Herren' %>,

                ich unterrichte Informatik<%= s[:school].empty? ? '' : " an der Schule #{s[:school]}" %><%= s[:city].empty? ? '' : " in #{s[:city]}" %> und betreibe den Hackschule Workspace, eine kostenlose, nicht kommerzielle Lernplattform für Schülerinnen und Schüler (<%= root %>). Es gibt keine Werbung und keine Gebühren; die Schülerinnen und Schüler programmieren im Browser und lesen kurze Porträts der Menschen hinter dem, was sie lernen.

                Auf <%= pages.size == 1 ? 'dieser Seite' : 'diesen Seiten' %> würden wir gern <%= r['picture_de'] || 'Ihr Bild' %> zeigen<%= r['context_de'] ? ", #{r['context_de']}" : '' %>:
                <%= pages.join("\n") %>

                Das Bild stammt von <%= r['source'] %>. Wir würden es »<%= credit %>« nennen, mit einem Link zur Quelle, oder so, wie Sie es wünschen.

                Dürfen wir es dort zeigen? Bis wir von Ihnen hören, ist es auf der Seite nicht zu sehen. Wenn Sie das nicht möchten, genügt eine kurze Antwort.

                Vielen Dank und freundliche Grüße
                <%= [s[:name], [s[:role], s[:school]].reject(&:empty?).join(', '), s[:email]].reject(&:empty?).join("\n") %>
            TEXT
        },
    }.freeze

    # [subject, body] of the letter for a picture and its record
    def self.letter(picture, record, sender: self.sender, root: web_root)
        r = record
        s = sender
        language = LETTERS.key?(r['language'].to_s) ? r['language'].to_s : 'en'
        pages = picture[:pages].map { |p| "#{root}#{p[:url]}" }
        credit = picture[:credit] || r['credit'] || ''
        b = binding
        LETTERS[language].map { |_, template| ERB.new(template).result(b).strip }
    end

    def self.h(value)
        CGI.escapeHTML(value.to_s)
    end

    def self.icon(state)
        label, icon = STATES[state]
        "<i class='bi #{icon}' title='#{h(label)}' aria-label='#{h(label)}'></i>"
    end

    def self.mailto(contact, subject, body)
        "mailto:#{ERB::Util.url_encode(contact.to_s)}?subject=#{ERB::Util.url_encode(subject)}&body=#{ERB::Util.url_encode(body)}"
    end

    # One card: the picture, where it is, the record, the letter
    def self.card_html(picture, record, src, hidden: [])
        state = state(record)
        subject, body = letter(picture, record)
        contact = record['contact'].to_s
        email = contact.include?('@') && !contact.start_with?('http')
        pages = picture[:pages].map { |p| "<a href='#{h(p[:url])}'>#{h(p[:title])}</a>" }.join(', ')
        facts = []
        facts << "<dt>Rechte</dt><dd>#{h(record['holder'])}</dd>" if record['holder']
        facts << "<dt>Kontakt</dt><dd>#{email ? h(contact) : "<a href='#{h(contact)}' target='_blank' rel='noopener'>#{h(contact)}</a>"}</dd>" unless contact.empty?
        facts << "<dt>Quelle</dt><dd><a href='#{h(record['source'])}' target='_blank' rel='noopener'>#{h(record['source'])}</a></dd>" if record['source']
        facts << "<dt>Angefragt</dt><dd>#{h(record['asked'])}</dd>" if record['asked']
        facts << "<dt>Antwort</dt><dd>#{h(record['answered'])}</dd>" if record['answered']
        facts << "<dt>Notiz</dt><dd>#{h(record['note'])}</dd>" if record['note']
        missing = []
        missing << 'Wer die Rechte hat, steht noch nicht fest.' unless record['holder']
        missing << 'Der Kontakt fehlt noch.' if contact.empty?
        missing << 'Die Quelle fehlt noch.' unless record['source']
        hint = missing.empty? ? '' : "<p class='pr-missing'><i class='bi bi-search'></i>#{h(missing.join(' '))}</p>"
        unless hidden.empty?
            hint += "<p class='pr-missing'><i class='bi bi-eye-slash'></i>Nur in der Entwicklung: #{hidden.map { |u| "<code>#{h(u)}</code>" }.join(', ')}. " \
                'Der Link im Brief führt dort ins Leere, bis die Seite öffentlich ist.</p>'
        end
        send = if state == 'open' || state == 'asked'
            button = if email
                "<a class='btn btn-sm btn-primary' href='#{h(mailto(contact, subject, body))}' data-mailto='#{h(contact)}'><i class='bi bi-envelope'></i> Im Mailprogramm öffnen</a>"
            elsif !contact.empty?
                "<a class='btn btn-sm btn-primary' href='#{h(contact)}' target='_blank' rel='noopener'><i class='bi bi-box-arrow-up-right'></i> Kontaktformular öffnen</a>"
            else
                ''
            end
            "<details class='pr-letter'#{state == 'open' && missing.empty? ? ' open' : ''}><summary>Brief#{record['language'] == 'de' ? '' : ' (englisch)'}</summary>" \
                "<input class='form-control form-control-sm pr-subject' value='#{h(subject)}' aria-label='Betreff'>" \
                "<textarea class='form-control form-control-sm pr-body' rows='14' aria-label='Text'>#{h(body)}</textarea>" \
                "<div class='pr-actions'>#{button}<button type='button' class='btn btn-sm btn-outline-secondary pr-copy'><i class='bi bi-clipboard'></i> Kopieren</button></div></details>"
        else
            ''
        end
        credit = picture[:credit_html] ? "<p class='pr-credit'>#{picture[:credit_html]}</p>" : "<p class='pr-credit pr-none'>Keine Bildzeile</p>"
        "<section class='pr-picture pr-#{state}' data-state='#{state}'>" \
            "<img src='#{h(src)}' alt='' loading='lazy'>" \
            "<div class='pr-body-col'><h2>#{icon(state)}<code>#{h(picture[:file])}</code></h2>" \
            "<p class='pr-pages'>Auf: #{pages}</p>#{credit}<dl>#{facts.join}</dl>#{hint}#{send}</div></section>"
    end

    # The page's content (static/picture-rights.html): everything from the
    # files escaped, except the credit line, which is the page's own HTML.
    # image gives the address a picture is served at (Main.convert_image),
    # dev_only whether a page is not public yet.
    def self.page_html(root = ROOT, image: ->(_path) { '' }, dev_only: ->(_url) { false })
        records = records(root)
        pictures = scan(root)
        known = pictures.map { |p| p[:file] }
        # pictures with a letter still to send or answer that are no longer on
        # a page: replaced or removed
        gone = records.select { |_, r| ['open', 'asked'].include?(state(r)) }.keys - known
        counts = Hash.new(0)
        cards = pictures.sort_by { |p| [STATES.keys.index(state(records[p[:file]] || {})), p[:file]] }.map do |picture|
            record = records[picture[:file]] || {}
            counts[state(record)] += 1
            hidden = picture[:pages].map { |p| p[:url] }.select { |u| dev_only.(u) }
            card_html(picture, record, image.(File.join(root, picture[:file])), hidden: hidden)
        end
        filters = [['all', 'Alle', pictures.size]] + STATES.map { |key, (label, _)| [key, label, counts[key]] }
        buttons = filters.map do |key, label, count|
            "<button type='button' class='btn btn-sm btn-outline-secondary#{key == 'all' ? ' active' : ''}' data-filter='#{key}'>" \
                "#{h(label)} <span class='badge text-bg-secondary'>#{count}</span></button>"
        end.join
        stale = gone.empty? ? '' : "<p class='pr-gone'>Nicht mehr auf einer Seite, aber noch in <code>#{FILE}</code>: #{gone.map { |f| "<code>#{h(f)}</code>" }.join(', ')}</p>"
        "<div class='pr-filters'>#{buttons}</div>#{stale}#{cards.join}"
    end
end
