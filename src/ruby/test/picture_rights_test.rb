require 'minitest/autorun'
require 'date'
require 'tmpdir'
require 'fileutils'
require_relative '../include/picture_rights'

class PictureRightsTest < Minitest::Test
    PERSONEN = <<~MARKDOWN
        # Personen

        <!-- part: ada -->
        ### Ada Lovelace

        <div class='person'>
        <img src='ada.webp' alt='Ada'>
        <p><strong>Ada Lovelace</strong> (1815–1852).</p>
        <p class='person-credit'>Bild: Alfred Chalon, gemeinfrei (<a href='https://example.org'>Wikimedia Commons</a>)</p>
        </div>
        <!-- /part -->

        <!-- part: grace -->
        ### Grace Hopper

        <div class='person'>
        <img src='hopper.webp' alt='Grace'>
        <p><strong>Grace Hopper</strong> (1906–1992).</p>
        </div>
        <!-- /part -->

        <!-- year: 1950 -->
        <!-- birth year not published; placed by a degree -->
        <!-- part: pair -->
        ### Zwei

        <div class='person pair'>
        <div class='portraits'><img src='one.webp' alt='Eins'><img src='two.webp' alt='Zwei'></div>
        <p>Zwei Menschen.</p>
        <p class='person-credit'>Foto: Archiv der Firma, zugeschnitten</p>
        </div>
        <!-- /part -->
    MARKDOWN

    ACTIVITY = <<~MARKDOWN
        # Ein Neuron lernt

        <div class='gallery'>
        <figure><img src='own.webp' alt='Eigenes'><figcaption>Zwei Maße des Schnabels</figcaption></figure>
        <figure><img src='drawing.webp' alt='Fremdes'><figcaption>Die Pinguine. Zeichnung: Allison Horst, CC0</figcaption></figure>
        <figure><img src='photo.webp' alt='Fremdes'><figcaption>Die Station. Foto: Universität</figcaption></figure>
        </div>
    MARKDOWN

    def with_content
        Dir.mktmpdir do |root|
            FileUtils.mkdir_p(File.join(root, 'reference', 'personen'))
            File.write(File.join(root, 'reference', 'personen', 'personen.md'), PERSONEN)
            FileUtils.mkdir_p(File.join(root, 'neuron'))
            File.write(File.join(root, 'neuron', 'neuron.md'), ACTIVITY)
            File.write(File.join(root, 'picture-rights.yaml'), <<~YAML)
                neuron/photo.webp:
                  holder: Universität
                  contact: presse@example.org
                  source: https://example.org/station
                  language: de
                  status: asked
                  asked: 2026-10-27
                reference/personen/gone.webp:
                  status: asked
            YAML
            yield root
        end
    end

    def test_finds_what_needs_permission_and_skips_free_and_own_pictures
        with_content do |root|
            files = PictureRights.scan(root).map { |p| p[:file] }.sort
            assert_equal ['neuron/photo.webp', 'reference/personen/hopper.webp',
                          'reference/personen/one.webp', 'reference/personen/two.webp'], files
        end
    end

    def test_a_person_box_links_to_its_part
        with_content do |root|
            hopper = PictureRights.scan(root).find { |p| p[:file].end_with?('hopper.webp') }
            assert_equal '/reference/personen#grace', hopper[:pages].first[:url]
            assert_nil hopper[:credit]
            two = PictureRights.scan(root).find { |p| p[:file].end_with?('two.webp') }
            assert_equal 'Foto: Archiv der Firma, zugeschnitten', two[:credit]
        end
    end

    def test_the_letter_names_the_page_the_source_and_the_sender
        with_content do |root|
            photo = PictureRights.scan(root).find { |p| p[:file] == 'neuron/photo.webp' }
            record = PictureRights.records(root)['neuron/photo.webp']
            subject, body = PictureRights.letter(photo, record,
                sender: { :name => 'N. N.', :role => 'Lehrkraft', :school => 'Schule X', :email => 'k@example.org', :city => 'Berlin' },
                root: 'https://ws.example')
            assert_includes subject, 'Dürfen wir'
            assert_includes body, 'Sehr geehrte Damen und Herren,'
            assert_includes body, 'an der Schule Schule X in Berlin'
            assert_includes body, 'https://ws.example/neuron'
            assert_includes body, 'https://example.org/station'
            assert_includes body, '»Foto: Universität«'
            # every address on a line of its own, easy to find and to click
            assert_includes body, "Sie finden sie hier:\nhttps://ws.example\n\n"
            assert_includes body, "von dieser Seite:\nhttps://example.org/station\nWir würden es"
            body.lines.grep(/https?:/).each { |line| assert_match(%r{\Ahttps?://\S+\n?\z}, line) }
            assert_includes body, 'Bis wir von Ihnen hören, ist es auf der Seite nicht zu sehen.'
            assert_includes body, 'zeigen wir gern eines, das Sie uns schicken'
            assert body.end_with?("N. N.\nLehrkraft, Schule X\nk@example.org")
        end
    end

    def test_the_letter_asks_for_a_birth_year_not_published
        with_content do |root|
            found = PictureRights.scan(root)
            two = found.find { |p| p[:file].end_with?('two.webp') }
            hopper = found.find { |p| p[:file].end_with?('hopper.webp') }
            assert two[:ask_birth_year]
            refute hopper[:ask_birth_year]
            sender = { :name => 'N. N.', :role => '', :school => '', :email => '', :city => '' }
            _, asking = PictureRights.letter(two, {}, sender: sender, root: 'https://ws.example')
            _, plain = PictureRights.letter(hopper, {}, sender: sender, root: 'https://ws.example')
            assert_includes asking, "a short reply is all it takes.\n\nOne more question"
            assert_includes asking, "whichever you prefer.\n\nThank you"
            refute_includes plain, 'year of birth'
            assert_includes asking, 'may we show it there?'
            assert_includes plain, "a short reply is all it takes.\n\nThank you"
            assert_includes plain, "You can see it here:\nhttps://ws.example\n\n"
            plain.lines.grep(/https?:/).each { |line| assert_match(%r{\Ahttps?://\S+\n?\z}, line) }
            _, german = PictureRights.letter(two, { 'language' => 'de' }, sender: sender, root: 'https://ws.example')
            assert_includes german, "genügt eine kurze Antwort.\n\nNoch eine Frage"
            assert_includes german, "wie es Ihnen lieber ist.\n\nVielen Dank"
        end
    end

    BOOKS = <<~MARKDOWN
        # Ein Buch

        <div class='book with-cover'>
        <img class='book-cover' src='cover-frei.webp' alt='Cover von »Frei«'>
        <p><strong>Frei</strong>, 2020.</p>
        <p class='book-credit'>Cover: die Autorin, CC0</p>
        </div>

        <div class='book with-cover'>
        <img class='book-cover' src='cover-verlag.webp' alt='Cover von »Verlag«'>
        <p><strong>Verlag</strong>, 2018.</p>
        <p class='book-credit'>Cover: Ein Verlag (<a href='https://example.org'>Downloads</a>)</p>
        </div>
    MARKDOWN

    def test_a_publishers_cover_needs_permission_and_gives_way_to_the_icon
        require 'nokogiri'
        Dir.mktmpdir do |root|
            FileUtils.mkdir_p(File.join(root, 'buch'))
            md = File.join(root, 'buch', 'buch.md')
            File.write(md, BOOKS)
            found = PictureRights.scan(root)
            assert_equal ['buch/cover-verlag.webp'], found.map { |p| p[:file] }
            assert_equal 'Cover: Ein Verlag (Downloads)', found.first[:credit]
            live = Nokogiri::HTML(BOOKS.lines.drop(2).join)
            PictureRights.withhold(live, md, root: root)
            boxes = live.css('div.book')
            assert_equal ['cover-frei.webp'], live.css('img').map { |i| i['src'] }
            assert_equal 'book', boxes[1]['class']
            assert_equal 1, boxes[1].css('i.bi-book').size
            assert_empty boxes[1].css('p.book-credit')
            File.write(File.join(root, 'picture-rights.yaml'), "buch/cover-verlag.webp:\n  status: granted\n")
            File.utime(Time.now + 5, Time.now + 5, File.join(root, 'picture-rights.yaml'))
            granted = Nokogiri::HTML(BOOKS.lines.drop(2).join)
            PictureRights.withhold(granted, md, root: root)
            assert_equal 2, granted.css('img').size
        end
    end

    def test_pictures_without_permission_stay_off_the_page
        require 'nokogiri'
        with_content do |root|
            md = File.join(root, 'reference', 'personen', 'personen.md')
            html = "<div class='person'><img src='ada.webp'><p class='person-credit'>Bild: gemeinfrei</p></div>" \
                   "<div class='person'><img src='hopper.webp'><p>Grace</p></div>" \
                   "<div class='person pair'><div class='portraits'><img src='one.webp'><img src='two.webp'></div>" \
                   "<p class='person-credit'>Foto: Archiv der Firma</p></div>"
            live = Nokogiri::HTML(html)
            PictureRights.withhold(live, md, root: root)
            assert_equal ['ada.webp'], live.css('img').map { |i| i['src'] }
            assert_empty live.css('div.portraits')
            assert_equal ['Bild: gemeinfrei'], live.css('p.person-credit').map(&:text)

            development = Nokogiri::HTML(html)
            PictureRights.withhold(development, md, development: true, root: root)
            assert_equal 4, development.css('img').size
            assert_equal 3, development.css('img.picture-withheld').size

            File.write(File.join(root, 'picture-rights.yaml'), "reference/personen/hopper.webp:\n  status: granted\n")
            File.utime(Time.now + 5, Time.now + 5, File.join(root, 'picture-rights.yaml'))
            granted = Nokogiri::HTML(html)
            PictureRights.withhold(granted, md, root: root)
            assert_equal ['ada.webp', 'hopper.webp'], granted.css('img').map { |i| i['src'] }
        end
    end

    def test_the_page_counts_states_and_names_stale_records
        with_content do |root|
            html = PictureRights.page_html(root, dev_only: ->(url) { url == '/neuron' })
            assert_includes html, "data-filter='asked'>Angefragt <span class='badge text-bg-secondary'>1</span>"
            assert_includes html, "data-filter='open'>Offen <span class='badge text-bg-secondary'>3</span>"
            assert_includes html, 'Nur in der Entwicklung: <code>/neuron</code>'
            assert_includes html, '<code>reference/personen/gone.webp</code>'
            assert_includes html, 'mailto:presse%40example.org?subject='
        end
    end
end
