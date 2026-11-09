require 'cgi'
require 'fileutils'
require 'json'
require 'securerandom'
require_relative 'workspace_files'

# Publishing a Telefonspiel (docs/plan-telefonspiel.md, »Submitting«,
# »Moderation«, »The Telefonbuch«): a student submits a game from
# /workspace/telefon with a title, a line of description and a number from
# of three or four digits; the server keeps a copy of the game's folder;
# the maintainer publishes or rejects it (with a reason) from a page the
# notification mail links to; the student is told by mail either way and
# can take a game back at any time. Published games are listed in the
# public Telefonbuch, sorted by title, without names.
#
# Plain Ruby without the database: phone_games_api.rb stores the PhoneGame
# nodes and calls these.
module PhoneGames
    ROOT = '/internal/phone-games'
    FOLDER = 'telefon'                       # where games are in the Workspace
    LANGUAGES = %w[.rb .py .js].freeze
    # what is copied besides the game: its music and other files it reads
    KEEP = %w[.rb .py .js .mp3 .ogg .wav .m4a .json .txt .csv].freeze
    MAX_FILE = 20 * 1024 * 1024
    MAX_TOTAL = 40 * 1024 * 1024
    MAX_FILES = 200
    MAX_TITLE = 60
    MAX_DESCRIPTION = 140
    MAX_REASON = 1000
    STATES = %w[pending published rejected].freeze
    # the sentences a game says and the music it plays, as the emulator
    # finds them (src/phone/emulator/telefon: SAY, YOUTUBE)
    SAY = /\bsay\s*\(?\s*(['"])((?:\\.|(?!\1)[^\\\n])*)\1/
    YOUTUBE = %r{https://(?:www\.|m\.|music\.)?(?:youtube\.com/(?:watch\?v=|shorts/)|youtu\.be/)[\w-]{6,}(?:[?&][\w=&%.-]*)?}
    MUSIC_FILE = /\bmusic\s*\(?\s*(['"])([^'"\n]+\.(?:mp3|ogg|wav|m4a))\1/
    # the game names itself in its code: title '…' and subtitle '…' (Ruby),
    # title('…') (Python, JavaScript), each at the start of a line
    TITLE = /^[ \t]*title\s*\(?\s*(['"])((?:\\.|(?!\1)[^\\\n])*)\1/
    SUBTITLE = /^[ \t]*subtitle\s*\(?\s*(['"])((?:\\.|(?!\1)[^\\\n])*)\1/

    class Refused < StandardError
        attr_reader :problem

        def initialize(problem, message)
            @problem = problem
            super(message)
        end
    end

    # { title:, subtitle: } as the game's code states them (nil where missing)
    def self.names(code)
        pick = ->(re) { (m = code.to_s.match(re)) ? clean_text(m[2].gsub(/\\(['"\\])/, '\1'), re == TITLE ? MAX_TITLE : MAX_DESCRIPTION) : nil }
        { :title => pick.(TITLE), :subtitle => pick.(SUBTITLE) }
    end

    # The names as the game itself states them while it runs: the lines
    # »Titel: …« and »Untertitel: …« of telefon --test (the protocol's title
    # and subtitle, the same for every language)
    def self.names_from_run(transcript)
        pick = ->(prefix, max) { (line = transcript.to_s.lines.find { |l| l.start_with?(prefix) }) ? clean_text(line.delete_prefix(prefix), max) : nil }
        { :title => pick.('Titel: ', MAX_TITLE), :subtitle => pick.('Untertitel: ', MAX_DESCRIPTION) }
    end

    # The games a student can submit, each with its names from the code:
    # [{ name:, title:, subtitle: }]
    def self.games(ws)
        candidates(ws).map do |name|
            code = ws.read_text(File.join(FOLDER, name), :max => 512 * 1024) rescue ''
            { :name => name }.merge(names(code))
        end
    end

    def self.new_id
        SecureRandom.hex(12)
    end

    def self.valid_id?(id)
        id.to_s.match?(/\A[0-9a-f]{24}\z/)
    end

    # A number a game may have: three or four digits (000 to 9999)
    def self.valid_number?(number)
        number.to_s.match?(/\A\d{3,4}\z/)
    end

    # The letters on the keys of a phone (ITU E.161), as under the digits of
    # the Telefonbuch's phone
    KEYPAD = { 'ABC' => '2', 'DEF' => '3', 'GHI' => '4', 'JKL' => '5', 'MNO' => '6', 'PQRS' => '7', 'TUV' => '8', 'WXYZ' => '9' }.freeze

    # A number as a student may type it: three or four digits or letters
    # (a vanity number like DINO). [digits, word], the word nil without
    # letters; nil when it is no number.
    def self.parse_number(text)
        text = text.to_s.strip.upcase
        return nil unless text.match?(/\A[0-9A-Z]{3,4}\z/)

        digits = text.chars.map { |c| c.match?(/\d/) ? c : KEYPAD.find { |letters, _| letters.include?(c) }.last }.join
        [digits, text.match?(/[A-Z]/) ? text : nil]
    end

    # How a game's number is written: DINO (3466), or the digits alone
    def self.label(game)
        game[:word] ? "#{game[:word]} (#{game[:number]})" : game[:number].to_s
    end

    def self.clean_text(text, max)
        text.to_s.gsub(/[[:cntrl:]]/, ' ').gsub(/\s+/, ' ').strip[0, max]
    end

    # The games a student can submit: the program files of /workspace/telefon
    def self.candidates(ws)
        return [] unless ws && ws.entry(FOLDER)[:type] == :dir

        ws.list(FOLDER).select { |e| e[:type] == :file && LANGUAGES.include?(File.extname(e[:name])) }.map { |e| e[:name] }
    rescue WorkspaceFiles::Refused
        []
    end

    # What a submission copies: the game and the files beside it it may use
    # (music, data), from /workspace/telefon only, without subfolders.
    # [{ name:, size: }]; Refused when the game is missing or too much.
    def self.plan(ws, file)
        name = File.basename(file.to_s)
        raise Refused.new(:no_workspace, 'Dein Workspace ist gerade nicht erreichbar.') unless ws
        unless candidates(ws).include?(name)
            raise Refused.new(:no_game, "In /workspace/#{FOLDER} gibt es kein Spiel #{name}.")
        end

        files = ws.list(FOLDER).select { |e| e[:type] == :file && KEEP.include?(File.extname(e[:name]).downcase) && e[:size] <= MAX_FILE }
        files = files.first(MAX_FILES)
        total = files.sum { |e| e[:size] }
        if total > MAX_TOTAL
            raise Refused.new(:too_big, "Im Ordner #{FOLDER} liegen zusammen mehr als #{MAX_TOTAL / 1024 / 1024} MB. Nimm große Dateien heraus, die das Spiel nicht braucht.")
        end

        files.map { |e| { :name => e[:name], :size => e[:size] } }
    end

    # Copies the planned files into ROOT/<id>
    def self.copy(ws, files, id)
        dir = File.join(ROOT, id)
        FileUtils.mkdir_p(dir)
        files.each do |f|
            File.binwrite(File.join(dir, f[:name]), ws.read(File.join(FOLDER, f[:name]), :max => MAX_FILE))
        end
        dir
    end

    # The list the line reads (src/phone/line/menu.py): every published game
    # with its folder there (games/<id>, the copy)
    def self.manifest(published)
        games = published.map { |g| { :number => g[:number], :title => g[:title], :folder => "games/#{g[:id]}", :file => g[:file] } }
        JSON.pretty_generate(:games => games)
    end

    def self.write_manifest(published)
        FileUtils.mkdir_p(ROOT)
        path = File.join(ROOT, 'published.json')
        text = manifest(published)
        return if File.exist?(path) && File.read(path) == text

        File.write("#{path}.tmp", text)
        File.rename("#{path}.tmp", path)
    end

    # A ticket to try a game on the line before it is published (the button
    # »Ausprobieren« on /telefon-freigabe): ROOT/trials.json, which the line
    # reads (src/phone/line/line.py), for this one copy, for an hour; old
    # tickets go when a new one is made
    TRIAL_SECONDS = 3600

    def self.trial(id, file)
        FileUtils.mkdir_p(ROOT)
        path = File.join(ROOT, 'trials.json')
        now = Time.now.to_i
        trials = (JSON.parse(File.read(path)) rescue {})
        trials = {} unless trials.is_a?(Hash)
        trials.select! { |_, t| t.is_a?(Hash) && t['expires'].to_i > now }
        token = SecureRandom.hex(16)
        trials[token] = { 'folder' => "games/#{id}", 'file' => File.basename(file.to_s), 'expires' => now + TRIAL_SECONDS }
        File.write("#{path}.tmp", JSON.generate(trials))
        File.rename("#{path}.tmp", path)
        token
    end

    def self.remove(id)
        FileUtils.rm_rf(File.join(ROOT, id)) if valid_id?(id)
    end

    def self.read_game(id, file)
        path = File.join(ROOT, id, File.basename(file.to_s))
        File.file?(path) ? File.read(path, :mode => 'rb').force_encoding('UTF-8').scrub('�') : nil
    end

    # The fixed sentences and the music of a game's code, in the order they
    # stand: [[:say, text], [:music, source]]
    def self.events(code)
        code = code.to_s
        found = []
        code.scan(SAY) { m = Regexp.last_match; found << [m.begin(0), :say, m[2].gsub(/\\(['"\\])/, '\1')] }
        code.scan(MUSIC_FILE) { m = Regexp.last_match; found << [m.begin(0), :music, m[2]] }
        code.scan(YOUTUBE) do
            m = Regexp.last_match
            # a link inside a sentence is a word of it, not music
            next if found.any? { |at, kind, text| kind == :say && at < m.begin(0) && code[at...m.begin(0)].count("\n").zero? && text.include?(m[0]) }

            found << [m.begin(0), :music, m[0]]
        end
        found.sort_by(&:first).map { |_, kind, text| [kind, text] }
    end

    # What the line makes in advance for a game (src/phone/emulator/telefon,
    # watch_music): the fixed texts of say with their mood, other texts that
    # look like sentences, the YouTube links in one form per video. The
    # moderation publishes only when the sentences are made (/prepared of
    # the speech service), so the first caller does not wait.
    MOOD = /\bmood\s*[:=]\s*(?:['"]|:)(\w+)/
    SENTENCE = /(?<!\w)(['"])([A-ZÄÖÜ](?:(?!\1)[^\\\n])*?[.!?…])\1/
    MOODS = %w[neutral amused angry disgusted drunk sleepy surprised whisper].freeze
    # texts filled in while the game runs (Python f'…', JavaScript `…`); of
    # these and of Ruby's "…#{…}…" only the sentences without a gap
    SAY_FILLED = /\bsay\s*\(\s*(?:f(['"])((?:\\.|(?!\1)[^\\\n])*)\1|`((?:\\.|[^\\`])*)`)/

    # the text in sentences, as the phone fetches them (src/phone/core/game.py)
    def self.sentences(text)
        text.to_s.split.join(' ').split(/(?<=[.!?…:;])\s+/).reject(&:empty?)
    end

    def self.prepared_items(id)
        dir = File.join(ROOT, id.to_s)
        return { :sentences => [], :music => [] } unless valid_id?(id) && Dir.exist?(dir)

        sentences_found = []
        music = []
        Dir.children(dir).sort.select { |n| LANGUAGES.include?(File.extname(n)) }.first(50).each do |name|
            code = File.read(File.join(dir, name), :mode => 'rb').force_encoding('UTF-8').scrub('')
            said = []
            found = []
            code.to_enum(:scan, SAY).each { found << [Regexp.last_match, Regexp.last_match[2], '#{'] }
            code.to_enum(:scan, SAY_FILLED).each { m = Regexp.last_match; found << [m, m[2] || m[3], '{'] }
            found.each do |m, literal, gap|
                said << (m.begin(0)...m.end(0))
                line_end = code.index("\n", m.end(0)) || code.length
                mood = code[m.end(0)...line_end][MOOD, 1]
                sentences(literal.gsub(/\\(['"\\`])/, '\1')).each do |sentence|
                    sentences_found << { :text => sentence, :mood => MOODS.include?(mood) ? mood : nil } unless sentence.include?(gap)
                end
            end
            code.to_enum(:scan, SENTENCE).each do
                m = Regexp.last_match
                next if said.any? { |r| r.cover?(m.begin(2)) }

                sentences(m[2]).each { |sentence| sentences_found << { :text => sentence, :mood => nil } unless sentence.include?('#{') }
            end
            code.scan(YOUTUBE) { |link| music << youtube_link(link) }
        end
        { :sentences => sentences_found.uniq, :music => music.uniq }
    end

    # a YouTube link in the one form the speech service caches it under
    # (src/phone/core/game.py: youtube_link of youtube_start)
    def self.youtube_link(link)
        link = link.sub(/([?&])(t|start)=(\d+h)?(\d+m)?(\d+)?s?(?=&|#|\z)/) { $3 || $4 || $5 ? ($~.post_match.start_with?('&') ? $1 : '') : $& }
        link = link.sub(/[?&]\z/, '').sub('?&', '?')
        m = link.match(%r{\Ahttps://(?:www\.|m\.|music\.)?(?:youtube\.com/(?:watch\?(?:.*&)?v=|shorts/)|youtu\.be/)([\w-]{6,})})
        m ? "https://www.youtube.com/watch?v=#{m[1]}" : link
    end

    # The order of the phone book: by title, as people read it (umlauts
    # with their letter, case and leading articles ignored)
    def self.sort_key(title)
        t = title.to_s.downcase.tr('äöüß', 'aous').sub(/\A(der|die|das|ein|eine)\s+/, '')
        [t.gsub(/[^a-z0-9 ]/, ''), title.to_s]
    end

    def self.phone_book(games)
        games.sort_by { |g| sort_key(g[:title]) }
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # --- the mails ------------------------------------------------------

    def self.mail_to_maintainer(game, student, link)
        subject = "Telefonspiel zur Freigabe: #{label(game)} #{game[:title]}"
        html = "<p>#{h(student)} hat ein Telefonspiel eingereicht:</p>" \
            "<p><b>#{h(label(game))} #{h(game[:title])}</b><br>#{h(game[:description])}</p>" \
            "<p><a href='#{h(link)}'>Ansehen, veröffentlichen oder ablehnen</a></p>"
        [subject, html]
    end

    def self.mail_published(game, link)
        subject = "Dein Telefonspiel ist im Telefonbuch: #{label(game)} #{game[:title]}"
        html = "<p>Dein Telefonspiel <b>#{h(game[:title])}</b> ist veröffentlicht. Im Telefonbuch hat es die Nummer <b>#{h(label(game))}</b>.</p>" \
            "<p><a href='#{h(link)}'>Zu deinem Spiel</a></p>"
        [subject, html]
    end

    def self.mail_rejected(game, reason, link)
        subject = "Dein Telefonspiel wurde nicht veröffentlicht: #{game[:title]}"
        html = "<p>Dein Telefonspiel <b>#{h(game[:title])}</b> wurde nicht veröffentlicht. Der Grund:</p>" \
            "<blockquote>#{h(reason).gsub("\n", '<br>')}</blockquote>" \
            "<p>Du kannst es ändern und noch einmal einreichen: <a href='#{h(link)}'>zu deinem Spiel</a>.</p>"
        [subject, html]
    end

    # --- what the student sees -------------------------------------------

    STATE_TEXT = {
        'pending' => 'wartet auf Freigabe',
        'published' => 'veröffentlicht',
        'rejected' => 'nicht veröffentlicht',
    }.freeze

    # A game as the student's card shows it (no files, no email)
    def self.public_view(game)
        {
            :id => game[:id], :number => game[:number], :word => game[:word], :title => game[:title],
            :description => game[:description], :file => game[:file], :state => game[:state],
            :stateText => STATE_TEXT[game[:state]], :reason => game[:state] == 'rejected' ? game[:reason] : nil,
            :submitted => game[:submitted], :decided => game[:decided],
        }
    end

    # Entries to try the book with (/telefonbuch?muster, in development
    # only): offices that do not exist, with descriptions of every length
    SAMPLE_TITLES = [
        'Amt für Wartezeiten', 'Antragsannahme Nord', 'Antragsannahme Süd', 'Abteilung für Abteilungen', 'Beschwerdestelle für Beschwerden',
        'Büro für Formulare', 'Durchschlagverwaltung', 'Ermittlung der Zuständigkeit', 'Fundbüro für Akten', 'Gebührenstelle',
        'Hausmeisterei des Ministeriums', 'Informationsschalter (geschlossen)', 'Kopierraum', 'Landesamt für Stempel', 'Meldestelle für Meldungen',
        'Nachfragestelle', 'Ordnungsamt, Ablage 3', 'Poststelle, Eingang', 'Poststelle, Ausgang', 'Referat für Verweise',
        'Registratur', 'Schalter 7 (bitte Schalter 8 benutzen)', 'Stelle für Bescheinigungen', 'Terminvergabe für Termine', 'Umzugsbehörde',
        'Verwaltung der Verwaltung', 'Vorzimmer', 'Wartenummernausgabe', 'Zentrale Dienste', 'Zweigstelle der Zentrale',
    ].freeze
    SAMPLE_DESCRIPTIONS = [
        'Montags geschlossen.',
        'Bitte ziehen Sie eine Wartenummer.',
        'Sprechzeiten nach Vereinbarung, Vereinbarungen nach Sprechzeit.',
        'Zuständig für alles, was nicht in die Zuständigkeit anderer Stellen fällt, sofern diese nicht zuständig sind.',
        'Anträge nur in dreifacher Ausfertigung, mit Durchschlag und Durchschlag des Durchschlags.',
        'Auskünfte erteilt die Auskunft.',
        'Vorübergehend dauerhaft geschlossen. Bitte wenden Sie sich an die Stelle, die Sie an uns verwiesen hat.',
        '',
    ].freeze

    def self.sample_entries
        SAMPLE_TITLES.each_with_index.map do |title, i|
            entry = { :number => format('%03d', 101 + i * 29 % 880), :title => title, :description => SAMPLE_DESCRIPTIONS[i * 5 % SAMPLE_DESCRIPTIONS.size] }
            # one with a vanity number, to see how the book shows it
            title == 'Amt für Wartezeiten' ? entry.merge(:number => parse_number('WART')[0], :word => 'WART') : entry
        end
    end

    # The numbers someone else holds, with a pending or published game
    def self.taken_numbers(games, email)
        games.reject { |g| g[:email] == email || g[:state] == 'rejected' }.map { |g| g[:number] }.uniq.sort
    end
end
