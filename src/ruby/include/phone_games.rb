require 'cgi'
require 'fileutils'
require 'json'
require 'securerandom'
require_relative 'workspace_files'

# Publishing a Telefonspiel (docs/plan-telefonspiel.md, »Submitting«,
# »Moderation«, »The Telefonbuch«): a student submits a game from
# /workspace/telefon with a title, a line of description and a number from
# 001 to 999 (000 is chance); the server keeps a copy of the game's folder;
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
    RANDOM = '000'                           # the number that plays a random game
    STATES = %w[pending published rejected].freeze
    # the sentences a game says and the music it plays, as the emulator
    # finds them (src/phone/emulator/telefon: SAY, YOUTUBE)
    SAY = /\bsay\s*\(?\s*(['"])((?:\\.|(?!\1)[^\\\n])*)\1/
    YOUTUBE = %r{https://(?:www\.|m\.|music\.)?(?:youtube\.com/(?:watch\?v=|shorts/)|youtu\.be/)[\w-]{6,}(?:[?&][\w=&%.-]*)?}
    MUSIC_FILE = /\bmusic\s*\(?\s*(['"])([^'"\n]+\.(?:mp3|ogg|wav|m4a))\1/

    class Refused < StandardError
        attr_reader :problem

        def initialize(problem, message)
            @problem = problem
            super(message)
        end
    end

    def self.new_id
        SecureRandom.hex(12)
    end

    def self.valid_id?(id)
        id.to_s.match?(/\A[0-9a-f]{24}\z/)
    end

    # A number a game may have: three digits, not 000
    def self.valid_number?(number)
        number.to_s.match?(/\A\d{3}\z/) && number.to_s != RANDOM
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
        subject = "Telefonspiel zur Freigabe: #{game[:number]} #{game[:title]}"
        html = "<p>#{h(student)} hat ein Telefonspiel eingereicht:</p>" \
            "<p><b>#{h(game[:number])} #{h(game[:title])}</b><br>#{h(game[:description])}</p>" \
            "<p><a href='#{h(link)}'>Ansehen, veröffentlichen oder ablehnen</a></p>"
        [subject, html]
    end

    def self.mail_published(game, link)
        subject = "Dein Telefonspiel ist im Telefonbuch: #{game[:number]} #{game[:title]}"
        html = "<p>Dein Telefonspiel <b>#{h(game[:title])}</b> ist veröffentlicht. Im Telefonbuch hat es die Nummer <b>#{h(game[:number])}</b>.</p>" \
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
            :id => game[:id], :number => game[:number], :title => game[:title],
            :description => game[:description], :file => game[:file], :state => game[:state],
            :stateText => STATE_TEXT[game[:state]], :reason => game[:state] == 'rejected' ? game[:reason] : nil,
            :submitted => game[:submitted], :decided => game[:decided],
        }
    end

    # The numbers someone else holds, with a pending or published game
    def self.taken_numbers(games, email)
        games.reject { |g| g[:email] == email || g[:state] == 'rejected' }.map { |g| g[:number] }.uniq.sort
    end
end
