require 'net/http'
require 'shellwords'
require_relative 'phone_games'

# Publishing Telefonspiele (PhoneGames, docs/plan-telefonspiel.md): the
# student's card on the activity page (phone_publish.js), the maintainer's
# decision (/telefon-freigabe), the public Telefonbuch (/telefonbuch).
# A submission is a PhoneGame node -[:BY]-> its User, its files a copy
# under PhoneGames::ROOT/<id>. Moved out of main.rb, which loads it.
class Main < Sinatra::Base
    # One at a time decides who gets a number
    @@phone_games_mutex = Mutex.new

    def phone_games_all
        neo4j_query(<<~END_OF_QUERY).map { |row| phone_game_from(row['g'], row['email']) }
            MATCH (g:PhoneGame)-[:BY]->(u:User)
            RETURN g, u.email AS email;
        END_OF_QUERY
    end

    def phone_game_from(node, email)
        {
            :id => node[:id], :number => node[:number], :word => node[:word], :title => node[:title], :description => node[:description],
            :file => node[:file], :state => node[:state], :reason => node[:reason], :transcript => node[:transcript],
            :submitted => node[:submitted], :decided => node[:decided], :email => email,
            # the newest error of a call (PhoneCalls), for its author
            :error => node[:last_error], :error_at => node[:last_error_at], :errors => node[:errors].to_i,
            # not in the book nor the phone's menu, reached by its number (a part of another game)
            :hidden => node[:hidden] == true,
        }
    end

    def phone_game(id)
        return nil unless PhoneGames.valid_id?(id)

        phone_games_all.find { |g| g[:id] == id }
    end

    def phone_games_publish_list
        PhoneGames.write_manifest(phone_games_all.select { |g| g[:state] == 'published' })
    rescue StandardError => e
        STDERR.puts "Telefonbuch: the list for the line could not be written: #{e.message}"
    end

    def phone_games_link
        "#{WEB_ROOT}/telefonspiel#veroeffentlichen"
    end

    # What the card needs: the student's games, the numbers others hold,
    # the files they can submit
    def phone_games_state
        email = @session_user[:email]
        all = phone_games_all
        ws = task_workspace_files
        {
            :games => all.select { |g| g[:email] == email }.sort_by { |g| -g[:submitted].to_i }.map { |g| PhoneGames.public_view(g) },
            :taken => PhoneGames.taken_numbers(all, email),
            :files => PhoneGames.games(ws),
            :folder => PhoneGames::FOLDER,
            :workspace => !ws.nil?,                     # false: no Workspace yet, so no files either
        }
    end

    # The beginning of the game as telefon --test plays it without keys, in
    # the student's Workspace as a guest (as the phone check runs games):
    # for the maintainer, never in this container
    def phone_game_transcript(file)
        command = Shellwords.join(['telefon', '--test', '--seconds', '10', '--keys', '-', File.join(PhoneGames::FOLDER, file)]) + ' 2>&1 | head -c 20000'
        result = workspace_runtime.run_command(fs_tag_for_email(@session_user[:email]), :command => command,
            :uid => GUEST_UID, :home => '/tmp', :seconds => 20)
        return 'Der Workspace lief beim Einreichen nicht; kein Probelauf.' if result.nil?

        result[:output].to_s.force_encoding('UTF-8').scrub('�')[0, 20000]
    end

    # The program's syntax, checked in the student's Workspace as a guest
    # (PhoneGames::SYNTAX): { ok:, output: }, or nil when the Workspace does
    # not run (then the maintainer sees the test run instead)
    def phone_game_syntax(rel)
        check = PhoneGames::SYNTAX[File.extname(rel)]
        return nil unless check

        command = Shellwords.join(check.(File.join('/workspace', rel))) + ' 2>&1 | head -c 4000; exit ${PIPESTATUS[0]}'
        result = workspace_runtime.run_command(fs_tag_for_email(@session_user[:email]), :command => "bash -c #{Shellwords.escape(command)}",
            :uid => GUEST_UID, :home => '/tmp', :seconds => 15)
        return nil if result.nil? || result[:timed_out]

        { :ok => result[:status].to_i.zero?, :output => result[:output].to_s.force_encoding('UTF-8').scrub('�')[0, 4000] }
    end

    post '/api/phone/mine' do
        assert(user_logged_in?)
        respond(phone_games_state)
    end

    post '/api/phone/submit' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:file, :number], :max_value_lengths => { :file => 200, :number => 4 })
        email = @session_user[:email]
        number, word = PhoneGames.parse_number(data[:number])
        halt 400, { :error => 'number' }.to_json unless number
        ws = task_workspace_files
        begin
            files = PhoneGames.plan(ws, data[:file])
        rescue PhoneGames::Refused, WorkspaceFiles::Refused => e
            halt 400, { :error => 'files', :message => e.message }.to_json
        end
        path = data[:file].to_s
        file = File.basename(path)
        # a program that does not even start cannot be published: its
        # syntax checked in the Workspace first (the error with its line)
        syntax = phone_game_syntax(File.join(PhoneGames::FOLDER, path))
        if syntax && !syntax[:ok]
            halt 400, { :error => 'syntax', :message => "#{path} startet nicht: Die Sprache meldet einen Fehler. Korrigier ihn und reich das Spiel dann ein.",
                        :output => syntax[:output] }.to_json
        end
        # the names are the game's own: as it states them in the test run
        # (title '…', subtitle '…' in any language), else as its code has them
        transcript = phone_game_transcript(path)
        ran = PhoneGames.names_from_run(transcript)
        code = PhoneGames.names(ws.read_text(File.join(PhoneGames::FOLDER, path), :max => 512 * 1024))
        title, description = ran[:title] || code[:title], ran[:subtitle] || code[:subtitle]
        halt 400, { :error => 'title' }.to_json unless title
        halt 400, { :error => 'subtitle' }.to_json unless description
        id = PhoneGames.new_id
        @@phone_games_mutex.synchronize do
            all = phone_games_all
            if PhoneGames.taken_numbers(all, email).include?(number)
                halt 409, { :error => 'taken', :message => "Die Nummer #{word || number} hat schon ein anderes Spiel." }.to_json
            end
            PhoneGames.copy(ws, files, id, path)
            phone_game_transcribe_later(id)
            # a pending version of the same number is replaced by this one; a
            # published one stays playable until this one is approved
            all.select { |g| g[:email] == email && g[:number] == number && g[:state] != 'published' }.each do |old|
                neo4j_query('MATCH (g:PhoneGame {id: $id}) DETACH DELETE g;', :id => old[:id])
                PhoneGames.remove(old[:id])
            end
            neo4j_query(<<~END_OF_QUERY, :email => email, :id => id, :number => number, :word => word, :title => title, :description => description, :file => file, :transcript => transcript, :now => Time.now.to_i)
                MATCH (u:User {email: $email})
                CREATE (g:PhoneGame {id: $id, number: $number, word: $word, title: $title, description: $description, file: $file,
                    state: 'pending', transcript: $transcript, submitted: $now})-[:BY]->(u);
            END_OF_QUERY
        end
        game = phone_game(id)
        mail_subject, html = PhoneGames.mail_to_maintainer(game, email, "#{WEB_ROOT}/telefon-freigabe?id=#{id}")
        ADMIN_USERS.each do |admin|
            deliver_mail do
                to admin
                from SMTP_FROM
                subject mail_subject
                html
            end
        rescue StandardError => e
            STDERR.puts "Telefonspiel #{id}: mail to #{admin} failed: #{e.message}"
        end
        log_event "Telefonspiel eingereicht von #{email}: #{word || number} #{title}"
        respond(phone_games_state)
    end

    # Taking back: the game and its copy go at once, its number is free
    post '/api/phone/withdraw' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:id])
        game = phone_game(data[:id])
        halt 404 unless game && game[:email] == @session_user[:email]
        neo4j_query('MATCH (g:PhoneGame {id: $id}) DETACH DELETE g;', :id => game[:id])
        PhoneGames.remove(game[:id])
        phone_games_publish_list
        log_event "Telefonspiel zurückgezogen von #{game[:email]}: #{PhoneGames.label(game)} #{game[:title]}"
        respond(phone_games_state)
    end

    # Another free number for a game (pending or published)
    post '/api/phone/renumber' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:id, :number], :max_value_lengths => { :number => 4 })
        number, word = PhoneGames.parse_number(data[:number])
        halt 400, { :error => 'number' }.to_json unless number
        @@phone_games_mutex.synchronize do
            all = phone_games_all
            game = all.find { |g| g[:id] == data[:id] }
            halt 404 unless game && game[:email] == @session_user[:email] && game[:state] != 'rejected'
            if PhoneGames.taken_numbers(all, game[:email]).include?(number) ||
                    all.any? { |g| g[:email] == game[:email] && g[:id] != game[:id] && g[:number] == number && g[:state] != 'rejected' }
                halt 409, { :error => 'taken', :message => "Die Nummer #{word || number} ist schon vergeben." }.to_json
            end
            neo4j_query('MATCH (g:PhoneGame {id: $id}) SET g.number = $number, g.word = $word;', :id => game[:id], :number => number, :word => word)
        end
        phone_games_publish_list
        respond(phone_games_state)
    end

    # The maintainer decides: publish, or reject with a reason; the student
    # gets a mail either way
    # How far the speech service has made a game's fixed sentences and fetched
    # its music: { total:, ready:, missing:, music:, music_ready: }, or nil
    # when it does not answer
    PHONE_SPEECH_URL = ENV['PHONE_SPEECH'] || 'http://phone_speech:8000'

    # What the copy's audio files say, by the speech service (Whisper), in
    # the background after a submission: the review shows it beside each
    # recording. One file at a time; what fails stays out (the page says so).
    def phone_game_transcribe_later(id)
        Thread.new do
            texts = {}
            PhoneGames.audio_files(id).each do |name|
                uri = URI("#{PHONE_SPEECH_URL}/transcribe")
                data = File.binread(File.join(PhoneGames::ROOT, id, name))
                response = Net::HTTP.start(uri.host, uri.port, :open_timeout => 5, :read_timeout => 600) do |http|
                    http.post(uri.path, data, 'Content-Type' => 'application/octet-stream')
                end
                texts[name] = JSON.parse(response.body)['text'].to_s if response.is_a?(Net::HTTPSuccess)
            rescue StandardError => e
                STDERR.puts "Telefonspiel #{id}: #{name} not written out: #{e.class}: #{e.message}"
            end
            PhoneGames.write_transcripts(id, texts) if Dir.exist?(File.join(PhoneGames::ROOT, id))
        end
    end

    def phone_game_prepared(id)
        items = PhoneGames.prepared_items(id)
        uri = URI("#{PHONE_SPEECH_URL}/prepared")
        response = Net::HTTP.start(uri.host, uri.port, :open_timeout => 3, :read_timeout => 10) do |http|
            http.post(uri.path, items.to_json, 'Content-Type' => 'application/json')
        end
        response.is_a?(Net::HTTPSuccess) ? JSON.parse(response.body, :symbolize_names => true) : nil
    rescue StandardError
        nil
    end

    def phone_game_ready?(prepared)
        prepared && prepared[:ready] == prepared[:total]
    end

    post '/api/phone/prepared' do
        admins_only!
        data = parse_request_data(:required_keys => [:id])
        halt 404 unless PhoneGames.valid_id?(data[:id])
        prepared = phone_game_prepared(data[:id])
        respond(:prepared => prepared, :ready => phone_game_ready?(prepared))
    end

    # An audio file of a submitted copy, for the review's players:
    # administrators only, and only the audio files of that copy
    get '/telefon-freigabe/datei' do
        halt 403 unless admin_logged_in?
        id = params['id'].to_s
        file = File.basename(params['datei'].to_s)
        halt 404 unless PhoneGames.valid_id?(id) && PhoneGames.audio_files(id).include?(file)
        content_type({ '.mp3' => 'audio/mpeg', '.ogg' => 'audio/ogg', '.wav' => 'audio/wav', '.m4a' => 'audio/mp4' }[File.extname(file).downcase])
        cache_control :private, :no_cache
        send_file File.join(PhoneGames::ROOT, id, file)
    end

    post '/api/phone/decide' do
        admins_only!
        data = parse_request_data(:required_keys => [:id, :decision], :optional_keys => [:reason],
            :max_value_lengths => { :reason => PhoneGames::MAX_REASON * 2 })
        game = phone_game(data[:id])
        halt 404 unless game
        halt 409, { :error => 'decided' }.to_json unless game[:state] == 'pending'
        reason = data[:reason].to_s.strip[0, PhoneGames::MAX_REASON]
        case data[:decision]
        when 'publish'
            # the first caller should not wait for sentences being made
            halt 409, { :error => 'not_ready' }.to_json unless phone_game_ready?(phone_game_prepared(game[:id]))
            @@phone_games_mutex.synchronize do
                # the version it replaces leaves the phone book now
                phone_games_all.select { |g| g[:email] == game[:email] && g[:number] == game[:number] && g[:state] == 'published' }.each do |old|
                    neo4j_query('MATCH (g:PhoneGame {id: $id}) DETACH DELETE g;', :id => old[:id])
                    PhoneGames.remove(old[:id])
                end
                neo4j_query("MATCH (g:PhoneGame {id: $id}) SET g.state = 'published', g.decided = $now, g.reason = null;", :id => game[:id], :now => Time.now.to_i)
            end
            mail_subject, html = PhoneGames.mail_published(game, phone_games_link)
        when 'reject'
            halt 400, { :error => 'reason' }.to_json if reason.empty?
            neo4j_query("MATCH (g:PhoneGame {id: $id}) SET g.state = 'rejected', g.decided = $now, g.reason = $reason;", :id => game[:id], :now => Time.now.to_i, :reason => reason)
            PhoneGames.remove(game[:id])
            mail_subject, html = PhoneGames.mail_rejected(game, reason, phone_games_link)
        else
            halt 400
        end
        begin
            deliver_mail do
                to game[:email]
                from SMTP_FROM
                subject mail_subject
                html
            end
        rescue StandardError => e
            STDERR.puts "Telefonspiel #{game[:id]}: mail to the student failed: #{e.message}"
        end
        phone_games_publish_list
        log_event "Telefonspiel #{data[:decision] == 'publish' ? 'veröffentlicht' : 'abgelehnt'} von #{@session_user[:email]}: #{PhoneGames.label(game)} #{game[:title]}"
        respond(:state => data[:decision] == 'publish' ? 'published' : 'rejected')
    end

    # Games that stopped with an error during a call (PhoneCalls.collect):
    # each published one leaves the Telefonbuch (state broken, its copy
    # kept, its number its author's) and its author gets a mail with the
    # error, once; the line's list is written anew.
    def phone_games_take_out(crashed)
        taken = []
        @@phone_games_mutex.synchronize do
            crashed.each do |c|
                game = phone_games_all.find { |g| g[:number] == c[:game] && g[:state] == 'published' }
                next unless game

                neo4j_query("MATCH (g:PhoneGame {id: $id}) SET g.state = 'broken', g.decided = $now;", :id => game[:id], :now => Time.now.to_i)
                taken << [game, c[:text]]
            end
        end
        return if taken.empty?

        phone_games_publish_list
        taken.each do |game, error|
            mail_subject, html = PhoneGames.mail_broken(game, error, phone_games_link)
            begin
                deliver_mail do
                    to game[:email]
                    from SMTP_FROM
                    subject mail_subject
                    html
                end
            rescue StandardError => e
                STDERR.puts "Telefonspiel #{game[:id]}: mail about its error failed: #{e.message}"
            end
            log_event "Telefonspiel aus dem Telefonbuch genommen (Fehler bei einem Anruf): #{PhoneGames.label(game)} #{game[:title]}"
        end
    end

    # The maintainer changes how a game stands in the Telefonbuch: its title
    # and subtitle (pending or published). The Telefonbuch and the line's
    # menu read them from here; the game's own title '…' in its code was
    # only read when it was submitted
    # Shown in the Telefonbuch and the phone's menu or not: a game that is
    # part of another one stays reachable by its number and by transfer
    post '/api/phone/hide' do
        admins_only!
        data = parse_request_data(:required_keys => [:id, :hidden])
        game = phone_game(data[:id])
        halt 404 unless game && %w[published broken pending].include?(game[:state])
        hidden = data[:hidden].to_s == 'true'
        neo4j_query('MATCH (g:PhoneGame {id: $id}) SET g.hidden = $hidden;', :id => game[:id], :hidden => hidden)
        # a game taken out after an error, switched on: back in the book
        if !hidden && game[:state] == 'broken'
            neo4j_query("MATCH (g:PhoneGame {id: $id}) SET g.state = 'published';", :id => game[:id])
        end
        phone_games_publish_list
        log_event "Telefonspiel #{hidden ? 'aus dem Telefonbuch ausgeblendet' : 'im Telefonbuch eingeblendet'} von #{@session_user[:email]}: #{PhoneGames.label(game)} #{game[:title]}"
        respond(:hidden => hidden)
    end

    post '/api/phone/rename' do
        admins_only!
        data = parse_request_data(:required_keys => [:id, :title, :description],
            :max_value_lengths => { :title => PhoneGames::MAX_TITLE * 2, :description => PhoneGames::MAX_DESCRIPTION * 2 })
        game = phone_game(data[:id])
        halt 404 unless game && game[:state] != 'rejected'
        title = PhoneGames.clean_text(data[:title], PhoneGames::MAX_TITLE)
        description = PhoneGames.clean_text(data[:description], PhoneGames::MAX_DESCRIPTION)
        halt 400, { :error => 'title' }.to_json if title.empty?
        halt 400, { :error => 'description' }.to_json if description.empty?
        neo4j_query('MATCH (g:PhoneGame {id: $id}) SET g.title = $title, g.description = $description;',
                    :id => game[:id], :title => title, :description => description)
        phone_games_publish_list
        log_event "Telefonspiel umbenannt von #{@session_user[:email]}: #{PhoneGames.label(game)} #{game[:title]} → #{title}"
        respond(:title => title, :description => description)
    end

    # --- the pages --------------------------------------------------------

    # In the profile, beside the shared live apps: the person's
    # Telefonspiele with their state (nothing without one)
    def phone_games_profile_html
        return '' unless user_logged_in?

        games = phone_games_all.select { |g| g[:email] == @session_user[:email] }.sort_by { |g| g[:number] }
        return '' if games.empty?

        badge = { 'pending' => 'text-bg-warning', 'published' => 'text-bg-success', 'rejected' => 'text-bg-secondary', 'broken' => 'text-bg-danger' }
        items = games.map do |g|
            "<a class='list-group-item list-group-item-action d-flex justify-content-between align-items-center' href='/telefonspiel#veroeffentlichen'>" \
                "<span><span class='font-monospace me-2'>#{PhoneGames.h(PhoneGames.label(g))}</span><strong>#{PhoneGames.h(g[:title])}</strong></span>" \
                "<span class='badge #{badge[g[:state]]}'>#{PhoneGames::STATE_TEXT[g[:state]]}</span></a>" +
                (g[:error] ? "<div class='list-group-item text-danger small'><i class='bi bi-exclamation-triangle' aria-hidden='true'></i> " \
                    "#{PhoneGames.h(PhoneGames.label(g))} ist bei einem Anruf mit einem Fehler abgebrochen und steht nicht mehr im Telefonbuch; was die Sprache gemeldet hat, steht in der Aktivität.</div>" : '')
        end.join
        "<section id='telefonspiele' class='my-4'><h3>Deine Telefonspiele</h3>" \
            "<p>Veröffentlichte Spiele stehen im <a href='/telefonbuch'>Telefonbuch</a>. Ändern, eine andere Nummer wählen oder zurückziehen kannst du sie in der Aktivität.</p>" \
            "<div class='list-group'>#{items}</div></section>"
    end

    # /telefon-freigabe?id=…: everything about one submission, with the two
    # buttons (administrators only, main.rb guards the path)
    # /telefon-freigabe without an id: every submission, those waiting
    # first, each linked to its page (the same the mail links to)
    def phone_games_list_html
        h = ->(t) { PhoneGames.h(t) }
        all = phone_games_all
        head = "<h1 class='mt-4'>Telefonbuch</h1>"
        # waiting for approval: each linked to its page (the same the mail links to)
        pending = all.select { |g| g[:state] == 'pending' }.sort_by { |g| -(g[:submitted] || 0).to_i }
        io = +head
        io << "<h2 class='h4 mt-4'>Warten auf Freigabe#{pending.empty? ? '' : " <span class='text-muted fw-normal'>#{pending.size}</span>"}</h2>"
        if pending.empty?
            io << "<p class='text-muted'>Kein Spiel wartet auf Freigabe.</p>"
        else
            io << "<div class='list-group'>"
            pending.each do |g|
                io << "<a class='list-group-item list-group-item-action' href='/telefon-freigabe?id=#{h.(g[:id])}'>" \
                      "<div class='d-flex flex-wrap gap-2 align-items-baseline'><b>#{h.(PhoneGames.label(g))}</b> <b>#{h.(g[:title])}</b>" \
                      "<span class='text-muted small'>#{h.(g[:email])}, #{h.(g[:file])}</span></div>" \
                      "<div class='text-muted small'>#{h.(g[:description])}</div></a>"
            end
            io << '</div>'
        end
        # in the Telefonbuch: sorted by title as the book is; title and
        # subtitle to edit in place, in the book or not: a hidden game (a part
        # of another one) is reached by its number and by transfer, a game
        # taken out after an error is back when switched on
        listed = PhoneGames.phone_book(all.select { |g| %w[published broken].include?(g[:state]) })
        shown = listed.count { |g| g[:state] == 'published' && !g[:hidden] }
        io << "<h2 class='h4 mt-4'>Veröffentlicht <span class='text-muted fw-normal phone-manage-count'>#{listed.size}, davon #{shown} im Telefonbuch</span></h2>" \
              "<p class='text-muted'>Titel und Untertitel änderst du direkt in der Zeile. Ausgeblendete Spiele stehen nicht im Telefonbuch und werden am Telefon nicht vorgelesen; " \
              "über ihre Nummer und mit <code>transfer</code> sind sie weiter erreichbar, etwa als Teil eines anderen Spiels.</p>"
        if listed.empty?
            io << "<p class='text-muted'>Noch ist kein Spiel veröffentlicht.</p>"
        else
            io << "<div class='phone-manage'>"
            listed.each do |g|
                broken = g[:state] == 'broken'
                inside = !broken && !g[:hidden]
                note = if broken
                    "<i class='bi bi-exclamation-triangle-fill' aria-hidden='true'></i>Bei einem Anruf mit einem Fehler abgebrochen, aus dem Telefonbuch genommen. " \
                        "<a href='/telefon-freigabe?id=#{h.(g[:id])}'>Fehler ansehen</a>"
                elsif g[:hidden]
                    "<i class='bi bi-eye-slash' aria-hidden='true'></i>Ausgeblendet: nur über die Nummer erreichbar"
                else
                    ''
                end
                io << "<form class='phone-manage-row#{inside ? '' : ' phone-manage-out'}#{broken ? ' phone-manage-broken' : ''}' data-id='#{h.(g[:id])}' data-broken='#{broken}'>" \
                      "<div class='phone-manage-number'>#{g[:word] ? "<span class='phone-manage-word'>#{h.(g[:word])}</span>" : ''}<span>#{h.(g[:number])}</span></div>" \
                      "<div class='phone-manage-fields'>" \
                      "<input class='phone-manage-title' name='title' maxlength='#{PhoneGames::MAX_TITLE}' value='#{h.(g[:title])}' aria-label='Titel von #{h.(PhoneGames.label(g))}' spellcheck='false'>" \
                      "<input class='phone-manage-description' name='description' maxlength='#{PhoneGames::MAX_DESCRIPTION}' value='#{h.(g[:description])}' aria-label='Untertitel von #{h.(PhoneGames.label(g))}'>" \
                      "<div class='phone-manage-note'>#{note}</div>" \
                      "</div><div class='phone-manage-actions'>" \
                      "<button type='submit' class='btn btn-sm btn-primary phone-manage-save' hidden>Speichern</button>" \
                      "<div class='form-check form-switch'><input class='form-check-input phone-manage-shown' type='checkbox' role='switch' id='shown-#{h.(g[:id])}'#{inside ? ' checked' : ''}>" \
                      "<label class='form-check-label' for='shown-#{h.(g[:id])}'>Im Telefonbuch</label></div>" \
                      "<a class='phone-manage-details' href='/telefon-freigabe?id=#{h.(g[:id])}' title='Details: Code, Probelauf, Abschrift' aria-label='Details zu #{h.(PhoneGames.label(g))}'><i class='bi bi-three-dots' aria-hidden='true'></i></a>" \
                      "<span class='phone-manage-state small' aria-live='polite'></span></div></form>"
            end
            io << '</div>'
            io << "<script src='/include/phone_manage.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
        end
        io
    end

    # the game's program, highlighted as code blocks on the pages are
    # (Rouge, by the file's language)
    def phone_game_code_html(code, file)
        return "<pre class='pg-code'>Die Datei fehlt.</pre>" unless code

        lexer = { '.rb' => Rouge::Lexers::Ruby, '.py' => Rouge::Lexers::Python, '.js' => Rouge::Lexers::Javascript }[File.extname(file.to_s)]
        return "<pre class='pg-code'>#{PhoneGames.h(code)}</pre>" unless lexer

        "<pre class='pg-code long-code'>#{Rouge::Formatters::HTML.new.format(lexer.new.lex(code))}</pre>"
    end

    def phone_game_review_page
        return phone_games_list_html if params[:id].to_s.empty?

        game = phone_game(params[:id].to_s)
        return "<p>Dieses Spiel gibt es nicht mehr: zurückgezogen oder schon entschieden und gelöscht.</p>" unless game

        h = ->(t) { PhoneGames.h(t) }
        code = PhoneGames.read_game(game[:id], game[:file])
        events = PhoneGames.events(code)
        files = Dir.exist?(File.join(PhoneGames::ROOT, game[:id])) ? Dir.children(File.join(PhoneGames::ROOT, game[:id])).sort : []
        state = { 'pending' => 'wartet auf Freigabe', 'published' => 'veröffentlicht', 'rejected' => "abgelehnt: #{game[:reason]}" }[game[:state]]
        transcripts = PhoneGames.transcripts(game[:id])
        audio = PhoneGames.audio_files(game[:id])
        written = ->(file) { transcripts.key?(file) ? (transcripts[file].empty? ? '<span class=\'text-muted\'>(nichts verstanden)</span>' : "»#{h.(transcripts[file])}«") : '<span class=\'text-muted\'>wird noch abgeschrieben …</span>' }
        list = events.map do |kind, text|
            case kind
            when :say then "<li>#{h.(text)}</li>"
            when :play then "<li><i class='bi bi-mic'></i> <code>#{h.(text)}</code>: #{audio.include?(File.basename(text)) ? written.(File.basename(text)) : '<span class=\'text-danger\'>die Datei fehlt</span>'}</li>"
            else "<li class='text-muted'><i class='bi bi-music-note'></i> #{h.(text)}</li>"
            end
        end.join
        # every audio file of the copy to listen to, with what Whisper heard
        recordings = if audio.empty?
            ''
        else
            rows = audio.map do |file|
                src = "/telefon-freigabe/datei?id=#{h.(game[:id])}&amp;datei=#{CGI.escape(file)}"
                "<li class='mb-3'><code>#{h.(file)}</code><audio class='d-block mt-1' controls preload='none' src='#{src}'></audio>" \
                    "<div class='small mt-1'>#{written.(file)}</div></li>"
            end.join
            "<h2 class='mt-4'>Aufnahmen und Musik</h2><p>Jede Tondatei der Kopie zum Anhören, darunter, was Whisper auf dem Server darin verstanden hat " \
                '(bei Musik oft nichts oder Unsinn). Im Telefonbuch kann jeder sie hören.</p>' \
                "<ul class='list-unstyled'>#{rows}</ul>"
        end
        # the game on the Telefonbuch's line, in its sandbox, before anyone
        # else can call it (phone_review.js loads the phone on the click)
        try = if code
            token = PhoneGames.trial(game[:id], game[:file])
            "<div class='pg-try mb-4'><button class='btn btn-outline-primary pg-try-button'><i class='bi bi-telephone'></i> Ausprobieren</button>" \
                "<div class='pg-try-phone' data-src='/telefonbuch/telefon/?line&amp;try=#{token}' hidden></div></div>"
        else
            ''
        end
        # title and subtitle as the Telefonbuch shows them, to change
        rename = if game[:state] == 'rejected'
            ''
        else
            "<form class='pg-rename mb-4' data-id='#{h.(game[:id])}'><h2 class='h5'>Im Telefonbuch</h2>" \
                "<div class='mb-2'><label class='form-label' for='pg-title'>Titel</label>" \
                "<input class='form-control' id='pg-title' maxlength='#{PhoneGames::MAX_TITLE}' value='#{h.(game[:title])}' required></div>" \
                "<div class='mb-2'><label class='form-label' for='pg-description'>Untertitel</label>" \
                "<input class='form-control' id='pg-description' maxlength='#{PhoneGames::MAX_DESCRIPTION}' value='#{h.(game[:description])}' required></div>" \
                "<button class='btn btn-outline-primary' type='submit'><i class='bi bi-check-lg'></i> Speichern</button>" \
                "<span class='pg-rename-result ms-2'></span>" \
                "<p class='form-text mb-0'>Telefonbuch und Menü der Leitung zeigen das gleich; das Programm selbst bleibt, wie es ist.</p></form>"
        end
        buttons = if game[:state] == 'pending'
            "<div class='pg-decide' data-id='#{h.(game[:id])}'>" \
                "<p class='pg-prepared text-body-secondary'><span class='spinner-border spinner-border-sm' aria-hidden='true'></span> Sätze werden vorbereitet …</p>" \
                "<button class='btn btn-success pg-publish' disabled><i class='bi bi-check-lg'></i> Veröffentlichen</button>" \
                "<div class='mt-3'><label class='form-label' for='pg-reason'>Grund für die Ablehnung (die Schülerin oder der Schüler liest ihn)</label>" \
                "<textarea class='form-control' id='pg-reason' rows='3' maxlength='#{PhoneGames::MAX_REASON}'></textarea>" \
                "<button class='btn btn-outline-danger mt-2 pg-reject' disabled><i class='bi bi-x-lg'></i> Ablehnen</button></div>" \
                "<p class='pg-result mt-2'></p></div>"
        else
            ''
        end
        "<p class='mt-4 mb-0'><a href='/telefon-freigabe'><i class='bi bi-arrow-left'></i> Alle Telefonspiele</a></p>" \
            "<h1 class='mt-2'>#{h.(PhoneGames.label(game))} <span class='pg-title'>#{h.(game[:title])}</span></h1>" \
            "<p class='abstract pg-description'>#{h.(game[:description])}</p>" \
            "<table class='table'><tr><th>Von</th><td>#{h.(game[:email])}</td></tr>" \
            "<tr><th>Eingereicht</th><td>#{Time.at(game[:submitted].to_i).strftime('%d.%m.%Y %H:%M')}</td></tr>" \
            "<tr><th>Stand</th><td>#{h.(state)}</td></tr>" \
            "<tr><th>Dateien</th><td><div class='d-flex flex-wrap gap-1'>#{files.map { |f| "<code>#{h.(f)}</code>" }.join(' ')}</div></td></tr></table>" \
            "#{try}#{rename}#{buttons}" \
            "<h2 class='mt-4'>Was das Spiel sagt und spielt</h2><p>Die festen Sätze, die eigenen Aufnahmen (mit dem, was Whisper darin verstanden hat) und die Musik, in der Reihenfolge des Programms; Sätze, die erst beim Spielen entstehen, fehlen.</p><ol>#{list}</ol>" \
            "#{recordings}" \
            "<h2 class='mt-4'>Probelauf</h2><p>Der Anfang des Spiels mit <code>telefon --test</code>, ohne eine Taste, beim Einreichen im Workspace.</p><pre class='pg-transcript'>#{h.(game[:transcript])}</pre>" \
            "<h2 class='mt-4'>Programm: <code>#{h.(game[:file])}</code></h2>#{phone_game_code_html(code, game[:file])}" \
            "<script src='/include/phone_review.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
    end

    # /telefonbuch: the published games, sorted by title, as the pages of a
    # book; the phone number or that one is being worked on (public)
    def phone_book_page
        phone_games_publish_list
        games = phone_games_all.select { |g| g[:state] == 'published' && !g[:hidden] }
        games += PhoneGames.sample_entries if DEVELOPMENT && params.key?('muster')     # to try the pages
        games = PhoneGames.phone_book(games)
        number = Main.phone_number
        entries = games.map do |g|
            { :number => g[:number], :word => g[:word], :title => g[:title], :description => g[:description] }
        end
        # the remark at the end of the book: the real number and how to dial
        # a game directly (the maintainer: enough there, no label beside the
        # book); without one, that one is being worked on
        line = if number
            direct = defined?(PHONE_DIRECT_DIAL) && PHONE_DIRECT_DIAL
            game = '555'     # the example the maintainer chose, whatever the book holds
            direct ? "Von jedem Telefon: #{number}. Wer die Nummer eines Spiels gleich dahinter wählt, kommt sofort zum Spiel, zum Beispiel #{number}-#{game}." :
                "Von jedem Telefon: #{number}, dann die Nummer des Spiels eingeben."
        else
            'Wir arbeiten an einer echten Telefonnummer, unter der man die Spiele auch mit einem Telefon anrufen kann.'
        end
        # the notes stand at the end of the book, as a remark (phone_book.js)
        notes = { :line => line,
                  :own => 'Schreib dein eigenes Telefonspiel:', :link => 'hier entlang', :href => '/telefonspiel' }
        "<script>window.PHONE_BOOK = #{JSON.generate(entries).gsub('</', '<\/')}; " \
            "window.PHONE_BOOK_NOTES = #{JSON.generate(notes).gsub('</', '<\/')};</script>"
    end
end
