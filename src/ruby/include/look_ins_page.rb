# When the administration looks in: seeing the site as another person
# (impersonate), the log of every such access with its reason, and the
# notice the person gets about it (LookIn, Privacy). Moved out of main.rb,
# which loads it.
class Main < Sinatra::Base
    # Every look-in by an administrator, with or without a reason, goes into
    # an internal audit log (who, when, what, whose account); the student is
    # told only when a reason is given (LookIn), as decided. A year is kept.
    ADMIN_ACCESS_LOG = '/internal/admin-access.jsonl'
    @@admin_access_mutex = Mutex.new
    def log_admin_access(email, kind, reason)
        now = Time.now.to_i
        line = JSON.generate({ 'ts' => now, 'admin' => @session_user[:email], 'kind' => kind, 'account' => email, 'reason' => reason.to_s }) + "\n"
        @@admin_access_mutex.synchronize do
            kept = (File.exist?(ADMIN_ACCESS_LOG) ? File.readlines(ADMIN_ACCESS_LOG) : []).select do |l|
                (JSON.parse(l)['ts'].to_i rescue 0) > now - 365 * 24 * 3600
            end
            AtomicFile.write(ADMIN_ACCESS_LOG, kept.join + line)
        end
    rescue => e
        STDERR.puts "Admin access log failed: #{e.class}: #{e.message}"
    end
    def admin_access_entries(limit = 50)
        return [] unless File.exist?(ADMIN_ACCESS_LOG)

        File.readlines(ADMIN_ACCESS_LOG).last(limit).reverse.filter_map { |l| JSON.parse(l) rescue nil }
    end
    def admin_access_html
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        rows = admin_access_entries
        return "<p class='text-body-secondary'>Noch keine.</p>" if rows.empty?

        kinds = { 'workspace' => 'Workspace geöffnet', 'impersonate' => 'nachgeahmt', 'stop' => 'Workspace gestoppt', 'cleanup' => 'Platz geschaffen' }
        "<table class='table table-sm people-table'><thead><tr><th>Wann</th><th>Wer</th><th>Was</th><th>Konto</th><th>Grund</th></tr></thead><tbody>" +
            rows.map { |r| "<tr><td>#{h.(Time.at(r['ts'].to_i).strftime('%d.%m.%Y %H:%M'))}</td><td>#{h.(r['admin'])}</td><td>#{h.(kinds[r['kind']] || r['kind'])}</td><td>#{h.(r['account'])}</td><td>#{r['reason'].to_s.empty? ? '–' : h.(r['reason'])}</td></tr>" }.join +
            '</tbody></table>'
    end
    # details: what was done (LookIn cleanup: which folders), shown with it.
    def record_look_in(email, kind, reason_text, details: nil)
        # A test account is the teachers' own: no reason, no notice.
        return if @@test_accounts.key?(email)

        reason = LookIn.reason(reason_text, :admin => admin_logged_in?)
        halt 400, { :error => 'reason_missing' }.to_json if reason.nil?
        log_admin_access(email, kind, reason) if admin_logged_in?
        return if reason.empty?

        by = @session_user[:email]
        neo4j_query(<<~END_OF_QUERY, :email => email, :ts => Time.now.to_i, :kind => kind, :reason => reason,
            MATCH (u:User {email: $email})
            CREATE (l:LookIn {ts: $ts, kind: $kind, reason: $reason, details: $details, by_email: $by_email, by_name: $by_name, seen: false})-[:FOR]->(u);
        END_OF_QUERY
            :by_email => by, :by_name => ((@@invitations[by] || {})[:name] || by), :details => details)
    end
    # As the student: the session belongs to it until »Zurück«, and remembers
    # who started (impersonated_by), also across a second impersonation.
    post '/api/impersonate' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:email], :optional_keys => [:reason],
            :max_body_length => 2048, :max_string_length => 1024)
        email = data[:email].to_s.downcase
        halt 403 unless may_look_in?(email)
        record_look_in(email, 'impersonate', data[:reason])
        ensure_user_account(email)
        neo4j_query(<<~END_OF_STRING, {:sid => current_session_key, :email => email, :by => @session_user[:email]})
            MATCH (s:Session {sid: $sid})-[r:FOR]->(:User), (u:User {email: $email})
            DELETE r
            CREATE (s)-[:FOR]->(u)
            SET s.impersonated_by = COALESCE(s.impersonated_by, $by);
        END_OF_STRING
        respond(:yay => 'sure')
    end
    # Back to one's own account after an impersonation.
    post '/api/stop_impersonation' do
        assert(user_logged_in?)
        halt 400 unless @impersonated_by
        neo4j_query(<<~END_OF_STRING, {:sid => current_session_key, :email => @impersonated_by})
            MATCH (s:Session {sid: $sid})-[r:FOR]->(:User), (u:User {email: $email})
            DELETE r
            CREATE (s)-[:FOR]->(u)
            REMOVE s.impersonated_by;
        END_OF_STRING
        respond(:yay => 'sure')
    end
    # The student has read who looked into its account.
    post '/api/look_ins_seen' do
        assert(user_logged_in?)
        halt 403 if @impersonated_by
        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email])
            MATCH (l:LookIn {seen: false})-[:FOR]->(:User {email: $email})
            SET l.seen = true;
        END_OF_QUERY
        respond(:yay => 'sure')
    end
    # Who looked into this student's account, newest first.
    def look_ins(only_unseen: false, limit: 20)
        return [] unless user_logged_in?

        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :unseen => only_unseen, :limit => limit).map { |row| row['l'] }
            MATCH (l:LookIn)-[:FOR]->(:User {email: $email})
            WHERE NOT $unseen OR l.seen = false
            RETURN l ORDER BY l.ts DESC LIMIT $limit;
        END_OF_QUERY
    end
    # In the profile: who looked into this account lately, and why.
    def look_ins_profile_html
        looks = look_ins(:limit => 20)
        return '' if looks.empty?

        items = looks.map { |l| "<li>#{LookIn.notice_html(l)}</li>" }.join
        "<h3 class='mt-4'>Wer sich dein Konto angesehen hat</h3>" \
            "<p>Lehrkräfte können sich deinen Workspace und deine Seiten ansehen, etwa um dir bei einem Fehler zu helfen oder eine Abgabe anzuschauen. Hier steht, wer das zuletzt getan hat und warum.</p>" \
            "<ul class='look-in-list mb-4'>#{items}</ul>"
    end
    def look_in_bar_html
        return '' unless user_logged_in?

        if @impersonated_by
            name = (@@invitations[@session_user[:email]] || {})[:name] || @session_user[:email]
            return "<div class='look-in-bar look-in-bar-impersonating'><div class='container look-in-bar-inner'><i class='bi bi-person-vcard' aria-hidden='true'></i>" \
                "<span>Du siehst die Seite als <strong>#{CGI.escapeHTML(name)}</strong>.</span>" \
                "<button type='button' class='btn btn-sm btn-light bu-stop-impersonation'><i class='bi bi-arrow-return-left' aria-hidden='true'></i>Zurück zu deinem Konto</button></div></div>"
        end
        unseen = look_ins(:only_unseen => true)
        return '' if unseen.empty?

        lines = unseen.reverse.map { |l| "<p>#{LookIn.notice_html(l)}</p>" }.join
        "<div class='look-in-bar look-in-bar-notice'><div class='container look-in-bar-inner'><i class='bi bi-info-circle' aria-hidden='true'></i><div>#{lines}</div>" \
            "<button type='button' class='btn btn-sm btn-outline-secondary bu-look-ins-seen'>Verstanden</button></div></div>"
    end
end
