# Accounts and their administration: resetting a test account, deleting an
# account (at once, announced with notice, on a person's own request with a
# code), the deletion log that is repeated after a restore, suspending
# access, the export of one's own data, and the overview of people for the
# administration (/personen). Moved out of main.rb, which loads it.
class Main < Sinatra::Base
    # A teacher's test account back to a fresh start (TestAccounts): its
    # Workspace and exam Workspaces emptied, its databases new, its task
    # results, attempts and hand-ins gone. Only this account's own data;
    # exams keep it assigned, nothing of anyone else changes.
    def reset_test_account(email)
        raise ArgumentError, "#{email} is not a test account" unless @@test_accounts.key?(email)

        stop_server(email)
        FileUtils.rm_rf("/user/#{fs_tag_for_email(email)}")
        neo4j_query(<<~END_OF_STRING, :email => email, :server_tag => gen_server_tag, :server_sid => gen_server_sid)
            MATCH (u:User {email: $email})
            SET u.server_tag = $server_tag, u.server_sid = $server_sid;
        END_OF_STRING
        # The Workspaces of the exams it takes
        neo4j_query(<<~END_OF_STRING, :email => email).each do |row|
            MATCH (:User {email: $email})-[:TAKES]->(t:Test)
            RETURN t.tag AS tag;
        END_OF_STRING
            container_name = fs_tag_for_email("#{email}#{row['tag']}")
            workspace_runtime.stop_workspace(container_name, :timeout => shell_timeout(:docker_kill))
            FileUtils.rm_rf("/user/#{container_name}")
        end
        # Databases: the extra ones gone, the own MySQL and Neo4j database new
        neo4j_query(<<~END_OF_STRING, :email => email).each do |row|
            MATCH (:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
            RETURN d.name AS name;
        END_OF_STRING
            name = row['name'].to_s
            next unless DatabaseProvisioning.extra_database?(name)

            client = Mysql2::Client.new(host: 'mysql', username: 'root', password: MYSQL_ROOT_PASSWORD)
            client.query("DROP DATABASE IF EXISTS `#{name}`;")
            client.query(DatabaseProvisioning.extra_database_drop_login_statement(name))
            client.close
        end
        neo4j_query(<<~END_OF_STRING, :email => email)
            MATCH (:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
            DETACH DELETE d;
        END_OF_STRING
        reset_mysql(email)
        reset_neo4j(email)
        # Task results, attempts and hand-ins
        neo4j_query(<<~END_OF_STRING, :email => email).each do |row|
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState)
            RETURN s.task AS task;
        END_OF_STRING
            FileUtils.rm_f(submission_path(row['task'].to_s, email))
        end
        neo4j_query(<<~END_OF_STRING, :email => email)
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState)
            DETACH DELETE s;
        END_OF_STRING
        neo4j_query(<<~END_OF_STRING, :email => email)
            MATCH (a:TaskAttempt)-[:BY]->(:User {email: $email})
            DETACH DELETE a;
        END_OF_STRING
    end
    # An account with everything that belongs to it (AccountDeletion): the
    # Workspace and the exams' Workspaces, the databases, tasks, hand-ins,
    # sessions and the User node. Someone still invited starts with an empty
    # account at the next login. Returns what went, for the answer and the log.
    def delete_account(email)
        # An administrator's account is never deleted, by no path (the routes
        # say so first; this is the last guard).
        return nil if ADMIN_USERS.include?(email)

        user = neo4j_query('MATCH (u:User {email: $email}) RETURN u;', :email => email).first
        return nil unless user

        db_login = db_login_for_email(email)
        taken = neo4j_query(<<~END_OF_QUERY, :email => email).map { |row| row['tag'].to_s }
            MATCH (:User {email: $email})-[:TAKES]->(t:Test)
            RETURN t.tag AS tag;
        END_OF_QUERY
        owned = neo4j_query(<<~END_OF_QUERY, :email => email).map { |row| row['tag'].to_s }
            MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test)
            RETURN t.tag AS tag;
        END_OF_QUERY
        instances = taken + owned.map { |tag| Main.test_demo_tag(email, tag) }
        extra_databases = neo4j_query(<<~END_OF_QUERY, :email => email).map { |row| row['name'].to_s }
            MATCH (:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
            RETURN d.name AS name;
        END_OF_QUERY
        task_keys = neo4j_query(<<~END_OF_QUERY, :email => email).map { |row| row['task'].to_s }
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState)
            RETURN s.task AS task;
        END_OF_QUERY

        stop_server(email)
        ([fs_tag_for_email(email)] + instances.map { |tag| fs_tag_for_email("#{email}#{tag}") }).each do |container_name|
            workspace_runtime.stop_workspace(container_name, :timeout => shell_timeout(:docker_kill))
            FileUtils.rm_rf("/user/#{container_name}")
        end

        logins = [db_login] + instances.map { |tag| DatabaseIdentity.ephemeral_login(db_login, tag) }
        mysql = logins.flat_map { |login| DatabaseProvisioning.mysql_drop_statements(login) }
        extra_databases.select { |name| DatabaseProvisioning.extra_database?(name) }.each do |name|
            mysql << "DROP DATABASE IF EXISTS `#{name}`;" << DatabaseProvisioning.extra_database_drop_login_statement(name)
        end
        run_mysql_statements(mysql)
        run_neo4j_statements(logins.flat_map { |login| DatabaseProvisioning.neo4j_drop_statements(login) })

        task_keys.each { |task| FileUtils.rm_f(submission_path(task, email)) }
        # the person's own Git repositories (»Meine Repositorys«)
        git_id = (neo4j_query('MATCH (u:User {email: $email}) RETURN u.git_id AS id;', :email => email).first || {})['id']
        GitServer.delete_folder(git_id) if git_id.to_s.match?(GitServer::GIT_ID)
        AccountDeletion.node_statements.each { |statement| neo4j_query(statement, :email => email) }
        # The key goes with the account; the server forgets it at once.
        Main.git_refresh!
        note_deletion(email)
        Main.refresh_nginx_config()

        {
            :workspaces => 1 + instances.size,
            :databases => logins.size * 2 + extra_databases.size,
            :tasks => task_keys.size,
        }
    end
    DELETION_LOG = '/internal/deleted-accounts.jsonl'
    LAST_DELETION = 'last_deletion'
    @@deletion_log_mutex = Mutex.new
    # So that a restored backup does not bring a deleted account back
    # (AccountDeletion, »Deletions after a restore«): the log outside Neo4j,
    # and the time in Neo4j, which a restore sets back.
    def note_deletion(email)
        months = defined?(BACKUP_RETENTION_MONTHS) ? BACKUP_RETENTION_MONTHS : nil
        now = Time.now
        @@deletion_log_mutex.synchronize do
            entries = AccountDeletion.parse_log(File.exist?(DELETION_LOG) ? File.read(DELETION_LOG) : '')
            known = neo4j_query('MATCH (s:Setting {key: $key}) RETURN s.seq AS seq;', :key => LAST_DELETION).first
            seq = AccountDeletion.next_seq(entries, known && known['seq'])
            kept = AccountDeletion.prune(entries, months)
            text = kept.map { |e| JSON.generate(e) + "\n" }.join + AccountDeletion.log_line(email, LOGIN_CODE_SALT, :now => now, :seq => seq)
            AtomicFile.write(DELETION_LOG, text)
            neo4j_query('MERGE (s:Setting {key: $key}) SET s.at = $at, s.seq = $seq;', :key => LAST_DELETION, :at => now.to_i, :seq => seq)
        end
    end
    # A copy of the deletion log off the server, to the administrators
    # (ADMIN_USERS, like the mail when someone deletes its own account): the
    # whole log in each mail, so the newest one is enough
    # (MailTexts.deletion_log). Sent when the log
    # changed since the last mail, from the check every few minutes; a mail
    # that fails is tried again there. DELETION_LOG_MAILED holds the
    # checksum of what went out last.
    DELETION_LOG_MAILED = '/internal/deleted-accounts.mailed'
    def mail_deletion_log
        return if ADMIN_USERS.empty? || !File.exist?(DELETION_LOG)

        text = File.read(DELETION_LOG)
        digest = Digest::SHA256.hexdigest(text)
        return if File.exist?(DELETION_LOG_MAILED) && File.read(DELETION_LOG_MAILED).strip == digest

        entries = AccountDeletion.parse_log(text)
        mail_subject, html = MailTexts.deletion_log(:web_root => WEB_ROOT, :time => Time.now, :count => entries.size,
            :last_seq => entries.map { |e| e['seq'].to_i }.max.to_i, :signature => mail_signature)
        ADMIN_USERS.each do |admin|
            deliver_mail(:attachments => [{ :filename => 'deleted-accounts.jsonl', :content => text }]) do
                to admin
                from SMTP_FROM
                subject mail_subject
                html
            end
        end
        AtomicFile.write(DELETION_LOG_MAILED, digest + "\n")
        STDERR.puts "Deletion log mailed to the administrators (#{entries.size} entries)"
    end
    # After a restore, by itself: every account deleted after the backup is
    # deleted again (AccountDeletion.to_delete_after_restore), before the
    # first request after the start and then every few minutes. Before the first deletion that
    # noted its time in Neo4j, only »Löschungen wiederholen« with the date of
    # the backup can do it. Returns the addresses.
    def repeat_deletions_after_restore
        last = neo4j_query('MATCH (s:Setting {key: $key}) RETURN s.at AS at, s.seq AS seq;', :key => LAST_DELETION).first
        return [] unless last && last['at']

        entries = AccountDeletion.parse_log(File.exist?(DELETION_LOG) ? File.read(DELETION_LOG) : '')
        later = AccountDeletion.entries_after(entries, last['at'], last['seq'])
        return [] if later.empty?

        emails = neo4j_query('MATCH (u:User) RETURN u.email AS email;').map { |row| row['email'].to_s }
        again = AccountDeletion.to_delete_after_restore(entries, emails, LOGIN_CODE_SALT, last['at'], last['seq'])
        Main.load_invitations()
        again.each { |email| delete_account(email) }
        # Neo4j is up to date again, also when none of them came back (only
        # forward: deleting again has just noted a later time and number)
        neo4j_query(<<~END_OF_QUERY, :key => LAST_DELETION, :at => later.map { |e| e['deleted_at'].to_i }.max, :seq => later.map { |e| e['seq'].to_i }.max)
            MERGE (s:Setting {key: $key})
            SET s.at = CASE WHEN s.at > $at THEN s.at ELSE $at END,
                s.seq = CASE WHEN s.seq > $seq THEN s.seq ELSE $seq END;
        END_OF_QUERY
        STDERR.puts "Restore noticed (#{later.size} deletion(s) after #{Time.at(last['at'].to_i)} missing in Neo4j): #{again.size} account(s) deleted again"
        again
    end
    # After restoring a backup made at backup_time: delete again every
    # account deleted since. Returns the addresses.
    post '/api/admin/repeat_deletions' do
        admins_only!
        data = parse_request_data(:required_keys => [:backup_date])
        backup_time = Time.parse(data[:backup_date].to_s) rescue nil
        halt 400, { :error => 'invalid_date' }.to_json unless backup_time
        entries = AccountDeletion.parse_log(File.exist?(DELETION_LOG) ? File.read(DELETION_LOG) : '')
        emails = neo4j_query('MATCH (u:User) RETURN u.email AS email;').map { |row| row['email'].to_s }
        again = AccountDeletion.to_delete_again(entries, emails, LOGIN_CODE_SALT, backup_time)
        again.each { |email| delete_account(email) }
        STDERR.puts "Deletions repeated by #{@session_user[:email]} for a backup of #{backup_time}: #{again.size}"
        respond(:deleted => again)
    end
    # An administrator deletes an account, on request or by the deletion
    # rules (Datenschutzerklärung, »Speicherdauer«). The address typed a
    # second time is the confirmation.
    post '/api/admin/delete_account' do
        admins_only!
        data = parse_request_data(:required_keys => [:email, :confirmation], :optional_keys => [:notify], :types => { :notify => Object })
        email = AccountDeletion.normalize_email(data[:email])
        halt 400, { :error => 'invalid_email' }.to_json unless email
        halt 400, { :error => 'confirmation_mismatch' }.to_json unless AccountDeletion.normalize_email(data[:confirmation]) == email
        halt 400, { :error => 'own_account' }.to_json if email == @session_user[:email]
        halt 400, { :error => 'admin_account' }.to_json if ADMIN_USERS.include?(email)
        Main.load_invitations()
        removed = delete_account(email)
        halt 404, { :error => 'no_account' }.to_json unless removed
        log_event "Account deleted by #{@session_user[:email]}: #{email} #{removed.inspect}"
        still_invited = @@invitations.key?(email)
        if data[:notify] == true
            mail_subject, html = MailTexts.account_deleted(:web_root => WEB_ROOT, :signature => mail_signature, :still_invited => still_invited)
            deliver_mail do
                to email
                from SMTP_FROM
                subject mail_subject
                html
            end
        end
        respond(:deleted => email, :removed => removed, :still_invited => still_invited, :notified => data[:notify] == true)
    end
    # Announced deletion (Löschkonzept): a mail to the person with the date,
    # DELETION_NOTICE_DAYS later; until then they can take out their data or
    # ask to keep the account as a guest. Doing nothing means it is deleted:
    # run_scheduled_deletions, every hour.
    DELETION_NOTICE_DAYS = 14
    post '/api/admin/announce_deletion' do
        admins_only!
        data = parse_request_data(:required_keys => [:email])
        email = AccountDeletion.normalize_email(data[:email])
        halt 400, { :error => 'invalid_email' }.to_json unless email
        halt 400, { :error => 'own_account' }.to_json if email == @session_user[:email]
        halt 400, { :error => 'admin_account' }.to_json if ADMIN_USERS.include?(email)
        halt 404, { :error => 'no_account' }.to_json if neo4j_query('MATCH (u:User {email: $email}) RETURN u.email;', :email => email).empty?
        date = Date.today + DELETION_NOTICE_DAYS
        mail_subject, html = MailTexts.deletion_notice(:web_root => WEB_ROOT, :date => date, :signature => mail_signature)
        reply = defined?(CONTACT_EMAIL) ? CONTACT_EMAIL.to_s : ''
        # The notice first: the deletion is set only once the mail server took
        # the mail, so nobody is deleted without the two weeks' notice
        # (AccountDeletion.announce). Taken is not read: a bounce still has to
        # be noticed in the mailbox of SMTP_FROM.
        result = AccountDeletion.announce(
            :send => lambda do
                deliver_mail do
                    to email
                    from SMTP_FROM
                    reply_to reply unless reply.empty?
                    subject mail_subject
                    html
                end
            end,
            :schedule => lambda do
                neo4j_query('MATCH (u:User {email: $email}) SET u.deletion_at = $date, u.deletion_announced_at = $now, u.deletion_announced_by = $me;',
                    :email => email, :date => date.to_s, :now => Time.now.to_i, :me => @session_user[:email])
            end,
            :on_error => ->(e) { STDERR.puts "Deletion of #{email} not announced, the mail failed: #{e.class}: #{e.message}" }
        )
        halt 502, { :error => 'mail_failed' }.to_json if result == :mail_failed
        STDERR.puts "Deletion of #{email} announced by #{@session_user[:email]} for #{date}"
        respond(:email => email, :deletion_at => date.to_s)
    end
    # Suspending access (a parent's objection under review, a compromised
    # account, a school that asks to stop): sessions, login, the running
    # Workspace, shared links and web apps end at once; nothing is deleted.
    post '/api/admin/suspend_access' do
        admins_only!
        data = parse_request_data(:required_keys => [:email], :optional_keys => [:reason], :max_value_lengths => { :reason => 300 })
        email = data[:email].to_s.downcase
        halt 400, { :error => 'admin_account' }.to_json if ADMIN_USERS.include?(email)
        halt 404, { :error => 'no_account' }.to_json if neo4j_query('MATCH (u:User {email: $email}) RETURN 1 AS yes;', :email => email).empty?
        neo4j_query(<<~END_OF_QUERY, :email => email, :now => Time.now.to_i, :me => @session_user[:email], :reason => data[:reason].to_s.strip[0, 300])
            MATCH (u:User {email: $email})
            SET u.suspended_at = $now, u.suspended_by = $me, u.suspended_reason = $reason
            WITH u
            OPTIONAL MATCH (s:Session)-[:FOR]->(u)
            DETACH DELETE s;
        END_OF_QUERY
        neo4j_query('MATCH (:User {email: $email})-[:SHARES_LIVE_APP]->(s:LiveAppShare) SET s.active = false;', :email => email)
        stop_server(email)
        Main.refresh_nginx_config()
        log_event "Access of #{email} suspended by #{@session_user[:email]}"
        respond(:ok => true)
    end
    post '/api/admin/lift_suspension' do
        admins_only!
        data = parse_request_data(:required_keys => [:email])
        neo4j_query('MATCH (u:User {email: $email}) REMOVE u.suspended_at, u.suspended_by, u.suspended_reason;', :email => data[:email].to_s.downcase)
        Main.refresh_nginx_config()
        log_event "Suspension of #{data[:email]} lifted by #{@session_user[:email]}"
        respond(:ok => true)
    end
    post '/api/admin/cancel_deletion' do
        admins_only!
        data = parse_request_data(:required_keys => [:email])
        neo4j_query('MATCH (u:User {email: $email}) REMOVE u.deletion_at, u.deletion_announced_at, u.deletion_announced_by;', :email => data[:email].to_s.downcase)
        STDERR.puts "Announced deletion of #{data[:email]} cancelled by #{@session_user[:email]}"
        respond(:ok => true)
    end
    # The person keeps the account as a guest: into a group of the guests'
    # school (Nutzungsordnung), and the announced deletion is off.
    post '/api/admin/keep_as_guest' do
        admins_only!
        data = parse_request_data(:required_keys => [:email, :group])
        email = data[:email].to_s.downcase
        halt 400, { :error => 'not_a_guest_group' }.to_json if neo4j_query(<<~END_OF_QUERY, :name => data[:group]).empty?
            MATCH (:Group {name: $name})-[:AT]->(:School {guests: true}) RETURN 1 AS ok;
        END_OF_QUERY
        neo4j_query(<<~END_OF_QUERY, :email => email, :name => data[:group], :now => Time.now.to_i, :me => @session_user[:email])
            MATCH (u:User {email: $email}), (g:Group {name: $name})
            MERGE (i:Invitation {email: $email})
            ON CREATE SET i.teacher = false, i.created_at = $now, i.created_by = $me, i.name = u.name
            MERGE (i)-[:MEMBER_OF]->(g)
            REMOVE u.deletion_at, u.deletion_announced_at, u.deletion_announced_by;
        END_OF_QUERY
        Main.note_roster_change!
        STDERR.puts "#{email} kept as a guest in #{data[:group]} by #{@session_user[:email]}"
        respond(:ok => true)
    end
    # Deletes the accounts whose announced date has come, with a short mail
    # that it is done. Runs in its own thread (no request).
    def run_scheduled_deletions
        due = neo4j_query('MATCH (u:User) WHERE u.deletion_at IS NOT NULL AND u.deletion_at <= $today RETURN u.email AS email;',
            :today => Date.today.to_s).map { |r| r['email'] }
        return if due.empty?

        Main.load_invitations()
        due.each do |email|
            next if ADMIN_USERS.include?(email)

            removed = delete_account(email)
            next unless removed

            STDERR.puts "Announced deletion carried out: #{email} #{removed.inspect}"
            mail_subject, html = MailTexts.account_deleted(:web_root => WEB_ROOT, :signature => mail_signature, :still_invited => @@invitations.key?(email))
            deliver_mail do
                to email
                from SMTP_FROM
                subject mail_subject
                html
            end
        rescue => e
            STDERR.puts "Announced deletion of #{email} failed: #{e.class}: #{e.message}"
        end
    end
    # The person deletes its own account (Datenschutzerklärung, »Wer nicht
    # einverstanden ist«), never a teacher looking in as the person. Two
    # steps: the address typed again, then a code that comes by mail. The
    # administration hears of it afterwards.
    DELETION_CODE_MINUTES = 10
    DELETION_CODES_PER_ADDRESS = RateLimit.new(:limit => 3, :period => 3600)
    @@deletion_codes = {}
    @@deletion_codes_mutex = Mutex.new
    post '/api/request_account_deletion' do
        assert(user_logged_in?)
        halt 403 if @impersonated_by
        halt 400, { :error => 'admin_account' }.to_json if ADMIN_USERS.include?(@session_user[:email])
        data = parse_request_data(:required_keys => [:confirmation])
        email = @session_user[:email]
        halt 400, { :error => 'confirmation_mismatch' }.to_json unless AccountDeletion.normalize_email(data[:confirmation]) == email
        halt 429, { :error => 'rate_limited' }.to_json unless DEVELOPMENT || DELETION_CODES_PER_ADDRESS.allow?(email)
        code = Authentication.generate_login_code(:development => DEVELOPMENT)
        @@deletion_codes_mutex.synchronize do
            @@deletion_codes[email] = { :code => code, :expires => Time.now.to_i + DELETION_CODE_MINUTES * 60, :tries => 0 }
        end
        mail_subject, html = MailTexts.deletion_code(:code => code, :minutes => DELETION_CODE_MINUTES, :signature => mail_signature)
        deliver_mail do
            to email
            from SMTP_FROM
            subject mail_subject
            html
        end
        respond(:sent => true)
    end
    post '/api/delete_my_account' do
        assert(user_logged_in?)
        halt 403 if @impersonated_by
        halt 400, { :error => 'admin_account' }.to_json if ADMIN_USERS.include?(@session_user[:email])
        data = parse_request_data(:required_keys => [:confirmation, :code])
        email = @session_user[:email]
        halt 400, { :error => 'confirmation_mismatch' }.to_json unless AccountDeletion.normalize_email(data[:confirmation]) == email
        valid = @@deletion_codes_mutex.synchronize do
            pending = @@deletion_codes[email]
            if pending.nil? || pending[:expires] < Time.now.to_i || pending[:tries] >= 5
                @@deletion_codes.delete(email)
                nil
            else
                pending[:tries] += 1
                ok = Rack::Utils.secure_compare(pending[:code], data[:code].to_s.strip)
                @@deletion_codes.delete(email) if ok
                ok
            end
        end
        halt 400, { :error => valid.nil? ? 'code_expired' : 'wrong_code' }.to_json unless valid
        Main.load_invitations()
        name = (@@invitations[email] || {})[:name]
        groups = @@group_members.select { |group, members| members.include?(email) && !@@test_groups.include?(group) }.keys.sort
        removed = delete_account(email)
        log_event "Account deleted by its owner: #{email} #{removed.inspect}"
        mail_subject, html = MailTexts.account_deleted_by_owner(:email => email, :name => name == email ? nil : name, :groups => groups,
            :still_invited => @@invitations.key?(email), :signature => 'Workspace')
        ADMIN_USERS.each do |admin|
            deliver_mail do
                to admin
                from SMTP_FROM
                subject mail_subject
                html
            end
        end
        clear_session_cookies
        respond(:deleted => true)
    end
    # »Meine Daten herunterladen« (AccountExport): built in a temporary
    # folder, then streamed as one ZIP together with the Workspace folder, so
    # a big Workspace never sits in memory. tools/zip_stream.py runs as the
    # Workspace user, never follows a link and stores links as links.
    get '/api/export_my_data' do
        assert(user_logged_in?)
        halt 403 if @impersonated_by
        email = @session_user[:email]
        # One per person, EXPORT_LIMIT at once (include/my_data.rb): a page
        # that says so instead of an empty download.
        claim = Main.claim_export(email)
        unless claim == :ok
            content_type 'text/html'
            text = claim == :mine ? 'Dein Download läuft schon. Warte, bis er fertig ist.' : 'Gerade laufen andere Downloads. Versuch es in ein paar Minuten noch einmal.'
            halt 429, "<!doctype html><meta charset='utf-8'><title>Meine Daten</title><body style='font-family: sans-serif; max-width: 40em; margin: 3em auto;'>" \
                "<p>#{text}</p><p><a href='/profil#my-data'>Zurück zum Profil</a></p></body>"
        end
        # Folders a tool makes again stay out unless asked (»alle=1«); the
        # archive lists them (tools/rebuildable.py, weggelassen.txt).
        everything = params['alle'] == '1'
        dir = Dir.mktmpdir('export')
        begin
            rows = {
                :user => neo4j_query_expect_one('MATCH (u:User {email: $email}) RETURN u;', :email => email)['u'],
                :groups => my_groups,
                :task_states => neo4j_query('MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState) RETURN s;', :email => email).map { |r| r['s'] },
                :attempts => neo4j_query('MATCH (a:TaskAttempt)-[:BY]->(:User {email: $email}) RETURN a ORDER BY a.ts;', :email => email).map { |r| r['a'] },
                :look_ins => neo4j_query('MATCH (l:LookIn)-[:FOR]->(:User {email: $email}) RETURN l ORDER BY l.ts;', :email => email).map { |r| r['l'] },
                :live_apps => neo4j_query('MATCH (:User {email: $email})-[:SHARES_LIVE_APP]->(s:LiveAppShare) RETURN s;', :email => email).map { |r| r['s'] },
                :sessions => neo4j_query('MATCH (s:Session)-[:FOR]->(:User {email: $email}) WHERE s.impersonated_by IS NULL RETURN s;', :email => email).map { |r| r['s'] },
                :feedback => neo4j_query('MATCH (f:ActivityFeedback)-[:BY]->(:User {email: $email}) RETURN f;', :email => email).map { |r| r['f'] },
            }
            File.write(File.join(dir, 'konto.json'), AccountExport.account_json(rows))
            File.write(File.join(dir, 'LIESMICH.txt'), AccountExport::README)
            FileUtils.mkdir_p(File.join(dir, 'abgaben'))
            rows[:task_states].each do |state|
                path = submission_path(state[:task].to_s, email)
                AccountExport.unpack_submission(path, File.join(dir, 'abgaben', AccountExport.folder_for(state[:task]))) if File.file?(path)
            end
            login = db_login_for_email(email)
            DatabaseIdentity.validate!(login)
            # Every MySQL database: the own one and those made in the profile.
            extra = neo4j_query(<<~END_OF_QUERY, :email => email).map { |r| r['name'].to_s }.select { |n| DatabaseProvisioning.extra_database?(n) }
                MATCH (:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
                RETURN d.name AS name;
            END_OF_QUERY
            FileUtils.mkdir_p(File.join(dir, 'mysql'))
            # A dump that fails is not silently missing: the archive says so in
            # nicht-enthalten.txt, and the log has it.
            missing = []
            ([login] + extra).each do |database|
                target = File.join(dir, 'mysql', "#{database}.sql")
                next if system(MYSQL_ROOT_ENV, 'docker', 'exec', '-e', 'MYSQL_PWD', 'workspace_mysql_1', 'mysqldump', '--user=root', database,
                    :out => target, :err => File::NULL)

                FileUtils.rm_f(target)
                missing << "MySQL-Datenbank #{database}"
            end
            # The own Neo4j database, read with the person's own login, as the
            # dump that `neo4j_bolt load` in the Workspace reads.
            neo4j_target = File.join(dir, 'neo4j.dump')
            unless system({ 'NEO4J_URI' => 'bolt://neo4j:7687', 'NEO4J_USERNAME' => login, 'NEO4J_DATABASE' => login,
                            'NEO4J_PASSWORD' => Main.gen_password_for_email(email, NEO4J_PASSWORD_SALT) },
                          'neo4j_bolt', 'dump', '-o', neo4j_target, :err => File::NULL)
                FileUtils.rm_f(neo4j_target)
                missing << 'Neo4j-Datenbank'
            end
            unless missing.empty?
                STDERR.puts "Export for #{email}: not included: #{missing.join(', ')}"
                File.write(File.join(dir, 'nicht-enthalten.txt'),
                    "Diese Teile ließen sich nicht exportieren und fehlen im Archiv:\n\n" + missing.map { |m| "- #{m}\n" }.join +
                    "\nMeist heißt das, dass es die Datenbank noch nicht gibt (noch nie benutzt). Sonst bitte an die Administration schreiben.\n")
            end
            # The archive is written as the Workspace user: the folder
            # becomes theirs instead of readable for everyone.
            FileUtils.chown_R(WORKSPACE_UID, WORKSPACE_GID, dir)
        rescue StandardError
            FileUtils.rm_rf(dir)
            Main.release_export(email)
            raise
        end

        workspace_root = "/user/#{fs_tag_for_email(email)}"
        args = ['setpriv', "--reuid=#{WORKSPACE_UID}", "--regid=#{WORKSPACE_GID}", '--clear-groups',
            'python3', '-I', File.join(__dir__, 'tools', 'zip_stream.py'), '--exclude', 'workspace/.cache']
        args << '--skip-rebuildable' unless everything
        args << "=#{dir}"
        args << "workspace=#{File.join(workspace_root, 'workspace')}" if File.directory?(File.join(workspace_root, 'workspace'))
        @respond_streaming = true
        content_type 'application/zip'
        attachment AccountExport.filename(email)
        response.headers['Cache-Control'] = 'no-store'
        # The profile's button waits for this cookie to say the download has
        # started: a link that downloads tells the page nothing.
        if params['token'].to_s =~ /\A[a-z0-9]{8,32}\z/
            response.set_cookie('export_started', :value => params['token'], :path => '/', :max_age => 120,
                :same_site => :strict, :httponly => false)
        end
        stream do |out|
            begin
                IO.popen(args, 'rb', :err => File::NULL) do |io|
                    while (chunk = io.read(65_536))
                        out << chunk
                    end
                end
            ensure
                FileUtils.rm_rf(dir)
                Main.release_export(email)
            end
        end
    end
    # Everyone invited or with an account, for the administration: address,
    # name, role, groups, schools, whether there is an account, when last
    # active, the disk space of the Workspace, and for students and guests
    # the teachers of their groups. query matches part of the name, address,
    # group, school or teacher (so a teacher's name lists its students).
    def people_overview(query = '')
        Main.load_invitations()
        accounts = neo4j_query('MATCH (u:User) RETURN u.email AS email, u.name AS name;').to_h { |row| [row['email'], row['name']] }
        activity = activity_by_email
        disk = workspace_disk_usage[:by_tag]
        deletion_at = neo4j_query('MATCH (u:User) WHERE u.deletion_at IS NOT NULL RETURN u.email AS email, u.deletion_at AS day;').to_h { |r| [r['email'], r['day']] }
        terms_at = neo4j_query('MATCH (u:User) WHERE u.terms_accepted_at IS NOT NULL RETURN u.email AS email, u.terms_accepted_at AS at;').to_h { |r| [r['email'], r['at']] }
        suspended = neo4j_query('MATCH (u:User) WHERE u.suspended_at IS NOT NULL RETURN u.email AS email, u.suspended_at AS at, u.suspended_reason AS reason;').to_h { |r| [r['email'], { :at => r['at'], :reason => r['reason'] }] }
        guests = Main.roster_imported ? Set.new(neo4j_query(<<~END_OF_QUERY).map { |r| r['email'] }) : Set.new
            MATCH (i:Invitation)-[:MEMBER_OF]->(:Group)-[:AT]->(:School {guests: true}) RETURN DISTINCT i.email AS email;
        END_OF_QUERY
        groups_of = Hash.new { |hash, key| hash[key] = [] }
        @@group_members.each { |group, members| members.each { |email| groups_of[email] << group unless @@test_groups.include?(group) } }
        name_of = ->(email) { n = (@@invitations[email] || {})[:name] || accounts[email]; n.nil? || n == email ? nil : n }
        teachers_of = Roster.teachers_of_members(:teachers => @@teachers, :group_members => @@group_members, :skip => @@test_groups)
        q = query.to_s.strip.downcase
        schools_of = Hash.new { |hash, key| hash[key] = [] }
        if Main.roster_imported
            neo4j_query(<<~END_OF_QUERY).each { |r| schools_of[r['email']] |= [r['school']] }
                MATCH (i:Invitation)-[:MEMBER_OF|AT]->(x)
                OPTIONAL MATCH (x)-[:AT]->(gs:School)
                WITH i, CASE WHEN x:School THEN x.name ELSE gs.name END AS school
                WHERE school IS NOT NULL
                RETURN DISTINCT i.email AS email, school;
            END_OF_QUERY
        end
        (@@invitations.keys | accounts.keys).map do |email|
            name = (@@invitations[email] || {})[:name] || accounts[email] || email
            role = if ADMIN_USERS.include?(email) then 'Administration'
                elsif @@test_accounts.key?(email) then 'Testkonto'
                elsif @@teacher_emails.include?(email) then 'Lehrkraft'
                elsif guests.include?(email) then 'Gast'
                else 'Lernende'
            end
            active = activity[email]
            teachers = %w[Lernende Gast].include?(role) ? (teachers_of[email] || []).map { |t| { :email => t, :name => name_of.(t) } }.sort_by { |t| [t[:name].to_s.downcase, t[:email]] } : []
            { :email => email, :name => name == email ? nil : name, :role => role, :groups => groups_of[email].sort, :teachers => teachers,
              :schools => schools_of[email].sort, :account => accounts.key?(email), :invited => @@invitations.key?(email),
              :active_on => active && active[:day] ? active[:day].to_s : nil, :active_source => active && active[:day] ? active[:source] : nil,
              :disk_kb => accounts.key?(email) ? disk[fs_tag_for_email(email)] : nil, :deletion_at => deletion_at[email],
              :guest => guests.include?(email), :terms_accepted_at => terms_at[email], :suspended => suspended[email] }
        end.select do |p|
            q.empty? || p[:email].include?(q) || p[:name].to_s.downcase.include?(q) || p[:groups].any? { |g| g.downcase.include?(q) } ||
                p[:schools].any? { |g| g.downcase.include?(q) } ||
                p[:teachers].any? { |t| t[:email].include?(q) || t[:name].to_s.downcase.include?(q) }
        end.sort_by { |p| [p[:name].to_s.downcase, p[:email]] }
    end
    # school: a school's name (or '-' for none); role: one of the roles;
    # sort: 'active' puts those inactive longest (and never) first.
    post '/api/admin/people' do
        admins_only!
        data = parse_request_data(:optional_keys => [:query, :limit, :school, :role, :sort, :state], :types => { :limit => Integer }, :max_body_length => 1024)
        everyone = people_overview(data[:query].to_s.strip.empty? ? '' : data[:query])
        counts = PEOPLE_STATES.keys.to_h { |state| [state, 0] }
        states = Hash.new { |hash, key| hash[key] = [] }
        cross = people_in_several_schools
        year_ago = (Date.today - 365).to_s
        everyone.each do |p|
            people_states(p, cross, year_ago).each do |state|
                counts[state] += 1
                states[p[:email]] << state
            end
        end
        people = everyone
        state = data[:state].to_s
        people = people.select { |p| states[p[:email]].include?(state) } if PEOPLE_STATES.key?(state)
        school = data[:school].to_s
        people = people.select { |p| school == '-' ? p[:schools].empty? : p[:schools].include?(school) } unless school.empty?
        people = people.select { |p| p[:role] == data[:role] } unless data[:role].to_s.empty?
        people = people.sort_by { |p| [p[:active_on] || '', p[:name].to_s.downcase, p[:email]] } if data[:sort] == 'active'
        people = people.sort_by { |p| [-p[:disk_kb].to_i, p[:name].to_s.downcase, p[:email]] } if data[:sort] == 'disk'
        usage = workspace_disk_usage
        respond(:people => people.first([[data[:limit] || 100, 1].max, 10_000].min), :total => people.size,
            :fat_kb => usage[:fat_kb], :measured_at => usage[:measured_at], :counts => counts, :everyone => everyone.size)
    end
    # What /personen offers as filters (»Aufräumen« and the special cases),
    # each with its count. Administration and test accounts are never in them.
    PEOPLE_STATES = {
        'ohne-einladung' => 'Konto ohne Einladung',
        'ohne-konto' => 'eingeladen, nie angemeldet',
        'lange-nicht-da' => 'über ein Jahr nicht da',
        'keine-spur' => 'keine Spur einer Nutzung',
        'mehrere-schulen' => 'in Gruppen mehrerer Schulen',
        'loeschung' => 'Löschung angekündigt',
        'gesperrt' => 'gesperrt',
    }.freeze
    def people_states(p, cross, year_ago)
        return [] if %w[Administration Testkonto].include?(p[:role])

        states = []
        states << 'ohne-einladung' if p[:account] && !p[:invited]
        states << 'ohne-konto' if !p[:account] && p[:invited] && %w[Lernende Gast].include?(p[:role])
        # Only with a date as evidence (activity_by_email); an account
        # without any trace is a case of its own.
        states << 'lange-nicht-da' if p[:account] && p[:active_on] && p[:active_on] < year_ago
        states << 'keine-spur' if p[:account] && p[:active_on].nil?
        states << 'mehrere-schulen' if cross.include?(p[:email])
        states << 'loeschung' if p[:deletion_at]
        states << 'gesperrt' if p[:suspended]
        states
    end
    # Accounts whose address is no longer in any invitation list: what the
    # deletion rules remove at the end of a school year.
    post '/api/admin/uninvited_accounts' do
        admins_only!
        Main.load_invitations()
        rows = neo4j_query('MATCH (u:User) RETURN u.email AS email, u.name AS name, u.deletion_at AS deletion_at ORDER BY u.email;').reject do |row|
            @@invitations.key?(row['email'])
        end
        activity = activity_by_email
        usage = workspace_disk_usage
        respond(:accounts => rows.map { |row|
            active = activity[row['email']] || {}
            { :email => row['email'], :name => row['name'], :active_on => active[:day]&.to_s, :active_source => active[:day] ? active[:source] : nil,
              :disk_kb => usage[:by_tag][fs_tag_for_email(row['email'])], :deletion_at => row['deletion_at'] }
        }, :fat_kb => usage[:fat_kb])
    end
    # The logged-in teacher resets its own test account, and only that.
    post '/api/reset_test_account' do
        assert(teacher_logged_in?)
        test_email = @@test_accounts.key(@session_user[:email])
        halt 404 if test_email.nil?
        reset_test_account(test_email)
        log_event "Test account #{test_email} reset by #{@session_user[:email]}"
        respond(:reset => test_email)
    end
end
