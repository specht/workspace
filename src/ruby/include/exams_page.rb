# Leistungsüberprüfungen (exams): a teacher uploads a Workspace package,
# assigns students, starts and stops the exam and its demo, and gets the
# results as a PDF; docs/test-workspace-packages.md. Moved out of main.rb,
# which loads it.
class Main < Sinatra::Base
    post '/api/upload_test_archive' do
        assert(teacher_logged_in?)
        entry = params['file']
        filename = entry['filename']
        blob = entry['tempfile'].read
        sha1 = Digest::SHA1.hexdigest(blob)
        tag = RandomTag.generate(24)
        debug "Got a file called #{filename} with #{blob.size} bytes."

        # Validate the actual uploaded bytes and resolve/download every VSIX
        # before creating a visible Test entry. The original archive is kept
        # untouched; extension files and the locked manifest live beside it.
        Dir.mktmpdir('exam-upload-') do |dir|
            incoming = File.join(dir, 'package.tar.gz')
            File.binwrite(incoming, blob)
            manifest = TestWorkspaceExtensions.prepare!(incoming, tag: tag)
            FileUtils.mkdir_p(TestWorkspaceExtensions::ARCHIVES)
            archive_path = File.join(TestWorkspaceExtensions::ARCHIVES, sha1)
            File.binwrite(archive_path, blob) unless File.file?(archive_path)
            TestWorkspaceExtensions.publish!(manifest, tag: tag)
        end

        ts = Time.now.to_i
        neo4j_query_expect_one(<<~END_OF_STRING, {:tag => tag, :email => @session_user[:email], :sha1 => sha1, :size => blob.size, :filename => filename, :ts => ts})
            MATCH (u:User {email: $email})
            MERGE (f:File {sha1: $sha1})
            WITH u, f
            CREATE (u)<-[:BELONGS_TO]-(t:Test {tag: $tag})-[:USES]->(f)
            SET f.size = $size
            SET t.filename = $filename
            SET t.ts = $ts
            RETURN f.sha1;
        END_OF_STRING
        respond(:success => true, :yay => 'sure')
    rescue TestWorkspaceExtensions::Error, TestWorkspacePackage::ConfigError => e
        STDERR.puts "Exam upload rejected: #{e.class}: #{e.message}"
        status 422
        content_type :json
        JSON.generate(:success => false, :error => e.message)
    rescue StandardError => e
        STDERR.puts "Exam upload failed: #{e.class}: #{e.message}"
        STDERR.puts Array(e.backtrace).first(25).join("\n")
        status 500
        content_type :json
        JSON.generate(:success => false, :error => 'Interner Fehler beim Vorbereiten des Prüfungspakets. Bitte Server-Log prüfen.')
    end
    post '/api/get_my_test_archives' do
        assert(teacher_logged_in?)
        entries = []
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})<-[:BELONGS_TO]->(t:Test)-[:USES]->(f:File)
            OPTIONAL MATCH (t)<-[:TAKES]-(u2:User)
            RETURN t, f, COUNT(u2);
        END_OF_STRING
            entries << {
                tag: row['t'][:tag],
                running: row['t'][:running],
                size: bytes_to_str(row['f'][:size]),
                filename: row['t'][:filename],
                ts: row['t'][:ts],
                count: row['COUNT(u2)'],
            }
        end
        respond(:entries => entries)
    end
    post '/api/delete_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        # The exam's Workspaces and databases go with it: of every student who
        # took it, and the teacher's own demo.
        takers = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => @session_user[:email]}).map { |row| row['email'].to_s }
            MATCH (s:User)-[:TAKES]->(t:Test {tag: $tag})-[:BELONGS_TO]->(:User {email: $email})
            RETURN s.email AS email;
        END_OF_STRING
        instances = takers.map { |email| [email, data[:tag]] }
        unless neo4j_query('MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $tag}) RETURN t.tag;', :email => @session_user[:email], :tag => data[:tag]).empty?
            instances << [@session_user[:email], Main.test_demo_tag(@session_user[:email], data[:tag])]
        end
        instances.each do |email, tag|
            container_name = fs_tag_for_email("#{email}#{tag}")
            workspace_runtime.stop_workspace(container_name, :timeout => shell_timeout(:docker_kill))
            FileUtils.rm_rf("/user/#{container_name}")
        end
        unless instances.empty?
            logins = instances.map { |email, tag| DatabaseIdentity.ephemeral_login(db_login_for_email(email), tag) }
            run_mysql_statements(logins.flat_map { |login| DatabaseProvisioning.mysql_drop_statements(login) })
            run_neo4j_statements(logins.flat_map { |login| DatabaseProvisioning.neo4j_drop_statements(login) })
        end
        deleted = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => @session_user[:email]}).size
            MATCH (u:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $tag})
            WITH t, t.tag AS tag
            DETACH DELETE t
            RETURN tag;
        END_OF_STRING
        # The shared content-addressed VSIX cache is retained: other exams
        # may still reference these files. The manifest only for an exam that
        # was this teacher's, and only under a plain tag.
        if deleted > 0 && data[:tag].match?(/\A[a-z0-9]+\z/)
            FileUtils.rm_f(File.join(TestWorkspaceExtensions::MANIFESTS, "#{data[:tag]}.json"))
        end
        Main.refresh_nginx_config()
        respond(:ok => 'sure')
    end
    post '/api/get_assigned_users_for_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        emails = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :teacher_email => @session_user[:email]}).map { |x| x['u.email'] }
            MATCH (:User {email: $teacher_email})<-[:BELONGS_TO]-(t:Test {tag: $tag})<-[:TAKES]-(u:User)
            RETURN u.email;
        END_OF_STRING
        respond(:emails => emails)
    end
    post '/api/test_toggle_user' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :email])
        # An exam restricts a person's interface: only one's own students
        # (and test accounts), the address alone used to be enough.
        Main.load_invitations()
        halt 403 unless admin_logged_in? || workspace_user_visible_to?(data[:email], @session_user[:email])
        require_test_owner!(data[:tag])
        count = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]}).size
            MATCH (u:User {email: $email})-[:TAKES]->(t:Test {tag: $tag})
            RETURN u.email;
        END_OF_STRING
        if count == 0
            neo4j_query_expect_one(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]})
                MERGE (u:User {email: $email})
                WITH u
                MATCH (t:Test {tag: $tag})
                CREATE (u)-[:TAKES]->(t)
                RETURN u.email;
            END_OF_STRING
            Main.refresh_nginx_config()
            respond(:flag => true)
        else
            neo4j_query_expect_one(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]})
                MATCH (u:User {email: $email})-[r:TAKES]->(t:Test {tag: $tag})
                DELETE r
                RETURN u.email;
            END_OF_STRING
            Main.refresh_nginx_config()
            respond(:flag => false)
        end
    end
    # Who takes an exam, set for several people at once (a whole group from
    # the selection dialog): flag true adds, false removes. Only one's own
    # students (and test accounts), as for test_toggle_user; others are left
    # out. Answers with everyone who takes it now.
    post '/api/test_set_users' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :emails, :flag], :types => { :emails => Array, :flag => Object },
            :max_body_length => 64_000)
        require_test_owner!(data[:tag])
        Main.load_invitations()
        emails = data[:emails].map(&:to_s).uniq.select do |email|
            admin_logged_in? || workspace_user_visible_to?(email, @session_user[:email])
        end
        unless emails.empty?
            if data[:flag] == true
                neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :emails => emails})
                    MATCH (t:Test {tag: $tag})
                    UNWIND $emails AS email
                    MERGE (u:User {email: email})
                    MERGE (u)-[:TAKES]->(t);
                END_OF_STRING
            else
                neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :emails => emails})
                    MATCH (u:User)-[r:TAKES]->(:Test {tag: $tag})
                    WHERE u.email IN $emails
                    DELETE r;
                END_OF_STRING
            end
            Main.refresh_nginx_config()
        end
        taking = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]}).map { |row| row['email'] }
            MATCH (u:User)-[:TAKES]->(:Test {tag: $tag})
            RETURN u.email AS email;
        END_OF_STRING
        respond(:emails => taking)
    end
    # Starting or stopping a real test only changes whether assigned students
    # are routed into their existing exam workspace. Their files and databases
    # must survive any number of stop/start cycles.
    # An exam may be started, stopped and given students only by the teacher
    # it belongs to (or an admin).
    def require_test_owner!(tag)
        return if admin_logged_in?

        owned = neo4j_query(<<~END_OF_STRING, {:tag => tag, :email => @session_user[:email]}).size
            MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $tag})
            RETURN t.tag;
        END_OF_STRING
        halt 404 if owned == 0
    end
    post '/api/start_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        require_test_owner!(data[:tag])
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]})
            MATCH (t:Test {tag: $tag})
            SET t.running = TRUE;
        END_OF_STRING
        Main.refresh_nginx_config()
        respond(:ok => 'sure')
    end
    post '/api/stop_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        require_test_owner!(data[:tag])
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]})
            MATCH (t:Test {tag: $tag})
            REMOVE t.running;
        END_OF_STRING
        Main.refresh_nginx_config()
        respond(:ok => 'sure')
    end
    post '/api/start_test_demo' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        email = @session_user[:email]
        test_tag = data[:tag]

        neo4j_query_expect_one(<<~END_OF_QUERY, {:email => email, :test_tag => test_tag})
            MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $test_tag})
            RETURN t;
        END_OF_QUERY

        demo_tag = Main.test_demo_tag(email, test_tag)
        base_server_tag = @session_user[:server_tag]
        server_tag = "#{base_server_tag}#{demo_tag}"

        # The destructive reset below is deliberately confined to the teacher's
        # private demo namespace. Real student exam workspaces are never reset here.
        job = Main.enqueue_server_start(
            :email => email,
            :test_tag => demo_tag,
            :server_tag => server_tag
        ) do
            container_name = fs_tag_for_email("#{email}#{demo_tag}")
            workspace_runtime.stop_workspace(
                container_name,
                :timeout => shell_timeout(:docker_kill),
            )
            FileUtils.rm_rf("/user/#{container_name}")
            reset_test_demo_databases(email, demo_tag)
            start_server(
                email,
                test_tag,
                :server_tag => base_server_tag,
                :instance_tag => demo_tag,
            )
        end

        status = Main.wait_for_server_start_job(job)

        respond(
            :yay => 'sure',
            :server_tag => server_tag,
            :queued => ['queued', 'running'].include?(status[:status]),
            :status => status[:status],
            :error => status[:error]
        )
    end
    def my_running_tests
        unless user_logged_in?
            return []
        end
        entries = []
        neo4j_query(<<~END_OF_QUERY, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})-[:TAKES]->(t:Test {running: TRUE})-[:BELONGS_TO]->(tu:User)
            RETURN t, tu.email;
        END_OF_QUERY
            entries << {
                :tag => row['t'][:tag],
                :teacher => @@invitations[row['tu.email']][:name]
            }
        end
        entries
    end
    def test_results_html(emails, instance_tag)
        printed_at = Time.now.strftime("%d.%m.%Y %H:%M Uhr")
        document_title = if emails.size == 1
            display_name = (@@invitations[emails.first] || {})[:name] || emails.first
            "Leistungsüberprüfung – #{display_name}"
        else
            'Leistungsüberprüfung – Ergebnisse'
        end

        StringIO.open do |io|
            io.puts <<~END_OF_STRING
                <!DOCTYPE html>
                <html>
                <head>
                    <meta charset="utf-8">
                    <title>#{CGI.escapeHTML(document_title)}</title>
                    <link rel="stylesheet" href="/include/fonts.css?#{CACHE_BUSTER}">
                    <style>
                        @page {
                            margin: 12mm;
                        }
                        body {
                            font-family: '0xProto Nerd Font Mono', monospace;
                            font-size: 10pt;
                        }
                        .student + .student {
                            break-before: page;
                            page-break-before: always;
                        }
                        .student-meta {
                            border-bottom: 1px solid #999;
                            color: #555;
                            font-size: 9pt;
                            margin: 0 0 0.8em;
                            text-align: right;
                            padding-bottom: 0.35em;
                        }
                        .file {
                            margin: 0 0 1.25em;
                        }
                        .file-header {
                            align-items: baseline;
                            background: #ddd;
                            break-after: avoid;
                            page-break-after: avoid;
                            display: flex;
                            font-weight: bold;
                            gap: 1em;
                            padding: 0.2em 0.35em;
                        }
                        .file-meta {
                            flex-grow: 1;
                            text-align: right;
                        }
                        pre {
                            font-family: '0xProto Nerd Font Mono', monospace;
                            font-size: 10pt;
                            margin: 0.35em 0 0;
                            overflow-wrap: anywhere;
                            white-space: pre-wrap;
                        }
                        .line-number {
                            border-right: 1px solid #aaa;
                            color: #777;
                            display: inline-block;
                            margin-right: 0.7em;
                            padding-right: 0.7em;
                            text-align: right;
                            width: 3.5em;
                        }
                        .empty {
                            color: #666;
                            font-family: sans-serif;
                        }
                    </style>
                </head>

                <body>
            END_OF_STRING

            emails.each do |email|
                container_name = fs_tag_for_email("#{email}#{instance_tag}")
                workspace_path = "/user/#{container_name}/workspace"
                display_name = (@@invitations[email] || {})[:name] || email
                escaped_name = CGI.escapeHTML(display_name)

                io.puts "<section class='student'>"
                io.puts "<div class='student-meta'>#{escaped_name} | #{printed_at}</div>"

                unless File.directory?(workspace_path)
                    io.puts "<p class='empty'>Dieser Prüfungs-Workspace wurde noch nicht geöffnet.</p>"
                    io.puts "</section>"
                    next
                end

                files = TestWorkspacePackage.printable_files(workspace_path)
                if files.empty?
                    io.puts "<p class='empty'>Keine druckbaren Dateien gefunden.</p>"
                end

                files.each do |relative_path, path|
                    io.puts "<div class='file'>"
                    io.puts "<div class='file-header'>"
                    io.puts "<div>#{CGI.escapeHTML(relative_path)}</div>"
                    io.puts "</div>"
                    io.puts "<pre>"
                    File.foreach(path).with_index(1) do |line, index|
                        io.puts sprintf(
                            "<span class='line-number'>%4d</span>%s",
                            index,
                            CGI.escapeHTML(line.chomp),
                        )
                    end
                    io.puts "</pre>"
                    io.puts "</div>"
                end

                io.puts "</section>"
            end

            io.puts "</body>"
            io.puts "</html>"

            io.string
        end
    end
    get '/api/pdf_for_test/:tag' do
        assert(teacher_logged_in?)
        test_tag = params['tag']
        emails = neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :test_tag => test_tag}).map { |row| row['u.email'] }
            MATCH (u:User)-[:TAKES]->(t:Test {tag: $test_tag})-[:BELONGS_TO]->(:User {email: $email})
            RETURN u.email;
        END_OF_STRING

        requested_email = params['email'].to_s
        unless requested_email.empty?
            assert(emails.include?(requested_email), 'student is not assigned to this test')
            emails = [requested_email]
        end

        emails.sort_by! do |email|
            ((@@invitations[email] || {})[:name] || email).downcase
        end
        respond_raw_with_mimetype(test_results_html(emails, test_tag), 'text/html')
    end
    get '/api/pdf_for_test_demo/:tag' do
        assert(teacher_logged_in?)
        email = @session_user[:email]
        test_tag = params['tag']
        neo4j_query_expect_one(<<~END_OF_QUERY, {:email => email, :test_tag => test_tag})
            MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $test_tag})
            RETURN t;
        END_OF_QUERY

        demo_tag = Main.test_demo_tag(email, test_tag)
        respond_raw_with_mimetype(test_results_html([email], demo_tag), 'text/html')
    end
end
