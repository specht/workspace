# A person's Workspace on the site: starting it (start_server: the
# container, its folders, the databases, the nginx entry), stopping it, the
# routes to start, restart and reset it, to start it as an administrator
# or from a sharing link, and to share it and take the link back. The queue
# that runs the starts is server_start_queue.rb. Moved out of main.rb,
# which loads it.
class Main < Sinatra::Base
    def start_server(email, test_tag = nil, server_tag: nil, instance_tag: test_tag)
        email_with_test_tag = "#{email}#{instance_tag}"
        container_name = fs_tag_for_email(email_with_test_tag)
        persisted_db_login = db_login_for_email(email)
        database_email = test_tag ? "#{email}-#{instance_tag}" : email
        database_login = if test_tag
            DatabaseIdentity.ephemeral_login(persisted_db_login, instance_tag)
        else
            persisted_db_login
        end

        STDERR.puts ">>> Starting server with email #{email_with_test_tag} and container name #{container_name}"

        user_path = "/user/#{container_name}"
        config_path = "#{user_path}/config"
        workspace_path = "#{user_path}/workspace"

        FileUtils.mkdir_p(user_path)
        FileUtils.mkdir_p(config_path)
        FileUtils.mkdir_p(workspace_path)

        # These are bind-mounted into the code-server container, whose user runs
        # as UID/GID 1000. Only chown the mount points themselves here; never walk
        # the whole workspace on every launch.
        [user_path, config_path, workspace_path].each do |path|
            File.chown(1000, 1000, path)
        end

        # Root writes these into the student's folder, which the student
        # controls: UserFiles never follows a link they may have put there,
        # and hands new files to UID 1000 through the open descriptor.
        with_user_files(container_name, '.bashrc') do
            contents = UserFiles.read(workspace_path, '.bashrc')
            if contents && !contents.include?('GEM_HOME')
                prefix = contents.empty? || contents.end_with?("\n") ? '' : "\n"
                UserFiles.write(workspace_path, '.bashrc', "#{prefix}export GEM_HOME=\"$HOME/.gem\"\n", mode: :append)
            end
        end

        # Touch/update this file so housekeeping won't shut down the server immediately.
        with_user_files(container_name, '.hackschule') do
            UserFiles.write(workspace_path, '.hackschule', "https://youtu.be/Akaa9xHaw7E\n")
        end

        with_user_files(container_name, '.gitconfig') do
            UserFiles.write(workspace_path, '.gitconfig', <<~END_OF_STRING, create_only: true)
                [init]
                    defaultBranch = main
                [user]
                    name = #{(@@invitations[email] || {})[:name] || email.split('@').first}
                    email = #{email}
                [core]
                    editor = nano
            END_OF_STRING
        end

        # .my.cnf is replaced by a rename (AtomicFile), which replaces a link
        # instead of following it; it is only read through UserFiles.
        with_user_files(container_name, '.my.cnf') do
            desired = DatabaseProvisioning.my_cnf(
                database_login,
                Main.gen_password_for_email(database_email, MYSQL_PASSWORD_SALT),
            )
            if UserFiles.read(workspace_path, '.my.cnf') != desired
                AtomicFile.write("#{workspace_path}/.my.cnf", desired)
            end
            UserFiles.chown(workspace_path, '.my.cnf')
        end

        with_user_files(container_name, '.myclirc') do
            UserFiles.write(workspace_path, '.myclirc', <<~END_OF_STRING, create_only: true)
                [main]

                # Enables context sensitive auto-completion. If this is disabled the all
                # possible completions will be listed.
                smart_completion = True

                # Multi-line mode allows breaking up the sql statements into multiple lines. If
                # this is set to True, then the end of the statements must have a semi-colon.
                # If this is set to False then sql statements can't be split over multiple
                # lines. End of line (return) is considered as the end of the statement.
                multi_line = True

                # Destructive warning mode will alert you before executing a sql statement
                # that may cause harm to the database such as "drop table", "drop database"
                # or "shutdown".
                destructive_warning = True

                # log_file location.
                log_file = ~/.mycli.log

                # Default log level. Possible values are "CRITICAL", "ERROR", "WARNING", "INFO"
                # and "DEBUG". "NONE" disables logging.
                log_level = INFO

                # Log every query and its results to a file. Enable this by uncommenting the
                # line below.
                # audit_log = ~/.mycli-audit.log

                # Timing of sql statements and table rendering.
                timing = True

                # Beep after long-running queries are completed; 0 to disable.
                beep_after_seconds = 0

                # Table format. Possible values: ascii, double, github,
                # psql, plain, simple, grid, fancy_grid, pipe, orgtbl, rst, mediawiki, html,
                # latex, latex_booktabs, textile, moinmoin, jira, vertical, tsv, csv.
                # Recommended: ascii
                table_format = double

                # Syntax coloring style. Possible values (many support the "-dark" suffix):
                # manni, igor, xcode, vim, autumn, vs, rrt, native, perldoc, borland, tango, emacs,
                # friendly, monokai, paraiso, colorful, murphy, bw, pastie, paraiso, trac, default,
                # fruity.
                # Screenshots at http://mycli.net/syntax
                # Can be further modified in [colors]
                syntax_style = default

                # Keybindings: Possible values are emacs, vi.
                # Emacs mode: Ctrl-A is home, Ctrl-E is end. All emacs keybindings are available in the REPL.
                # When Vi mode is enabled you can use modal editing features offered by Vi in the REPL.
                key_bindings = emacs

                # Enabling this option will show the suggestions in a wider menu. Thus more items are suggested.
                wider_completion_menu = False

                # MySQL prompt
                # \\D - The full current date
                # \\d - Database name
                # \\h - Hostname of the server
                # \\m - Minutes of the current time
                # \\n - Newline
                # \\P - AM/PM
                # \\p - Port
                # \\R - The current time, in 24-hour military time (0-23)
                # \\r - The current time, standard 12-hour time (1-12)
                # \\s - Seconds of the current time
                # \\t - Product type (Percona, MySQL, MariaDB, TiDB)
                # \\A - DSN alias name (from the [alias_dsn] section)
                # \\u - Username
                # \\x1b[...m - insert ANSI escape sequence
                prompt = '\\d> '
                prompt_continuation = '->'

                # Skip intro info on startup and outro info on exit
                less_chatty = False

                # Use alias from --login-path instead of host name in prompt
                login_path_as_host = False

                # Cause result sets to be displayed vertically if they are too wide for the current window,
                # and using normal tabular format otherwise. (This applies to statements terminated by ; or \G.)
                auto_vertical_output = False

                # keyword casing preference. Possible values "lower", "upper", "auto"
                keyword_casing = upper

                # disabled pager on startup
                enable_pager = True

                # Choose a specific pager
                pager = 'less -S -F'

                # Custom colors for the completion menu, toolbar, etc.
                [colors]
                completion-menu.completion.current = 'bg:#ffffff #000000'
                completion-menu.completion = 'bg:#008888 #ffffff'
                completion-menu.meta.completion.current = 'bg:#44aaaa #000000'
                completion-menu.meta.completion = 'bg:#448888 #ffffff'
                completion-menu.multi-column-meta = 'bg:#aaffff #000000'
                scrollbar.arrow = 'bg:#003333'
                scrollbar = 'bg:#00aaaa'
                selected = '#ffffff bg:#6666aa'
                search = '#ffffff bg:#4444aa'
                search.current = '#ffffff bg:#44aa44'
                bottom-toolbar = 'bg:#222222 #aaaaaa'
                bottom-toolbar.off = 'bg:#222222 #888888'
                bottom-toolbar.on = 'bg:#222222 #ffffff'
                search-toolbar = 'noinherit bold'
                search-toolbar.text = 'nobold'
                system-toolbar = 'noinherit bold'
                arg-toolbar = 'noinherit bold'
                arg-toolbar.text = 'nobold'
                bottom-toolbar.transaction.valid = 'bg:#222222 #00ff5f bold'
                bottom-toolbar.transaction.failed = 'bg:#222222 #ff005f bold'

                # style classes for colored table output
                output.header = "#00ff5f bold"
                output.odd-row = ""
                output.even-row = ""
                output.null = "#808080"

                # SQL syntax highlighting overrides
                # sql.comment = 'italic #408080'
                # sql.comment.multi-line = ''
                # sql.comment.single-line = ''
                # sql.comment.optimizer-hint = ''
                # sql.escape = 'border:#FF0000'
                # sql.keyword = 'bold #008000'
                # sql.datatype = 'nobold #B00040'
                # sql.literal = ''
                # sql.literal.date = ''
                # sql.symbol = ''
                # sql.quoted-schema-object = ''
                # sql.quoted-schema-object.escape = ''
                # sql.constant = '#880000'
                # sql.function = '#0000FF'
                # sql.variable = '#19177C'
                # sql.number = '#666666'
                # sql.number.binary = ''
                # sql.number.float = ''
                # sql.number.hex = ''
                # sql.number.integer = ''
                # sql.operator = '#666666'
                # sql.punctuation = ''
                # sql.string = '#BA2121'
                # sql.string.double-quouted = ''
                # sql.string.escape = 'bold #BB6622'
                # sql.string.single-quoted = ''
                # sql.whitespace = ''

                # Favorite queries.
                [favorite_queries]

                # Use the -d option to reference a DSN.
                # Special characters in passwords and other strings can be escaped with URL encoding.
                [alias_dsn]
                # example_dsn = mysql://[user[:password]@][host][:port][/dbname]
            END_OF_STRING
        end

        # STDERR.puts ">>> Getting server state"
        state = with_timing("start_server #{container_name}: docker inspect") do
            get_server_state(container_name)
        end
        # STDERR.puts ">>> Server state is #{state.to_yaml}"

        unless state[:running]
            vscode_settings_rel = '.local/share/code-server/User/settings.json'
            existing_settings = with_user_files(container_name, 'settings.json') do
                UserFiles.read(workspace_path, vscode_settings_rel) || ''
            end

            user_config = existing_settings.to_s.strip.empty? ? {} : JSON.parse(existing_settings)
            original_config = existing_settings.nil? || existing_settings.strip.empty? ? nil : user_config.to_json
            default_config = JSON.parse(File.read('default-vscode-settings.json'))
            superseded_config = JSON.parse(File.read('superseded-vscode-settings.json')).reject { |k, _| k == '//' }
            user_config = VscodeSettings.merge(user_config, default_config, superseded_config)

            if DEVELOPMENT && (
                    email == 'student@example.com' ||
                    email.match?(/\Ae2e-\d+@example\.com\z/)
                )
                user_config['terminal.integrated.gpuAcceleration'] = 'off'
            end

            if DEVELOPMENT && email.match?(/\Ae2e-\d+@example\.com\z/)
                user_config['editor.autoIndent'] = 'none'
                user_config['editor.formatOnType'] = false
                user_config['editor.formatOnPaste'] = false
            end

            new_config = user_config.to_json
            if !existing_settings.nil? && original_config != new_config
                with_user_files(container_name, 'settings.json') do
                    UserFiles.write(workspace_path, vscode_settings_rel, JSON.pretty_generate(user_config))
                end
            end

            if test_tag
                with_user_files(container_name, 'coder.json') do
                    config = { 'query' => { 'folder' => '/workspace' } }
                    UserFiles.write(workspace_path, '.local/share/code-server/coder.json', config.to_json, create_only: true)
                end
            end

            STDERR.puts ">>> Enqueuing database initialization for #{database_login}"
            Main.enqueue_database_init(
                :email => database_email,
                :db_login => database_login,
            )

            if test_tag
                unless UserFiles.exist?(workspace_path, '.test_init')
                    sha1 = neo4j_query_expect_one(<<~END_OF_QUERY, {:test_tag => test_tag})['f.sha1']
                        MATCH (t:Test {tag: $test_tag})-[:USES]->(f:File)
                        RETURN f.sha1;
                    END_OF_QUERY

                    # Unpack files from archive. Git handling is package-controlled:
                    # fresh repositories are created by Workspace, preserve keeps an
                    # uploaded repository intact, and none leaves Git completely alone.
                    with_timing("start_server #{container_name}: unpack test archive") do
                        shell_ok(
                            "tar xf /internal/test_archives/#{sha1} -C #{workspace_path}",
                            :timeout => shell_timeout(:tar)
                        )
                    end

                    # Test archives may contain arbitrary nested files. This is the one
                    # place where a recursive ownership fix is necessary, and it only
                    # happens once when the archive is initially unpacked.
                    with_timing("start_server #{container_name}: chown unpacked test archive") do
                        shell_ok(
                            "chown -R 1000:1000 #{workspace_path}",
                            :timeout => shell_timeout(:chown)
                        )
                    end

                    git_mode = TestWorkspacePackage.validate_git_layout!(workspace_path)
                    package_vscode_config = TestWorkspacePackage.vscode_config(workspace_path)
                    unless package_vscode_config.empty?
                        settings = UserFiles.read(workspace_path, vscode_settings_rel)
                        vscode_config = settings.to_s.strip.empty? ? {} : JSON.parse(settings)
                        vscode_config.merge!(package_vscode_config)
                        UserFiles.write(workspace_path, vscode_settings_rel, JSON.pretty_generate(vscode_config))
                    end

                    # Git is initialized/configured after the code-server container is
                    # running so repository files are created directly as UID/GID 1000.
                    # git.mode: none deliberately skips all Workspace Git handling.
                    unless git_mode == 'none'
                        UserFiles.write(workspace_path, '.test_git_pending', '')
                    end

                    # Only mark package extraction complete after its configuration was
                    # parsed successfully. A malformed config can therefore be fixed and
                    # retried without leaving a half-initialized test workspace.
                    UserFiles.write(workspace_path, '.test_init', '')
                end

                # Prepare the same offline extension bundle for demos and students.
                # Re-stage on every container launch so stale or altered files are
                # never trusted by the privileged startup script.
                TestWorkspaceExtensions.stage!(tag: test_tag, workspace_path: workspace_path)
            end

            # STDERR.puts ">>> Getting IP address for mysql..."

            workspace_login = Main.workspace_login_for_email(email)
            database_environment = DatabaseProvisioning.workspace_environment(
                database_login,
                Main.gen_password_for_email(database_email, MYSQL_PASSWORD_SALT),
                Main.gen_password_for_email(database_email, NEO4J_PASSWORD_SALT),
            )

            # STDERR.puts ">>> Workspace login is #{workspace_login}, database login is #{database_login}"

            with_timing("start_server #{container_name}: docker run") do
                workspace_runtime.start_workspace(
                    :fs_tag => container_name,
                    :workspace_login => workspace_login,
                    :database_environment => database_environment,
                    # The account on the case server (»Ein Krimi im Terminal«);
                    # not in an exam, which has no network anyway.
                    :extra_environment => (CaseServer.enabled? && test_tag.nil? ?
                        CaseServer.workspace_environment(CaseServer.secret(LOGIN_CODE_SALT), email) : {})
                        .merge(Main.screenshot_account?(email) ? { 'HACKSCHULE_NO_WALKTHROUGH' => '1' } : {}),
                    :host_data_path => PATH_TO_HOST_DATA,
                    :test_mode => !test_tag.nil?,
                    :timeout => shell_timeout(:docker_run),
                )
            end
        end

        needs_nginx_refresh = !state[:running]
        git_pending_path = File.join(workspace_path, '.test_git_pending')
        if test_tag && File.exist?(git_pending_path)
            with_timing("start_server #{container_name}: initialize test git") do
                initialize_test_git(container_name, workspace_path, email)
            end
            needs_nginx_refresh = true
        end

        if needs_nginx_refresh
            with_timing("start_server #{container_name}: refresh nginx") do
                Main.refresh_nginx_config()
            end
        end

        return "#{server_tag}#{instance_tag}" if server_tag

        begin
            "#{@session_user[:server_tag]}#{instance_tag}"
        rescue
            ""
        end
    end

    def stop_server(email)
        container_name = fs_tag_for_email(email)

        neo4j_query(<<~END_OF_QUERY, :email => email, :updated_at => Time.now.to_i)
            MATCH (u:User {email: $email})-[:SHARES_LIVE_APP]->(s:LiveAppShare {active: TRUE})
            SET s.active = FALSE, s.updated_at = $updated_at;
        END_OF_QUERY

        workspace_runtime.stop_workspace(
            container_name,
            :timeout => shell_timeout(:docker_kill),
        )
        Main.reconcile_live_apps!(:refresh_nginx => false)
        Main.refresh_nginx_config()
    end
    post '/api/server_start_status' do
        assert(user_logged_in?)
        data = parse_request_data(:optional_keys => [:test_tag, :email])

        email = @session_user[:email]
        if data[:email] && data[:email] != email
            # Only for a teacher of this person.
            halt 403 unless teacher_logged_in? && may_look_in?(data[:email])
            email = data[:email]
        end

        key = Main.server_start_key(email, data[:test_tag])
        respond(Main.server_start_status(key))
    end
    post '/api/start_server' do
        assert(user_logged_in?)
        halt 403, { :error => 'terms_required' }.to_json if terms_required?
        data = parse_request_data(:optional_keys => [:test_tag])

        email = @session_user[:email]
        test_tag = data[:test_tag]
        # An exam's Workspace only for someone who takes it, and only while it
        # runs: after »Beenden« its answers stay as they were.
        if test_tag
            taking = neo4j_query(<<~END_OF_QUERY, :email => email, :tag => test_tag.to_s)
                MATCH (:User {email: $email})-[:TAKES]->(t:Test {tag: $tag, running: TRUE})
                RETURN t.tag;
            END_OF_QUERY
            halt 403, { :error => 'exam_not_running' }.to_json if taking.empty?
        end
        base_server_tag = @session_user[:server_tag]
        server_tag = "#{base_server_tag}#{test_tag}"

        job = Main.enqueue_server_start(
            :email => email,
            :test_tag => test_tag,
            :server_tag => server_tag
        ) do
            start_server(email, test_tag, :server_tag => base_server_tag)
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
    post '/api/restart_server' do
        assert(user_logged_in?)
        halt 403, { :error => 'terms_required' }.to_json if terms_required?

        email = @session_user[:email]
        active_workspace = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email)
            MATCH (u:User {email: $email})
            OPTIONAL MATCH (u)-[:TAKES]->(t:Test {running: TRUE})
            RETURN t;
        END_OF_QUERY
        test_tag = (active_workspace['t'] || {})[:tag]
        base_server_tag = @session_user[:server_tag]
        server_tag = "#{base_server_tag}#{test_tag}"

        job = Main.enqueue_server_start(
            :email => email,
            :test_tag => test_tag,
            :server_tag => server_tag
        ) do
            # The container is disposable. /workspace and /config are bind mounts,
            # so killing it clears every process without deleting persisted work.
            container_name = fs_tag_for_email("#{email}#{test_tag}")
            workspace_runtime.stop_workspace(
                container_name,
                :timeout => shell_timeout(:docker_kill),
            )
            Main.reconcile_live_apps!(:refresh_nginx => false)
            Main.refresh_nginx_config()
            start_server(email, test_tag, :server_tag => base_server_tag)
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
    post '/api/start_server_with_share_tag' do
        unless DEVELOPMENT || SHARE_STARTS_PER_CLIENT.allow?(client_ip)
            content_type :json
            halt 429, { :error => 'rate_limited' }.to_json
        end
        data = parse_request_data(:required_keys => [:share_tag])
        share_tag = data[:share_tag]

        user = neo4j_query(<<~END_OF_QUERY, :share_tag => share_tag.to_s, :now => Time.now.to_i).first
            MATCH (u:User {share_tag: $share_tag})
            WHERE u.share_expires IS NULL OR u.share_expires > $now
            RETURN u;
        END_OF_QUERY
        halt 404, { :error => 'share_expired' }.to_json unless user
        user = user['u']
        Main.load_invitations()
        halt 404, { :error => 'share_expired' }.to_json unless Main.may_use_workspace?(user[:email].to_s, user)

        job = Main.enqueue_server_start(
            :email => user[:email],
            :test_tag => nil,
            :server_tag => user[:server_tag]
        ) do
            start_server(user[:email], nil, :server_tag => user[:server_tag])
        end

        status = Main.wait_for_server_start_job(job)

        respond(
            :yay => 'sure',
            :share_tag => user[:share_tag],
            :queued => ['queued', 'running'].include?(status[:status]),
            :status => status[:status],
            :error => status[:error]
        )
    end
    post '/api/start_server_as_admin' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:email], :optional_keys => [:reason],
            :max_body_length => 2048, :max_string_length => 1024)
        email = data[:email]
        # Only the workspace of a student in one of the teacher's groups;
        # the address alone used to be enough. The reason reaches the student.
        halt 403 unless may_look_in?(email)
        record_look_in(email, 'workspace', data[:reason])
        ensure_user_account(email)

        # purge all WATCHING relationships (should be zero or one)
        neo4j_query(<<~END_OF_QUERY, {:email_self => @session_user[:email]})
            MATCH (u_self:User {email: $email_self})-[r:WATCHING]->(u:User)
            DELETE r;
        END_OF_QUERY

        # create new WATCHING relationship
        watch_tag = RandomTag.generate(48)
        target_user = neo4j_query_expect_one(<<~END_OF_QUERY, {:email_self => @session_user[:email], :email_other => email, :watch_tag => watch_tag})['u_other']
            MATCH (u_self:User {email: $email_self})
            MATCH (u_other:User {email: $email_other})
            CREATE (u_self)-[r:WATCHING]->(u_other)
            SET r.watch_tag = $watch_tag
            RETURN u_other;
        END_OF_QUERY

        job = Main.enqueue_server_start(
            :email => email,
            :test_tag => nil,
            :server_tag => target_user[:server_tag],
            :refresh_nginx_after => true
        ) do
            start_server(email, nil, :server_tag => target_user[:server_tag])
        end

        status = Main.wait_for_server_start_job(job)

        expires = session_cookie_expiry
        response.set_cookie(
            'hs_watch_tag',
            Authentication.domain_cookie_options(
                :value => watch_tag,
                :expires => expires,
                :domain => WEBSITE_HOST.split(':').first,
                :development => DEVELOPMENT,
            ),
        )
        respond(
            :yay => 'sure',
            :watch_tag => watch_tag,
            :queued => ['queued', 'running'].include?(status[:status]),
            :status => status[:status],
            :error => status[:error]
        )
    end

    post '/api/reset_server' do
        assert(user_logged_in?)

        email = @session_user[:email]
        data = parse_request_data(:required_keys => [:confirmation])
        assert(data[:confirmation] == email, 'confirmation_mismatch')

        stop_server(email)
        container_name = fs_tag_for_email(email)
        system("rm -rf /user/#{container_name}")
        log_event "Workspace reset by its owner (all files deleted): #{email}"

        # assign new server_tag and server_sid
        server_tag = gen_server_tag()
        server_sid = gen_server_sid()
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :server_tag => server_tag, :server_sid => server_sid})
            MATCH (u:User {email: $email})
            SET u.server_tag = $server_tag
            SET u.server_sid = $server_sid
        END_OF_STRING

        respond(:yay => 'sure')
    end
    # A link to the whole Workspace opens it for anyone who has it, without
    # login: it holds SHARE_DAYS, sharing again extends it with the same
    # link, and expire_shares! (hourly) takes expired ones out.
    SHARE_DAYS = 7
    post '/api/share_server' do
        assert(user_logged_in?)
        halt 403, { :error => 'terms_required' }.to_json if terms_required?

        share_tag = @session_user[:share_tag] || gen_share_tag()
        expires = Time.now.to_i + SHARE_DAYS * 24 * 3600
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :share_tag => share_tag, :expires => expires})
            MATCH (u:User {email: $email})
            SET u.share_tag = $share_tag, u.share_expires = $expires;
        END_OF_STRING

        Main.refresh_nginx_config()

        respond(:yay => 'sure', :share_tag => share_tag, :share_expires => expires)
    end

    post '/api/unshare_server' do
        assert(user_logged_in?)

        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]})
            MATCH (u:User {email: $email})
            REMOVE u.share_tag, u.share_expires;
        END_OF_STRING

        Main.refresh_nginx_config()

        respond(:yay => 'sure')
    end
end
