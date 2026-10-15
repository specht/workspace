require './include/helper.rb'
require './include/automatron.rb'
require './include/content_parts.rb'
require './include/vscode_settings.rb'
require './include/page_kinds.rb'
require './include/task_spec.rb'
require './include/task_check.rb'
require './include/task_card.rb'
require './include/task_store.rb'
require './include/class_board.rb'
require './include/submission_view.rb'
require './include/student_home.rb'
require './include/group_visibility.rb'
require './include/look_in.rb'
require './include/reference_index.rb'
require './include/template_freshness.rb'
require './include/anaglyph_viewer.rb'
require './include/atomic_file.rb'
require './include/authentication.rb'
require './include/database_provisioning.rb'
require './include/trusted_template.rb'
require './include/tutorial_screenshots.rb'
require './include/teacher_roles.rb'
require './include/test_workspace_package.rb'
require './include/test_workspace_extensions.rb'
require './include/workspace_credentials.rb'
require './include/workspace_runtime.rb'
require './include/tic80_storage.rb'
require 'base64'
require 'cgi'
require 'digest'
require 'mail'
require 'mysql2'
require 'neo4j_bolt'
require 'nokogiri'
require 'faye/websocket'
require 'open3'
require 'redcarpet'
require 'rouge'
require './include/er_lexer.rb'
require './include/cypher_lexer.rb'
require './include/code_highlight.rb'
require 'securerandom'
require 'shellwords'
require 'set'
require 'sinatra/base'
require 'sinatra/cookies'

require './credentials.rb'

Neo4jBolt.bolt_host = 'neo4japp'
Neo4jBolt.bolt_port = 7687

CACHE_BUSTER = SecureRandom.alphanumeric(12)
# Hand-ins run in the browser from a host of their own (SubmissionView);
# addresses to them are signed with a key that lives as long as the server.
SUBMISSION_VIEW_HOST = "abgaben.#{WEBSITE_HOST.split(':').first}"
SUBMISSION_VIEW_ROOT = WEB_ROOT.sub('//', '//abgaben.')
SUBMISSION_VIEW_SECRET = SecureRandom.hex(32)
RUBOCOP_LAYOUT_CONFIG_PATH = "/tmp/rubocop_layout.yml"

MODULE_ORDER = [:workspace, :codebites, :phpmyadmin, :neo4j, :tic80]
MODULE_LABELS = {
    :workspace  => 'Workspace',
    :codebites  => 'Code Bites',
    :phpmyadmin => 'phpMyAdmin',
    :neo4j      => 'Neo4j',
    :tic80      => 'TIC-80',
}

class Neo4jGlobal
    include Neo4jBolt
end

$neo4j = Neo4jGlobal.new

def assert(condition, message = 'assertion failed')
    raise message unless condition
end

def debug(message, index = 0)
    index = 0
    begin
        while index < caller_locations.size - 1 && ['transaction', 'neo4j_query', 'neo4j_query_expect_one'].include?(caller_locations[index].base_label)
            index += 1
        end
    rescue
        index = 0
    end
    # STDERR.puts caller_locations.to_yaml
    l = caller_locations[index]
    ls = ''
    begin
        ls = "#{l.path.sub('/app/', '')}:#{l.lineno} @ #{l.base_label}"
    rescue
        ls = "#{l[0].sub('/app/', '')}:#{l[1]}"
    end
    STDERR.puts "#{DateTime.now.strftime('%H:%M:%S')} [#{ls}] #{message}"
end

class RandomTag
    BASE_31_ALPHABET = '0123456789bcdfghjklmnpqrstvwxyz'
    def self.to_base31(i)
        result = ''
        while i > 0
            result += BASE_31_ALPHABET[i % 31]
            i /= 31
        end
        result
    end

    def self.generate(length = 12)
        self.to_base31(SecureRandom.hex(length).to_i(16))[0, length]
    end
end

def mail_html_to_plain_text(s)
    s.gsub('<p>', "\n\n").gsub(/<br\s*\/?>/, "\n").gsub(/<\/?[^>]*>/, '').strip
end

def deliver_mail(plain_text = nil, &block)
    mail = Mail.new do
        charset = 'UTF-8'
        message = self.instance_eval(&block)
        if plain_text.nil?
            html_part do
                content_type 'text/html; charset=UTF-8'
                body message
            end

            text_part do
                content_type 'text/plain; charset=UTF-8'
                body mail_html_to_plain_text(message)
            end
        else
            text_part do
                content_type 'text/plain; charset=UTF-8'
                body plain_text
            end
        end
    end
    if DEVELOPMENT
        STDERR.puts "Not sending mail in development mode!"
        STDERR.puts '-' * 40
        STDERR.puts "From:    #{mail.from.join('; ')}"
        STDERR.puts "To:      #{mail.to.join('; ')}"
        STDERR.puts "Subject: #{mail.subject}"
        STDERR.puts mail.text_part
        STDERR.puts '-' * 40
    else
        mail.deliver!
    end
end

class SetupDatabase
    include Neo4jBolt

    def setup(main)
        delay = 1
        10.times do
            begin
                neo4j_query("MATCH (n) RETURN n LIMIT 1;")
                setup_constraints_and_indexes(CONSTRAINTS_LIST, INDEX_LIST)
                debug "Setup finished."
                break
            rescue
                debug $!
                debug "Retrying setup after #{delay} seconds..."
                sleep delay
                delay += 1
            end
        end
    end
end

class Main < Sinatra::Base
    include Neo4jBolt
    helpers Sinatra::Cookies

    # Pages that were renamed; old links and bookmarks lead to the new page.
    MOVED_PAGES = {
        'videothek' => 'filmdatenbank',
        # Teacher pages that became part of their sandbox (2026-10): the app
        # tour is on the activity page, the teaching notes in its teacher note.
        'lehrmaterial-sortieren' => 'sandbox-sortieren',
        'lehrmaterial-datenstrukturen' => 'sandbox-datenstrukturen',
        'lehrmaterial-cpu' => 'sandbox-cpu',
        # The 2D Game Studio is a playground of its own; Lehrmaterial had only
        # its page and the list of teacher notes, which is in the profile now.
        'lehrmaterial-2d' => 'https://2d.hackschule.de/',
        'lehrmaterial' => 'lehrmaterial-einstieg',
        'lehrmaterial.html' => 'lehrmaterial-einstieg',
        # »HTML-Folien gestalten« became four Handbuch pages for web pages and
        # slides alike (2026-10); most of it is on the HTML page.
        'reference/html-folien' => 'reference/html',
    }.freeze

    # Handbuch: reference pages, served under /reference/<slug>.
    REFERENCE_ROOT = '/src/content/reference'
    REFERENCE_SECTIONS_PATH = '/src/content/reference/sections.yaml'

    set :host_authorization, {
        :permitted_hosts => [
            WEBSITE_HOST.split(':').first,
            ".#{WEBSITE_HOST.split(':').first}",
        ],
    }

    helpers do
        def svg_from_dot(dot_str)
            # Use Graphviz 'dot' from PATH to turn DOT into SVG (no temp files needed).
            svg, err, status = Open3.capture3('dot', '-Tsvg', stdin_data: dot_str)
            halt 500, { error: "Graphviz failed", detail: err }.to_json unless status.success?
            svg
        end

        def safe_regex(str)
            # very lightweight guardrails for classroom use:
            # - non-empty
            # - length cap (avoid pathological inputs)
            halt 400, { error: "regex must be a non-empty string" }.to_json if str.nil? || str.strip.empty?
            halt 413, { error: "regex too long" }.to_json if str.length > 2000
            str
        end

        def rubocop_format_ruby(code)
            Tempfile.create(["codebite", ".rb"]) do |f|
                f.write(code)
                f.flush

                cmd = [
                    "rubocop",
                    "--server",
                    "--config", RUBOCOP_LAYOUT_CONFIG_PATH,
                    "--only", "Layout",
                    "-x",               # layout-only autocorrect
                    f.path
                ]

                _out, err, status = Open3.capture3(*cmd)

                unless status.success?
                    # fallback without server
                    cmd.delete("--server")
                    _out2, err2, status2 = Open3.capture3(*cmd)
                    unless status2.success?
                        STDERR.puts "RuboCop failed: #{err}\n#{err2}"
                        return code
                    end
                end

                File.read(f.path)
            end
        end
    end

    def self.fs_tag_for_email(email)
        Digest::SHA2.hexdigest(email).to_i(16).to_s(36)[0, 16]
    end

    def fs_tag_for_email(email)
        Main.fs_tag_for_email(email)
    end

    def self.test_demo_tag(email, test_tag)
        digest = Digest::SHA2.hexdigest("#{email}:#{test_tag}")
        "demo#{digest[0, 20]}"
    end

    def self.tag_for_sid(sid)
        Digest::SHA2.hexdigest(LOGIN_CODE_SALT + sid)[0, 16].to_i(16).to_s(36)[0, 8].downcase
    end

    def tag_for_sid(sid)
        Main.tag_for_sid(sid)
    end

    def self.server_sid_for_email(email)
        return Digest::SHA2.hexdigest(LOGIN_CODE_SALT + email)
    end

    def server_sid_for_email(email)
        Main.server_sid_for_email(email)
    end

    # Shell/Docker command timeouts.
    #
    # All values are seconds and can be overridden through environment variables:
    #   HS_TIMEOUT_DEFAULT
    #   HS_TIMEOUT_DOCKER_INSPECT
    #   HS_TIMEOUT_DOCKER_NETWORK_INSPECT
    #   HS_TIMEOUT_NGINX_RELOAD
    #   HS_TIMEOUT_CHOWN
    #   HS_TIMEOUT_TAR
    #   HS_TIMEOUT_GIT
    #   HS_TIMEOUT_DOCKER_RUN
    #   HS_TIMEOUT_DOCKER_KILL
    #
    # The default values are intentionally conservative. They prevent a wedged
    # Docker/chown/tar command from blocking a Sinatra request forever, while
    # still allowing slow first starts to finish normally.
    def self.timeout_seconds(env_name, default)
        value = ENV[env_name]
        return default if value.nil? || value.strip.empty?
        Integer(value)
    rescue
        STDERR.puts ">>> Invalid #{env_name}=#{value.inspect}; using #{default}s"
        default
    end

    SHELL_TIMEOUTS = {
        :default => timeout_seconds('HS_TIMEOUT_DEFAULT', 30),
        :docker_inspect => timeout_seconds('HS_TIMEOUT_DOCKER_INSPECT', 10),
        :docker_network_inspect => timeout_seconds('HS_TIMEOUT_DOCKER_NETWORK_INSPECT', 10),
        :nginx_reload => timeout_seconds('HS_TIMEOUT_NGINX_RELOAD', 10),
        :chown => timeout_seconds('HS_TIMEOUT_CHOWN', 120),
        :tar => timeout_seconds('HS_TIMEOUT_TAR', 60),
        :git => timeout_seconds('HS_TIMEOUT_GIT', 120),
        :docker_run => timeout_seconds('HS_TIMEOUT_DOCKER_RUN', 60),
        :docker_kill => timeout_seconds('HS_TIMEOUT_DOCKER_KILL', 10),
    }

    def self.shell_timeout(name = :default)
        SHELL_TIMEOUTS.fetch(name, SHELL_TIMEOUTS[:default])
    end

    def shell_timeout(name = :default)
        Main.shell_timeout(name)
    end

    # Run a shell command with a hard timeout. We keep shell strings here because
    # the existing code already builds commands that rely on shell syntax and
    # environment interpolation.
    def self.shell_capture(command, timeout: nil, allow_failure: false, log_command: true)
        timeout ||= shell_timeout(:default)
        timeout = Integer(timeout)

        t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        # STDERR.puts ">>> CMD BEGIN timeout=#{timeout}s: #{command}" if log_command

        stdout_data = ''
        stderr_data = ''
        status = nil
        timed_out = false

        Open3.popen3(command, :pgroup => true) do |stdin, stdout, stderr, wait_thr|
            stdin.close

            stdout_thread = Thread.new { stdout.read }
            stderr_thread = Thread.new { stderr.read }

            unless wait_thr.join(timeout)
                timed_out = true
                begin
                    Process.kill('TERM', -wait_thr.pid)
                rescue
                    begin
                        Process.kill('TERM', wait_thr.pid)
                    rescue
                    end
                end

                sleep 1

                if wait_thr.alive?
                    begin
                        Process.kill('KILL', -wait_thr.pid)
                    rescue
                        begin
                            Process.kill('KILL', wait_thr.pid)
                        rescue
                        end
                    end
                end

                wait_thr.join(1)
            end

            status = wait_thr.value unless wait_thr.alive?

            if stdout_thread.join(1)
                stdout_data = stdout_thread.value rescue ''
            else
                stdout_thread.kill rescue nil
            end

            if stderr_thread.join(1)
                stderr_data = stderr_thread.value rescue ''
            else
                stderr_thread.kill rescue nil
            end
        end

        dt = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0

        if timed_out
            STDERR.puts ">>> CMD TIMEOUT after #{format('%.3f', dt)}s: #{command}"
            raise "Command timed out after #{timeout}s: #{command}"
        end

        unless allow_failure || (status && status.success?)
            STDERR.puts ">>> CMD ERROR after #{format('%.3f', dt)}s status=#{status&.exitstatus}: #{command}"
            raise "Command failed with status #{status&.exitstatus}: #{command}\n#{stderr_data}"
        end

        # STDERR.puts ">>> CMD END after #{format('%.3f', dt)}s status=#{status&.exitstatus}: #{command}" if log_command
        stdout_data
    end

    def self.shell_ok(command, timeout: nil, allow_failure: false)
        shell_capture(command, :timeout => timeout, :allow_failure => allow_failure)
        true
    end

    def shell_capture(command, timeout: nil, allow_failure: false)
        Main.shell_capture(command, :timeout => timeout, :allow_failure => allow_failure)
    end

    def shell_ok(command, timeout: nil, allow_failure: false)
        Main.shell_ok(command, :timeout => timeout, :allow_failure => allow_failure)
    end

    def self.workspace_runtime
        @workspace_runtime ||= WorkspaceRuntime::DirectDocker.new(
            :capture => lambda { |command, **options| Main.shell_capture(command, **options) },
            :ok => lambda { |command, **options| Main.shell_ok(command, **options) },
        )
    end

    def workspace_runtime
        Main.workspace_runtime
    end

    # Background queue for workspace launches. This keeps long-running Docker,
    # chown, tar, and nginx-refresh work out of request handlers while still
    # deduplicating repeated launch clicks for the same workspace.
    def self.server_start_key(email, test_tag = nil)
        fs_tag_for_email("#{email}#{test_tag}")
    end

    def self.server_start_wait_seconds
        value = ENV['HS_SERVER_START_WAIT_SECONDS']
        return 2.0 if value.nil? || value.strip.empty?
        Float(value)
    rescue
        STDERR.puts ">>> Invalid HS_SERVER_START_WAIT_SECONDS=#{value.inspect}; using 2.0s"
        2.0
    end

    def self.server_start_job_keep_seconds
        value = ENV['HS_SERVER_START_JOB_KEEP_SECONDS']
        return 3600 if value.nil? || value.strip.empty?
        Integer(value)
    rescue
        STDERR.puts ">>> Invalid HS_SERVER_START_JOB_KEEP_SECONDS=#{value.inspect}; using 3600s"
        3600
    end

    def self.ensure_server_start_queue!
        @@server_start_queue = Queue.new unless defined?(@@server_start_queue) && @@server_start_queue
        @@server_start_mutex = Mutex.new unless defined?(@@server_start_mutex) && @@server_start_mutex
        @@server_start_jobs = {} unless defined?(@@server_start_jobs) && @@server_start_jobs
    end

    def self.prune_server_start_jobs_locked!
        cutoff = Time.now - server_start_job_keep_seconds
        @@server_start_jobs.delete_if do |_key, job|
            job[:finished_at] && job[:finished_at] < cutoff
        end
    end

    def self.server_start_snapshot(job)
        return { :status => 'unknown' } unless job

        {
            :key => job[:key],
            :email => job[:email],
            :test_tag => job[:test_tag],
            :server_tag => job[:server_tag],
            :status => job[:status],
            :queued_at => job[:queued_at]&.to_i,
            :started_at => job[:started_at]&.to_i,
            :finished_at => job[:finished_at]&.to_i,
            :error => job[:error],
        }
    end

    def self.server_start_status(key)
        ensure_server_start_queue!

        @@server_start_mutex.synchronize do
            server_start_snapshot(@@server_start_jobs[key] || { :key => key, :status => 'unknown' })
        end
    end

    def self.enqueue_server_start(email:, test_tag:, server_tag:, refresh_nginx_after: false, &block)
        raise 'enqueue_server_start requires a block' unless block_given?

        ensure_server_start_queue!
        key = server_start_key(email, test_tag)

        @@server_start_mutex.synchronize do
            prune_server_start_jobs_locked!

            existing = @@server_start_jobs[key]
            if existing && ['queued', 'running'].include?(existing[:status])
                STDERR.puts ">>> Server start already #{existing[:status]}: #{key}"
                return existing
            end

            job = {
                :key => key,
                :email => email,
                :test_tag => test_tag,
                :server_tag => server_tag,
                :status => 'queued',
                :queued_at => Time.now,
                :started_at => nil,
                :finished_at => nil,
                :error => nil,
                :refresh_nginx_after => refresh_nginx_after,
                :runner => block,
            }

            @@server_start_jobs[key] = job
            @@server_start_queue << job
            STDERR.puts ">>> Server start queued: #{key} email=#{email} test_tag=#{test_tag.inspect}"
            job
        end
    end

    def self.wait_for_server_start_job(job, max_seconds: server_start_wait_seconds)
        deadline = Time.now + max_seconds.to_f

        loop do
            snapshot = server_start_status(job[:key])
            return snapshot unless ['queued', 'running'].include?(snapshot[:status])
            return snapshot if Time.now >= deadline
            sleep 0.05
        end
    end

    def self.server_start_worker_loop
        ensure_server_start_queue!

        loop do
            job = @@server_start_queue.pop

            @@server_start_mutex.synchronize do
                job[:status] = 'running'
                job[:started_at] = Time.now
                job[:error] = nil
            end

            begin
                STDERR.puts ">>> Server start worker: starting #{job[:key]}"
                job[:runner].call

                if job[:refresh_nginx_after]
                    STDERR.puts ">>> Server start worker: refresh nginx after #{job[:key]}"
                    Main.refresh_nginx_config()
                end

                @@server_start_mutex.synchronize do
                    job[:status] = 'ready'
                    job[:finished_at] = Time.now
                end
                STDERR.puts ">>> Server start worker: ready #{job[:key]}"
            rescue => e
                @@server_start_mutex.synchronize do
                    job[:status] = 'failed'
                    job[:finished_at] = Time.now
                    job[:error] = "#{e.class}: #{e.message}"
                end
                STDERR.puts ">>> Server start worker failed for #{job[:key]}: #{e.class}: #{e.message}"
                STDERR.puts e.backtrace.join("\n")
            ensure
                job.delete(:runner)
            end
        end
    end

    require './include/live_apps.rb'

    def workspace_url()
        if user_logged_in?
            server_tag = @session_user[:server_tag]
            user_with_test = neo4j_query_expect_one(<<~END_OF_QUERY, {:email => @session_user[:email]})
                MATCH (u:User {email: $email})
                OPTIONAL MATCH (u)-[:TAKES]->(t:Test {running: TRUE})
                RETURN u, t;
            END_OF_QUERY
            server_tag = "#{server_tag}#{(user_with_test['t'] || {})[:tag]}"
            "http://#{server_tag}.#{WEBSITE_HOST}/"
        else
            ''
        end
    end

    @@nginx_config_mutex = Mutex.new

    def self.refresh_nginx_config
        @@nginx_config_mutex.synchronize do
            refresh_nginx_config_locked
        end
    end

    def self.refresh_nginx_config_locked
        STDERR.puts ">>> Refreshing nginx config..."
        running_servers = {}
        workspace_runtime.running_workspaces(
            :timeout => shell_timeout(:docker_network_inspect),
        ).each_pair do |fs_tag, workspace|
            running_servers[fs_tag] = {
                :ip => workspace[:ip],
                :server_sid => nil,
                :server_tag => nil,
                :share_tag => nil,
                :watch_tags => {}, # watch_tag => server_id of watcher
                :live_apps => {},  # live share tag => port
            }
        end

        emails_and_server_tags = []

        # STDERR.puts "Got #{emails_and_server_tags.size} emails and server tags: #{emails_and_server_tags.to_yaml}"

        STDERR.puts ">>> Fetching users..."
        users = []
        # Here's an ugly hack: for some reason, after a day, this results in
        # Errno::EPIPE - Broken pipe, so we catch that error, and if it failed,
        # just recreate the neo4j connection and try again
        2.times do
            begin
                users = $neo4j.neo4j_query("MATCH (u:User) OPTIONAL MATCH (u)-[:TAKES]->(t:Test {running: TRUE}) RETURN u, t;").to_a
                break
            rescue
                $neo4j = Neo4jGlobal.new
            end
        end

        STDERR.puts ">>> Creating nginx config..."

        users.each do |row|
            user = row['u']
            test = row['t']
            email_with_test_tag = "#{user[:email]}#{(test || {})[:tag]}"
            server_tag = "#{user[:server_tag]}#{(test || {})[:tag]}"
            fs_tag = fs_tag_for_email(email_with_test_tag)
            if running_servers.include?(fs_tag)
                # sid_ip_pairs << "#{user[:server_sid]} #{running_servers[fs_tag][:ip]}:8443;"
                running_servers[fs_tag][:server_sid] = user[:server_sid]
                running_servers[fs_tag][:server_tag] = server_tag
                running_servers[fs_tag][:share_tag] = user[:share_tag]
            end
        end

        # Teacher test demos use the same DNS-disabled test runtime as students,
        # but live under a separate private workspace tag and are never shared.
        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (u:User)<-[:BELONGS_TO]-(t:Test)
            RETURN u, t;
        END_OF_QUERY
            user = row['u']
            test = row['t']
            demo_tag = test_demo_tag(user[:email], test[:tag])
            fs_tag = fs_tag_for_email("#{user[:email]}#{demo_tag}")
            next unless running_servers.include?(fs_tag)

            running_servers[fs_tag][:server_sid] = user[:server_sid]
            running_servers[fs_tag][:server_tag] = "#{user[:server_tag]}#{demo_tag}"
        end
        # STDERR.puts "sid_ip_pairs:"
        # STDERR.puts sid_ip_pairs.join("\n")

        # watch_tag_ip_pairs = []

        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (w:User)-[r:WATCHING]->(u:User)
            RETURN r.watch_tag, u, w;
        END_OF_QUERY
            watch_tag = row['r.watch_tag']
            user = row['u']
            watcher = row['w']
            fs_tag = fs_tag_for_email(user[:email])
            if running_servers.include?(fs_tag)
                # watch_tag_ip_pairs << "#{watch_tag} #{running_servers[fs_tag][:ip]}:8443;"
                running_servers[fs_tag][:watch_tags][watch_tag] = watcher[:server_sid]
            end
        end

        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (u:User)-[:SHARES_LIVE_APP]->(s:LiveAppShare {active: TRUE})
            RETURN u.email, s.port, s.tag;
        END_OF_QUERY
            fs_tag = fs_tag_for_email(row['u.email'])
            next unless running_servers.include?(fs_tag)
            running_servers[fs_tag][:live_apps][row['s.tag']] = row['s.port'].to_i
        end

        # STDERR.puts ">>> Got running servers: #{running_servers.to_yaml}"

        # STDERR.puts "watch_tag_ip_pairs:"
        # STDERR.puts watch_tag_ip_pairs.join("\n")

        nginx_config = <<~END_OF_STRING
            #log_format custom '$host $http_x_forwarded_proto $remote_addr - $remote_user [$time_local] "$request" '
            #                '$status $body_bytes_sent "$http_referer" '
            #                '"$http_user_agent" "$request_time"';

            map_hash_bucket_size 128;

            # Keep access logs useful without recording capability tokens, session
            # cookies, workspace tags or one-time login URLs.
            map $uri $hs_log_uri {
                default $uri;
                ~^/w/[a-z0-9]+(?:/|$) /w/[redacted];
                ~^/share/[a-z0-9]+(?:/|$) /share/[redacted];
                ~^/l/[a-z0-9]+(?:/|$) /l/[redacted];
            }

            log_format custom 'method=$request_method uri=$hs_log_uri status=$status share=$hs_is_share allowed=$hs_allowed';

            map $sent_http_content_type $expires {
                default                         off;
                text/html                       epoch;
                text/css                        max;
                application/javascript          max;
                ~image/                         max;
                ~font/                          max;
                application/x-font-ttf          max;
                application/x-font-otf          max;
                application/font-woff           max;
                application/font-woff2          max;
            }

            map $http_upgrade $connection_upgrade {
                default upgrade;
                ''      close;
            }

            # code-server HTML must arrive uncompressed so sub_filter can inject
            # the webfont stylesheet. Keep gzip for JS/CSS/other assets.
            map $http_accept $hs_code_accept_encoding {
                default gzip;
                ~*text/html "";
            }

            # Inject the browser-visible code font only into the code-server
            # workbench document. Internal HTML such as the web-worker extension
            # host has its own restrictive CSP and must remain untouched.
            map $request_uri $hs_code_font_head {
                default "</head>";
                ~^/(?:w/[a-z0-9]+/)?(?:\\?.*)?$ '<link rel="preload" href="/include/fonts/0xProtoNerdFontMono/0xProtoNerdFontMono-Regular.woff2" as="font" type="font/woff2" crossorigin><link rel="stylesheet" href="/include/fonts.css?#{CACHE_BUSTER}"></head>';
            }

            # ---------------------------
            # Workspace token extraction
            # ---------------------------

            # Extract token from /w/<token>/...
            map $uri $w_token {
                default "";
                ~^/w/(?<t>[a-z0-9]+)(?:/|$) $t;
            }

            # Extract token from <token>.#{WEBSITE_HOST.split(':').first}
            map $host $host_token {
                default "";
                ~^(?<t>[a-z0-9]+)\\.#{WEBSITE_HOST.split(':').first.gsub('.', '\\.')} $t;
            }

            # Effective token: prefer subdomain token, else /w/ token
            map "$host_token:$w_token" $hs_token {
                default $w_token;
                ~^(?<t>[^:]+): $t;
            }

            # Token -> upstream
            map $hs_token $hs_upstream {
                default "";
                #{running_servers.map { |fs_tag, info| if info[:server_tag] then "#{info[:server_tag]} http://#{info[:ip]}:8443;" else nil end}.compact.join("\n    ")}
                #{running_servers.map { |fs_tag, info| if info[:server_tag] then info[:watch_tags].map { |watch_tag, server_sid| "#{watch_tag} http://#{info[:ip]}:8443;" }.flatten.compact.join("\n    ") else nil end}.compact.join("\n    ")}
                #{running_servers.map { |fs_tag, info| if info[:share_tag] then "#{info[:share_tag]} http://#{info[:ip]}:8443;" else nil end}.compact.join("\n    ")}
            }

            # Token -> share flag (explicit allowlist)
            map $hs_token $hs_is_share {
                default 0;
                #{running_servers.map { |fs_tag, info| if info[:share_tag] then "#{info[:share_tag]} 1;" else nil end}.compact.join("\n    ")}
            }

            # Token -> required SID
            map $hs_token $hs_required_sid {
                default "";
                #{running_servers.map { |fs_tag, info| if info[:server_sid] then "#{info[:server_tag]} #{info[:server_sid]};" else nil end}.compact.join("\n    ")}
                #{running_servers.map { |fs_tag, info| if info[:server_tag] then info[:watch_tags].map { |watch_tag, server_sid | "#{watch_tag} #{server_sid};" }.flatten.compact.join("\n    ") else nil end}.compact.join("\n    ")}
            }

            # Shared live apps: stable tag -> workspace proxy + port
            map $host $hs_live_tag {
                default "";
                ~^live-(?<lt>[a-z0-9]+)\.#{WEBSITE_HOST.split(':').first.gsub('.', '\\.')}$ $lt;
            }

            map $hs_live_tag $hs_live_upstream {
                default "";
                #{running_servers.map { |_fs_tag, info| info[:live_apps].map { |tag, _port| "#{tag} http://#{info[:ip]}:8443;" } }.flatten.join("\n    ")}
            }

            map $hs_live_tag $hs_live_port {
                default "";
                #{running_servers.map { |_fs_tag, info| info[:live_apps].map { |tag, port| "#{tag} #{port};" } }.flatten.join("\n    ")}
            }

            map $hs_live_upstream $hs_live_no_upstream {
                ""      1;
                default 0;
            }

            # deny if upstream missing
            map $hs_upstream $hs_no_upstream {
                ""      1;
                default 0;
            }

            # deny if token is private but required SID missing (fail closed)
            map "$hs_is_share:$hs_required_sid" $hs_private_missing_sid {
                default 0;
                "0:"    1;
            }

            # Allowed if:
            # - share token, OR
            # - cookie SID matches required SID
            map "$hs_is_share:$cookie_hs_server_sid:$hs_required_sid" $hs_allowed {
                default 0;

                # Share token => allow regardless of cookie
                ~^1: 1;

                # Private token => cookie must equal required sid
                ~^0:(?<sid>[^:]+):\\k<sid>$ 1;
            }

            server {
                listen 80;
                server_name #{WEBSITE_HOST.split(':').first};
                client_max_body_size 100M;
                expires $expires;
                add_header Permissions-Policy "local-fonts=()" always;

                gzip on;
                gzip_comp_level 6;
                gzip_min_length 256;
                gzip_buffers 16 8k;
                gzip_proxied any;
                gzip_types
                    text/plain
                    text/css
                    text/js
                    text/xml
                    text/javascript
                    application/javascript
                    application/x-javascript
                    application/json
                    application/xml
                    application/rss+xml
                    image/svg+xml;

                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location = /cache { return 301 /cache/; }
                location ^~ /cache/ { alias /webcache/; }

                location = /brand { return 301 /brand/; }
                location ^~ /brand/ { alias /brand/; }

                location = /dl { return 301 /dl/; }
                location ^~ /dl/ { alias /dl/; }

                location / {
                    root /usr/share/nginx/html;
                    try_files $uri @ruby;
                }

                # nginx knows no type for .webmanifest.
                location = /site.webmanifest {
                    root /usr/share/nginx/html;
                    types { }
                    default_type application/manifest+json;
                }

                location @ruby {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://ruby:9292;
                }

                # Normalize /w/<token> -> /w/<token>/
                location ~ ^/w/[a-z0-9]+$ {
                    return 301 $uri/$is_args$args;
                }

                location ~ ^/w/[a-z0-9]+/ {
                    if ($hs_no_upstream) { return 404; }
                    if ($hs_private_missing_sid) { return 403; }
                    if ($hs_allowed = 0) { return 403; }

                    # strip /w/<token>
                    rewrite ^/w/[a-z0-9]+(.*)$ $1 break;

                    include /etc/nginx/snippets/proxy_ws_code.conf;
                    sub_filter_once on;
                    sub_filter '</head>' $hs_code_font_head;
                    proxy_pass $hs_upstream;
                }
            }

            # ==================================
            # Subdomains: <token>.#{WEBSITE_HOST.split(':').first}
            # ==================================
            server {
                listen 80;
                server_name ~^(?<t>[a-z0-9]+)\\.#{WEBSITE_HOST.split(':').first.gsub('.', '\.')}$;
                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;
                add_header Permissions-Policy "local-fonts=()" always;

                # ----------------------------------
                # /proxy/<port> -> <token>-<port>.#{WEBSITE_HOST.split(':').first}
                # ----------------------------------
                location ~ ^/proxy/(?<p>\\d+)(?<rest>/.*)?$ {
                    if ($hs_no_upstream) { return 404; }
                    if ($hs_private_missing_sid) { return 403; }
                    if ($hs_allowed = 0) { return 403; }

                    # Default rest to /
                    set $r $rest;
                    if ($r = "") { set $r /; }

                    # pick up external port from Host header (e.g. :8025)
                    set $port "";
                    if ($http_host ~* :(?<hp>\\d+)$) { set $port :$hp; }

                    return 302 #{DEVELOPMENT ? 'http:' : 'https:'}//${t}-${p}.#{WEBSITE_HOST.split(':').first}$port$r$is_args$args;
                }

                # Browser-visible Hackschule fonts for the code-server workbench.
                # These are public static assets; workspace authentication remains
                # on the code-server proxy itself.
                location = /include/fonts.css {
                    root /usr/share/nginx/html;
                    try_files $uri =404;
                }

                location ^~ /include/fonts/0xProtoNerdFontMono/ {
                    root /usr/share/nginx/html;
                    try_files $uri =404;
                }

                location / {
                    if ($hs_no_upstream) { return 404; }
                    if ($hs_private_missing_sid) { return 403; }
                    if ($hs_allowed = 0) { return 403; }

                    include /etc/nginx/snippets/proxy_ws_code.conf;
                    sub_filter_once on;
                    sub_filter '</head>' $hs_code_font_head;
                    proxy_pass $hs_upstream;
                }
            }

            # ==========================================
            # Port subdomains: <tag>-<port>.#{WEBSITE_HOST.split(':').first}
            # ==========================================
            server {
                listen 80;
                server_name ~^(?<t>[a-z0-9]+)-(?<p>\\d+)\\.#{WEBSITE_HOST.split(':').first.gsub('.', '\\.')}$;
                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;
                add_header Permissions-Policy "local-fonts=()" always;

                set $hs_token $t;

                location / {
                    if ($hs_no_upstream) { return 404; }
                    if ($hs_private_missing_sid) { return 403; }
                    if ($hs_allowed = 0) { return 403; }

                    rewrite ^/(.*)$ /proxy/$p/$1 break;

                    include /etc/nginx/snippets/proxy_ws.conf;

                    # First remove leaked /proxy/<port>/ redirects.
                    proxy_redirect ~^/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^/proxy/[0-9]+/?$ /;
                    proxy_redirect ~^https?://[^/]+/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^https?://[^/]+/proxy/[0-9]+/?$ /;
                    proxy_redirect ~^//[^/]+/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^//[^/]+/proxy/[0-9]+/?$ /;

                    # Any remaining absolute redirect must use the browser-visible host.
                    # $http_host includes :8025 in development.
                    proxy_redirect ~^(https?://)[^/]+(/.*)$ $1$http_host$2;
                    proxy_redirect ~^//[^/]+(/.*)$ //$http_host$1;

                    proxy_pass $hs_upstream;
                }
            }

            # ==========================================
            # Shared live apps: live-<tag>.#{WEBSITE_HOST.split(':').first}
            # ==========================================
            server {
                listen 80;
                server_name ~^live-[a-z0-9]+\.#{WEBSITE_HOST.split(':').first.gsub('.', '\\.')}$;
                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location = /_hs_live_auth {
                    internal;
                    proxy_pass http://ruby:9292/api/live_app_authorize;
                    proxy_method GET;
                    proxy_pass_request_body off;
                    proxy_set_header Content-Length "";
                    proxy_set_header Host $http_host;
                    proxy_set_header Cookie $http_cookie;
                }

                location / {
                    if ($hs_live_no_upstream) { return 404; }
                    auth_request /_hs_live_auth;

                    rewrite ^/(.*)$ /proxy/$hs_live_port/$1 break;
                    include /etc/nginx/snippets/proxy_ws.conf;

                    # Never expose Hackschule authentication material to student code.
                    proxy_set_header Cookie "";
                    proxy_set_header Authorization "";
                    proxy_hide_header Set-Cookie;

                    proxy_redirect ~^/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^/proxy/[0-9]+/?$ /;
                    proxy_redirect ~^https?://[^/]+/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^https?://[^/]+/proxy/[0-9]+/?$ /;
                    proxy_redirect ~^//[^/]+/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^//[^/]+/proxy/[0-9]+/?$ /;

                    # Any remaining absolute redirect must use the browser-visible host.
                    # $http_host includes :8025 in development.
                    proxy_redirect ~^(https?://)[^/]+(/.*)$ $1$http_host$2;
                    proxy_redirect ~^//[^/]+(/.*)$ //$http_host$1;

                    proxy_pass $hs_live_upstream;
                }
            }

            # ===========================================
            # Hand-ins in the browser (SubmissionView)
            # ===========================================
            server {
                listen 80;
                server_name #{SUBMISSION_VIEW_HOST};
                client_max_body_size 1M;
                access_log off;
                charset utf-8;

                location /v/ {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://ruby:9292;
                }

                location / {
                    return 404;
                }
            }

            # ===========
            # phpMyAdmin
            # ===========
            server {
                listen 80;
                server_name phpmyadmin.#{WEBSITE_HOST.split(':').first};

                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location / {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://phpmyadmin:80;
                }
            }

            # ======
            # Neo4j
            # ======
            server {
                listen 80;
                server_name ~^neo4j-(?<t>[a-z0-9]+)\.#{WEBSITE_HOST.split(':').first.gsub('.', '\.')}$;

                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location / {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://neo4j:7474;
                }
            }

            server {
                listen 80;
                server_name bolt.#{WEBSITE_HOST.split(':').first};

                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location / {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_set_header Sec-WebSocket-Protocol $http_sec_websocket_protocol;
                    proxy_buffering off;
                    proxy_read_timeout 1h;
                    proxy_send_timeout 1h;

                    proxy_pass http://neo4j:7687;
                }
            }
        END_OF_STRING

        STDERR.puts ">>> Writing nginx config..."

        AtomicFile.write('/nginx-snippets/proxy_ws.conf', <<~END_OF_STRING)
            proxy_http_version 1.1;
            proxy_set_header Host $http_host;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection upgrade;
            proxy_set_header Accept-Encoding gzip;
        END_OF_STRING

        AtomicFile.write('/nginx-snippets/proxy_ws_code.conf', <<~END_OF_STRING)
            proxy_http_version 1.1;
            proxy_set_header Host $http_host;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection upgrade;
            proxy_set_header Accept-Encoding $hs_code_accept_encoding;
        END_OF_STRING

        AtomicFile.write('/nginx/default.conf', nginx_config)

        STDERR.puts ">>> Sending HUP to nginx to reload nginx config"
        begin
            shell_ok("docker kill -s HUP workspace_nginx_1", :timeout => shell_timeout(:nginx_reload))
        rescue => hup_error
            nginx_state = shell_capture(
                "docker ps -a --filter name=^/workspace_nginx_1$ --format '{{.State}}'",
                :timeout => shell_timeout(:docker_inspect)
            ).strip
            raise hup_error if nginx_state == 'running'
            STDERR.puts ">>> nginx is not running yet (state: #{nginx_state.empty? ? 'absent' : nginx_state}); it will read the published config on startup"
        end
    end

    private_class_method :refresh_nginx_config_locked

    def self.convert_image(image_path)
        image_sha1 = Digest::SHA1.hexdigest(File.read(image_path))[0, 16]
        target_path = "/webcache/#{image_sha1}.webp"
        unless FileUtils.uptodate?(target_path, [image_path])
            STDERR.puts "Copying #{image_path} to cache..."
            FileUtils.cp(image_path, target_path)
        end
        image_sha1
    end

    # Width-specific variants are only needed by overview cards. Restrict this
    # to normal converted cache entries; data-noconvert images use a different key.
    def self.ensure_image_width(image_url, width)
        match = image_url&.match(%r{\A/cache/([0-9a-f]{16})\.webp\z})
        return unless match

        source_path = "/webcache/#{match[1]}.webp"
        return unless File.exist?(source_path)

        target_path = "/webcache/#{match[1]}-#{width}.webp"
        return if FileUtils.uptodate?(target_path, [source_path])

        command = "convert #{source_path} -resize #{width}x #{target_path}"
        system(command)
        if $? != 0
            STDERR.puts command
            STDERR.puts "...conversion of #{source_path} failed!"
        end
    end

    def self.inject_autotoc(root, options = {})
        options[:inject_numbers] = true unless options.include?(:inject_numbers)
        h2_list = root.css('h2').reject { |x| x['data-autotoc'] == 'ignore' }
        if h2_list.size > 1
            h2_list.each.with_index do |h2, index|
                autotoc_container = Nokogiri::XML::Node.new('div', root)
                autotoc_container['class'] = 'autotoc-container'
                label = h2.text.strip
                if options[:inject_numbers]
                    label = "#{index + 1}. #{h2.text}".strip
                    h2.content = label
                end
                autotoc_container['data-label'] = label
                h2.add_previous_sibling(autotoc_container)
                autotoc_container.add_child(h2)
                p = autotoc_container.next_element
                while p && p.name != 'h2'
                    pnext = p.next_element
                    autotoc_container.add_child(p)
                    p = pnext
                end
            end
        end
    end

    DEFAULT_PAGE_DESCRIPTION = 'Der Hackschule Workspace ist eine browserbasierte Entwicklungsumgebung für den Informatikunterricht mit Visual Studio Code, Terminal, Git und zahlreichen Tutorials.'

    def self.normalize_page_metadata_text(text)
        CGI.unescapeHTML(text.to_s)
            .delete("\u00ad")
            .gsub(/\s+/, ' ')
            .strip
    end

    def self.truncate_page_metadata_text(text, max_length = 180)
        return text if text.length <= max_length

        shortened = text[0, max_length + 1].sub(/\s+\S*\z/, '').rstrip
        shortened = text[0, max_length].rstrip if shortened.empty?
        "#{shortened}…"
    end

    def self.inject_page_metadata(html, request_path)
        doc = Nokogiri::HTML(html)
        body = doc.at_css('body') || doc
        homepage = ['/', '/index.html'].include?(request_path)

        heading = nil
        unless homepage
            heading = body.at_css('h1') || body.at_css('h2')
        end

        heading_text = normalize_page_metadata_text(heading&.text)
        page_title = if heading_text.empty?
            'Hackschule Workspace'
        else
            "#{heading_text} · Hackschule Workspace"
        end

        description_node = nil
        description_node = body.at_css('.abstract') unless homepage
        if description_node.nil?
            description_node = body.css('p').find do |paragraph|
                normalize_page_metadata_text(paragraph.text).length >= 40
            end
        end

        description = normalize_page_metadata_text(description_node&.text)
        description = DEFAULT_PAGE_DESCRIPTION if description.empty?
        description = truncate_page_metadata_text(description)

        escaped_title = CGI.escapeHTML(page_title)
        escaped_description = CGI.escapeHTML(description)
        description_tag = %(<meta name="description" content="#{escaped_description}">)

        result = html.sub(
            /<title\b[^>]*>.*?<\/title>/mi,
            "<title>#{escaped_title}</title>"
        )

        description_pattern = /<meta\s+name=["']description["']\s+content=["'][^"']*["']\s*\/?>/i
        if result.match?(description_pattern)
            result = result.sub(description_pattern, description_tag)
        else
            result = result.sub(
                /<\/title>/i,
                "</title>\n    #{description_tag}"
            )
        end

        result
    end

    def self.content_catalog_signature
        digest = Digest::SHA256.new
        ['/src/content/sections.yaml', '/src/content/lehrmaterial/sections.yaml',
         REFERENCE_SECTIONS_PATH].each do |path|
            next unless File.exist?(path)
            digest.update(File.binread(path))
            digest.update("\0")
        end
        Dir['/src/content/**/*.md'].sort.each do |path|
            digest.update(path)
            digest.update("\0")
        end
        digest.hexdigest
    end

    def self.refresh_content_catalog
        return unless DEVELOPMENT
        signature = content_catalog_signature
        return if defined?(@@content_catalog_signature) &&
            @@content_catalog_signature == signature
        parse_content
    end

    def self.refresh_tutorial(slug)
        return unless DEVELOPMENT
        refresh_content_catalog
        path = if defined?(@@content_paths) && @@content_paths
            @@content_paths[slug]
        end
        return unless path && File.file?(path)

        # Generation is intentionally outside @@parse_content_mutex. The
        # generator may launch a Workspace and make requests back to Ruby.
        TutorialScreenshots.prepare(File.read(path), path)
        parse_content(slug)
    end

    # Renders the Markdown of a content file to a Nokogiri document:
    # Redcarpet, reference include boxes, cached images and videos, external
    # links and syntax highlighting. The block may change the Markdown first
    # (the Kenney gallery uses this). Hyphenation is the browser's
    # (hyphens: auto in default.css).
    def self.render_content_markdown(markdown, path, redcarpet)
        markdown = yield(markdown) if block_given?
        html = redcarpet.render(markdown)
        root = Nokogiri::HTML(html)
        wrap_reference_includes(root)

        root.css('img').each do |img|
            begin
                src = img.attr('src')
                next if src.nil?
                if DEVELOPMENT && !src.start_with?('/cache/')
                    img['data-tutorial-screenshot-source'] = src
                end
                next if src.start_with?('/cache/')
                image_path = File.join(File.dirname(path), src)
                next unless File.exist?(image_path)
                if img.attr('data-noconvert')
                    sha1 = Digest::SHA1.hexdigest(image_path)
                    extension = File.extname(image_path)
                    target_path = "/webcache/#{sha1}#{extension}"
                    FileUtils.cp(image_path, target_path) unless FileUtils.uptodate?(target_path, [image_path])
                    img['src'] = "/cache/#{sha1}#{extension}"
                else
                    sha1 = convert_image(image_path)
                    img['src'] = "/cache/#{sha1}.webp"
                    add_media_variants(img, image_path) { |variant_path| "/cache/#{convert_image(variant_path)}.webp" }
                end
                if img.classes.include?('full')
                    img.wrap("<div class='scroll-x'>")
                end
            rescue
                STDERR.puts img.to_s
                raise
            end
        end
        # A script next to the page (<script src='x.js'>), e.g. the ER-Werkstatt
        # core for the Handbuch playground: served from the cache by content
        # hash, so a changed file is never stale in a browser.
        root.css('script[src]').each do |script|
            src = script['src'].to_s
            next if src.empty? || src.start_with?('/') || src =~ /\A[a-z]+:/i
            script_path = File.join(File.dirname(path), src)
            next unless File.file?(script_path)
            sha1 = Digest::SHA1.hexdigest(File.binread(script_path))[0, 16]
            target_path = "/webcache/#{sha1}.js"
            FileUtils.cp(script_path, target_path) unless File.exist?(target_path)
            script['src'] = "/cache/#{sha1}.js"
        end
        root.css('svg').each do |img|
            if img.classes.include?('full')
                img.wrap("<div class='scroll-x'>")
            end
        end
        root.css('video').each do |video|
            src = video.attr('src')
            image_path = File.join(File.dirname(path), src)
            next unless File.exist?(image_path)
            video['src'] = cache_video(image_path)
            add_media_variants(video, image_path) { |variant_path| cache_video(variant_path) }
        end
        root.css('a').each do |a|
            href = a.attr('href')
            if href.index('https://') == 0
                a['target'] = '_blank'
            end
        end
        highlight_code(root)
        root
    end

    def self.cache_video(video_path)
        sha1 = Digest::SHA1.hexdigest(File.read(video_path))[0, 16]
        target_path = "/webcache/#{sha1}.mp4"
        FileUtils.cp(video_path, target_path) unless FileUtils.uptodate?(target_path, [video_path])
        "/cache/#{sha1}.mp4"
    end

    # A generated image or video can have variants beside it (x-dark.webp,
    # x-anaglyph.webp, x-dark-anaglyph.webp, x-crt.webp; variants: in the
    # screenshot README). media_variants.js shows the one that fits the theme
    # and the toggles in the corner (glasses, CRT).
    MEDIA_VARIANTS = %w[dark anaglyph dark-anaglyph crt folie]

    def self.add_media_variants(element, media_path)
        extension = File.extname(media_path)
        MEDIA_VARIANTS.each do |variant|
            variant_path = media_path.delete_suffix(extension) + "-#{variant}#{extension}"
            element["data-variant-#{variant}"] = yield(variant_path) if File.exist?(variant_path)
        end
    end

    def self.highlight_code(root)
        CodeHighlight.apply(root)
    end

    # _include() leaves two marker divs around the included part (see
    # ContentParts.expand_includes); everything between them becomes one box
    # with a link to the part on its reference page.
    def self.wrap_reference_includes(root)
        root.css('div.reference-include-begin').each do |start|
            box = root.document.create_element('div', 'class' => 'reference-include')
            start.add_previous_sibling(box)
            node = start.next_sibling
            start.remove
            while node && !(node.element? && node['class'] == 'reference-include-end')
                following = node.next_sibling
                box.add_child(node)
                node = following
            end
            next unless node
            # _include(…, done: id): tasks.js folds the box once task id is
            # solved; include_fold.js once the part was done on another page.
            %w[data-done data-part data-title data-short].each do |name|
                box[name] = node[name] if node[name]
            end
            link = root.document.create_element('p', 'class' => 'reference-include-link')
            # The tooltip names where the text comes from, like the breadcrumb
            # on the reference page: »Workspace: Grundlagen › Repository klonen«.
            anchor = root.document.create_element('a', 'href' => node['data-href'], 'title' => node['data-label'].to_s)
            anchor.add_child(root.document.create_element('i', 'class' => 'bi bi-book'))
            anchor.add_child(root.document.create_text_node('Im Handbuch nachschlagen'))
            link.add_child(anchor)
            box.add_child(link)
            node.remove
        end
    end

    # Replaces the task slots left by TaskCard.mark_slots with task cards,
    # numbered in page order.
    def self.insert_task_cards(root, task_slots, page_tasks, redcarpet, path)
        render = lambda { |md| redcarpet.render(md.to_s) }
        number = 0
        root.css('div.task-slot').each do |slot|
            id = task_slots[slot['data-slot'].to_i]
            task = page_tasks && page_tasks[:by_id][id]
            html = if task
                number += 1
                TaskCard.html(task, :activity => page_tasks[:activity], :number => number, :render => render)
            else
                STDERR.puts "#{path}: _task(#{id}): no such task in tasks.yaml"
                TaskCard.missing_html(id)
            end
            fragment = root.fragment(html)
            highlight_code(fragment)
            slot.replace(fragment)
        end
    end

    def self.parse_content(only_slug = nil)
        @@parse_content_mutex ||= Mutex.new
        @@parse_content_mutex.synchronize do
            parse_content_locked(only_slug)
        end
    end

    def self.parse_content_locked(only_slug = nil)
        partial = !only_slug.nil?
        parse_started_at = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        STDERR.puts(partial ? "Parsing content: #{only_slug}..." : "Parsing content...")
        # Keep the student and teacher catalogs independent while sharing the
        # Markdown renderer, image cache, and screenshot generator.
        student_sections = YAML.load(File.read('/src/content/sections.yaml'))
        # Teacher-only pages (src/content/lehrmaterial) are optional; there
        # are none at the moment.
        teaching_sections = File.exist?('/src/content/lehrmaterial/sections.yaml') ?
            (YAML.load(File.read('/src/content/lehrmaterial/sections.yaml')) || []) : []
        # Reference pages (Handbuch) live in their own catalog and are
        # served under /reference/<slug>. The catalog is optional.
        reference_sections = File.exist?(REFERENCE_SECTIONS_PATH) ?
            (YAML.load(File.read(REFERENCE_SECTIONS_PATH)) || []) : []
        section_order = student_sections.map { |section| section['key'] }
        teaching_section_order = teaching_sections.map { |section| section['key'] }
        reference_section_order = reference_sections.map { |section| section['key'] }
        sections = student_sections.map { |section| section.merge('catalog' => 'student') } +
            teaching_sections.map do |section|
                section.merge('catalog' => 'teaching', 'teacher_only' => true)
            end +
            reference_sections.map { |section| section.merge('catalog' => 'reference') }
        parsed_sections = {}
        parsed_content = {}
        parsed_content_paths = {}
        paths = []
        seen_paths = Set.new()
        sections.each do |section|
            parsed_sections[section['key']] = {}
            section.each_pair do |k, v|
                parsed_sections[section['key']][k.to_sym] = v
            end
            parsed_sections[section['key']][:entries] = []
            (section['entries'] || []).each do |path|
                dev_only = path[0] == '.'
                path = path.sub(/^\./, '')
                content_root = case section['catalog']
                    when 'teaching' then '/src/content/lehrmaterial'
                    when 'reference' then REFERENCE_ROOT
                    else '/src/content'
                end
                slug = section['catalog'] == 'reference' ? "reference/#{path}" : path
                raise "duplicate content slug: #{slug}" unless seen_paths.add?(slug)
                # <name>.md is the page; a teacher note (lehrkraft.md) never is.
                original_path = ContentParts.page_file("#{content_root}/#{path}", path)
                paths << {:section => section['key'], :path => path, :slug => slug, :original_path => original_path, :dev_only => dev_only}
            end
        end
        Dir['/src/content/**/+*.md'].each do |path|
            original_path = path
            path = File.basename(path)
            next if path.include?('/')
            next unless path.include?('+')
            path = path.sub('+', '')
            unless seen_paths.include?(path)
                STDERR.puts "Got custom path: #{path} (#{original_path})"
                paths << {:section => 'misc', :path => path, :dev_only => false, :original_path => original_path, :extra => true}
                seen_paths << path
            end
        end

        parsed_kenney = if partial && defined?(@@kenney) && @@kenney
            @@kenney
        else
            {}
        end
        unless partial
            Dir['/src/content/anaglyph/kenney/*/*.webp'].sort.each do |path|
                kit = path.split('/').last(2).first
                model = path.split('/').last(2).last.sub('.webp', '')
                parsed_kenney[kit] ||= []
                parsed_kenney[kit] << model
            end

            parsed_kenney.keys.each do |kit|
                paths << {:section => 'misc', :path => kit, :original_path => "/src/content/anaglyph/#{kit}.md", :dev_only => false, :kenney => true, :extra => true}
            end
        end

        if partial
            paths.select! do |entry|
                entry_path = entry[:original_path]
                next false unless entry_path
                (entry[:slug] || File.basename(entry_path, '.md')
                    .sub(/^[0-9]+\-/, '')
                    .sub('+', '')) == only_slug
            end
            return false if paths.empty?
        end

        redcarpet = Redcarpet::Markdown.new(Redcarpet::Render::HTML, {:fenced_code_blocks => true})
        @@parse_content_count ||= 0
        @@parse_content_count += 1
        parse_content_count = @@parse_content_count

        # Kenney previews are already WebP files, so don't send every image through
        # the generic image-cache pipeline below. That used to spawn one `cp`
        # process per preview image. Copy the complete tree once, with one process,
        # and let generated gallery HTML point at /cache/kenney/... directly.
        if parse_content_count == 1 && !parsed_kenney.empty?
            FileUtils.mkdir_p('/webcache/kenney')
            unless system('cp', '-ru', '/src/content/anaglyph/kenney/.', '/webcache/kenney/')
                raise 'Failed to prepare Kenney preview cache'
            end
        end

        paths.each do |entry|
            entry_started_at = Process.clock_gettime(Process::CLOCK_MONOTONIC)
            section = entry[:section]
            path = entry[:original_path]
            # Kenney kit pages are built once; a later full parse (in
            # development, after a content change) keeps them instead of
            # dropping them, which made every kit page a 404 until a restart.
            if parse_content_count > 1 && entry[:kenney]
                previous = defined?(@@content) && @@content && @@content[entry[:path]]
                parsed_content[entry[:path]] = previous if previous
                next
            end
            markdown = nil
            page_meta = {}
            page_references = []
            page_tasks = nil
            task_slots = []
            unless entry[:kenney]
                next unless path
                markdown = File.read(path)
                markdown.gsub!(/_include_file\(([^)]+)\)/) do |match|
                    options = $1.split(',').map { |x| x.strip }
                    StringIO.open do |io|
                        io.puts "```#{options[1]}_lineno"
                        io.puts File.read(File.join(File.dirname(path), options[0]))
                        io.puts "```"
                        io.string
                    end
                end
                markdown.gsub!(/_include_svg\(([^)]+)\)/) do |match|
                    options = $1.split(',').map { |x| x.strip }
                    StringIO.open do |io|
                        svg = File.read(File.join(File.dirname(path), options[0]))
                        dom = Nokogiri::XML(svg)
                        dom.css('svg').first['class'] = (dom.css('svg').first['class'].to_s.split(' ') + ['full']).join(' ')
                        dom.css('svg').each do |s|
                            s.remove_attribute('width')
                            s.remove_attribute('height')
                        end
                        io.puts dom.css('svg').first.to_s
                        io.string
                    end
                end
                # Reference building blocks (docs/aufgaben.md): _include(slug#part)
                # inserts a part of a reference page, _uses(a, b) links to parts,
                # and part markers become anchors on the reference page itself.
                page_meta = ContentParts.meta(markdown)
                page_references = PageKinds.references_of(page_meta, markdown, ContentParts::INCLUDE)
                content_warning = lambda { |message| STDERR.puts "#{path}: #{message}" }
                page_included_parts = ContentParts.included_parts(markdown)
                markdown = ContentParts.expand_includes(markdown,
                    :page_dir => File.dirname(path), :reference_root => REFERENCE_ROOT,
                    :variables => ContentParts.page_variables(page_meta), :warn => content_warning)
                markdown = ContentParts.expand_uses(markdown,
                    :references => page_meta['reference'], :reference_root => REFERENCE_ROOT,
                    :warn => content_warning)
                markdown = ContentParts.fill_variables(markdown, ContentParts.page_variables(page_meta))
                markdown = ContentParts.mark_parts(markdown)
                # Tasks (docs/aufgaben.md): tasks.yaml next to the page holds
                # them, _task(id) on a line of its own places a card.
                tasks_path = File.join(File.dirname(path), 'tasks.yaml')
                if File.file?(tasks_path)
                    begin
                        task_data = TaskSpec.load_file(tasks_path)
                        page_tasks = {
                            :activity => task_data['activity'],
                            :by_id => task_data['tasks'].to_h { |t| [t['id'], t] },
                        }
                    rescue TaskSpec::Error, Psych::Exception => e
                        STDERR.puts "#{tasks_path}: tasks not loaded: #{e.message}"
                    end
                end
                markdown, task_slots = TaskCard.mark_slots(markdown)
            else
                kit = entry[:path]
                markdown = StringIO.open do |io|
                    title = kit.split('-').map { |x| x.capitalize}. join(' ')
                    io.puts "# #{title}"
                    io.puts
                    io.puts <<~EOS
                        Um das #{title} verwenden zu können, musst du es erst installieren. Öffne dazu ein Terminal, indem du <kbd>Strg</kbd><kbd>J</kbd> drückst. Führe dann den folgenden Befehl aus:

                        ```bash
                        ./download.rb #{kit}
                        ```

                        <div class='hint'>
                            Hinweis: Achte auf die genaue Schreibweise des Befehls.
                        </div>

                        Du kannst dann mit dem Befehl `model` ein Modell zu deiner Szene hinzufügen, also z. B.:

                        ```ini
                        model = #{kit}/#{parsed_kenney[kit].first}
                        ```

                        <div class='hint'>
                            Achtung: Vergiss nicht, den Namen des Kits (<code>#{kit}/</code>) vor dem Modellnamen anzugeben!
                        </div>

                        Die folgenden Modelle stehen zur Auswahl:

                        __KENNEY_GALLERY__
                    EOS
                    io.string
                end
            end
            slug = entry[:slug] || File.basename(path, '.md').sub(/^[0-9]+\-/, '').sub('+', '')
            parsed_content_paths[slug] = path unless entry[:kenney]
            root = render_content_markdown(markdown, path, redcarpet) do |md|
                if entry[:kenney]
                    gallery = StringIO.open do |io|
                        io.puts "<div class='kenney-gallery'>"
                        parsed_kenney[entry[:path]].each do |model|
                            io.puts "<div><img alt='' src='/cache/kenney/#{entry[:path]}/#{model}.webp'><div>#{model}</div></div>"
                        end
                        io.puts "</div>"
                        io.string
                    end
                    md.sub!('__KENNEY_GALLERY__', gallery)
                end
                md
            end
            insert_task_cards(root, task_slots, page_tasks, redcarpet, path) unless task_slots.empty?
            # Reference pages: every h2 and h3 gets an anchor for the Handbuch index.
            outline = parsed_sections.dig(section, :catalog) == 'reference' ? ReferenceIndex.anchor_headings(root) : nil
            search_sections = outline ? ReferenceIndex.search_sections(root) : nil
            inject_autotoc(root)

            # Scripts a page loads itself (the colour mixer) get a fresh
            # address with each version, like default.css: nginx lets
            # browsers keep /include/*.js forever.
            html = root.to_html.gsub(%r{(<script src=['"]/include/[a-z_]+\.js)(['"])}) do
                "#{$1}?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}#{$2}"
            end
            # root.css('h2, h3, h4, h5, h6').each do |e|
            #     if slug == 'c'
            #         level = e.name.sub('h', '').to_i
            #         STDERR.puts "#{'  ' * (level - 2)}- #{e.text}"
            #     end
            # end
            meta = root.css('.meta').first
            entry_elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - entry_started_at
            STDERR.puts "Added content: #{slug} (#{format('%.3f', entry_elapsed)}s)"
            parsed_content[slug] = {
                :html => html,
                :dev_only => entry[:dev_only],
                :teacher_only => parsed_sections.dig(section, :teacher_only) == true,
                # What kind of page this is (PageKinds.header shows it).
                :section => section,
                :kind => page_meta['kind'],
                :language => page_meta['language'],
                :duration => page_meta['duration'],
                :references => page_references,
                # Reference parts the page includes ('slug#part'), so a box
                # can shrink once someone has been through it elsewhere.
                :included_parts => page_included_parts,
                # Template to clone (template:, template_name:), for the
                # »new version« hint; nil if the page has none.
                :template => TemplateFreshness.from_meta(page_meta),
                :outline => outline,
                :search_sections => search_sections,
            }
            if page_tasks
                parsed_content[slug][:tasks] = page_tasks.merge(:on_page => task_slots & page_tasks[:by_id].keys)
            end
            # Teacher note next to the page (docs/aufgaben.md): rendered like the
            # page, shown only to teachers by teacher_note_panel.
            note_path = File.join(File.dirname(path), ContentParts::NOTE_FILE)
            if !entry[:kenney] && File.file?(note_path)
                note_root = render_content_markdown(File.read(note_path), note_path, redcarpet)
                parsed_content[slug][:teacher_note] = (note_root.at_css('body') || note_root).inner_html
            end
            unless entry[:extra]
                parsed_sections[section][:entries] << slug
                if meta
                    meta = YAML.load(meta)
                    if meta['image']
                        parts = meta['image'].split(':')
                        image_path = File.join(File.dirname(path), parts[0])
                        if File.file?(image_path)
                            sha1 = convert_image(image_path)
                            parsed_content[slug][:image] = "/cache/#{sha1}.webp"

                            # The dark file as the screenshot tool names it:
                            # box-anaglyph.webp → box-dark-anaglyph.webp.
                            extension = File.extname(image_path)
                            base = image_path.delete_suffix(extension)
                            dark_image_path = base.sub(/(-anaglyph)?\z/) { "-dark#{$1}" } + extension
                            if File.exist?(dark_image_path)
                                dark_sha1 = convert_image(dark_image_path)
                                parsed_content[slug][:image_dark] = "/cache/#{dark_sha1}.webp"
                            end
                        else
                            # A generated screenshot may not exist on first load.
                            parsed_content[slug][:image] = '/images/keyboard.png'
                        end

                        parsed_content[slug][:image_x] = (parts[1] || '50').to_i
                        parsed_content[slug][:image_y] = (parts[2] || '50').to_i
                        parsed_content[slug][:needs_contrast] = meta['needs_contrast']
                    end
                end
                begin
                    parsed_content[slug][:title] = root.css('h1').first.to_s.sub('<h1>', '').sub('</h1>', '').strip
                rescue
                end
                begin
                    parsed_content[slug][:abstract] = root.css('.abstract').first.text
                rescue
                end
                begin
                    parsed_content[slug][:image] ||= root.css('img').first.attr('src')
                    parsed_content[slug][:image_x] ||= 50
                    parsed_content[slug][:image_y] ||= 50
                rescue
                end
            end
        end
        if partial
            parsed_content.each_pair do |slug, content|
                @@content[slug] = content
            end
            parsed_content_paths.each_pair do |slug, path|
                @@content_paths[slug] = path
            end
        else
            # Publish only complete parse results. Concurrent requests continue
            # to read the previous complete snapshot while this method is working.
            @@section_order = section_order
            @@teaching_section_order = teaching_section_order
            @@reference_section_order = reference_section_order
            @@sections = parsed_sections
            @@kenney = parsed_kenney
            @@content = parsed_content
            @@content_paths = parsed_content_paths
            @@content_catalog_signature = content_catalog_signature
        end

        parse_elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - parse_started_at
        label = partial ? "content #{only_slug}" : 'content'
        STDERR.puts "Finished parsing #{label} in #{format('%.3f', parse_elapsed)}s"
        true
    end

    private_class_method :parse_content_locked

    def codebite_broadcast(email, payload)
        msg = payload.is_a?(String) ? payload : payload.to_json
        conns = @@codebite_mutex.synchronize do
            (@@codebite_ws_clients[email] || {}).values.dup
        end
        conns.each do |ws|
            begin
                ws.send(msg)
            rescue
                # ignore broken sockets; cleanup happens on close
            end
        end
    end

    def kill_codebite_run(email, reason: "replaced")
        run = nil
        @@codebite_mutex.synchronize do
            run = @@codebite_runs[email]
            @@codebite_runs.delete(email)
        end
        return unless run

        pid = run[:pid]
        codebite_broadcast(email, { action: "codebite_status", status: "killing", reason: reason })

        begin
            # Try graceful terminate
            Process.kill("TERM", pid) rescue nil

            # Wait briefly, then hard kill
            t0 = Time.now
            while Time.now - t0 < 1.0
                break unless run[:wait_thr]&.alive?
                sleep 0.05
            end
            if run[:wait_thr]&.alive?
                Process.kill("KILL", pid) rescue nil
            end
        ensure
            # Stop reader threads
            (run[:threads] || []).each do |thr|
                thr.kill rescue nil
            end
            run[:wait_thr]&.join(0.2) rescue nil
        end

        codebite_broadcast(email, {
            action: "codebite_exit",
            exit_code: 1,
            killed: true
        })
    end

    def codebite_run_lock(email)
        @@codebite_mutex.synchronize do
            @@codebite_run_locks[email] ||= Mutex.new
        end
    end

    @@load_invitations_mutex = Mutex.new
    @@admin_ws_mutex = Mutex.new
    @@login_request_mutex = Mutex.new
    @@qr_login_mutex = Mutex.new
    @@mysql_database_locks_mutex = Mutex.new
    @@mysql_database_locks = {}

    def mysql_database_lock(email)
        @@mysql_database_locks_mutex.synchronize do
            @@mysql_database_locks[email] ||= Mutex.new
        end
    end

    def self.load_invitations
        @@load_invitations_mutex.synchronize do
            load_invitations_locked
        end
    end

    def self.load_invitations_locked
        invitations = {}
        user_groups = {}
        user_group_order = []
        explicit_teacher_emails = Set.new
        # Every group an address is listed in, in file order. invitations[email][:group]
        # stays the first one, as before; the class board uses all of them, so a
        # student can be in 10b and in the WP course.
        group_members = {}

        current_group = 'Administrator'

        ADMIN_USERS.each do |email|
            user_group_order << current_group unless user_group_order.include?(current_group)
            invitations[email] = { :group => current_group, :name => email }
            user_groups[current_group] ||= []
            user_groups[current_group] << email
        end

        current_group = '(keine Gruppe)'
        group_admins = {}
        Dir['/invitations/*.txt'].sort.each do |path|
            next if File.basename(path) == '_template.txt'
            File.open(path) do |f|
                f.each_line do |line|
                    next if line.strip.empty?
                    next if line.strip[0] == '#'
                    if line[0] == '>'
                        current_group = line[1, line.size - 1].strip
                        group_admins[current_group] ||= []
                        user_group_order << current_group unless user_group_order.include?(current_group)
                    elsif line[0] == '+'
                        # + grants administration of the current group, as before.
                        email = TeacherRoles.normalize_email(line[1..])
                        (group_admins[current_group] ||= []) << email if email
                    elsif line[0] == '*'
                        # * designates a teacher without granting group privileges.
                        email = TeacherRoles.normalize_email(line[1..])
                        explicit_teacher_emails << email if email
                    else
                        parts = line.strip.split(' ')
                        email = parts.last.delete_prefix('<').delete_suffix('>').downcase
                        members = (group_members[current_group] ||= [])
                        members << email unless members.include?(email)
                        unless invitations[email]
                            user_groups[current_group] ||= []
                            user_groups[current_group] << email
                            invitations[email] = { :group => current_group }
                        end
                        # In several groups, the name may stand in just one of
                        # the lines; a line with only the address keeps it.
                        if parts.size > 1
                            name = parts[0, parts.size - 1].join(' ')
                            invitations[email][:name] = name
                        else
                            invitations[email][:name] ||= email
                        end
                    end
                end
            end
        end
        teachers = {}
        group_admins.each_pair do |group, emails|
            emails.each do |email|
                teachers[email] ||= Set.new()
                teachers[email] << group
            end
        end
        # Every group administrator has a test account, alone in a test group
        # of its own that only this teacher administers: to try hand-ins,
        # galleries and exams as a student would, away from the real classes
        # (LookIn). The test groups come last.
        test_accounts = {}
        teachers.keys.each do |teacher|
            test_email = LookIn.test_account_email(teacher)
            next if test_email.nil? || invitations[test_email]
            teacher_name = (invitations[teacher] || {})[:name] || teacher.split('@').first
            test_group = "Testgruppe (#{teacher_name})"
            test_accounts[test_email] = teacher
            group_members[test_group] = [test_email]
            user_groups[test_group] = [test_email]
            user_group_order << test_group unless user_group_order.include?(test_group)
            invitations[test_email] = { :group => test_group, :name => "Testkonto (#{teacher_name})" }
            teachers[teacher] << test_group
        end
        teacher_emails = TeacherRoles.resolve(
            :invited_emails => invitations.keys,
            :administrators => ADMIN_USERS,
            :group_administrators => teachers.keys,
            :designated_teachers => explicit_teacher_emails,
        )

        # Keep readers on the previous complete snapshot until parsing finishes.
        @@invitations = invitations
        @@user_groups = user_groups
        @@user_group_order = user_group_order
        @@teachers = teachers # group administration rights only
        @@teacher_emails = teacher_emails # teachers, with or without group rights
        @@group_members = group_members # all memberships, for the class board
        @@test_accounts = test_accounts # test account email => its teacher
    end

    private_class_method :load_invitations_locked

    def self.prepare_downloads()
        unless File.exist?("/dl/working-with-files.tar.gz")
            STDERR.puts "Preparing /dl/working-with-files.tar.bz2..."
            FileUtils.rm_rf('/dl/working-with-files')
            FileUtils.mkpath('/dl/working-with-files')
            system("wget -O /dl/working-with-files/alice.txt https://www.gutenberg.org/cache/epub/11/pg11.txt")
            system("wget -O /dl/working-with-files/stallman.jpg https://upload.wikimedia.org/wikipedia/commons/c/c2/Richard_Stallman_at_Marlboro_College.jpg")
            system("wget -O /dl/working-with-files/jay.webm https://upload.wikimedia.org/wikipedia/commons/transcoded/7/75/Jay_Feeding.webm/Jay_Feeding.webm.360p.vp9.webm")
            system("wget -O /dl/working-with-files/zork.zip https://github.com/devshane/zork/archive/refs/heads/master.zip")
            system("wget -O /dl/working-with-files/music-releases.tar.bz2 https://github.com/specht/workspace-files/raw/main/music-releases.tar.bz2")
            system("echo 'Nothing to see here, just a hidden file!' > /dl/working-with-files/.hidden")
            system("tar cvzf /dl/working-with-files.tar.gz -C /dl working-with-files")
            FileUtils.rm_rf('/dl/working-with-files')
        end
    end

    configure do
        CONSTRAINTS_LIST = [
            'Code/sha1',
            'Database/name',
            'File/sha1',
            'Language/name',
            'LiveAppShare/tag',
            'LoginRequest/tag',
            'QrLoginRequest/tag',
            'Session/sid',
            'Solved/id',
            'Submission/id',
            'Task/name',
            'TaskState/key',
            'Pin/key',
            'Test/tag',
            'User/email',
            'User/db_login',
            'User/server_tag',
            'User/share_tag',
        ]
        INDEX_LIST = [
            'Submission/success',
            'Test/running',
            'TaskAttempt/task',
        ]

        setup = SetupDatabase.new()

        setup.wait_for_neo4j()
        @@clients = {}
        @@email_for_client_id = {}
        @@client_ids_for_email = {}
        @@threads_for_client_id = {}
        $neo4j.setup_constraints_and_indexes(CONSTRAINTS_LIST, INDEX_LIST)
        self.reconcile_live_apps!(:refresh_nginx => false)
        self.refresh_nginx_config()

        @@content = {}
        # Current commit of each template (docs/aufgaben.md, »Neue Version einer Vorlage«).
        @@template_latest = TemplateFreshness::LatestCache.new
        self.parse_content()
        self.load_invitations()
        self.prepare_downloads()

        @@brand_header = ''
        begin
            @@brand_header = File.read('/brand/header.html')
        rescue
        end

        @@codebite_ws_clients ||= {}     # email => { client_id => ws }
        @@codebite_runs ||= {}           # email => { pid:, wait_thr:, threads:, started_at:, task:, language: }
        @@codebite_mutex ||= Mutex.new   # protects @@codebite_runs + @@codebite_ws_clients
        @@codebite_run_locks ||= {}      # serializes start/stop lifecycle per user

        Thread.new do
            loop do
                system("ruby housekeeping.rb")
                sleep 3600
            end
        end

        Thread.new do
            loop do
                sleep 10
                Main.reconcile_live_apps!
            end
        end
        # --- RuboCop server + config (layout-only formatting) ---
        begin
            unless File.exist?(RUBOCOP_LAYOUT_CONFIG_PATH)
                File.write(
                    RUBOCOP_LAYOUT_CONFIG_PATH,
                    <<~YAML
                      AllCops:
                        NewCops: disable

                      Layout/IndentationWidth:
                        Width: 4
                    YAML
                )
            end

            # Start rubocop server once per Sinatra process
            # (safe to call multiple times; it will no-op if already started)
            system("rubocop --start-server >/dev/null 2>&1")
        rescue => e
            STDERR.puts "RuboCop server start failed: #{e.class}: #{e.message}"
        end

        @@db_init_queue ||= Queue.new
        @@db_init_mutex ||= Mutex.new
        @@db_init_queued_keys ||= Set.new

        Thread.new do
            loop do
                job = @@db_init_queue.pop
                key = job[:key]

                begin
                    STDERR.puts ">>> DB init worker: starting #{key}"
                    Main.init_mysql(job[:email], job[:db_login])
                    Main.init_neo4j(job[:email], job[:db_login])
                    STDERR.puts ">>> DB init worker: finished #{key}"
                rescue => e
                    STDERR.puts ">>> DB init worker failed for #{key}: #{e.class}: #{e.message}"
                    STDERR.puts e.backtrace.join("\n")
                ensure
                    @@db_init_mutex.synchronize do
                        @@db_init_queued_keys.delete(key)
                    end
                end
            end
        end

        self.ensure_server_start_queue!
        Thread.new do
            Main.server_start_worker_loop
        end
    end

    before '*' do
        # abgaben.<host> serves hand-ins and nothing else: no session, no
        # pages, no API (SubmissionView).
        if request.host == SUBMISSION_VIEW_HOST && !(request.path.start_with?('/v/') && %w[GET HEAD].include?(request.request_method))
            halt 404
        end
        origin_sensitive = %w(POST PUT PATCH DELETE).include?(request.request_method) ||
            ['/ws', '/ws_codebites', '/logout'].include?(request.path)
        if origin_sensitive && !Authentication.same_origin?(
            :origin => request.env['HTTP_ORIGIN'],
            :referer => request.env['HTTP_REFERER'],
            :expected_origin => WEB_ROOT,
        )
            content_type :json
            halt 403, { :error => 'invalid_origin' }.to_json
        end

        @session_user = nil
        sid = current_session_sid
        if sid && sid.match?(/\A[0-9A-Za-z]+\z/)
            results = neo4j_query(<<~END_OF_QUERY, :sid => sid).to_a
                MATCH (s:Session {sid: $sid})-[:FOR]->(u:User)
                RETURN s, u;
            END_OF_QUERY

            if results.size == 1
                session = results.first['s']
                session_expiry = begin
                    DateTime.parse(session[:expires].to_s)
                rescue ArgumentError, TypeError
                    nil
                end

                if session_expiry && session_expiry > DateTime.now
                    email = results.first['u'][:email]
                    @session_user = {
                        :email => email.downcase,
                        :server_tag => results.first['u'][:server_tag],
                        :share_tag => results.first['u'][:share_tag],
                    }
                    results.first['u'].each_pair do |k, v|
                        next if @session_user.include?(k.to_sym)
                        @session_user[k.to_sym] = v
                    end
                    @session_user[:show_workspace] = true unless @session_user.include?(:show_workspace)
                    # Set while a teacher looks at the site as this student.
                    @impersonated_by = session[:impersonated_by]

                    expires = session_cookie_expiry
                    if DEVELOPMENT || request.cookies.include?('hs_sid')
                        set_session_cookie(sid, expires)
                    end

                    server_sid = results.first['u'][:server_sid]
                    if server_sid && request.cookies['hs_server_sid'] != server_sid
                        set_server_sid_cookie(server_sid, expires)
                    end
                else
                    neo4j_query(<<~END_OF_QUERY, :sid => sid)
                        MATCH (s:Session {sid: $sid})
                        DETACH DELETE s;
                    END_OF_QUERY
                end
            end
        end

        if sid && @session_user.nil?
            clear_session_cookies
        elsif sid.nil?
            clear_domain_cookie('hs_server_sid') if request.cookies.include?('hs_server_sid')
            clear_domain_cookie('hs_watch_tag') if request.cookies.include?('hs_watch_tag')
        end

        if @session_user && @session_user[:db_login].nil?
            @session_user[:db_login] = Main.db_login_for_email(
                @session_user[:email]
            )
        end
    end

    def self.enqueue_database_init(email:, db_login:)
        DatabaseIdentity.validate!(db_login)
        key = "#{email}:#{db_login}"

        STDERR.puts ">>> Waiting for DB init mutex to enqueue #{key}..."
        @@db_init_mutex.synchronize do
            return false if @@db_init_queued_keys.include?(key)

            @@db_init_queued_keys << key
            @@db_init_queue << {
                key: key,
                email: email,
                db_login: db_login,
            }

            STDERR.puts ">>> DB init queued: #{key}"
            true
        end
    end

    def this_is_a_page_for_logged_in_users!
        if @session_user.nil?
            redirect "#{WEB_ROOT}/login", 302
        end
    end

    def this_is_a_page_for_logged_in_admins!
        assert(admins_logged_in?)
    end

    def user_logged_in?
        return (!@session_user.nil?)
    end

    # Teaching resources are available to designated teachers without any
    # access to group administration, students' accounts, or test archives.
    def teacher_account_logged_in?
        user_logged_in? && @@teacher_emails.include?(@session_user[:email])
    end

    # Existing administrative permissions stay tied to + group assignments.
    def teacher_logged_in?
        admin_logged_in? || (user_logged_in? && @@teachers.include?(@session_user[:email]))
    end

    def admin_logged_in?
        user_logged_in? && ADMIN_USERS.include?(@session_user[:email])
    end

    def gen_server_tag()
        RandomTag::generate(12)
    end

    def gen_server_sid()
        RandomTag::generate(48)
    end

    def gen_share_tag()
        RandomTag::generate(48)
    end

    def session_cookie_name
        Authentication.session_cookie_name(:development => DEVELOPMENT)
    end

    def current_session_sid
        raw = request.cookies[session_cookie_name]
        raw ||= request.cookies['hs_sid'] unless DEVELOPMENT
        return nil unless raw.is_a?(String)

        raw.split(',').first
    end

    def session_cookie_expiry
        Time.now + 3600 * 24 * Authentication::SESSION_LIFETIME_DAYS
    end

    def set_session_cookie(sid, expires = session_cookie_expiry)
        response.set_cookie(
            session_cookie_name,
            Authentication.session_cookie_options(
                :value => sid,
                :expires => expires,
                :development => DEVELOPMENT,
            ),
        )
        clear_legacy_session_cookie
    end

    def set_server_sid_cookie(server_sid, expires = session_cookie_expiry)
        response.set_cookie(
            'hs_server_sid',
            Authentication.domain_cookie_options(
                :value => server_sid,
                :expires => expires,
                :domain => WEBSITE_HOST.split(':').first,
                :development => DEVELOPMENT,
            ),
        )
    end

    def clear_legacy_session_cookie
        response.set_cookie(
            'hs_sid',
            Authentication.domain_cookie_options(
                :value => '',
                :expires => Time.at(0),
                :domain => WEBSITE_HOST.split(':').first,
                :development => DEVELOPMENT,
            ),
        )
    end

    def clear_domain_cookie(name)
        response.set_cookie(
            name,
            Authentication.domain_cookie_options(
                :value => '',
                :expires => Time.at(0),
                :domain => WEBSITE_HOST.split(':').first,
                :development => DEVELOPMENT,
            ),
        )
    end

    def clear_session_cookies
        response.set_cookie(
            session_cookie_name,
            Authentication.session_cookie_options(
                :value => '',
                :expires => Time.at(0),
                :development => DEVELOPMENT,
            ),
        )
        clear_legacy_session_cookie
        clear_domain_cookie('hs_server_sid')
        clear_domain_cookie('hs_watch_tag')
    end

    def cleanup_login_requests
        cleanup_params = {
            :now => Time.now.to_i,
            :max_attempts => Authentication::LOGIN_REQUEST_MAX_ATTEMPTS,
        }
        neo4j_query(<<~END_OF_QUERY, cleanup_params)
            MATCH (l:LoginRequest)
            WHERE COALESCE(l.expires_at, 0) <= $now
               OR COALESCE(l.attempts, 0) >= $max_attempts
            DETACH DELETE l;
        END_OF_QUERY
    end

    def cleanup_qr_login_requests
        neo4j_query(<<~END_OF_QUERY, :now => Time.now.to_i)
            MATCH (q:QrLoginRequest)
            WHERE COALESCE(q.expires_at, 0) <= $now
            DETACH DELETE q;
        END_OF_QUERY
    end

    post '/api/qr_login/start' do
        headers 'Cache-Control' => 'no-store'
        tag = RandomTag::generate(16)
        browser_secret = RandomTag::generate(32)
        approval_secret = RandomTag::generate(32)
        now = Time.now.to_i
        expires_at = Authentication.qr_login_request_expires_at(:now => now)
        approval_url = "#{WEB_ROOT}/qr-login##{tag}.#{approval_secret}"

        begin
            svg, qr_error, qr_status = Open3.capture3(
                'qrencode', '-t', 'SVG', '-m', '2', '-o', '-',
                :stdin_data => approval_url,
            )
        rescue Errno::ENOENT => e
            STDERR.puts ">>> QR login unavailable: #{e.message}"
            content_type :json
            halt 503, { :error => 'qr_login_unavailable' }.to_json
        end
        unless qr_status.success?
            STDERR.puts ">>> qrencode failed: #{qr_error}"
            content_type :json
            halt 503, { :error => 'qr_login_unavailable' }.to_json
        end

        create_params = {
            :tag => tag,
            :browser_secret_sha256 => Digest::SHA256.hexdigest(browser_secret),
            :approval_secret_sha256 => Digest::SHA256.hexdigest(approval_secret),
            :created_at => now,
            :expires_at => expires_at,
        }
        @@qr_login_mutex.synchronize do
            cleanup_qr_login_requests
            neo4j_query_expect_one(<<~END_OF_QUERY, create_params)
                CREATE (q:QrLoginRequest {
                    tag: $tag,
                    browser_secret_sha256: $browser_secret_sha256,
                    approval_secret_sha256: $approval_secret_sha256,
                    created_at: $created_at,
                    expires_at: $expires_at
                })
                RETURN q.tag;
            END_OF_QUERY
        end

        payload = {
            :status => 'pending',
            :tag => tag,
            :browser_secret => browser_secret,
            :qr_image => "data:image/svg+xml;base64,#{Base64.strict_encode64(svg)}",
            :expires_at => expires_at,
        }
        # The raw approval URL is useful to the browser tests, but production
        # clients only receive the rendered QR code.
        payload[:approval_url] = approval_url if DEVELOPMENT
        respond(payload)
    end

    post '/api/qr_login/approve' do
        headers 'Cache-Control' => 'no-store'
        unless user_logged_in?
            content_type :json
            halt 401, { :error => 'qr_login_requires_login' }.to_json
        end

        data = parse_request_data(:required_keys => [:tag, :approval_secret])
        tag = data[:tag].to_s.downcase.strip
        approval_secret = data[:approval_secret].to_s.downcase.strip
        unless Authentication.valid_qr_login_tag?(tag) &&
               Authentication.valid_qr_login_secret?(approval_secret)
            content_type :json
            halt 400, { :error => 'invalid_qr_login_request' }.to_json
        end

        approval_params = {
            :tag => tag,
            :approval_secret_sha256 => Digest::SHA256.hexdigest(approval_secret),
            :email => @session_user[:email],
            :now => Time.now.to_i,
        }
        approval_status = @@qr_login_mutex.synchronize do
            cleanup_qr_login_requests
            row = neo4j_query(<<~END_OF_QUERY, approval_params).to_a.first
                MATCH (q:QrLoginRequest {
                    tag: $tag,
                    approval_secret_sha256: $approval_secret_sha256
                })
                WHERE q.expires_at > $now
                OPTIONAL MATCH (q)-[:APPROVED_BY]->(existing:User)
                RETURN existing.email AS approved_email;
            END_OF_QUERY
            next :expired unless row

            approved_email = row['approved_email']
            next :claimed if approved_email && approved_email != @session_user[:email]

            neo4j_query_expect_one(<<~END_OF_QUERY, approval_params)
                MATCH (q:QrLoginRequest {
                    tag: $tag,
                    approval_secret_sha256: $approval_secret_sha256
                })
                WHERE q.expires_at > $now
                MATCH (u:User {email: $email})
                MERGE (q)-[:APPROVED_BY]->(u)
                SET q.approved_at = $now
                RETURN q.tag;
            END_OF_QUERY
            :approved
        end

        case approval_status
        when :approved
            respond(:status => 'approved')
        when :claimed
            content_type :json
            halt 409, { :error => 'qr_login_already_approved' }.to_json
        else
            content_type :json
            halt 410, { :error => 'qr_login_expired' }.to_json
        end
    end

    post '/api/qr_login/status' do
        headers 'Cache-Control' => 'no-store'
        data = parse_request_data(:required_keys => [:tag, :browser_secret])
        tag = data[:tag].to_s.downcase.strip
        browser_secret = data[:browser_secret].to_s.downcase.strip
        unless Authentication.valid_qr_login_tag?(tag) &&
               Authentication.valid_qr_login_secret?(browser_secret)
            content_type :json
            halt 400, { :error => 'invalid_qr_login_request' }.to_json
        end

        lookup_params = {
            :tag => tag,
            :browser_secret_sha256 => Digest::SHA256.hexdigest(browser_secret),
            :now => Time.now.to_i,
        }
        qr_result = @@qr_login_mutex.synchronize do
            cleanup_qr_login_requests
            row = neo4j_query(<<~END_OF_QUERY, lookup_params).to_a.first
                MATCH (q:QrLoginRequest {
                    tag: $tag,
                    browser_secret_sha256: $browser_secret_sha256
                })
                WHERE q.expires_at > $now
                OPTIONAL MATCH (q)-[:APPROVED_BY]->(u:User)
                RETURN q.expires_at AS expires_at, u.email AS approved_email;
            END_OF_QUERY
            next { :status => :expired } unless row
            unless row['approved_email']
                next { :status => :pending, :expires_at => row['expires_at'].to_i }
            end

            sid = RandomTag::generate(24)
            session_expires = (DateTime.now + Authentication::SESSION_LIFETIME_DAYS).to_s
            complete_params = lookup_params.merge(
                :email => row['approved_email'],
                :sid => sid,
                :session_expires => session_expires,
            )
            login_result = neo4j_query(<<~END_OF_QUERY, complete_params).to_a.first
                MATCH (q:QrLoginRequest {
                    tag: $tag,
                    browser_secret_sha256: $browser_secret_sha256
                })-[:APPROVED_BY]->(u:User {email: $email})
                WHERE q.expires_at > $now
                DETACH DELETE q
                WITH u
                CREATE (s:Session {sid: $sid, expires: $session_expires})-[:FOR]->(u)
                RETURN s.sid AS sid, u.email AS email, u.server_sid AS server_sid;
            END_OF_QUERY
            next { :status => :expired } unless login_result

            { :status => :approved, :login_result => login_result }
        end

        case qr_result[:status]
        when :pending
            respond(:status => 'pending', :expires_at => qr_result[:expires_at])
        when :approved
            expires = session_cookie_expiry
            set_session_cookie(qr_result[:login_result]['sid'], expires)
            server_sid = qr_result[:login_result]['server_sid']
            set_server_sid_cookie(server_sid, expires) if server_sid
            respond(:status => 'approved')
        else
            respond(:status => 'expired')
        end
    end

    post '/api/qr_login/cancel' do
        headers 'Cache-Control' => 'no-store'
        data = parse_request_data(:required_keys => [:tag, :browser_secret])
        tag = data[:tag].to_s.downcase.strip
        browser_secret = data[:browser_secret].to_s.downcase.strip

        if Authentication.valid_qr_login_tag?(tag) &&
           Authentication.valid_qr_login_secret?(browser_secret)
            cancel_params = {
                :tag => tag,
                :browser_secret_sha256 => Digest::SHA256.hexdigest(browser_secret),
            }
            @@qr_login_mutex.synchronize do
                neo4j_query(<<~END_OF_QUERY, cancel_params)
                    MATCH (q:QrLoginRequest {
                        tag: $tag,
                        browser_secret_sha256: $browser_secret_sha256
                    })
                    DETACH DELETE q;
                END_OF_QUERY
            end
        end

        respond(:status => 'cancelled')
    end

    # The account of an invited person, created as at its first login: a
    # teacher may look into a student (or its test account) that has never
    # logged in.
    def ensure_user_account(email)
        # Create the user node before the login-request lock. Neo4j's uniqueness
        # constraint keeps this idempotent even if two browsers arrive together.
        user = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email)['n']
            MERGE (n:User {email: $email})
            RETURN n;
        END_OF_QUERY
        user[:db_login] = Main.db_login_for_email(email)
        unless user[:name]
            name = @@invitations[email][:name]
            user = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email, :name => name)['n']
                MATCH (n:User {email: $email})
                SET n.name = $name
                RETURN n;
            END_OF_QUERY
        end

        if user[:server_tag].nil? || user[:server_sid].nil?
            neo4j_query(<<~END_OF_QUERY, {:email => email, :server_tag => gen_server_tag(), :server_sid => gen_server_sid()})
                MATCH (u:User {email: $email})
                SET u.server_tag = COALESCE(u.server_tag, $server_tag)
                SET u.server_sid = COALESCE(u.server_sid, $server_sid)
            END_OF_QUERY
        end
        user
    end

    post '/api/request_login' do
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:email])
        email = data[:email].downcase.strip

        unless @@invitations.include?(email)
            # A test account (specht+test@…) does not make »specht« ambiguous.
            candidates = @@invitations.keys.select do |x|
                x[0, email.size] == email && !@@test_accounts.key?(x)
            end
            email = candidates.first if candidates.size == 1
        end

        unless @@invitations.include?(email)
            content_type :json
            halt 404, { :error => 'no_invitation_found' }.to_json
        end

        user = ensure_user_account(email)

        login_request = @@login_request_mutex.synchronize do
            cleanup_login_requests
            now = Time.now.to_i
            minimum_expiry = now + Authentication::LOGIN_REQUEST_RESEND_SECONDS
            existing_params = {
                :email => email,
                :minimum_expiry => minimum_expiry,
                :max_attempts => Authentication::LOGIN_REQUEST_MAX_ATTEMPTS,
            }
            existing = neo4j_query(<<~END_OF_QUERY, existing_params).to_a.first
                MATCH (r:LoginRequest)-[:FOR]->(u:User {email: $email})
                WHERE r.expires_at > $minimum_expiry
                  AND COALESCE(r.attempts, 0) < $max_attempts
                RETURN r
                ORDER BY r.created_at DESC
                LIMIT 1;
            END_OF_QUERY

            rate_state = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email)
                MATCH (u:User {email: $email})
                RETURN COALESCE(u.login_request_created_at, 0) AS last_created_at;
            END_OF_QUERY

            if existing
                request_node = existing['r']
                resend = now - request_node[:last_sent_at].to_i >= Authentication::LOGIN_REQUEST_RESEND_SECONDS
                if resend
                    neo4j_query(<<~END_OF_QUERY, {:tag => request_node[:tag], :now => now})
                        MATCH (r:LoginRequest {tag: $tag})
                        SET r.last_sent_at = $now;
                    END_OF_QUERY
                end
                {
                    :tag => request_node[:tag],
                    :code => request_node[:code],
                    :send_mail => resend,
                }
            else
                last_created_at = rate_state['last_created_at'].to_i
                if now - last_created_at < Authentication::LOGIN_REQUEST_RESEND_SECONDS
                    next { :rate_limited => true }
                end

                # Replace a nearly-expired request rather than handing the browser
                # a code that may stop working a few seconds later.
                neo4j_query(<<~END_OF_QUERY, {:email => email})
                    MATCH (r:LoginRequest)-[:FOR]->(u:User {email: $email})
                    DETACH DELETE r;
                END_OF_QUERY

                tag = RandomTag::generate(12)
                code = Authentication.generate_login_code(:development => DEVELOPMENT)
                expires_at = Authentication.login_request_expires_at(:now => now)
                create_request_params = {
                    :email => email,
                    :tag => tag,
                    :code => code,
                    :created_at => now,
                    :expires_at => expires_at,
                }
                neo4j_query_expect_one(<<~END_OF_QUERY, create_request_params)
                    MATCH (u:User {email: $email})
                    SET u.login_request_created_at = $created_at
                    CREATE (r:LoginRequest)-[:FOR]->(u)
                    SET r.tag = $tag,
                        r.code = $code,
                        r.created_at = $created_at,
                        r.last_sent_at = $created_at,
                        r.expires_at = $expires_at,
                        r.attempts = 0
                    RETURN u.email;
                END_OF_QUERY
                { :tag => tag, :code => code, :send_mail => true }
            end
        end

        if login_request[:rate_limited]
            content_type :json
            halt 429, { :error => 'login_request_rate_limited' }.to_json
        end

        broadcast_login_codes()

        if login_request[:send_mail] && !email.include?('@example.com')
            if DEVELOPMENT
                STDERR.puts 'Not sending login code email in development mode.'
            else
                STDERR.puts "Sending login code email to invited user #{email}: #{login_request[:code]}"
                deliver_mail do
                    to email
                    # bcc SMTP_FROM
                    from SMTP_FROM

                    subject "Dein Anmeldecode lautet #{login_request[:code]}"

                    StringIO.open do |io|
                        io.puts "<p>Hallo!</p>"
                        io.puts "<p>Dein Anmeldecode lautet:</p>"
                        io.puts "<p style='font-size: 200%;'>#{login_request[:code]}</p>"
                        io.puts "<p>Der Code ist für zehn Minuten gültig. Nachdem du dich angemeldet hast, bleibst du für ein ganzes Jahr angemeldet (falls du dich nicht wieder abmeldest).</p>"
                        io.puts "<p>Falls du diese E-Mail nicht angefordert hast, hat jemand versucht, sich mit deiner E-Mail-Adresse auf <a href='https://#{WEBSITE_HOST}/'>https://#{WEBSITE_HOST}/</a> anzumelden. In diesem Fall musst du nichts weiter tun (es sei denn, du befürchtest, dass jemand anderes Zugriff auf dein E-Mail-Konto hat – dann solltest du dein E-Mail-Passwort ändern).</p>"
                        io.puts "<p>Viele Grüße,<br />Michael Specht</p>"
                        io.string
                    end
                end
            end
        end
        respond(:ok => 'yay', :tag => login_request[:tag])
    end

    post '/api/complete_login' do
        data = parse_request_data(:required_keys => [:tag, :code])
        tag = data[:tag].downcase.strip
        code = data[:code].strip

        unless Authentication.valid_login_tag?(tag) && Authentication.valid_login_code?(code)
            content_type :json
            halt 401, { :error => 'invalid_login_code' }.to_json
        end

        login_result = @@login_request_mutex.synchronize do
            now = Time.now.to_i
            row = neo4j_query(<<~END_OF_QUERY, :tag => tag).to_a.first
                MATCH (r:LoginRequest {tag: $tag})-[:FOR]->(u:User)
                RETURN r, u;
            END_OF_QUERY

            next nil unless row

            request_node = row['r']
            unless Authentication.login_request_active?(
                :expires_at => request_node[:expires_at],
                :attempts => request_node[:attempts],
                :now => now,
            )
                neo4j_query(<<~END_OF_QUERY, :tag => tag)
                    MATCH (r:LoginRequest {tag: $tag})
                    DETACH DELETE r;
                END_OF_QUERY
                next nil
            end

            unless request_node[:code].to_s == code
                attempts = request_node[:attempts].to_i + 1
                if attempts >= Authentication::LOGIN_REQUEST_MAX_ATTEMPTS
                    neo4j_query(<<~END_OF_QUERY, :tag => tag)
                        MATCH (r:LoginRequest {tag: $tag})
                        DETACH DELETE r;
                    END_OF_QUERY
                else
                    neo4j_query(<<~END_OF_QUERY, {:tag => tag, :attempts => attempts})
                        MATCH (r:LoginRequest {tag: $tag})
                        SET r.attempts = $attempts;
                    END_OF_QUERY
                end
                next nil
            end

            sid = RandomTag::generate(24)
            session_expires = (DateTime.now + Authentication::SESSION_LIFETIME_DAYS).to_s
            complete_params = {
                :tag => tag,
                :code => code,
                :now => now,
                :max_attempts => Authentication::LOGIN_REQUEST_MAX_ATTEMPTS,
                :sid => sid,
                :session_expires => session_expires,
            }
            neo4j_query_expect_one(<<~END_OF_QUERY, complete_params)
                MATCH (r:LoginRequest {tag: $tag, code: $code})-[:FOR]->(u:User)
                WHERE r.expires_at > $now
                  AND COALESCE(r.attempts, 0) < $max_attempts
                DETACH DELETE r
                WITH u
                REMOVE u.login_request_created_at
                CREATE (s:Session {sid: $sid, expires: $session_expires})-[:FOR]->(u)
                RETURN s.sid AS sid, u.email AS email, u.server_sid AS server_sid;
            END_OF_QUERY
        end

        broadcast_login_codes()

        unless login_result
            content_type :json
            halt 401, { :error => 'invalid_login_code' }.to_json
        end

        expires = session_cookie_expiry
        set_session_cookie(login_result['sid'], expires)
        set_server_sid_cookie(login_result['server_sid'], expires) if login_result['server_sid']
        respond(:ok => 'yay')
    end

    # Who may look into whose account (LookIn): administrators into anyone's,
    # teachers into their own students'.
    def may_look_in?(email)
        Main.load_invitations()
        LookIn.allowed?(@session_user && @session_user[:email], email,
            :admins => ADMIN_USERS, :teachers => @@teachers, :group_members => @@group_members)
    end

    # The reason a teacher gave for looking in, kept for the student's
    # notice; nil (and a 400) when a teacher gave none.
    def record_look_in(email, kind, reason_text)
        # A test account is the teachers' own: no reason, no notice.
        return if @@test_accounts.key?(email)

        reason = LookIn.reason(reason_text, :admin => admin_logged_in?)
        halt 400, { :error => 'reason_missing' }.to_json if reason.nil?
        return if reason.empty?

        by = @session_user[:email]
        neo4j_query(<<~END_OF_QUERY, :email => email, :ts => Time.now.to_i, :kind => kind, :reason => reason,
            MATCH (u:User {email: $email})
            CREATE (l:LookIn {ts: $ts, kind: $kind, reason: $reason, by_email: $by_email, by_name: $by_name, seen: false})-[:FOR]->(u);
        END_OF_QUERY
            :by_email => by, :by_name => ((@@invitations[by] || {})[:name] || by))
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
        sid = current_session_sid
        neo4j_query(<<~END_OF_STRING, {:sid => sid, :email => email, :by => @session_user[:email]})
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
        neo4j_query(<<~END_OF_STRING, {:sid => current_session_sid, :email => @impersonated_by})
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

    # The bar at the top: as a student for a teacher, or the student's
    # notice that someone looked in.
    def look_in_bar_html
        return '' unless user_logged_in?

        if @impersonated_by
            name = (@@invitations[@session_user[:email]] || {})[:name] || @session_user[:email]
            return "<div class='look-in-bar look-in-bar-impersonating'><i class='bi bi-person-vcard'></i>" \
                "<span>Du siehst die Seite als <strong>#{CGI.escapeHTML(name)}</strong>.</span>" \
                "<button type='button' class='btn btn-sm btn-light bu-stop-impersonation'>Zurück zu deinem Konto</button></div>"
        end
        unseen = look_ins(:only_unseen => true)
        return '' if unseen.empty?

        lines = unseen.reverse.map { |l| "<p>#{LookIn.notice_html(l)}</p>" }.join
        "<div class='look-in-bar look-in-bar-notice'><i class='bi bi-info-circle'></i><div>#{lines}</div>" \
            "<button type='button' class='btn btn-sm btn-outline-secondary bu-look-ins-seen'>Verstanden</button></div>"
    end

    def get_server_state(tag)
        workspace_runtime.workspace_state(
            tag,
            :timeout => shell_timeout(:docker_inspect),
        )
    end

    def self.gen_password_for_email(email, salt)
        WorkspaceCredentials.password_for_email(email, salt)
    end

    def self.db_login_for_email(email)
        DatabaseIdentity::Neo4jAllocator.new($neo4j).allocate!(email)
    end

    def db_login_for_email(email)
        Main.db_login_for_email(email)
    end

    def self.init_mysql(email, db_login)
        DatabaseIdentity.validate!(db_login)
        mysql_password = Main.gen_password_for_email(email, MYSQL_PASSWORD_SALT)
        STDERR.puts "Setting up MySQL user #{db_login} with database #{db_login}"
        Open3.popen2("docker exec -i workspace_mysql_1 mysql --user=root --password=#{MYSQL_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
            DatabaseProvisioning.mysql_statements(db_login, mysql_password)
                .each { |statement| stdin.puts statement }
            stdin.close
            status = wait_thr.value
            raise "MySQL provisioning failed for #{db_login}" unless status.success?
        end
    end

    def init_mysql(email)
        Main.init_mysql(email, db_login_for_email(email))
    end

    def reset_mysql(email)
        db_login = db_login_for_email(email)
        STDERR.puts "Removing database for MySQL user #{db_login}"
        Open3.popen2("docker exec -i workspace_mysql_1 mysql --user=root --password=#{MYSQL_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
            stdin.puts "DROP DATABASE IF EXISTS `#{db_login}`;"
            stdin.close
            status = wait_thr.value
            raise "MySQL reset failed for #{db_login}" unless status.success?
        end
        Main.init_mysql(email, db_login)
    end

    def self.init_neo4j(email, db_login)
        DatabaseIdentity.validate!(db_login)
        neo4j_password = Main.gen_password_for_email(email, NEO4J_PASSWORD_SALT)
        STDERR.puts "Setting up Neo4j user #{db_login} with database #{db_login}"

        Open3.popen2("docker exec -i workspace_neo4j_1 bin/cypher-shell -u neo4j -p #{NEO4J_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
            DatabaseProvisioning.neo4j_statements(db_login, neo4j_password)
                .each { |statement| stdin.puts statement }
            stdin.close
            status = wait_thr.value
            raise "Neo4j provisioning failed for #{db_login}" unless status.success?
        end
    end

    def init_neo4j(email)
        Main.init_neo4j(email, db_login_for_email(email))
    end

    def reset_neo4j(email)
        db_login = db_login_for_email(email)
        role = DatabaseProvisioning.neo4j_role_name(db_login)
        Open3.popen2("docker exec -i workspace_neo4j_1 bin/cypher-shell -u neo4j -p #{NEO4J_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
            stdin.puts <<~END_OF_STRING
                DROP DATABASE `#{db_login}` IF EXISTS;
                DROP USER `#{db_login}` IF EXISTS;
                DROP ROLE `#{role}` IF EXISTS;
            END_OF_STRING
            stdin.close
            status = wait_thr.value
            raise "Neo4j reset failed for #{db_login}" unless status.success?
        end
        Main.init_neo4j(email, db_login)
    end

    def reset_test_demo_databases(email, demo_tag)
        persisted_db_login = db_login_for_email(email)
        db_login = DatabaseIdentity.ephemeral_login(persisted_db_login, demo_tag)
        DatabaseIdentity.validate!(db_login)
        database_email = "#{email}-#{demo_tag}"

        Open3.popen2("docker exec -i workspace_mysql_1 mysql --user=root --password=#{MYSQL_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
            stdin.puts "DROP DATABASE IF EXISTS `#{db_login}`;"
            stdin.close
            status = wait_thr.value
            raise "MySQL test demo reset failed for #{db_login}" unless status.success?
        end

        Open3.popen2("docker exec -i workspace_neo4j_1 bin/cypher-shell -u neo4j -p #{NEO4J_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
            stdin.puts "DROP DATABASE `#{db_login}` IF EXISTS;"
            stdin.close
            status = wait_thr.value
            raise "Neo4j test demo reset failed for #{db_login}" unless status.success?
        end

        Main.init_mysql(database_email, db_login)
        Main.init_neo4j(database_email, db_login)
    end

    def with_timing(label)
        t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        # STDERR.puts ">>> BEGIN #{label}"
        result = yield
        dt = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0
        STDERR.puts ">>> COMPLETED #{label} (#{format('%.3f', dt)}s)"
        result
    rescue => e
        dt = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0 rescue 0
        STDERR.puts ">>> ERROR #{label} after #{format('%.3f', dt)}s: #{e.class}: #{e.message}"
        raise
    end

    WORKSPACE_UID = 1000
    # nobody: runs commands that students type into a task card.
    GUEST_UID = '65534:65534'
    WORKSPACE_GID = 1000

    # Turn the requested value into a portable Unix login name. The container
    # init script validates this again before renaming the image's build-time
    # `abc` account. Keep this deterministic so logs are readable.
    def self.sanitize_workspace_login(value)
        login = value.to_s.downcase
        login = login.gsub(/[^a-z0-9._-]+/, '-').gsub(/\A[.-]+|[.-]+\z/, '')
        login = 'student' if login.empty?
        login = "user-#{login}" unless login.match?(/\A[a-z_]/)
        login[0, 32]
    end

    def self.workspace_login_for_email(email)
        screenshot_email = ENV['TUTORIAL_SCREENSHOT_EMAIL'].to_s
        screenshot_user = ENV['TUTORIAL_SCREENSHOT_WORKSPACE_USER'].to_s
        if DEVELOPMENT && !screenshot_email.empty? &&
            email.to_s.casecmp?(screenshot_email) && !screenshot_user.empty?
            return sanitize_workspace_login(screenshot_user)
        end

        sanitize_workspace_login(email.to_s.split('@', 2).first)
    end

    def workspace_login_for_email(email)
        Main.workspace_login_for_email(email)
    end

    def ensure_user_directory(path)
        FileUtils.mkdir_p(path)

        # Only change the directories on the path, never recursively visit files.
        current = path
        while current.start_with?('/user/')
            File.chown(WORKSPACE_UID, WORKSPACE_GID, current)
            break if File.dirname(current) == '/user'
            current = File.dirname(current)
        end
    end

    def chown_user_file(path)
        File.chown(WORKSPACE_UID, WORKSPACE_GID, path) if File.exist?(path)
    end

    def initialize_test_git(container_name, workspace_path, email)
        pending_path = File.join(workspace_path, '.test_git_pending')
        return false unless File.exist?(pending_path)

        git_mode = TestWorkspacePackage.git_mode(workspace_path)
        return false if git_mode == 'none'

        display_name = (@@invitations[email] || {})[:name] || email.split('@').first
        git = lambda do |*args|
            command = [
                'docker', 'exec', '-u', '1000:1000',
                "hs_code_#{container_name}",
                'git', '-C', '/workspace', *args,
            ].map { |part| Shellwords.escape(part.to_s) }.join(' ')
            shell_ok(command, :timeout => shell_timeout(:git))
        end

        git.call('init', '-b', 'main') if git_mode == 'fresh'
        git.call('config', '--local', 'user.name', display_name)
        git.call('config', '--local', 'user.email', email)
        git.call('config', '--local', 'commit.gpgSign', 'false')

        exclude_path = TestWorkspacePackage.write_git_exclude(workspace_path)
        chown_user_file(exclude_path)

        if git_mode == 'fresh'
            git.call('add', '-A')
            git.call('commit', '--allow-empty', '-m', 'Ausgangszustand')
        end

        FileUtils.rm_f(pending_path)
        initialized_path = File.join(workspace_path, '.test_git_init')
        File.open(initialized_path, 'w') do |_f|
        end
        chown_user_file(initialized_path)
        true
    end

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

        # Files written by this Ruby process are created as root, so explicitly
        # hand them to the code-server user after creating/updating them.
        chown_user_file = lambda do |path|
            File.chown(1000, 1000, path) if File.exist?(path)
        end

        bashrc_path = "#{workspace_path}/.bashrc"
        if File.exist?(bashrc_path)
            contents = File.read(bashrc_path)
            unless contents.include?('GEM_HOME')
                File.open(bashrc_path, 'a') do |f|
                    f.puts 'export GEM_HOME="$HOME/.gem"'
                end
            end
            chown_user_file.call(bashrc_path)
        end

        # Touch/update this file so housekeeping won't shut down the server immediately.
        hackschule_path = "#{workspace_path}/.hackschule"
        File.open(hackschule_path, 'w') do |f|
            f.puts "https://youtu.be/Akaa9xHaw7E"
        end
        chown_user_file.call(hackschule_path)

        gitconfig_path = "#{workspace_path}/.gitconfig"
        unless File.exist?(gitconfig_path)
            File.open(gitconfig_path, 'w') do |f|
                f.puts <<~END_OF_STRING
                    [init]
                        defaultBranch = main
                    [user]
                        name = #{@@invitations[email][:name] || email.split('@').first}
                        email = #{email}
                    [core]
                        editor = nano
                END_OF_STRING
            end
        end
        chown_user_file.call(gitconfig_path)

        my_cnf_path = "#{workspace_path}/.my.cnf"
        DatabaseProvisioning.sync_my_cnf(
            my_cnf_path,
            database_login,
            Main.gen_password_for_email(database_email, MYSQL_PASSWORD_SALT),
        )
        chown_user_file.call(my_cnf_path)

        myclirc_path = "#{workspace_path}/.myclirc"
        unless File.exist?(myclirc_path)
            File.open(myclirc_path, 'w') do |f|
                f.puts <<~END_OF_STRING
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
        end
        chown_user_file.call(myclirc_path)

        # STDERR.puts ">>> Getting server state"
        state = with_timing("start_server #{container_name}: docker inspect") do
            get_server_state(container_name)
        end
        # STDERR.puts ">>> Server state is #{state.to_yaml}"

        unless state[:running]
            vscode_settings_path = "#{workspace_path}/.local/share/code-server/User/settings.json"
            ensure_user_directory(File.dirname(vscode_settings_path))

            unless File.exist?(vscode_settings_path)
                File.open(vscode_settings_path, 'w') do |f|
                    f.puts JSON.pretty_generate({})
                end
            end

            user_config = JSON.parse(File.read(vscode_settings_path))
            original_config = user_config.to_json
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
            if original_config != new_config
                File.open(vscode_settings_path, 'w') do |f|
                    f.write JSON.pretty_generate(user_config)
                end
            end
            chown_user_file.call(vscode_settings_path)

            if test_tag
                coder_config_path = "#{workspace_path}/.local/share/code-server/coder.json"
                ensure_user_directory(File.dirname(coder_config_path))

                unless File.exist?(coder_config_path)
                    File.open(coder_config_path, 'w') do |f|
                        config = {
                            'query' => {
                                'folder' => '/workspace'
                            }
                        }
                        f.write config.to_json
                    end
                end
                chown_user_file.call(coder_config_path)
            end

            STDERR.puts ">>> Enqueuing database initialization for #{database_login}"
            Main.enqueue_database_init(
                :email => database_email,
                :db_login => database_login,
            )

            if test_tag
                test_init_mark_path = "#{workspace_path}/.test_init"

                unless File.exist?(test_init_mark_path)
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
                        vscode_config = JSON.parse(File.read(vscode_settings_path))
                        vscode_config.merge!(package_vscode_config)

                        File.open(vscode_settings_path, 'w') do |f|
                            f.write JSON.pretty_generate(vscode_config)
                        end
                        chown_user_file.call(vscode_settings_path)
                    end

                    # Git is initialized/configured after the code-server container is
                    # running so repository files are created directly as UID/GID 1000.
                    # git.mode: none deliberately skips all Workspace Git handling.
                    unless git_mode == 'none'
                        git_pending_path = File.join(workspace_path, '.test_git_pending')
                        File.open(git_pending_path, 'w') do |_f|
                        end
                        chown_user_file.call(git_pending_path)
                    end

                    # Only mark package extraction complete after its configuration was
                    # parsed successfully. A malformed config can therefore be fixed and
                    # retried without leaving a half-initialized test workspace.
                    File.open(test_init_mark_path, 'w') do |_f|
                    end
                    chown_user_file.call(test_init_mark_path)
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

    post '/api/start_mysql' do
        assert(user_logged_in?)
        email = @session_user[:email]
        init_mysql(email)
        respond(:yay => 'sure')
    end

    post '/api/reset_mysql' do
        assert(user_logged_in?)
        email = @session_user[:email]
        reset_mysql(email)
        respond(:yay => 'sure')
    end

    post '/api/start_neo4j' do
        assert(user_logged_in?)
        email = @session_user[:email]
        init_neo4j(email)
        user = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email)['u']
            MATCH (u:User {email: $email})
            RETURN u;
        END_OF_QUERY
        respond(:yay => 'sure', :browser_url => "#{NEO4J_WEB_ROOT.sub('neo4j.', 'neo4j-' + user[:server_tag] + '.')}?dbms=bolt#{DEVELOPMENT ? '' : '+s'}://bolt.#{WEBSITE_HOST}#{DEVELOPMENT ? '' : ':443'}")
    end

    post '/api/reset_neo4j' do
        assert(user_logged_in?)
        email = @session_user[:email]
        reset_neo4j(email)
        respond(:yay => 'sure')
    end

    post '/api/server_start_status' do
        assert(user_logged_in?)
        data = parse_request_data(:optional_keys => [:test_tag, :email])

        email = @session_user[:email]
        if data[:email] && teacher_logged_in?
            email = data[:email]
        end

        key = Main.server_start_key(email, data[:test_tag])
        respond(Main.server_start_status(key))
    end

    post '/api/start_server' do
        assert(user_logged_in?)
        data = parse_request_data(:optional_keys => [:test_tag])

        email = @session_user[:email]
        test_tag = data[:test_tag]
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
        data = parse_request_data(:required_keys => [:share_tag])
        share_tag = data[:share_tag]

        user = neo4j_query_expect_one(<<~END_OF_QUERY, :share_tag => share_tag)['u']
            MATCH (u:User {share_tag: $share_tag})
            RETURN u;
        END_OF_QUERY

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

    post '/api/share_server' do
        assert(user_logged_in?)

        # create server_tag for user
        share_tag = gen_share_tag()
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :share_tag => share_tag})
            MATCH (u:User {email: $email})
            SET u.share_tag = $share_tag;
        END_OF_STRING

        Main.refresh_nginx_config()

        respond(:yay => 'sure', :share_tag => share_tag)
    end

    post '/api/unshare_server' do
        assert(user_logged_in?)

        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]})
            MATCH (u:User {email: $email})
            REMOVE u.share_tag;
        END_OF_STRING

        Main.refresh_nginx_config()

        respond(:yay => 'sure')
    end

    def bytes_to_str(ai_Size)
        if ai_Size < 1024
            return "#{ai_Size} B"
        elsif ai_Size < 1024 * 1024
            return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0)} kB"
        elsif ai_Size < 1024 * 1024 * 1024
            return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0)} MB"
        elsif ai_Size < 1024 * 1024 * 1024 * 1024
            return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0 / 1024.0)} GB"
        end
        return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0 / 1024.0 / 1024.0)} TB"
    end

    # The strip above a content page: activity (kind, duration, reference
    # pages it uses) or reference page (where it sits, who uses it).
    def page_kind_header(slug)
        toggle = teacher_note_toggle(slug)
        header = PageKinds.header(slug, @@content, @@sections, :development => DEVELOPMENT, :extra => toggle)
        header.empty? && !toggle.empty? ? "<div class='page-kind'>#{toggle}</div>" : header
    end

    def page_kind_class(slug)
        PageKinds.container_class(slug)
    end

    # --- Tasks (docs/aufgaben.md) --------------------------------------------

    # Task cards are off for students until an instance sets
    # TASKS_ENABLED = true in credentials.rb. Teachers always see them, so
    # they can try tasks before switching them on.
    def tasks_enabled_for_everyone?
        defined?(TASKS_ENABLED) && TASKS_ENABLED == true
    end

    def tasks_visible?
        tasks_enabled_for_everyone? || teacher_account_logged_in?
    end

    # Hides the cards, or loads tasks.js, depending on who is looking.
    def task_page_setup(slug)
        tasks = (@@content[slug] || {})[:tasks]
        return '' if tasks.nil? || tasks[:on_page].nil? || tasks[:on_page].empty?
        return "<style>.task-card, .tasks-on-only { display: none; }</style>" unless tasks_visible?

        config = {
            :activity => tasks[:activity],
            :loggedIn => user_logged_in?,
            :preview => !tasks_enabled_for_everyone?,
        }
        # Choice options in an order of their own for each person (tasks.js
        # moves them; the values stay the original option numbers).
        if user_logged_in?
            config[:order] = TaskCard.option_orders(@session_user[:email], tasks[:activity], tasks[:on_page], tasks[:by_id])
        end
        # Text that only stands in for the cards (an exercise list) is hidden
        # while the cards are shown.
        previews = tasks[:on_page].any? { |id| (tasks[:by_id][id] || {}).dig('check', 'preview') == 'anaglyph' }
        # SQL and Cypher answers in the colours of the code blocks, with the
        # keyword lists of the same lexers (code_input.js).
        code = tasks[:on_page].any? { |id| %w[sql cypher].include?((tasks[:by_id][id] || {}).dig('check', 'type')) }
        code_input = if code
            keywords = {
                :sql => Rouge::Lexers::SQL.keywords.to_a,
                :sqlTypes => Rouge::Lexers::SQL.keywords_type.to_a,
                :cypher => Rouge::Lexers::WorkspaceCypher::KEYWORDS,
            }
            "<script>window.CODE_INPUT_KEYWORDS = #{keywords.to_json};</script>" \
                "<script src='/include/code_input.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
        end
        "<style>.tasks-off-only { display: none; }</style>" \
            "<script>window.TASK_PAGE = #{config.to_json};</script>" \
            "#{previews ? "<script src='/include/anaglyph_preview.js?#{CACHE_BUSTER}'></script>" : ''}" \
            "#{code_input}" \
            "<script src='/include/tasks.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
    end

    # Handbuch boxes (_include) this person has already been through on
    # another page: detailed the first time, one line after. A part counts
    # as done once a task is solved on another page that includes it; the
    # line names that page. Loads include_fold.js only if something folds.
    def include_fold_setup(slug)
        parts = (@@content[slug] || {})[:included_parts] || []
        return '' if parts.empty? || !user_logged_in? || !tasks_visible?

        solved = my_task_states.values.select { |s| StudentHome::DONE.include?(s[:status].to_s) }
            .map { |s| s[:task].to_s.split('/', 2).first }.uniq
        return '' if solved.empty?

        seen = {}
        parts.each do |part|
            other = @@content.find do |other_slug, c|
                other_slug != slug && c[:tasks] && (c[:included_parts] || []).include?(part) &&
                    solved.include?(c[:tasks][:activity])
            end
            seen[part] = other[1][:title].to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '') if other
        end
        return '' if seen.empty?

        "<script>window.SEEN_PARTS = #{seen.to_json.gsub('</', '<\/')};</script>" \
            "<script src='/include/include_fold.js?#{CACHE_BUSTER}'></script>"
    end

    # Loads media_variants.js on a page with image or video variants.
    def media_variants_setup(slug)
        return '' unless (@@content[slug] || {})[:html].to_s.include?('data-variant-')
        # In development with the time, like default.css: the script changes
        # without Ruby restarting, and browsers keep it for years.
        "<script src='/include/media_variants.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
    end

    # On a page with a template: loads template_notice.js, which asks
    # /api/template/status in the background. Same switch as the tasks.
    def template_notice_setup(slug)
        template = (@@content[slug] || {})[:template]
        return '' if template.nil? || !user_logged_in? || !tasks_visible?

        "<script>window.TEMPLATE_PAGE = #{{ :slug => slug }.to_json};</script>" \
            "<script src='/include/template_notice.js?#{CACHE_BUSTER}'></script>"
    end

    # :outdated, :current, :absent, :other or :unavailable for one person's
    # clone of a template (TemplateFreshness). Reads only.
    def template_status_for(email, template)
        latest = @@template_latest.get(template[:url])
        return :unavailable if latest.nil?

        output = workspace_runtime.run_script(fs_tag_for_email(email), :script => TemplateFreshness::SCRIPT,
            :args => ["/workspace/#{template[:folder]}", latest],
            :uid => "#{WORKSPACE_UID}:#{WORKSPACE_GID}", :timeout => 10)
        TemplateFreshness.status(template, latest, TemplateFreshness.parse(output))
    end

    # Pages that name a template: [[slug, title, template]], by title.
    def template_pages
        @@content.select { |_, c| c[:template] && !c[:dev_only] }
            .map { |slug, c| [slug, c[:title].to_s.gsub(/<[^>]*>/, '').gsub('&shy;', ''), c[:template]] }
            .sort_by { |_, title, _| title.downcase }
    end

    # sql tasks: the person's own MySQL database, logged in as them (never
    # as root), read-only per query (SqlCheck.mysql_runner).
    def task_sql_runner
        email = @session_user[:email]
        login = @session_user[:db_login] || db_login_for_email(email)
        return nil if login.nil? || !DatabaseIdentity.valid?(login)

        SqlCheck.mysql_runner(:host => 'mysql', :username => login, :database => login,
            :password => Main.gen_password_for_email(email, MYSQL_PASSWORD_SALT))
    end

    # cypher tasks: the person's own Neo4j database on the students' server,
    # logged in as them (never as an administrator), read-only per query
    # (CypherCheck.runner). nil until their Neo4j login exists.
    def task_cypher_runner
        email = @session_user[:email]
        login = @session_user[:db_login] || db_login_for_email(email)
        return nil if login.nil? || !DatabaseIdentity.valid?(login)

        CypherCheck.runner(:uri => 'bolt://neo4j:7687', :login => login,
            :password => Main.gen_password_for_email(email, NEO4J_PASSWORD_SALT))
    end

    # What an sql check names when it cannot run: which database, which login,
    # whether the Workspace runs, what /workspace/.my.cnf says (never the
    # password). The last two are only looked up when needed.
    def task_sql_env
        email = @session_user[:email]
        login = @session_user[:db_login] || db_login_for_email(email)
        {
            :login => login, :database => login, :host => 'mysql',
            :workspace => lambda {
                state = begin
                    get_server_state(fs_tag_for_email(email))
                rescue StandardError
                    nil
                end
                state.nil? ? nil : state[:running] == true
            },
            :my_cnf => lambda {
                ws = task_workspace_files
                next nil if ws.nil?
                next :missing unless ws.entry('.my.cnf')[:type] == :file

                text = begin
                    ws.read_text('.my.cnf', :max => 4096)
                rescue StandardError
                    ''
                end
                text.scan(/^\s*(user|database)\s*=\s*(\S+)\s*$/).to_h
            },
        }
    end

    # The task for 'activity/id', with its check version, or nil.
    def find_task(key)
        activity, id = key.to_s.split('/', 2)
        return nil unless activity.to_s =~ /\A[a-z0-9_-]+\z/ && id.to_s =~ TaskSpec::ID_PATTERN

        entry = @@content.values.find { |c| (c[:tasks] || {})[:activity] == activity }
        task = entry && entry[:tasks][:by_id][id]
        return nil unless task

        { :key => "#{activity}/#{id}", :task => task, :version => TaskSpec.check_version(task) }
    end

    # Values of the ${...} variables a task uses, for the logged-in person.
    # A value is nil when it cannot be known right now (Workspace not running).
    def task_variables(task, key)
        names = TaskCheck.variables_used(task['check'])
        email = @session_user[:email]
        vars = {}
        if names.include?('workspace.ip')
            state = begin
                get_server_state(fs_tag_for_email(email))
            rescue StandardError
                nil
            end
            vars['workspace.ip'] = state && state[:running] ? state[:ip] : nil
        end
        if names.include?('workspace.live_url')
            server_tag = @session_user[:server_tag]
            vars['workspace.live_url'] = server_tag ? WEB_ROOT.sub('//', "//#{server_tag}-5500.") : nil
        end
        if names.include?('person.codeword')
            vars['person.codeword'] = TaskCheck.codeword("#{LOGIN_CODE_SALT}:task-codeword", email, key)
        end
        vars
    end

    def render_task_markdown(markdown)
        return nil if markdown.nil? || markdown.to_s.strip.empty?

        @@task_redcarpet ||= Redcarpet::Markdown.new(Redcarpet::Render::HTML, {:fenced_code_blocks => true, :tables => true})
        # Feedback boxes are light and style plain code themselves
        # (.task-feedback pre code), so no highlighting here.
        @@task_redcarpet.render(markdown.to_s)
    end

    SUBMISSIONS_ROOT = '/internal/submissions'

    # The student's Workspace folder on the server, read-only, or nil if the
    # Workspace was never opened.
    def task_workspace_files
        path = "/user/#{fs_tag_for_email(@session_user[:email])}/workspace"
        File.directory?(path) ? WorkspaceFiles.new(path) : nil
    end

    # git status of a repository, read as the student in their container, or
    # in a reader container if it is not running (WorkspaceRuntime).
    def task_git_state(repo)
        WorkspaceFiles.clean(repo)
        output = workspace_runtime.git_status(fs_tag_for_email(@session_user[:email]),
            :repo => repo, :uid => "#{WORKSPACE_UID}:#{WORKSPACE_GID}",
            :script => GitCheck::STATUS_SCRIPT, :timeout => 30, :host_data_path => PATH_TO_HOST_DATA)
        GitCheck.complete?(output) ? GitCheck.parse(output) : nil
    end

    # Languages in which a person has solved a Code Bites task.
    def codebite_solved_languages(email, codebite)
        neo4j_query(<<~END_OF_QUERY, :email => email, :task => codebite).map { |row| row['lang'] }
            MATCH (:User {email: $email})<-[:BY]-(s:Solved)-[:FOR]->(:Task {name: $task}), (s)-[:IN]->(l:Language)
            RETURN DISTINCT l.name AS lang;
        END_OF_QUERY
    end

    # A Code Bites task was just solved: tick every practice-set task that
    # points to it (check type code), so the class board shows it without
    # the student pressing anything.
    def sync_code_tasks(email, codebite)
        languages = codebite_solved_languages(email, codebite)
        @@content.each_value do |c|
            next unless c[:tasks]

            c[:tasks][:by_id].each do |id, task|
                next unless task['check']['type'] == 'code' && task['check']['codebite'] == codebite

                found = { :key => "#{c[:tasks][:activity]}/#{id}", :task => task, :version => TaskSpec.check_version(task) }
                result = TaskCheck.check(task, '', :code_solved => lambda { |_| languages })
                next unless result.status == :correct

                store_task_result(found, 'correct', result.message.to_s.sub(/\.\z/, ''), email)
            end
        end
    rescue StandardError => e
        STDERR.puts "sync_code_tasks #{codebite} #{email}: #{e.class}: #{e.message}"
    end

    # A submit task that shows its scenes with the Anaglyph viewer, or 404.
    def anaglyph_preview_task(key)
        found = find_task(key)
        check = found && found[:task]['check']
        halt 404 unless check && check['type'] == 'submit' && check['preview'] == 'anaglyph'
        found
    end

    # Whose submission »who« is (ich: one's own, otherwise the state key), if
    # the logged-in person may see it: their own, or one of a group they teach.
    def anaglyph_submission_owner(task_key, who)
        own = @session_user[:email]
        return own if who == 'ich' || who == TaskStore.state_key(own, task_key)
        Main.load_invitations()
        class_groups.each do |group|
            (@@group_members[group] || []).each do |email|
                return email if TaskStore.state_key(email, task_key) == who
            end
        end
        nil
    end

    # preview: anaglyph tasks on the class page: a picture of every handed-in
    # scene (anaglyph_preview.js); a click shows the scenes full size.
    def class_scene_galleries_html(activity_key, tasks, members)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        io = +''
        tasks.each do |t|
            found = find_task("#{activity_key}/#{t[:id]}")
            check = found && found[:task]['check']
            next unless check && check['type'] == 'submit' && check['preview'] == 'anaglyph'
            handed_in = members.select { |m| File.file?(submission_path(found[:key], m[:email])) }
            io << "<h3 class='scene-gallery-title'>Galerie: #{h.(t[:title])}</h3>"
            if handed_in.empty?
                io << "<p>Noch hat niemand eine Szene abgegeben.</p>"
                next
            end
            io << "<p class='scene-gallery-note'>Jede Szene aus der Ansicht, die in ihrer Datei unter <code>camera = …</code> steht. Ein Klick zeigt sie groß; dort lässt sie sich drehen, und die Pfeiltasten blättern weiter.</p>"
            io << "<div class='scene-gallery'>"
            handed_in.each do |m|
                scene = "abgaben/#{found[:key]}/#{TaskStore.state_key(m[:email], found[:key])}.ini"
                io << "<figure class='scene-gallery-item' data-scene='#{h.(scene)}' data-name='#{h.(m[:name])}'>"
                io << "<button type='button' title='Groß ansehen'><img alt='Die Szene von #{h.(m[:name])}'></button>"
                io << "<figcaption>#{h.(m[:name])}</figcaption></figure>"
            end
            io << "</div>"
        end
        io << "<script src='/include/anaglyph_preview.js?#{CACHE_BUSTER}'></script>" unless io.empty?
        io
    end

    def submission_path(task_key, email = @session_user[:email])
        File.join(SUBMISSIONS_ROOT, "#{TaskStore.state_key(email, task_key)}.tar.gz")
    end

    def store_task_result(found, status, answer, email = @session_user[:email])
        key = TaskStore.state_key(email, found[:key])
        now = Time.now.to_i
        previous = neo4j_query(<<~END_OF_QUERY, :email => email, :key => key).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s;
        END_OF_QUERY
        props = TaskStore.next_state(previous && previous['s'],
            :task => found[:key], :version => found[:version], :status => status,
            :answer => answer, :now => now)
        neo4j_query(<<~END_OF_QUERY, props.merge(:email => email, :key => key))
            MATCH (u:User {email: $email})
            MERGE (s:TaskState {key: $key})
            MERGE (u)-[:HAS_TASK_STATE]->(s)
            SET s.task = $task, s.version = $version, s.status = $status,
                s.last_answer = $last_answer, s.attempts = $attempts,
                s.first_status = $first_status, s.first_correct = $first_correct,
                s.updated = $updated
            CREATE (a:TaskAttempt {task: $task, version: $version, status: $status, ts: $updated})-[:BY]->(u);
        END_OF_QUERY
        TaskStore.public_state(props, found[:version])
    end

    # The teacher note of a page (lehrkraft.md), for teachers only: a small
    # button at the right end of the page strip, so the page can be shown on
    # a projector as it is, and the note itself below it, closed.
    def teacher_note?(slug)
        !(@@content[slug] || {})[:teacher_note].nil? && teacher_account_logged_in?
    end

    def teacher_note_toggle(slug)
        return '' unless teacher_note?(slug)

        "<button type='button' class='teacher-note-toggle' aria-expanded='false' aria-controls='teacher-note'>" \
            "<i class='bi bi-person-video3' aria-hidden='true'></i>Notiz für Lehrkräfte</button>"
    end

    def teacher_note_panel(slug)
        return '' unless teacher_note?(slug)

        "<div class='teacher-note' id='teacher-note' hidden><div class='teacher-note-body'>#{@@content[slug][:teacher_note]}</div></div>" \
            "<script>document.querySelector('.teacher-note-toggle').addEventListener('click', function () {" \
            "var open = this.getAttribute('aria-expanded') !== 'true'; this.setAttribute('aria-expanded', open);" \
            "document.getElementById('teacher-note').hidden = !open; });</script>"
    end

    # --- Class board (docs/aufgaben.md, »Klassenübersicht«) ---------------------

    # Groups whose class board the logged-in person may see: administrators see
    # all, group administrators (+ in the invitation files) their groups.
    def class_groups
        return [] unless user_logged_in?

        groups = (defined?(@@group_members) ? @@group_members : {}).keys
        return groups.sort_by { |g| @@user_group_order.index(g) || 0 } if admin_logged_in?

        own = @@teachers[@session_user[:email]] || Set.new
        groups.select { |g| own.include?(g) }.sort_by { |g| @@user_group_order.index(g) || 0 }
    end

    def class_member?(group, email)
        class_groups.include?(group) && (@@group_members[group] || []).include?(email)
    end

    def class_members(group)
        (@@group_members[group] || []).map do |email|
            { :email => email, :name => ((@@invitations[email] || {})[:name] || email) }
        end
    end

    # Activities that place tasks on their page, with their tasks in page
    # order: [{ slug:, activity:, title:, tasks: [{ id:, title:, type:, version:, prediction: }] }]
    def activities_with_tasks
        @@content.select { |_, c| c[:tasks] && !(c[:tasks][:on_page] || []).empty? && (DEVELOPMENT || !c[:dev_only]) && !c[:teacher_only] }
            .map do |slug, c|
                tasks = c[:tasks][:on_page].map do |id|
                    t = c[:tasks][:by_id][id]
                    { :id => id, :title => t['title'], :type => t['check']['type'],
                      :version => TaskSpec.check_version(t), :prediction => t['prediction'] == true }
                end
                { :slug => slug, :activity => c[:tasks][:activity],
                  :title => c[:title].to_s.gsub(/<[^>]*>/, '').gsub('&shy;', ''), :tasks => tasks }
            end
    end

    # Activities for the class board: [[activity, title, tasks]].
    def class_activities
        activities_with_tasks.map { |a| [a[:activity], a[:title], a[:tasks]] }.sort_by { |_, title, _| title.downcase }
    end

    # The logged-in person's TaskStates as { 'activity/id' => state }, once per request.
    def my_task_states
        return {} unless user_logged_in?

        @my_task_states ||= neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email]).to_h { |row| [row['s'][:task], row['s']] }
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState)
            RETURN s;
        END_OF_QUERY
    end

    def my_progress
        @my_progress ||= StudentHome.progress(activities_with_tasks, my_task_states)
    end

    # Groups the logged-in person is a member of (all of them, not only the first).
    def my_groups
        return [] unless user_logged_in? && defined?(@@group_members)

        @@group_members.select { |_, members| members.include?(@session_user[:email]) }.keys
    end

    # Activities teachers pinned for groups: [{ slug:, group:, ts: }].
    def pins_for_groups(groups)
        return [] if groups.empty?

        neo4j_query(<<~END_OF_QUERY, :groups => groups).map { |row| { :slug => row['p'][:slug], :group => row['p'][:group], :ts => row['p'][:ts] } }
            MATCH (p:Pin) WHERE p.group IN $groups
            RETURN p ORDER BY p.ts DESC;
        END_OF_QUERY
    end

    # Activities a student may see in the catalog (and a teacher may pin).
    def pinnable_slugs
        @@section_order.reject { |key| (@@sections[key] || {})[:hidden] }
            .flat_map { |key| (@@sections[key] || {})[:entries] || [] }
            .select { |slug| @@content[slug] && (DEVELOPMENT || !@@content[slug][:dev_only]) }
    end

    def plain_title(slug)
        (@@content[slug] || {})[:title].to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '').strip
    end

    # Start page for logged-in students: activities a teacher pinned (M5).
    def student_home
        return '' unless user_logged_in? && tasks_visible?
        return '' unless my_running_tests.empty?

        allowed = pinnable_slugs
        pinned = pins_for_groups(my_groups).select { |p| allowed.include?(p[:slug]) }.uniq { |p| p[:slug] }.map do |p|
            { :slug => p[:slug], :title => plain_title(p[:slug]), :abstract => @@content[p[:slug]][:abstract], :group => p[:group],
              :template_folder => (@@content[p[:slug]][:template] || {})[:folder] }
        end
        html = StudentHome.html(:progress => my_progress, :pinned => pinned)
        html += "<script src='/include/home_templates.js?#{CACHE_BUSTER}'></script>" if pinned.any? { |p| p[:template_folder] }
        html
    end

    # Teachers pin an activity for a group; it shows first on the students' start page.
    post '/api/class/pin' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :slug], :max_value_lengths => {:group => 200, :slug => 100})
        halt 403 unless class_groups.include?(data[:group])
        halt 404 unless pinnable_slugs.include?(data[:slug])
        neo4j_query(<<~END_OF_QUERY, :key => "#{data[:group]}|#{data[:slug]}", :group => data[:group], :slug => data[:slug], :by => @session_user[:email], :ts => Time.now.to_i)
            MERGE (p:Pin {key: $key})
            SET p.group = $group, p.slug = $slug, p.by = $by, p.ts = $ts;
        END_OF_QUERY
        respond(:pinned => data[:slug])
    end

    post '/api/class/unpin' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :slug], :max_value_lengths => {:group => 200, :slug => 100})
        halt 403 unless class_groups.include?(data[:group])
        neo4j_query(<<~END_OF_QUERY, :key => "#{data[:group]}|#{data[:slug]}")
            MATCH (p:Pin {key: $key}) DETACH DELETE p;
        END_OF_QUERY
        respond(:unpinned => data[:slug])
    end

    def class_board_states(emails, activity)
        states = {}
        neo4j_query(<<~END_OF_QUERY, :emails => emails, :prefix => "#{activity}/").each do |row|
            MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE u.email IN $emails AND s.task STARTS WITH $prefix
            RETURN u.email AS email, s;
        END_OF_QUERY
            id = row['s'][:task].to_s.split('/', 2)[1]
            (states[row['email']] ||= {})[id] = row['s']
        end
        states
    end

    # Pinned activities of a group, with controls to pin and unpin.
    def class_pins_html(group)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        pins = pins_for_groups([group])
        io = +"<div class='class-pins' data-group='#{h.(group)}'>"
        io << "<span class='class-pins-label'>#{Icons.tabler('pin')} Angeheftet für #{h.(group)}:</span>"
        if pins.empty?
            io << "<span class='class-pins-none'>nichts</span>"
        else
            pins.each do |p|
                io << "<span class='class-pin'><a href='/#{h.(p[:slug])}'>#{h.(plain_title(p[:slug]))}</a>"
                io << "<button type='button' class='class-unpin' data-slug='#{h.(p[:slug])}' title='Lösen' aria-label='#{h.(plain_title(p[:slug]))} lösen'>#{Icons.tabler('x')}</button></span>"
            end
        end
        io << "<select class='form-select form-select-sm class-pin-select' aria-label='Aktivität zum Anheften'><option value=''>Aktivität anheften …</option>"
        pinned = pins.map { |p| p[:slug] }
        pinnable_slugs.reject { |s| pinned.include?(s) }.sort_by { |s| plain_title(s).downcase }.each do |s|
            io << "<option value='#{h.(s)}'>#{h.(plain_title(s))}</option>"
        end
        io << "</select><div class='class-pins-note'>Angeheftetes steht bei allen in der Gruppe oben auf der Startseite und lässt sich dort nicht ausblenden.</div></div>"
        io
    end

    # »Vorlage aktuell?« for the activity shown, if it has a template:
    # a button in the toolbar, results below it (class_board.js).
    def class_template_html(group, activity_key)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        slug = @@content.find { |_, c| (c[:tasks] || {})[:activity] == activity_key && c[:template] }&.first
        return '' if slug.nil?

        "<div class='class-template' data-group='#{h.(group)}' data-slug='#{h.(slug)}'>" \
            "<button type='button' class='btn btn-sm btn-outline-secondary class-template-check' " \
            "title='Sieht nach, ob alle die neueste Version der Vorlage haben. Holt nichts und ändert nichts.'>" \
            "<i class='bi bi-arrow-repeat' aria-hidden='true'></i>Vorlage aktuell?</button></div>"
    end

    # The people of a group on the class page: login codes while someone is
    # waiting, and for every student whether its workspace runs, with
    # »Workspace öffnen« and »Nachahmen« (LookIn). class_board.js fills
    # the codes and the status from the teachers' websocket.
    def class_people_html(group)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        members = class_members(group)
        test_group = members.all? { |m| @@test_accounts.key?(m[:email]) }
        io = +"<section class='class-people' data-group='#{h.(group)}'>"
        if test_group
            io << "<p>Mit deinem Testkonto probierst du aus, was Lernende sehen: Aufgaben, Abgaben, die Galerie und Leistungsüberprüfungen. Es gehört nur zu dieser Gruppe, die nur du siehst.</p>"
        end
        # The admin page's server cards, filled from the teachers' websocket
        # (server_stats), see class_board.js.
        bar = ->(label) { "<div class='col-6'>#{label}<div class='progress' role='progressbar' aria-label='#{label}'><div class='progress-bar progress-bar-striped' style='width: 0%'></div></div><p>&nbsp;</p></div>" }
        io << "<div class='row class-server-cards' hidden>"
        io << "<div class='col-md-6 mb-3'><div class='server-stats class-server-cpu'><h4>CPU / RAM</h4><i class='bi bi-cpu' aria-hidden='true'></i><hr><div class='row'>#{bar.('CPU')}#{bar.('RAM')}</div></div></div>"
        io << "<div class='col-md-6 mb-3'><div class='server-stats class-server-storage'><h4>Storage</h4><i class='bi bi-nvme' aria-hidden='true'></i><hr><div class='row'>#{bar.('System')}#{bar.('User data')}</div></div></div>"
        io << "</div>"
        io << "<div class='class-people-scroll'><table class='table align-middle class-people-table'>"
        io << "<thead><tr><th>Name</th><th>Workspace</th><th class='text-end'>CPU</th><th class='text-end'>RAM</th><th></th></tr></thead><tbody>"
        members.each do |m|
            email = m[:email]
            test = @@test_accounts.key?(email)
            io << "<tr data-email='#{h.(email)}'><td class='class-person'>#{h.(m[:name])}</td>"
            io << "<td class='class-ws-status' data-fs-tag='#{h.(fs_tag_for_email(email))}'><span class='class-ws-dot' aria-hidden='true'></span><span class='class-ws-text'>aus</span></td>"
            io << "<td class='class-ws-cpu text-end'>–</td><td class='class-ws-ram text-end'>–</td><td class='class-person-actions'>"
            if may_look_in?(email)
                attrs = "data-email='#{h.(email)}' data-name='#{h.(m[:name])}'#{test ? " data-test='1'" : ''}"
                io << "<button type='button' class='btn btn-sm btn-outline-secondary bu-class-open-workspace' #{attrs} title='Workspace öffnen'><i class='bi bi-code-slash' aria-hidden='true'></i><span class='class-btn-label'>Workspace öffnen</span></button> "
                io << "<button type='button' class='btn btn-sm btn-outline-secondary bu-class-impersonate' #{attrs} title='Nachahmen'><i class='bi bi-person-vcard' aria-hidden='true'></i><span class='class-btn-label'>Nachahmen</span></button>"
            end
            io << "</td></tr>"
        end
        io << "</tbody></table></div></section>"
        io
    end

    # What a teacher looked at last on the class page: the group, and per
    # group the activity. Stored with the account (teachers change computers).
    def class_prefs
        JSON.parse(@session_user[:class_prefs].to_s)
    rescue JSON::ParserError
        {}
    end

    def remember_class_prefs(group, activity_key)
        prefs = class_prefs
        return if prefs['group'] == group && (prefs['activities'] || {})[group] == activity_key

        prefs['group'] = group
        (prefs['activities'] ||= {})[group] = activity_key
        json = prefs.to_json
        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :prefs => json)
            MATCH (u:User {email: $email})
            SET u.class_prefs = $prefs;
        END_OF_QUERY
        @session_user[:class_prefs] = json
    end

    CLASS_TABS = [
        ['fortschritt', 'Fortschritt', 'bi-check2-square'],
        ['lernende', 'Lernende', 'bi-people'],
        ['startseite', 'Startseite', 'bi-pin-angle'],
    ].freeze

    # The class page (/klasse): the group up top, waiting login codes right
    # under it (never missed), and three tabs, each with one job: what the
    # group did in an activity, the people and their Workspaces, and what
    # is pinned on their start page.
    def class_board_page
        groups = class_groups
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        return "<p>Du betreust noch keine Gruppe.</p>" if groups.empty?

        prefs = class_prefs
        group = [params['group'], prefs['group']].find { |g| groups.include?(g) } || groups.first
        tab = CLASS_TABS.any? { |key, _, _| key == params['tab'] } ? params['tab'] : 'fortschritt'
        io = +"<form class='class-board-select' method='get' action='/klasse'>"
        io << "<label>Gruppe <select class='form-select' name='group' onchange='this.form.submit()'>"
        groups.each { |g| io << "<option value='#{h.(g)}'#{g == group ? ' selected' : ''}>#{h.(g)} (#{(@@group_members[g] || []).size})</option>" }
        io << "</select></label><input type='hidden' name='tab' value='#{h.(tab)}'>"
        io << "<noscript><button class='btn btn-primary'>Anzeigen</button></noscript>"
        # For the projector: the group's codes, big, names only (class_board.js).
        io << "<button type='button' class='btn btn-outline-primary class-codes-show'><i class='bi bi-projector' aria-hidden='true'></i>Anmelde-Codes zeigen</button></form>"
        io << "<div class='class-login-codes' data-group='#{h.(group)}' hidden><h2><i class='bi bi-key' aria-hidden='true'></i>Anmelde-Codes</h2>" \
            "<p>Diese Personen aus #{h.(group)} warten gerade auf ihren Code:</p>" \
            "<table class='table table-sm'><tbody></tbody></table></div>"
        io << "<div class='class-tabs' role='tablist' aria-label='Ansicht'>"
        CLASS_TABS.each do |key, label, icon|
            count = key == 'lernende' ? " <span class='class-tab-count'>#{class_members(group).size}</span>" : ''
            io << "<button type='button' role='tab' class='class-tab' id='class-tab-#{key}' data-tab='#{key}' aria-controls='class-panel-#{key}' " \
                "aria-selected='#{key == tab}'#{key == tab ? '' : " tabindex='-1'"}><i class='bi #{icon}' aria-hidden='true'></i>#{label}#{count}</button>"
        end
        io << "</div>"
        panel = ->(key, html) { "<section class='class-panel' role='tabpanel' id='class-panel-#{key}' aria-labelledby='class-tab-#{key}'#{key == tab ? '' : ' hidden'}>#{html}</section>" }
        io << panel.('fortschritt', class_progress_html(group))
        io << panel.('lernende', class_people_html(group))
        io << panel.('startseite', class_pins_html(group))
        io
    end

    # The tab »Fortschritt«: the activity, a toolbar (look into the
    # Workspaces now, is the template current) and the board.
    def class_progress_html(group)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        activities = class_activities
        prefs = class_prefs
        wanted = [params['activity'], (prefs['activities'] || {})[group]]
        activity = wanted.compact.map { |key| activities.find { |a, _, _| a == key } }.compact.first || activities.first
        return "<p>Noch keine Aktivität hat Aufgaben auf ihrer Seite.</p>" if activity.nil?

        activity_key, title, tasks = activity
        remember_class_prefs(group, activity_key)
        slug = @@content.find { |_, c| (c[:tasks] || {})[:activity] == activity_key }&.first
        io = +"<div class='class-toolbar'>"
        io << "<form method='get' action='/klasse' class='class-activity-select'>"
        io << "<input type='hidden' name='group' value='#{h.(group)}'><input type='hidden' name='tab' value='fortschritt'>"
        io << "<label>Aktivität <select class='form-select' name='activity' onchange='this.form.submit()'>"
        activities.each { |a, t, _| io << "<option value='#{h.(a)}'#{a == activity_key ? ' selected' : ''}>#{h.(t)}</option>" }
        io << "</select></label><noscript><button class='btn btn-primary'>Anzeigen</button></noscript></form>"
        io << "<a class='class-activity-link' href='/#{h.(slug)}' title='#{h.(title)} öffnen'><i class='bi bi-box-arrow-up-right' aria-hidden='true'></i>Zur Seite</a>" if slug
        live = tasks.select { |t| TaskCheck::LIVE_TYPES.include?(t[:type]) }
        unless live.empty?
            io << "<div class='dropdown class-live' data-group='#{h.(group)}'>"
            io << "<button type='button' class='btn btn-sm btn-outline-primary dropdown-toggle' data-bs-toggle='dropdown' aria-expanded='false'>"
            io << "<i class='bi bi-search' aria-hidden='true'></i>Jetzt nachsehen</button><ul class='dropdown-menu class-live-menu'>"
            io << "<li><span class='dropdown-item-text class-live-note'>Liest die Workspaces der Gruppe so, wie die Karte es tut, und ändert nichts.</span></li>"
            live.each do |t|
                n = tasks.index(t) + 1
                io << "<li><button type='button' class='dropdown-item class-live-check' data-task='#{h.(activity_key)}/#{h.(t[:id])}'>#{n}. #{h.(t[:title])}</button></li>"
            end
            io << "</ul></div>"
        end
        io << class_template_html(group, activity_key)
        io << "</div>"
        io << "<div class='class-template-results' aria-live='polite'></div>"
        io << "<div class='class-live-results' aria-live='polite'></div>"

        members = class_members(group)
        # One's own tries and hand-ins, below the class (teachers who are not
        # members of the group, e.g. to try a task before the class does).
        me = @session_user[:email]
        viewer = { :email => me, :name => 'Du (Lehrkraft)' }
        states = class_board_states((members.map { |m| m[:email] } + [me]).uniq, activity_key)
        board = ClassBoard.build(members, tasks, states, :viewer => viewer)
        io << ClassBoard.html(board, :activity => activity_key,
            :submission_url => lambda { |email, id| "/api/task/submission/#{activity_key}/#{id}?email=#{CGI.escape(email)}" })
        io << class_scene_galleries_html(activity_key, tasks, board[:own] ? members + [viewer] : members)
        io
    end

    def print_content_overview(catalog = :student)
        Main.refresh_content_catalog if DEVELOPMENT
        assert([:student, :teaching, :reference].include?(catalog))
        assert(teacher_account_logged_in?) if catalog == :teaching
        html = StringIO.open do |io|
            running_tests = catalog == :student ? my_running_tests() : []
            if running_tests.empty?
                section_order = case catalog
                    when :teaching then @@teaching_section_order
                    when :reference then (defined?(@@reference_section_order) ? @@reference_section_order : [])
                    else @@section_order
                end
                if catalog == :reference
                    pages = section_order.flat_map do |section_key|
                        section = @@sections[section_key]
                        section[:entries].reject { |entry| (!DEVELOPMENT) && @@content[entry][:dev_only] }.map do |slug|
                            content = @@content[slug]
                            { :slug => slug, :title => content[:title],
                              :outline => content[:outline], :sections => content[:search_sections],
                              :section_label => section[:label], :section_icon => section[:icon] }
                        end
                    end
                    io.puts ReferenceIndex.html(pages)
                    section_order = []
                end
                section_order.each do |section_key|
                    section = @@sections[section_key]
                    # hidden: true in sections.yaml: the pages exist, the
                    # catalog does not show them.
                    next if section[:hidden]
                    next if section[:entries].reject do |entry|
                        (!DEVELOPMENT) && @@content[entry][:dev_only]
                    end.empty?
                    io.puts "<h2><div class='squircle'><img alt='' src='#{section[:icon]}'></div> #{section[:label]}</h2>"
                    if section[:description]
                        io.puts "<p style='margin-top: -1em; margin-bottom: 1em;'>#{section[:description]}</p>"
                    end
                    io.puts "<div class='row'>"
                    entries = section[:entries].reject do |entry|
                        (!DEVELOPMENT) && @@content[entry][:dev_only]
                    end
                    entries.each.with_index do |slug, index|
                        hor2 = ((entries.length % 2) == 1) && (index == entries.length - 1)
                        hor3 = false
                        if entries.length % 3 == 1
                            hor3 = index >= entries.length - 4
                        elsif entries.length % 3 == 2
                            hor3 = index >= entries.length - 2
                        end

                        content = @@content[slug]
                        io.puts "<div class='hscol #{hor2 ? 'hor2' : ''} #{hor3 ? 'hor3' : ''}'>"
                        io.puts "<a href='/#{slug}' class='tutorial_card3'>"
                        io.puts "<h4>#{content[:title]}</h4>"
                        io.puts "<div>"
                        additional_classes = []
                        if content[:needs_contrast] == 'light'
                            additional_classes << 'dark-only-bg-contrast-light'
                        end
                        [content[:image], content[:image_dark]].compact.each do |image_url|
                            Main.ensure_image_width(image_url, 1024)
                        end
                        image_style = "object-position: #{content[:image_x]}% #{content[:image_y]}%;"
                        if content[:image_dark]
                            io.puts "<img alt='' class='#{(additional_classes + ['theme-image-light']).join(' ')}' src='#{content[:image].sub('.webp', '-1024.webp')}' style='#{image_style}'>"
                            io.puts "<img alt='' class='#{(additional_classes + ['theme-image-dark']).join(' ')}' src='#{content[:image_dark].sub('.webp', '-1024.webp')}' style='#{image_style}'>"
                        else
                            io.puts "<img alt='' class='#{additional_classes.join(' ')}' src='#{(content[:image] || '/images/white.webp').sub('.webp', '-1024.webp')}' style='#{image_style}'>"
                        end
                        io.puts "<div class='shade'></div>"
                        io.puts "<div class='card-content'>"
                        # Title and kind share one line; a kind that does not
                        # fit next to the title is left out (default.css).
                        io.puts "<div class='card-title-row'>"
                        io.puts "#{content[:dev_only] ? '<span class="badge badge-sm bg-danger">dev</span> ' : ''}<h4>#{content[:title]}</h4>"
                        io.puts StudentHome.card_badges(content[:kind], content[:language]) if catalog == :student
                        io.puts "</div>"
                        io.puts "<div class='abstract'>#{content[:abstract]}</div>"
                        io.puts "</div>"
                        io.puts "</div>"
                        io.puts "</a>"
                        io.puts "</div>"
                    end
                    io.puts "</div>"
                end
            else
                io.puts "<div style='text-align: center;'>"
                io.puts "<h2>Leistungsüberprüfung</h2>"
                io.puts "<p style='text-align: center;'>"
                io.puts "Momentan läuft eine Leistungsüberprüfung. Klicke auf den entsprechenden Button, um den dazugehörigen Workspace zu öffnen."
                io.puts "</p>"
                running_tests.each do |entry|
                    io.puts "<p style='text-align: center;'><button class='btn btn-success bu-launch-test' data-tag='#{entry[:tag]}'><i class='bi bi-code-slash'></i>Leistungsüberprüfung bei #{entry[:teacher]}</button></p>"
                end
                io.puts "</div>"
            end
            io.string
        end
        doc = Nokogiri::HTML::DocumentFragment.parse(html)
        Main.inject_autotoc(doc, inject_numbers: false)
        doc.to_html
    end

    # Use the same server-side visibility rule for the initial table and the
    # live WebSocket updates. A teacher may view only their assigned groups.
    # A teacher sees the workspace of a student in any group it administers,
    # not only in the student's first one (GroupVisibility).
    def workspace_user_visible_to?(email, viewer_email)
        return false unless email

        GroupVisibility.visible?(email, :group_members => @@group_members,
            :viewer_groups => @@teachers[viewer_email] || Set.new(),
            :admin => ADMIN_USERS.include?(viewer_email))
    end

    def print_workspaces()
        assert(teacher_logged_in?)

        cleanup_login_requests

        email_for_tag = {}
        registered_emails = []
        neo4j_query(<<~END_OF_STRING).each do |row|
            MATCH (u:User) RETURN u.email;
        END_OF_STRING
            email = row['u.email']
            email_for_tag[fs_tag_for_email(email)] = email
            registered_emails << email
        end
        du_for_fs_tag = {}
        begin
            du_for_fs_tag = JSON.parse(File.read('/internal/du_for_fs_tag.json'))
        rescue
        end

        STDERR.puts "email_for_tag: #{email_for_tag.to_yaml}"

        info_for_tag = workspace_runtime.workspace_network_info(
            :timeout => shell_timeout(:docker_inspect),
        )

        StringIO.open do |io|
            io.puts "<div style='max-width: 100%; overflow-x: auto;'>"
            io.puts "<table class='table table-sm' id='table_admin_workspaces'>"
            active_users = Dir['/user/*'].map do |path|
                path.split('/').last
            end.select do |user_tag|
                user_tag =~ /^[0-9a-z]{16}$/
            end
            active_users = Set.new(active_users)
            # Each student once, under the first of its groups this teacher
            # administers (all groups for an administrator): a student in
            # Klasse 12 and in the LK appears for both teachers.
            viewer_groups = @@teachers[@session_user[:email]] || Set.new()
            listed = Set.new()
            @@user_group_order.each do |group|
                next unless admin_logged_in? || viewer_groups.include?(group)
                sub = StringIO.open do |io2|
                    ((@@user_groups[group] || []) | (@@group_members[group] || [])).each do |email|
                        next if listed.include?(email)
                        listed << email
                        user_tag = fs_tag_for_email(email)
                        next unless active_users.include?(user_tag)
                        io2.puts "<tr id='tr_hs_code_#{user_tag}'>"
                        io2.puts "<td><code>#{user_tag}</code></td>"
                        io2.puts "<td>#{@@invitations[email_for_tag[user_tag]][:name]}</td>"
                        # io2.puts "<td class='td_ip'>#{(info_for_tag[user_tag] || {})[:ip] || '&ndash;'}</td>"
                        io2.puts "<td class='td_cpu'>&ndash;</td>"
                        io2.puts "<td class='td_ram'>&ndash;</td>"
                        io2.puts "<td class='td_net'>&ndash;</td>"
                        io2.puts "<td class='td_disk'>&ndash;</td>"
                        if du_for_fs_tag[user_tag]
                            io2.puts "<td>#{bytes_to_str(du_for_fs_tag[user_tag] * 1024).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')}</td>"
                        else
                            io2.puts "<td>&ndash;</td>"
                        end
                        io2.puts "<td class='td_pid'>&ndash;</td>"
                        io2.puts "<td><button class='btn btn-sm btn-success bu-open-workspace-as-admin' data-email='#{email_for_tag[user_tag]}'><i class='bi bi-code-slash'></i>&nbsp;Workspace öffnen</button></td>"
                        if admin_logged_in?
                            io2.puts "<td><button class='btn btn-sm btn-warning bu-impersonate' data-email='#{email_for_tag[user_tag]}'><i class='bi bi-person-vcard'></i>&nbsp;Impersonate</button></td>"
                        end
                        io2.puts "</tr>"
                    end
                    io2.string
                end
                unless sub.empty?
                    io.puts "<thead>"
                    io.puts "<tr><th colspan='#{admin_logged_in? ? 10 : 9}' style='background-color: rgba(0,0,0,0); padding: 1em 0;'><h4>#{group}</h4></th></tr>"
                    io.puts "<tr>"
                    io.puts "<th>Tag</th>"
                    io.puts "<th>Name</th>"
                    # io.puts "<th>IP</th>"
                    io.puts "<th style='width: 5.2em;'>CPU</th>"
                    io.puts "<th>RAM</th>"
                    io.puts "<th>Network</th>"
                    io.puts "<th>Disk I/O</th>"
                    io.puts "<th>Storage</th>"
                    io.puts "<th>#PID</th>"
                    io.puts "<th>Workspace</th>"
                    if admin_logged_in?
                        io.puts "<th>Impersonate</th>"
                    end
                    io.puts "</tr>"
                    io.puts "</thead>"
                    io.puts sub
                end
            end
            io.puts "</table>"
            io.puts "</div>"
            io.string
        end
    end

    def stub
        StringIO.open do |io|
            io.puts "<em>Dieser Artikel ist noch nicht vollständig. Schreib eine E-Mail an <a href='mailto:specht@gymnasiumsteglitz.de'>specht@gymnasiumsteglitz.de</a>, wenn du dabei helfen möchtest, ihn fertigzustellen.</em>"
            io.string
        end
    end

    def broadcast_login_codes
        clients = @@admin_ws_mutex.synchronize do
            @@clients.map do |client_id, ws|
                [ws, @@email_for_client_id[client_id]]
            end
        end
        return if clients.empty?

        lines = []
        active_params = {
            :now => Time.now.to_i,
            :max_attempts => Authentication::LOGIN_REQUEST_MAX_ATTEMPTS,
        }
        neo4j_query(<<~END_OF_STRING, active_params).each do |row|
            MATCH (l:LoginRequest)-[:FOR]->(u:User)
            WHERE l.expires_at > $now
              AND COALESCE(l.attempts, 0) < $max_attempts
            RETURN l.code, l.expires_at, u.email
            ORDER BY l.expires_at;
        END_OF_STRING
            email = row['u.email']
            next if @@teachers.include?(email)
            lines << {
                :email => email,
                :code => row['l.code'],
                :expires_at => row['l.expires_at'].to_i,
            }
        end
        # Every teacher of one of the student's groups sees the code, with
        # the groups it is for; the page lets a teacher of several groups
        # choose which to show.
        clients.each do |ws, ws_email|
            shown = GroupVisibility.login_codes_for(lines,
                :group_members => @@group_members, :group_order => @@user_group_order,
                :viewer_groups => @@teachers[ws_email] || Set.new(),
                :admin => ADMIN_USERS.include?(ws_email))
            ws.send({:action => 'login_codes', :lines => shown[:lines], :groups => shown[:groups]}.to_json)
        end
    end

    get '/ws' do
        assert(teacher_logged_in?)
        if Faye::WebSocket.websocket?(request.env)
            ws = Faye::WebSocket.new(request.env)

            viewer_email = @session_user[:email]
            ws.on(:open) do |event|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                ws.send({:hello => 'world'})
                @@admin_ws_mutex.synchronize do
                    @@clients[client_id] = ws
                    @@email_for_client_id[client_id] = @session_user[:email]
                    @@client_ids_for_email[@session_user[:email]] ||= []
                    @@client_ids_for_email[@session_user[:email]] << client_id
                end
                email_for_tag = {}
                neo4j_query(<<~END_OF_STRING).each do |row|
                    MATCH (u:User) RETURN u.email;
                END_OF_STRING
                    email = row['u.email']
                    email_for_tag[fs_tag_for_email(email)] = email
                end
                docker_stats_thread = Thread.new do
                    loop do
                        lines = {}
                        workspace_runtime.workspace_stats.each_pair do |fs_tag, stat_line|
                            email = email_for_tag[fs_tag]
                            next unless workspace_user_visible_to?(email, viewer_email)
                            lines[fs_tag] = {
                                :name => (@@invitations[email] || {})[:name],
                                :group => (@@invitations[email] || {})[:group],
                                :stats => stat_line
                            }
                        end
                        ws.send({:stats => lines}.to_json)
                        sleep 1
                    end
                end
                # The server's figures (CPU, RAM, storage): on the admin page,
                # and for teachers above their students' Workspaces (class page).
                host_stats_thread = Thread.new do
                    loop do
                        data = {}

                        mpstat = `mpstat 1 1 | grep 'Average' |grep 'all'`.strip
                        parts = mpstat.split(/\s+/)
                        cpu_idle = (parts.last.to_f * 100).to_i / 100.0
                        cpu_used = ((100.0 - cpu_idle) * 100).to_i / 100.0
                        data[:cpu_usage] = cpu_used

                        num_cores = `grep -c ^processor /proc/cpuinfo`.to_i
                        data[:num_cores] = num_cores
                        meminfo = `free -k | grep 'Mem:'`

                        parts = meminfo.split(/\s+/)
                        ram_total = parts[1].to_i * 1024
                        ram_available = parts[6].to_i * 1024
                        ram_used = ram_total - ram_available
                        data[:ram_total] = bytes_to_str(ram_total).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:ram_used] = bytes_to_str(ram_used).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:ram_percent] = (ram_used * 100.0 / ram_total * 100).to_i.to_f / 100

                        diskinfo = `df -kP /`.split("\n").last.strip
                        parts = diskinfo.split(/\s+/)
                        disk_total = parts[1].to_i * 1024
                        disk_free = parts[3].to_i * 1024
                        disk_used = disk_total - disk_free
                        data[:disk_root_total] = bytes_to_str(disk_total).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:disk_root_used] = bytes_to_str(disk_used).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:disk_root_percent] = (disk_used * 100.0 / disk_total * 100).to_i.to_f / 100

                        diskinfo = `df -kP /user`.split("\n").last.strip
                        parts = diskinfo.split(/\s+/)
                        disk_total = parts[1].to_i * 1024
                        disk_free = parts[3].to_i * 1024
                        disk_used = disk_total - disk_free
                        data[:disk_user_total] = bytes_to_str(disk_total).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:disk_user_used] = bytes_to_str(disk_used).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:disk_user_percent] = (disk_used * 100.0 / disk_total * 100).to_i.to_f / 100

                        ws.send({:server_stats => data}.to_json)

                        sleep 5
                    end
                end
                keep_threads = @@admin_ws_mutex.synchronize do
                    if @@clients.include?(client_id)
                        @@threads_for_client_id[client_id] = {
                            :docker_stats => docker_stats_thread,
                            :host_stats => host_stats_thread,
                        }
                        true
                    else
                        false
                    end
                end
                unless keep_threads
                    docker_stats_thread.kill
                    host_stats_thread.kill if host_stats_thread
                end
                broadcast_login_codes()
                client_count = @@admin_ws_mutex.synchronize { @@clients.size }
                debug "Got #{client_count} connected clients!"
            end

            ws.on(:close) do |event|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                threads = nil
                client_count = @@admin_ws_mutex.synchronize do
                    @@clients.delete(client_id)
                    @@email_for_client_id.delete(client_id)
                    client_ids = @@client_ids_for_email[@session_user[:email]]
                    if client_ids
                        client_ids.delete(client_id)
                        @@client_ids_for_email.delete(@session_user[:email]) if client_ids.empty?
                    end
                    threads = @@threads_for_client_id.delete(client_id)
                    @@clients.size
                end
                (threads || {}).each_value { |thread| thread.kill if thread }
                debug "Got #{client_count} connected clients!"
            end

            ws.on(:message) do |msg|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                begin
                    request = {}
                    unless msg.data.empty?
                        request = JSON.parse(msg.data)
                    end
                    if request['hello'] == 'world'
                        ws.send({:status => 'welcome'}.to_json)
                    end
                rescue StandardError => e
                    STDERR.puts e
                end
            end
            ws.rack_response
        end
    end

    # get '/json' do
    #     params = request.params
    #     STDERR.puts params.to_yaml
    #     if params['fn'] == 'version'
    #     elsif params['fn'] == 'dir'
    #         respond(:folders => [{:name => 'games'}], :files => [])
    #     end
    # end

    get '/export/1.1/:tag' do
        tag = params['tag']
        assert(%w(html linux mac rpi win winxp).include?(tag))
        redirect "#{WEB_ROOT}/tic80/export/1.1/#{tag}"
    end

    # TIC-80's files live in the person's Workspace under /workspace/TIC-80;
    # the browser bridge (docker/tic80/hackschule-persistence.js) loads them
    # and sends each changed file back (Tic80Storage).
    TIC80_WORKSPACE_DIR = 'TIC-80'
    TIC80_MAX_REQUEST_BYTES = 2 * Tic80Storage::MAX_TOTAL_BYTES

    def tic80_with_storage(email)
        container_name = fs_tag_for_email(email)
        workspace_path = File.join('/user', container_name, 'workspace')
        metadata_path = File.join('/internal', 'tic80', container_name)
        ensure_user_directory(workspace_path)
        FileUtils.mkdir_p(metadata_path)
        root = File.join(workspace_path, TIC80_WORKSPACE_DIR)

        File.open(File.join(metadata_path, 'sync.lock'), File::RDWR | File::CREAT, 0o600) do |lock|
            lock.flock(File::LOCK_EX)
            Tic80Storage.prepare_root(root, :uid => WORKSPACE_UID, :gid => WORKSPACE_GID)
            yield root
        ensure
            lock.flock(File::LOCK_UN) rescue nil
        end
    end

    def tic80_json_body
        length = request.content_length.to_i
        halt 413, { :error => 'tic80_request_too_large' }.to_json if length > TIC80_MAX_REQUEST_BYTES
        body = request.body.read(TIC80_MAX_REQUEST_BYTES + 1).to_s
        halt 413, { :error => 'tic80_request_too_large' }.to_json if body.bytesize > TIC80_MAX_REQUEST_BYTES
        data = JSON.parse(body, :symbolize_names => true)
        halt 400, { :error => 'invalid_tic80_request' }.to_json unless data.is_a?(Hash)
        data
    rescue JSON::ParserError
        halt 400, { :error => 'invalid_json' }.to_json
    end

    # All files of /workspace/TIC-80, with contents; ?meta=1 without.
    get '/api/tic80/fs' do
        assert(user_logged_in?)
        tic80_with_storage(@session_user[:email]) do |root|
            respond(Tic80Storage.tree(root, :contents => params['meta'] != '1'))
        end
    end

    # The contents of some files: { paths: [...] }.
    post '/api/tic80/fs_read' do
        assert(user_logged_in?)
        data = tic80_json_body
        halt 400, { :error => 'invalid_tic80_request' }.to_json unless data[:paths].is_a?(Array)
        wanted = data[:paths].first(1000).map { |p| Tic80Storage.relative_path(p) rescue nil }.compact.to_set
        tic80_with_storage(@session_user[:email]) do |root|
            respond(Tic80Storage.tree(root, :contents => true, :only => wanted))
        end
    end

    # The files TIC-80 changed, each with the version it started from
    # (Tic80Storage.apply): nothing is overwritten that the browser has not
    # seen.
    post '/api/tic80/fs_changes' do
        assert(user_logged_in?)
        data = tic80_json_body
        tic80_with_storage(@session_user[:email]) do |root|
            begin
                respond(Tic80Storage.apply(root, data[:changes], :uid => WORKSPACE_UID, :gid => WORKSPACE_GID))
            rescue Tic80Storage::Invalid => e
                content_type :json
                halt 400, { :error => 'invalid_tic80_change', :detail => e.message }.to_json
            end
        end
    end

    get '/api/ping' do
        respond(:pong => 'yeah')
    end

    post '/api/convert_keras_modal_to_tensorflowjs' do
        file = params['file']
        filename = file['filename']
        assert(!filename.include?('/'))
        assert(filename.size > 0)
        blob = file['tempfile'].read
        sha1 = Digest::SHA1.hexdigest(blob)[0, 16]
        STDERR.puts "Read #{blob.size} bytes with SHA1 #{sha1}"
        path = "/internal/tfjs/#{sha1}"
        FileUtils.mkpath(File.dirname(path))
        File.open(path, 'w') do |f|
            f.write(blob)
        end
        out_path = File.join(File.dirname(path), "#{sha1}-out")
        FileUtils.mkpath(out_path)
        system("docker exec workspace_tensorflowjs_1 tensorflowjs_converter --input_format=keras \"#{path}\" \"#{out_path}\"")
        files = []
        Dir[File.join(out_path, '*')].each do |path|
            files << { :name => File.basename(path), :contents => Base64::encode64(File.read(path)) }
        end
        # FileUtils.rm(path)
        # FileUtils.rm_rf(out_path)
        respond(:uploaded => 'yeah', :files => files)
    end

    post '/api/toggle_show_module' do
        assert(user_logged_in?)

        params = parse_request_data(:required_keys => [:module])
        key = params[:module].to_sym
        assert(MODULE_ORDER.include?(key))

        flag = @session_user["show_#{key}".to_sym] == true
        new_flag = !flag

        email = @session_user[:email]
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :value => new_flag})
            MATCH (u:User {email: $email})
            SET u.show_#{key} = $value
            RETURN u;
        END_OF_STRING

        respond(:yay => 'sure')
    end

    # Checks an answer and, if it counts, remembers it (docs/aufgaben.md).
    post '/api/task/check' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task, :answer],
            :max_body_length => 16384,
            :max_value_lengths => {:task => 120, :answer => TaskStore::MAX_ANSWER})
        found = find_task(data[:task])
        halt 404 if found.nil?

        result = TaskCheck.check(found[:task], data[:answer],
            :variables => task_variables(found[:task], found[:key]),
            :workspace => task_workspace_files,
            :git => lambda { |repo| task_git_state(repo) },
            :code_solved => lambda { |codebite| codebite_solved_languages(@session_user[:email], codebite) },
            :runner => lambda { |command, seconds|
                workspace_runtime.run_command(fs_tag_for_email(@session_user[:email]), :command => command,
                    :uid => "#{WORKSPACE_UID}:#{WORKSPACE_GID}", :seconds => [[seconds.to_i, 1].max, 30].min)
            },
            # What the person typed runs as a guest: it can read /workspace but
            # not change or delete anything there (rule 1, checks only read).
            :guest_runner => lambda { |command, seconds|
                workspace_runtime.run_command(fs_tag_for_email(@session_user[:email]), :command => command,
                    :uid => GUEST_UID, :home => '/tmp', :seconds => [[seconds.to_i, 1].max, 30].min)
            },
            :sql => %w[sql schema].include?(found[:task]['check']['type']) ? task_sql_runner : nil,
            :sql_env => %w[sql schema].include?(found[:task]['check']['type']) ? task_sql_env : {},
            :cypher => found[:task]['check']['type'] == 'cypher' ? task_cypher_runner : nil)
        if result.problem == :reference_failed
            STDERR.puts "SQL task #{found[:key]}: the reference query failed for #{@session_user[:email]}"
        end
        answer = data[:answer]
        if (plan = result.plan)
            # submit: copy the files now; the stored answer says what was handed in.
            files = plan.files.size
            answer = [data[:answer].to_s.strip, "#{files == 1 ? '1 Datei' : "#{files} Dateien"}, #{SubmissionStore.size_text(plan.total)}"].reject(&:empty?).join(' · ')
            begin
                SubmissionStore.write(task_workspace_files, plan, submission_path(found[:key]))
            rescue StandardError => e
                STDERR.puts "Submission #{found[:key]} failed: #{e.class}: #{e.message}"
                result = TaskCheck::Result.new(:status => :unavailable,
                    :message => 'Die Abgabe hat nicht geklappt, vermutlich hat sich gerade eine Datei geändert. Versuch es gleich noch einmal.')
            end
        end
        state = result.counts? ? store_task_result(found, result.status.to_s, answer) : nil
        respond(
            :status => result.status.to_s,
            :message => render_task_markdown(result.message),
            :solution => render_task_markdown(result.solution),
            :read => render_task_markdown(result.read),
            :warnings => (result.warnings || []).map { |w| render_task_markdown(w) },
            :state => state,
        )
    end

    # Files a submit task with pick: offers from the student's Workspace.
    post '/api/task/options' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task], :max_value_lengths => {:task => 120})
        found = find_task(data[:task])
        halt 404 if found.nil? || found[:task]['check']['pick'].nil?

        ws = task_workspace_files
        options = ws ? SubmissionStore.candidates(ws, found[:task]['check']['pick']) : []
        respond(:options => options)
    end

    # A submission as .tar.gz: the logged-in person's own, or, with ?email=,
    # that of a student in a group the logged-in teacher looks after.
    # Whose hand-in a request may see: one's own, or (email=…) one of a
    # group the logged-in teacher looks after.
    def submission_owner_for(params)
        email = @session_user[:email]
        if params['email'] && params['email'] != email
            Main.load_invitations()
            allowed = class_groups.any? { |g| (@@group_members[g] || []).include?(params['email']) }
            halt 403 unless allowed
            email = params['email']
        end
        email
    end

    get '/api/task/submission/:activity/:id' do
        assert(user_logged_in?)
        found = find_task("#{params[:activity]}/#{params[:id]}")
        halt 404 if found.nil?
        email = submission_owner_for(params)
        path = submission_path(found[:key], email)
        halt 404 unless File.file?(path)

        suffix = email == @session_user[:email] ? '' : "-#{email.split('@').first.gsub(/[^a-zA-Z0-9._-]/, '')}"
        respond_raw_with_mimetype_and_filename(File.binread(path), 'application/gzip',
            "#{params[:activity]}-#{params[:id]}#{suffix}.tar.gz")
    end

    # The viewer on the class page: the files of a hand-in and the signed
    # address under which they run on abgaben.<host>.
    get '/api/task/submission_view/:activity/:id' do
        assert(user_logged_in?)
        found = find_task("#{params[:activity]}/#{params[:id]}")
        halt 404 if found.nil?
        email = submission_owner_for(params)
        path = submission_path(found[:key], email)
        halt 404 unless File.file?(path)

        token = SubmissionView.token(TaskStore.state_key(email, found[:key]), :secret => SUBMISSION_VIEW_SECRET)
        files = SubmissionView.files(path).map do |f|
            f.merge(:text => SubmissionView.text?(f[:path]) && f[:size] <= SubmissionView::MAX_TEXT,
                    :html => SubmissionView.html?(f[:path]))
        end
        response.headers['Cache-Control'] = 'no-store'
        respond(:files => files, :root => "#{SUBMISSION_VIEW_ROOT}/v/#{token}/",
            :download => "/api/task/submission/#{params[:activity]}/#{params[:id]}#{email == @session_user[:email] ? '' : "?email=#{CGI.escape(email)}"}")
    end

    # The text of one file of a hand-in, for the source view.
    get '/api/task/submission_text/:activity/:id' do
        assert(user_logged_in?)
        found = find_task("#{params[:activity]}/#{params[:id]}")
        halt 404 if found.nil?
        email = submission_owner_for(params)
        archive = submission_path(found[:key], email)
        file = SubmissionView.clean_path(params['path'])
        halt 404 unless File.file?(archive) && file && SubmissionView.text?(file)
        bytes = SubmissionView.read(archive, file, :max => SubmissionView::MAX_TEXT)
        halt 404 if bytes.nil?
        response.headers['Cache-Control'] = 'no-store'
        respond(:text => bytes.force_encoding('UTF-8').scrub('�'))
    end

    # abgaben.<host>/v/<token>/<path>: a file of a hand-in, sandboxed
    # (SubmissionView). The token is the only permission; no cookies.
    get '/v/:token/*' do
        halt 404 unless request.host == SUBMISSION_VIEW_HOST
        state_key = SubmissionView.verify(params[:token], :secret => SUBMISSION_VIEW_SECRET)
        halt 404 if state_key.nil?
        archive = File.join(SUBMISSIONS_ROOT, "#{state_key}.tar.gz")
        halt 404 unless File.file?(archive)
        rel = params['splat'].first.to_s
        wanted = rel.empty? ? '' : SubmissionView.clean_path(rel)
        halt 404 if wanted.nil?
        file = SubmissionView.resolve(SubmissionView.files(archive), wanted)
        halt 404 if file.nil?
        # A folder without its slash: relative addresses inside need it.
        redirect "/v/#{params[:token]}/#{file}" if file != wanted && !rel.end_with?('/')
        bytes = SubmissionView.read(archive, file)
        halt 404 if bytes.nil?
        SubmissionView.headers(file, WEB_ROOT).each { |k, v| response.headers[k] = v }
        respond_raw_with_mimetype(SubmissionView.for_viewer(file, bytes), SubmissionView.content_type(file))
    end

    # The Anaglyph viewer on the site (AnaglyphViewer, check option preview:
    # anaglyph): pictures of handed-in scenes on the task card and in the class
    # gallery. vorschau/ is the file someone is about to hand in, read from
    # their Workspace; abgaben/ a handed-in scene (ich: one's own), for that
    # person and the teachers of their groups. Everything else is the viewer
    # itself and the Kenney kits it loads.
    get '/anaglyph-viewer/*' do
        assert(user_logged_in?)
        rel = params['splat'].first.to_s
        if (m = rel.match(%r{\Avorschau/([a-z0-9_-]+)/([^/]+)/([^/]+\.ini)\z}))
            found = anaglyph_preview_task("#{m[1]}/#{m[2]}")
            ws = task_workspace_files
            halt 404 if ws.nil?
            pick = found[:task]['check']['pick']
            dir = File.dirname(pick)
            path = dir == '.' ? m[3] : "#{dir}/#{m[3]}"
            halt 404 unless SubmissionStore.candidates(ws, pick).include?(path)
            text = begin
                ws.read_text(path, max: 1_000_000)
            rescue StandardError
                nil
            end
            halt 404 if text.nil?
            response.headers['Cache-Control'] = 'no-store'
            respond_raw_with_mimetype(text, 'text/plain; charset=utf-8')
        elsif (m = rel.match(%r{\Aabgaben/([a-z0-9_-]+)/([^/]+)/(ich|[0-9a-f]{32})\.ini\z}))
            found = anaglyph_preview_task("#{m[1]}/#{m[2]}")
            email = anaglyph_submission_owner(found[:key], m[3])
            halt 404 if email.nil?
            text = begin
                AnaglyphViewer.scene_from_submission(submission_path(found[:key], email))
            rescue StandardError
                nil
            end
            halt 404 if text.nil?
            response.headers['Cache-Control'] = 'no-store'
            respond_raw_with_mimetype(text, 'text/plain; charset=utf-8')
        else
            path = begin
                AnaglyphViewer.file(rel)
            rescue StandardError => e
                STDERR.puts "Anaglyph viewer: #{e.message}"
                halt 503
            end
            halt 404 if path.nil?
            response.headers['Cache-Control'] = 'public, max-age=3600'
            respond_raw_with_mimetype(File.binread(path), AnaglyphViewer.mime(path))
        end
    end

    # Teachers: run a Workspace task (TaskCheck::LIVE_TYPES) for
    # every student of a group right now. Reads only; stores nothing, so the
    # students' own results stay as they are.
    post '/api/class/check_now' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :task], :max_value_lengths => {:group => 200, :task => 120})
        halt 403 unless class_groups.include?(data[:group])
        found = find_task(data[:task])
        halt 404 if found.nil?
        type = found[:task]['check']['type']
        halt 400 unless TaskCheck::LIVE_TYPES.include?(type)

        members = class_members(data[:group])
        results = {}
        lock = Mutex.new
        queue = Queue.new
        members.each { |m| queue << m }
        workers = Array.new([4, members.size].min) do
            Thread.new do
                loop do
                    m = begin
                        queue.pop(true)
                    rescue ThreadError
                        break
                    end
                    fs_tag = fs_tag_for_email(m[:email])
                    path = "/user/#{fs_tag}/workspace"
                    ws = File.directory?(path) ? WorkspaceFiles.new(path) : nil
                    git = lambda do |repo|
                        output = workspace_runtime.git_status(fs_tag, :repo => repo,
                            :uid => "#{WORKSPACE_UID}:#{WORKSPACE_GID}", :script => GitCheck::STATUS_SCRIPT, :timeout => 30,
                            :host_data_path => PATH_TO_HOST_DATA)
                        GitCheck.complete?(output) ? GitCheck.parse(output) : nil
                    end
                    result = begin
                        TaskCheck.check(found[:task], '', :variables => {}, :workspace => ws, :git => git)
                    rescue StandardError => e
                        STDERR.puts "check_now #{found[:key]} #{m[:email]}: #{e.class}: #{e.message}"
                        TaskCheck::Result.new(:status => :unavailable, :message => 'Konnte nicht geprüft werden.')
                    end
                    lock.synchronize do
                        results[m[:email]] = {
                            :status => result.status.to_s,
                            :problem => result.problem.to_s,
                            :message => result.message,
                        }
                    end
                end
            end
        end
        workers.each(&:join)
        # Markdown is rendered here, not in the threads: one renderer is shared.
        results.each_value { |r| r[:message] = render_task_markdown(r[:message]) }
        respond(:results => results, :checked_at => Time.now.to_i)
    end

    # Is the logged-in person's clone of the page's template out of date?
    # Looks only; the notice tells them to run git pull themselves.
    post '/api/template/status' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:slug], :max_value_lengths => {:slug => 120})
        template = (@@content[data[:slug]] || {})[:template]
        halt 404 if template.nil?

        status = template_status_for(@session_user[:email], template)
        respond(:status => status.to_s,
            :notice => status == :outdated ? render_task_markdown(TemplateFreshness.notice(template)) : nil)
    end

    # Teachers: the same look for every student of a group. Stores nothing,
    # tells no one; the teacher decides whether to say »git pull«.
    post '/api/class/template_status' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :slug], :optional_keys => [:force],
            :max_value_lengths => {:group => 200, :slug => 120})
        halt 403 unless class_groups.include?(data[:group])
        template = (@@content[data[:slug]] || {})[:template]
        halt 404 if template.nil?

        # force: ask the template's host now instead of the remembered answer.
        latest = data[:force] == 'yes' ? @@template_latest.refresh(template[:url]) : @@template_latest.get(template[:url])
        members = class_members(data[:group])
        results = {}
        if latest
            lock = Mutex.new
            queue = Queue.new
            members.each { |m| queue << m }
            workers = Array.new([4, members.size].min) do
                Thread.new do
                    loop do
                        m = begin
                            queue.pop(true)
                        rescue ThreadError
                            break
                        end
                        status = begin
                            template_status_for(m[:email], template)
                        rescue StandardError => e
                            STDERR.puts "template_status #{data[:slug]} #{m[:email]}: #{e.class}: #{e.message}"
                            :unavailable
                        end
                        lock.synchronize { results[m[:email]] = status.to_s }
                    end
                end
            end
            workers.each(&:join)
        end
        respond(:results => results, :reachable => !latest.nil?, :folder => template[:folder],
            :students_see_notice => tasks_enabled_for_everyone? ? true : false, :checked_at => Time.now.to_i)
    end

    # The logged-in person's stored answers for the tasks of one activity.
    post '/api/task/states' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:activity], :max_value_lengths => {:activity => 64})
        activity = data[:activity]
        assert(activity =~ /\A[a-z0-9_-]+\z/)
        entry = @@content.values.find { |c| (c[:tasks] || {})[:activity] == activity }
        halt 404 if entry.nil?

        states = {}
        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :prefix => "#{activity}/").each do |row|
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.task STARTS WITH $prefix
            RETURN s;
        END_OF_QUERY
            stored = row['s']
            id = stored[:task].to_s.split('/', 2)[1]
            task = entry[:tasks][:by_id][id]
            next if task.nil?
            states[id] = TaskStore.public_state(stored, TaskSpec.check_version(task))
        end
        respond(:states => states)
    end

    # Deletes the logged-in person's answer and attempts for one task. Always
    # allowed, also when tasks are switched off again.
    post '/api/task/delete' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:task], :max_value_lengths => {:task => 120})
        task = data[:task]
        assert(task =~ /\A[a-z0-9_-]+\/[a-z0-9-]+\z/)
        email = @session_user[:email]
        neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, task))
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            DETACH DELETE s;
        END_OF_QUERY
        neo4j_query(<<~END_OF_QUERY, :email => email, :task => task)
            MATCH (a:TaskAttempt {task: $task})-[:BY]->(:User {email: $email})
            DETACH DELETE a;
        END_OF_QUERY
        FileUtils.rm_f(submission_path(task, email))
        respond(:deleted => true)
    end

    # A new round: all hand-ins of one task in one group go, as if each person
    # had pressed »Abgabe löschen« (state, attempts, file). Only for the
    # group's teachers; only members of that group, not the teacher's own.
    post '/api/class/delete_submissions' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :task], :max_value_lengths => {:group => 200, :task => 120})
        halt 403 unless class_groups.include?(data[:group])
        found = find_task(data[:task])
        halt 404 if found.nil?
        halt 400 unless found[:task]['check']['type'] == 'submit'

        task = found[:key]
        emails = class_members(data[:group]).map { |m| m[:email] }
        deleted = emails.count { |email| File.file?(submission_path(task, email)) }
        neo4j_query(<<~END_OF_QUERY, :keys => emails.map { |email| TaskStore.state_key(email, task) })
            MATCH (:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.key IN $keys
            DETACH DELETE s;
        END_OF_QUERY
        neo4j_query(<<~END_OF_QUERY, :emails => emails, :task => task)
            MATCH (a:TaskAttempt {task: $task})-[:BY]->(u:User)
            WHERE u.email IN $emails
            DETACH DELETE a;
        END_OF_QUERY
        emails.each { |email| FileUtils.rm_f(submission_path(task, email)) }
        STDERR.puts "#{@session_user[:email]} deleted #{deleted} hand-ins of #{task} in #{data[:group]}"
        respond(:deleted => deleted)
    end

    def print_module_button(key)
        assert(user_logged_in?)
        active = @session_user["show_#{key}".to_sym]
        "<button class='btn btn-md #{active ? 'btn-success' : 'btn-outline-secondary'} bu-toggle-module' data-module='#{key}'><i class='bi #{active ? 'bi-check-lg' : 'bi-x-lg'}'></i>#{MODULE_LABELS[key]} im Menü #{active ? '' : 'nicht'} anzeigen</button>"
    end

    post '/api/set_brightness/:mode' do
        mode = params['mode']
        assert(%w(light dark auto).include?(mode))
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :mode => mode})
            MATCH (u:User {email: $email})
            SET u.brightness = $mode
            RETURN u;
        END_OF_STRING
    end

    # get '/pdf..*' do
    #     # Here's a hack to make live reload work with LaTeX PDFs
    #     # The LaTeX Workshop extensions recompiles the PDF on save
    #     # and then requests a funny URL to reload the PDF in the browser.
    #     # This route serves the main.pdf by looking for the latest
    #     # PDF file in the current user's workspace tree.

    #     # If the user uses a share tag, we need to look up the email
    #     # Otherwise, use the email of the logged in user

    #     referer_path = request.env['HTTP_REFERER']
    #     referer_path = URI.parse(referer_path).path
    #     share_tag = referer_path.split('/')[1]
    #     rows = neo4j_query(<<~END_OF_STRING, {:share_tag => share_tag})
    #         MATCH (u:User {share_tag: $share_tag})
    #         RETURN u.email;
    #     END_OF_STRING
    #     email = (rows.first || {})['u.email']
    #     email ||= (@session_user || {})[:email]
    #     assert(!(email.nil?))
    #     fs_tag = fs_tag_for_email(email)
    #     candidates = Dir["/user/#{fs_tag}/workspace/**/*.pdf"]
    #     candidates.sort! do |a, b|
    #         File.mtime(b) <=> File.mtime(a)
    #     end
    #     respond_with_file(candidates.first)
    # end

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
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => @session_user[:email]})
            MATCH (u:User {email: $email})<-[:BELONGS_TO]->(t:Test {tag: $tag})
            DETACH DELETE t;
        END_OF_STRING
        # The shared content-addressed VSIX cache is retained: other exams
        # may still reference these files.
        FileUtils.rm_f(File.join(TestWorkspaceExtensions::MANIFESTS, "#{data[:tag]}.json"))
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

    # Starting or stopping a real test only changes whether assigned students
    # are routed into their existing exam workspace. Their files and databases
    # must survive any number of stop/start cycles.
    post '/api/start_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]})
            MATCH (t:Test {tag: $tag})
            SET t.running = TRUE;
        END_OF_STRING
        Main.refresh_nginx_config()
    end

    post '/api/stop_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]})
            MATCH (t:Test {tag: $tag})
            REMOVE t.running;
        END_OF_STRING
        Main.refresh_nginx_config()
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

    def my_user_group_order
        if admin_logged_in?
            @@user_group_order
        else
            []
        end
    end

    def my_user_groups
        if admin_logged_in?
            @@user_groups
        else
            {}
        end
    end

    def my_invitations
        if admin_logged_in?
            @@invitations
        else
            {}
        end
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

    # SELECT
    #     table_name AS `table`,
    #     table_rows AS `rows`,
    #     data_length + index_length AS size
    # FROM
    #     information_schema.tables
    # WHERE
    #     table_schema = 'specht';

    # show create table specht.crew;

    post '/api/get_my_mysql_databases' do
        assert(user_logged_in?)
        client = Mysql2::Client.new(
            host: 'mysql',
            username: 'root',
            password: MYSQL_ROOT_PASSWORD
        )

        result = {}
        databases = []
        databases << @session_user[:db_login]
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
            RETURN d.name AS name;
        END_OF_STRING
            databases << row['name']
        end
        # Extra databases made before they had a login of their own get it
        # here, the first time the profile lists them.
        result[:logins] = {}
        result[:max_extra] = MYSQL_EXTRA_DATABASES_MAX
        databases.each do |database|
            next unless DatabaseProvisioning.extra_database?(database)

            Main.ensure_extra_database_login(client, @session_user[:email], database)
            result[:logins][database] = {
                :login => database,
                :password => Main.extra_database_password(@session_user[:email], database),
            }
        end
        databases.each do |database|
            result[:databases] ||= []
            result[:databases] << database
            result[:tables] ||= {}
            result[:tables][database] = {}
            result[:total] ||= {}
            result[:total][database] = {
                :rows => 0,
                :size => 0
            }
            client.query("SELECT table_name AS `table`, table_rows AS `rows`, data_length + index_length AS size FROM information_schema.tables WHERE table_schema = '#{database}';").each do |row|
                result[:tables][database][row['table']] = {
                    :rows => row['rows'],
                    :size => row['size'],
                }
                result[:total][database][:rows] += row['rows']
                result[:total][database][:size] += row['size']
            end
        end
        respond(:result => result)
    end

    MYSQL_EXTRA_DATABASES_MAX = 4

    post '/api/create_mysql_database' do
        assert(user_logged_in?)
        mysql_database_lock(@session_user[:email]).synchronize do
            count = neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email]})['count']
                MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
                RETURN COUNT(d) AS count;
            END_OF_STRING
            assert(count < MYSQL_EXTRA_DATABASES_MAX)
            database_name = "db_#{RandomTag.generate(12)}"
            client = Mysql2::Client.new(
                host: 'mysql',
                username: 'root',
                password: MYSQL_ROOT_PASSWORD
            )
            login = @session_user[:db_login]
            client.query("CREATE DATABASE #{database_name};")
            client.query("GRANT ALL ON `#{database_name}`.* TO '#{login}'@'%';")
            Main.ensure_extra_database_login(client, @session_user[:email], database_name)
            client.query("FLUSH PRIVILEGES;")
            neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :database_name => database_name})
                MATCH (u:User {email: $email})
                CREATE (d:Database {type: 'mysql', name: $database_name})<-[:HAS]-(u)
                RETURN d;
            END_OF_STRING
        end
        respond(:yay => 'sure')
    end

    # The password of an extra database's own login
    # (DatabaseProvisioning.extra_database_login_statements): derived, like
    # the others, so nothing has to be stored.
    def self.extra_database_password(email, database)
        gen_password_for_email("#{email}/#{database}", MYSQL_PASSWORD_SALT)
    end

    def self.ensure_extra_database_login(client, email, database)
        return unless DatabaseProvisioning.extra_database?(database)

        DatabaseProvisioning.extra_database_login_statements(database, extra_database_password(email, database))
            .each { |statement| client.query(statement) }
    end

    # Only one's own databases: the main one (db_login, reset instead) and
    # the ones created in the profile. Anything else was dropped before,
    # for any logged-in user who knew a name.
    def self.own_mysql_database?(email, db_login, name)
        return false unless name.is_a?(String) && name.match?(/\A[A-Za-z0-9_]{1,64}\z/)
        return true if name == db_login

        !$neo4j.neo4j_query(<<~END_OF_STRING, {:email => email, :name => name}).first.nil?
            MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql', name: $name})
            RETURN d.name;
        END_OF_STRING
    end

    post '/api/delete_mysql_database' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:database])
        mysql_database_lock(@session_user[:email]).synchronize do
            assert(Main.own_mysql_database?(@session_user[:email], @session_user[:db_login], data[:database]), 'not_your_database')
            client = Mysql2::Client.new(
                host: 'mysql',
                username: 'root',
                password: MYSQL_ROOT_PASSWORD
            )
            is_user_db = (data[:database] == @session_user[:db_login])
            client.query("DROP DATABASE IF EXISTS `#{data[:database]}`;")
            if DatabaseProvisioning.extra_database?(data[:database])
                client.query(DatabaseProvisioning.extra_database_drop_login_statement(data[:database]))
            end
            neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :database => data[:database]})
                MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql', name: $database})
                DETACH DELETE d;
            END_OF_STRING
            if is_user_db
                login = @session_user[:db_login]
                client.query("CREATE DATABASE `#{login}`;")
                client.query("GRANT ALL ON `#{login}`.* TO '#{login}'@'%';")
                client.query("FLUSH PRIVILEGES;")
            end
        end
    end

    @@codebite_content_mutex = Mutex.new

    def self.update_codebites
        @@codebite_content_mutex.synchronize do
            update_codebites_locked
        end
    end

    def self.update_codebites_locked
        # In development we want live reload: re-parse on every call.
        # In production we want to travel light: parse once per process.
        unless DEVELOPMENT
            return if defined?(@@codebite_sections) && @@codebite_sections &&
                defined?(@@codebite_tasks) && @@codebite_tasks
        end

        codebite_tasks = {}

        sections_path = "/src/codebites/tasks/tasks.yaml"
        codebite_sections = YAML.load_file(sections_path)

        # Parse embedded @@sections from single-file tasks without executing the Ruby code.
        section_marker = "\n__END__\n"

        parse_meta = lambda do |sections|
            # Prefer meta.yaml, fallback to meta.json
            if (y = sections["meta.yaml"]) && !y.strip.empty?
                begin
                    meta = YAML.safe_load(y) || {}
                    meta.is_a?(Hash) ? meta : {}
                rescue
                    {}
                end
            else
                {}
            end
        end

        parse_sections = lambda do |text|
            out = {}
            current = nil
            buf = +""

            text.each_line do |line|
                if line.start_with?("@@")
                    out[current] = buf.rstrip + "\n" if current
                    current = line.strip.sub("@@", "")
                    buf = +""
                else
                    buf << line
                end
            end

            out[current] = buf.rstrip + "\n" if current
            out
        end

        extract_title = lambda do |md|
            return nil if md.nil? || md.strip.empty?
            md.each_line do |line|
                if line =~ /^\s*\#{1,2}\s+(.+?)\s*$/
                    return Regexp.last_match(1).strip
                end
            end
            nil
        end

        codebite_sections.each do |section|
            section.fetch('entries', []).each do |task_id|
                task_rb_path = "/src/codebites/tasks/#{task_id}.rb"
                next unless File.exist?(task_rb_path)

                content = File.read(task_rb_path)
                idx = content.index(section_marker)
                sections_text = idx ? content[(idx + section_marker.length)..] : ""
                sections = parse_sections.call(sections_text)

                md = sections['task.md'] || ""
                heading = extract_title.call(md) || task_id

                meta = parse_meta.call(sections)
                difficulty = meta["difficulty"] || meta[:difficulty]
                difficulty = difficulty.to_i if difficulty
                difficulty = nil unless difficulty && difficulty.between?(1, 5)

                codebite_tasks[task_id] = {
                    mtime: File.mtime(task_rb_path),
                    heading: heading,
                    difficulty: difficulty,
                    section_key: section['key'],
                    section_title: section['title'],
                }
            end
        end

        @@codebite_sections = codebite_sections
        @@codebite_tasks = codebite_tasks
    end

    private_class_method :update_codebites_locked

    post '/api/codebites_get_tasks' do
        Main.update_codebites()
        result = {
            :sections => @@codebite_sections,
            :tasks => @@codebite_tasks,
        }
        if user_logged_in?
            result[:solved_tasks] = {}
            neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).map do |row|
                MATCH (s:Solved)-[:FOR]->(t:Task), (s)-[:BY]->(u:User {email: $email}), (s)-[:IN]->(l:Language)
                RETURN DISTINCT t.name, l.name;
            END_OF_STRING
                result[:solved_tasks][row['t.name']] ||= []
                result[:solved_tasks][row['t.name']] << row['l.name']
            end
        end
        respond(result)
    end

    post '/api/get_codebite' do
        data = parse_request_data(:required_keys => [:task])
        Main.update_codebites
        task = data[:task]
        assert(!(task.include?('/') || task.include?('.')))
        result = {}
        task_rb_path = "/src/codebites/tasks/#{task}.rb"
        halt 404 unless File.exist?(task_rb_path)

        content = File.read(task_rb_path)
        section_marker = "\n__END__\n"
        idx = content.index(section_marker)
        sections_text = idx ? content[(idx + section_marker.length)..] : ""

        # Parse @@sections (same mini format as the runner)
        sections = {}
        current = nil
        buf = +""
        sections_text.each_line do |line|
            if line.start_with?("@@")
                sections[current] = buf.rstrip + "\n" if current
                current = line.strip.sub("@@", "")
                buf = +""
            else
                buf << line
            end
        end
        sections[current] = buf.rstrip + "\n" if current

        md = sections['task.md'] || ""
        redcarpet = Redcarpet::Markdown.new(Redcarpet::Render::HTML, {:fenced_code_blocks => true})
        result[:section_key] = @@codebite_tasks[task][:section_key]
        result[:section_title] = @@codebite_tasks[task][:section_title]
        result[:problem] = redcarpet.render(md)
        result[:preferred_language] = neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email]})['preferred_language']
            MATCH (u:User {email: $email})
            RETURN COALESCE(u.preferred_language, "ruby") AS preferred_language;
        END_OF_STRING
        result[:solved] = neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :task => task}).map { |x| x['lang'] }
            MATCH (u:User {email: $email})<-[:BY]-(s:Solved)-[:FOR]->(t:Task {name: $task}), (s)-[:IN]->(l:Language)
            RETURN DISTINCT l.name AS lang;
        END_OF_STRING
        result[:starter] ||= {}
        {
            'ruby' => 'starter.rb',
            'python' => 'starter.py',
            'javascript' => 'starter.js',
        }.each do |lang, key|
            val = sections[key]
            result[:starter][lang] = val if val && !val.strip.empty?
        end
        respond(:result => result)
    end

    post '/api/set_preferred_language' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:language])
        language = data[:language]
        assert(%w(ruby python javascript).include?(language))
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :language => language})
            MATCH (u:User {email: $email})
            SET u.preferred_language = $language
            RETURN u;
        END_OF_STRING
        respond(:yay => 'sure')
    end

    get '/ws_codebites' do
        assert(user_logged_in?)
        if Faye::WebSocket.websocket?(request.env)
            ws = Faye::WebSocket.new(request.env)

            ws.on(:open) do |_|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                email = @session_user[:email]

                @@codebite_mutex.synchronize do
                    @@codebite_ws_clients[email] ||= {}
                    @@codebite_ws_clients[email][client_id] = ws
                end

                ws.send({ action: "codebite_ws_ready" }.to_json)
            end

            ws.on(:close) do |_|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                email = @session_user[:email]

                @@codebite_mutex.synchronize do
                    if @@codebite_ws_clients[email]
                        @@codebite_ws_clients[email].delete(client_id)
                        @@codebite_ws_clients.delete(email) if @@codebite_ws_clients[email].empty?
                    end
                end
            end

            ws.on(:message) do |msg|
                # Optional: handle ping or client-side subscription messages later
                # For now we can ignore or accept {"ping":true}
                begin
                    data = msg.data.to_s
                    if !data.empty?
                        obj = JSON.parse(data) rescue {}
                        if obj["ping"]
                            ws.send({ action: "pong" }.to_json)
                        end
                    end
                rescue
                end
            end

            return ws.rack_response
        end
        halt 400
    end

    post '/api/codebite_load_submissions' do
        assert(user_logged_in?)
        data = parse_request_data(required_keys: [:task, :language])

        user_has_correct_submissions = neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :task => data[:task], :language => data[:language]})['count'] > 0
            MATCH (u:User {email: $email})<-[:BY]-(s:Solved)-[:FOR]->(t:Task {name: $task}), (s)-[:IN]->(l:Language {name: $language})
            RETURN COUNT(s) AS count;
        END_OF_STRING

        submissions = {:successful => [], :tries => []}
        [true, false].each do |success|
            if user_has_correct_submissions
                next unless success
            else
                next if success
            end
            if success
                neo4j_query(<<~END_OF_STRING, {:task => data[:task], :language => data[:language], :success => success}).to_a.each do |x|
                    MATCH (l:Language {name: $language})<-[:IN]-(s:Submission {success: $success})-[:FOR]->(t:Task {name: $task}), (s)-[:WITH]->(c:Code)
                    RETURN s, c
                    ORDER BY c.size;
                END_OF_STRING
                    entry = {
                        :sha1 => x['c'][:sha1],
                        :line_count => x['c'][:line_count],
                        :size => x['c'][:size],
                        :lang => data[:language],
                        :success => success,
                    }
                    submissions[:successful] << entry
                end
            else
                neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :task => data[:task], :language => data[:language], :success => success}).to_a.each do |x|
                    MATCH (l:Language {name: $language})<-[:IN]-(s:Submission {success: $success})-[:FOR]->(t:Task {name: $task}),
                    (s)-[:WITH]->(c:Code), (s)-[r:BY]->(u:User {email: $email})
                    RETURN s, c
                    ORDER BY r.ts DESC;
                END_OF_STRING
                    entry = {
                        :sha1 => x['c'][:sha1],
                        :line_count => x['c'][:line_count],
                        :size => x['c'][:size],
                        :lang => data[:language],
                        :success => success,
                    }
                    submissions[:tries] << entry
                end
            end
        end

        respond(:submissions => submissions)
    end

    post '/api/codebite_load_submission_code' do
        data = parse_request_data(required_keys: [:sha1, :language])
        sha1 = data[:sha1]
        assert(sha1 =~ /\A[a-f0-9]{40}\z/)
        path = "/internal/codebites/#{sha1[0, 2]}/#{sha1[2, sha1.size - 2]}"
        code = nil
        highlighted_code = nil
        if File.exist?(path)
            code = File.read(path)
        end
        highlighted_path = "/internal/codebites/#{sha1[0, 2]}/#{sha1[2, sha1.size - 2]}.hl"
        if File.exist?(highlighted_path)
            highlighted_code = File.read(highlighted_path)
        end
        respond(:code => code, :formatted_code => highlighted_code)
    end

    post '/api/run_codebite' do
        run_lock = nil
        assert(user_logged_in?)
        data = parse_request_data(required_keys: [:task, :language, :code], :max_body_length => 100 * 1024, :max_string_length => 100 * 1024, :max_value_lengths => { :code => 100 * 1024 })

        task = data[:task].strip
        language = data[:language].strip
        code = data[:code].to_s.strip

        # if data[:language] == "ruby"
        #     code = rubocop_format_ruby(code).to_s
        # end

        lines = code.split("\n")
        lines.map! { |l| l.rstrip }
        code = lines.join("\n")
        code.rstrip!

        line_count = code.count("\n") + 1
        size = code.bytesize

        # basic guardrails
        assert(task =~ /\A[a-zA-Z0-9_\-]+\z/)
        assert(%w(ruby python javascript).include?(language))

        email = @session_user[:email]
        run_lock = codebite_run_lock(email)
        run_lock.lock

        # 0) kill any existing run for this user
        kill_codebite_run(email, reason: "new submission")

        # 1) handle: sha1, store file, DB link, etc.
        sha1 = Digest::SHA1.hexdigest(language + code)
        path = "/internal/codebites/#{sha1[0, 2]}/#{sha1[2, sha1.size - 2]}"
        FileUtils.mkpath(File.dirname(path))
        unless File.exist?(path)
            AtomicFile.write(path, code)
        end
        highlighted_path = "/internal/codebites/#{sha1[0, 2]}/#{sha1[2, sha1.size - 2]}.hl"
        unless File.exist?(highlighted_path)
            formatter = Rouge::Formatters::HTML.new
            lexer = case language
            when 'ruby'
                Rouge::Lexers::Ruby.new
            when 'python'
                Rouge::Lexers::Python.new
            when 'javascript'
                Rouge::Lexers::Javascript.new
            else
                Rouge::Lexers::PlainText.new
            end

            formatted_code = formatter.format(lexer.lex(code))
            # use first 10 lines
            formatted_code = formatted_code.split("\n")[0, 10].join("\n")
            AtomicFile.write(highlighted_path, formatted_code)
        end
        ts = Time.now.to_i
        submission_id = Digest::SHA1.hexdigest("#{task}-#{language}-#{sha1}")
        neo4j_query(<<~END_OF_STRING, {:email => email, :task => task, :lang => language, :sha1 => sha1, :line_count => line_count, :size => size, :ts => ts, :submission_id => submission_id})
            MERGE (u:User {email: $email})
            MERGE (t:Task {name: $task})
            MERGE (l:Language {name: $lang})
            MERGE (c:Code {sha1: $sha1})
            ON CREATE SET c.line_count = $line_count, c.size = $size

            WITH u,t,l,c

            MERGE (s:Submission {id: $submission_id})
            ON CREATE SET s.success = false

            MERGE (s)-[r:BY]->(u)
            ON CREATE SET r.ts = $ts
            MERGE (s)-[:FOR]->(t)
            MERGE (s)-[:IN]->(l)
            MERGE (s)-[:WITH]->(c);
        END_OF_STRING

        # 2) spawn runner
        cmd = ["/src/codebites/bin/run_task.rb", task, language, "--stream"]

        # codebite_broadcast(email, { action: "codebite_status", status: "starting", task: task, language: language })

        stdin, stdout, stderr, wait_thr = Open3.popen3(*cmd)

        # Important: send the code to stdin, then close it
        stdin.write(code)
        stdin.close

        pid = wait_thr.pid

        threads = []

        # helper to stream chunks as base64 (safe for JSON + binary)
        stream_chunk = lambda do |stream_name, io|
            begin
                while (chunk = io.readpartial(64))
                    codebite_broadcast(email, {
                        action: "codebite_output",
                        stream: stream_name,                 # "stdout" or "stderr"
                        data_b64: Base64.strict_encode64(chunk)
                    })
                end
            rescue EOFError
            rescue => e
                codebite_broadcast(email, {
                    action: "codebite_error",
                    message: "stream #{stream_name} failed: #{e.class}: #{e.message}"
                })
            ensure
                io.close rescue nil
            end
        end

        threads << Thread.new { stream_chunk.call("stdout", stdout) }
        threads << Thread.new { stream_chunk.call("stderr", stderr) }

        @@codebite_mutex.synchronize do
            @@codebite_runs[email] = {
                pid: pid,
                wait_thr: wait_thr,
                threads: threads.dup,
                started_at: Time.now.to_i,
                task: task,
                language: language
            }
        end

        # watcher thread to emit exit code
        watcher = Thread.new do
            status = wait_thr.value
            exit_code = status.exitstatus || 1

            # wait a moment for remaining output threads to drain
            threads.each { |t| t.join(0.2) rescue nil }

            codebite_broadcast(email, {
                action: "codebite_exit",
                exit_code: exit_code,
                language: language,
            })

            if exit_code == 0
                neo4j_query(<<~END_OF_STRING, {:submission_id => submission_id})
                    MATCH (s:Submission {id: $submission_id})
                    SET s.success = true
                END_OF_STRING
                solved_id = Digest::SHA1.hexdigest("#{email}-#{task}-#{language}")
                neo4j_query(<<~END_OF_STRING, {:email => email, :task => task, :language => language, :solved_id => solved_id})
                    MERGE (u:User {email: $email})
                    MERGE (t:Task {name: $task})
                    MERGE (l:Language {name: $language})
                    MERGE (s:Solved {id: $solved_id})
                    MERGE (s)-[:BY]->(u)
                    MERGE (s)-[:FOR]->(t)
                    MERGE (s)-[:IN]->(l)
                END_OF_STRING
                sync_code_tasks(email, task)
            end

            @@codebite_mutex.synchronize do
                # Only clear if it still points to this pid
                if @@codebite_runs[email] && @@codebite_runs[email][:pid] == pid
                    @@codebite_runs.delete(email)
                end
            end
        end

        @@codebite_mutex.synchronize do
            run = @@codebite_runs[email]
            run[:threads] << watcher if run && run[:pid] == pid
        end
        respond(yay: "running", pid: pid, code: code)
    ensure
        run_lock.unlock if run_lock&.owned?
    end

    post '/api/codebite_delete_submission' do
        assert(admin_logged_in?)
        data = parse_request_data(required_keys: [:sha1, :language])
        sha1 = data[:sha1]
        assert(sha1 =~ /\A[a-f0-9]{40}\z/)
        neo4j_query(<<~END_OF_STRING, {:sha1 => sha1, :language => data[:language]})
            MATCH (c:Code {sha1: $sha1})<-[:WITH]-(s:Submission)-[:IN]->(l:Language {name: $language})
            DETACH DELETE s;
        END_OF_STRING
        respond(:yay => "deleted")
    end

    post '/api/stop_codebite' do
        assert(user_logged_in?)
        email = @session_user[:email]

        running = codebite_run_lock(email).synchronize do
            # If nothing is running, just be nice and say ok.
            found = @@codebite_mutex.synchronize do
                @@codebite_runs.key?(email)
            end

            kill_codebite_run(email, reason: "stopped by user") if found
            found
        end

        respond(yay: "stopped", running_was: running)
    end

    post '/api/automatron' do
        data  = parse_request_data(:required_keys => [:regex])
        regex = safe_regex(data[:regex])

        # ε-NFA
        nfa = NFA.from_regex(regex)
        nfa.compact_ids!
        nfa.topo_relabel!

        # DFA (with subset trace)
        dfa, subset_trace = SubsetTrace.build_with_trace(nfa)

        # Minimized DFA (with table method trace)
        min, min_trace = DFAMinimizer.minimize_with_trace(dfa)

        # DOT
        nfa_dot           = DotRender.nfa(nfa)
        dfa_dot_explicit  = DotRender.dfa(dfa, title: 'DFA',                 sink_explicit: true,  q: 'r')
        dfa_dot_implicit  = DotRender.dfa(dfa, title: 'DFA_ImplicitSink',    sink_explicit: false, q: 'r')
        min_dot_explicit  = DotRender.dfa(min, title: 'MinDFA',              sink_explicit: true,  q: 's')
        min_dot_implicit  = DotRender.dfa(min, title: 'MinDFA_ImplicitSink', sink_explicit: false, q: 's')

        # SVG
        nfa_svg           = svg_from_dot(nfa_dot)
        dfa_svg           = svg_from_dot(dfa_dot_explicit)
        dfa_implicit_svg  = svg_from_dot(dfa_dot_implicit)
        min_svg           = svg_from_dot(min_dot_explicit)
        min_implicit_svg  = svg_from_dot(min_dot_implicit)

        # Grammar (build structure + pretty text)
        gram_struct = RegularGrammar.build(min)
        # Classic (step-by-step) grammar text (hides nonproductive targets)
        grammar     = RegularGrammar.to_html(gram_struct, hide_nonproductive: true)
        # Simplified / compact grammar (merging linear chains, e.g., S → ab)
        gram_simpl   = RegularGrammar.simplify(gram_struct)
        grammar_compact = RegularGrammar.to_html_compact(gram_simpl)

        # Random samples (positives from grammar, negatives from DFA)
        samples_mixed = GrammarSampler.sample_mixed(
            min, gram_struct,
            pos: 30,      # how many positives
            neg: 30,      # how many negatives
            max_steps: 30,
            eps_bias: 1.0,
            max_len: 20,
            stop_prob: 0.25,
            seed: Time.now.to_i      # remove/alter for fresh sets each call
        )

        # Unwrapped HTML snippets (each is a <section>…</section>)
        subset_html        = HtmlNotes.subset(subset_trace)
        minimization_html  = HtmlNotes.table_minimization(min_trace)

        payload = {
            input: { regex: regex },

            nfa: { dot: nfa_dot, svg: nfa_svg },

            dfa: {
                explicit_sink: { dot: dfa_dot_explicit, svg: dfa_svg },
                implicit_sink: { dot: dfa_dot_implicit, svg: dfa_implicit_svg, has_sink: !!DotRender.detect_sink(dfa) }
            },

            min_dfa: {
                explicit_sink: { dot: min_dot_explicit, svg: min_svg },
                implicit_sink: { dot: min_dot_implicit, svg: min_implicit_svg, has_sink: !!DotRender.detect_sink(min) }
            },

            grammar: grammar,
            grammar_compact: grammar_compact,
            samples_positive: samples_mixed[:positives],
            samples_negative: samples_mixed[:negatives],

            # Two separate, unwrapped sections for easy inclusion
            notes_subset_html:       subset_html,
            notes_minimization_html: minimization_html,

            # If you still want raw trace data for custom UIs, you can add:
            traces: { subset: subset_trace, minimization: min_trace }
        }

        respond(payload)
    end

    def print_codebites_admin_table
        assert(teacher_logged_in?)
        Main.update_codebites()

        email_for_tag = {}
        registered_emails = []
        neo4j_query(<<~END_OF_STRING).each do |row|
            MATCH (u:User) RETURN u.email;
        END_OF_STRING
            email = row['u.email']
            email_for_tag[fs_tag_for_email(email)] = email
            registered_emails << email
        end

        solved_tasks = {}
        tried_tasks = {}

        neo4j_query(<<~END_OF_STRING).each do |row|
            MATCH (u:User)<-[:BY]-(s:Solved)-[:FOR]->(t:Task), (s)-[:IN]->(l:Language)
            RETURN DISTINCT u.email, t.name, l.name;
        END_OF_STRING
            email = row['u.email']
            task = row['t.name']
            lang = row['l.name']
            solved_tasks[email] ||= {}
            solved_tasks[email][task] ||= []
            solved_tasks[email][task] << lang
        end
        neo4j_query(<<~END_OF_STRING).each do |row|
            MATCH (u:User)<-[:BY]-(s:Submission)-[:FOR]->(t:Task), (s)-[:IN]->(l:Language)
            RETURN DISTINCT u.email, t.name, l.name;
        END_OF_STRING
            email = row['u.email']
            task = row['t.name']
            lang = row['l.name']
            tried_tasks[email] ||= {}
            tried_tasks[email][task] ||= []
            tried_tasks[email][task] << lang
        end

        solution_for_task_and_lang = {}
        line_count_for_sha1 = {}
        neo4j_query(<<~END_OF_STRING, {}).to_a.each do |x|
            MATCH (l:Language)<-[:IN]-(s:Submission {success: TRUE})-[:FOR]->(t:Task), (s)-[:WITH]->(c:Code)
            RETURN s, c, l.name, t.name
            ORDER BY c.size;
        END_OF_STRING
            sha1 = x['c'][:sha1]
            lang = x['l.name']
            task = x['t.name']
            solution_for_task_and_lang[task] ||= {}
            solution_for_task_and_lang[task][lang] ||= []
            solution_for_task_and_lang[task][lang] << sha1
            line_count_for_sha1[sha1] = x['c'][:line_count]
        end

        StringIO.open do |io|
            io.puts "<div style='max-width: 100%; overflow-x: auto;'>"
            io.puts "<table class='table table-sm'>"
            io.puts "<thead>"
            io.puts "<tr>"
            io.puts "<th rowspan='2' style='text-align: left;'>Name</th>"
            @@codebite_sections.each do |section|
                io.puts "<th colspan='#{section['entries'].size}' style='text-align: left;'>#{section['title']}</th>"
            end
            io.puts "</tr>"
            io.puts "<tr>"
            @@codebite_tasks.each do |task_id, task_info|
                io.puts "<th><a href='/codebites/#{task_id}'>#{task_info[:heading].split.map { |x| x[0] }.join}</a></th>"
            end
            io.puts "</tr>"
            io.puts "</thead>"
            last_group = nil
            @@user_group_order.each do |group|
                (@@user_groups[group] || []).each do |email|
                    next unless (@@teachers[@session_user[:email]] || Set.new()).include?(group) || admin_logged_in? || email == @session_user[:email]
                    user_tag = fs_tag_for_email(email)
                    next unless tried_tasks.dig(email) || solved_tasks.dig(email)
                    if group != last_group
                        io.puts "<tr><td colspan='#{@@codebite_tasks.size + 1}' style='text-align: left; font-weight: bold; background-color: #eee;'>#{group}</td></tr>"
                        last_group = group
                    end
                    io.puts "<tr>"
                    io.puts "<td style='text-align: left;'>#{@@invitations[email][:name]}</td>"
                    @@codebite_tasks.each do |task_id, task_info|
                        io.puts "<td>"
                        %w(python ruby javascript).each do |lang|
                            state = 0
                            if (tried_tasks.dig(email, task_id) || []).include?(lang)
                                state = 1
                            end
                            if (solved_tasks.dig(email, task_id) || []).include?(lang)
                                state = 2
                            end
                            io.puts "<img alt='' src='/images/lang/#{lang.sub('javascript', 'js')}-128.png' class='state-#{state}'>"
                        end
                        io.puts "</td>"
                    end
                    io.puts "</tr>"
                end
            end
            io.puts "</table>"
            io.puts "</div>"
            @@codebite_tasks.each do |task_id, task_info|
                next unless solution_for_task_and_lang[task_id]
                io.puts "<h3>#{task_info[:heading]}</h3>"
                %w(python ruby javascript).each do |lang|
                    next unless solution_for_task_and_lang[task_id][lang]
                    solution_for_task_and_lang[task_id][lang].each do |sha1|
                        io.puts "<div class='masonry-container'>"
                        code = File.read("/internal/codebites/#{sha1[0, 2]}/#{sha1[2, sha1.size - 2]}.hl") rescue nil
                        io.puts "<div class='code-container' data-sha1='#{sha1}' data-lang='#{lang}' data-line-count='#{line_count_for_sha1[sha1]}'><pre>#{code}</pre></div>"
                        io.puts "</div>"
                    end
                end
            end

            io.string
        end
    end

    if DEVELOPMENT
        post '/api/tutorial_screenshots/auto_render' do
            data = parse_request_data(:required_keys => [:mode])
            halt 400 unless %w[on off].include?(data[:mode])
            content_type :json
            respond(TutorialScreenshots.set_auto_render(data[:mode] == 'on'))
        end

        get '/api/tutorial_screenshots/status' do
            slug = params['slug'].to_s
            halt 400 unless slug.match?(/\A[a-zA-Z0-9_-]+\z/)

            markdown_path = if defined?(@@content_paths) && @@content_paths
                @@content_paths[slug]
            end
            halt 404 unless markdown_path && File.file?(markdown_path)

            begin
                headers 'Cache-Control' => 'no-store'
                content_type :json
                respond(TutorialScreenshots.status(markdown_path))
            rescue => e
                STDERR.puts ">>> Tutorial screenshot status unavailable: #{e.class}: #{e.message}"
                status 503
                content_type :json
                {
                    :status => 'unavailable',
                    :monitor => false,
                    :error => e.message,
                }.to_json
            end
        end

        get '/api/tutorial_screenshots/image' do
            slug = params['slug'].to_s
            target = params['target'].to_s
            halt 400 unless slug.match?(/\A[a-zA-Z0-9_-]+\z/)
            halt 400 unless target.end_with?('.webp') &&
                !target.start_with?('/') &&
                !target.split('/').include?('..')

            markdown_path = if defined?(@@content_paths) && @@content_paths
                @@content_paths[slug]
            end
            halt 404 unless markdown_path && File.file?(markdown_path)

            tutorial_directory = File.expand_path(File.dirname(markdown_path))
            image_path = File.expand_path(target, tutorial_directory)
            halt 400 unless image_path.start_with?("#{tutorial_directory}/")
            halt 404 unless File.file?(image_path)

            headers 'Cache-Control' => 'no-store'
            respond_raw_with_mimetype(File.binread(image_path), 'image/webp')
        end
    end

    get '/*' do
        path = request.path
        slug = nil
        # Guard the complete teacher namespace before a development reload can
        # discover a newly added article that was not yet in @@content.
        requested_slug = path.delete_prefix('/')
        if !MOVED_PAGES.key?(requested_slug) && (['lehrmaterial', 'lehrmaterial.html'].include?(requested_slug) ||
            requested_slug.start_with?('lehrmaterial-') ||
            (@@content[requested_slug] || {})[:teacher_only])
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt 403 unless teacher_account_logged_in?
        end
        if DEVELOPMENT && path =~ /^\/(reference\/)?[a-zA-Z0-9_-]+$/
            Main.refresh_tutorial(path[1, path.size - 1])
        end
        running_tests = my_running_tests()
        if path[0, 7] == '/share/'
            share_tag = path.sub('/share/', '')
            STDERR.puts "OPENING SHARE WITH SHARE TAG #{share_tag}"
            begin
                share_user = neo4j_query_expect_one(<<~END_OF_STRING, :share_tag => share_tag)['u']
                    MATCH (u:User {share_tag: $share_tag})
                    RETURN u;
                END_OF_STRING
            rescue
                redirect "#{WEB_ROOT}/", 302
                return
            end
            path = '/share.html'
        end
        # STDERR.puts "Requested path: #{path}"
        # STDERR.puts "Available content slugs: #{@@content.keys.join(', ')}"
        if (moved = MOVED_PAGES[path[1, path.size - 1]])
            redirect moved.start_with?('https://') ? moved : "#{WEB_ROOT}/#{moved}", 301
            return
        end
        if @@content.include?(path[1, path.size - 1])
            slug = path[1, path.size - 1]
            path = '/a.html'
        end
        if path == '/'
            path = '/index.html'
        end
        if path == '/tic80'
            redirect "#{WEB_ROOT}/tic80/", 302
            return
        end
        # Teachers find their groups' login codes and workspaces on the class
        # page; the admin page (server, all workspaces) is for administrators.
        if path == '/admin'
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt 403 unless admin_logged_in?
        end
        # The profile is about the logged-in person; without one it would
        # fail while drawing its buttons.
        if path == '/profil'
            this_is_a_page_for_logged_in_users!
        end
        if path == '/klasse'
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt 403 if class_groups.empty?
        end
        if path == '/tic80/'
            s = File.read(File.join(@@static_dir, '/tic80/index.html'))
            s = TrustedTemplate.render(s, binding)
            respond_raw_with_mimetype(s, 'text/html')
            return
        end
        if path[0, 11] == '/codebites/'
            @codebite_slug = path[11, path.size - 11]
            path = 'codebites.html'
        end
        if path[0, 3] == '/l/'
            # Login codes are intentionally redeemed through POST
            # /api/complete_login so they never become part of browser history or
            # ordinary access-log URLs. Old bookmarked/copied login URLs simply
            # lead back to the login form.
            redirect "#{WEB_ROOT}/login", 302
            return
        end
        if path[0, 7] == '/logout'
            path = '/index.html'
            sid = current_session_sid
            if sid
                neo4j_query(<<~END_OF_QUERY, :sid => sid)
                    MATCH (s:Session {sid: $sid})
                    DETACH DELETE s;
                END_OF_QUERY
            end
            clear_session_cookies
            redirect "#{WEB_ROOT}/", 302
        end
        path = path + '.html' unless path.include?('.')
        path = File.join(@@static_dir, path)
        assert(File.absolute_path(path).start_with?(@@static_dir))
        respond_with_file(path) do |content, mime_type|
            if mime_type == 'text/html'
                template = File.read(File.join(@@static_dir, '_template.html'))
                if path.include?('codebites') && !path.include?('codebites_admin')
                    template = File.read(File.join(@@static_dir, '_template_codebites.html'))
                end
                rendered_page = TrustedTemplate.render_page(template, content, binding)
                Main.inject_page_metadata(rendered_page, request.path)
            end
        end
    end
end
