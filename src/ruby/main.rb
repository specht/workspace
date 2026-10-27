require 'etc'
require './include/helper.rb'
require './include/content_parts.rb'
require './include/vscode_settings.rb'
require './include/page_kinds.rb'
require './include/task_spec.rb'
require './include/code_languages.rb'
require './include/kata_map.rb'
require './include/solution_blocks.rb'
require './include/hint_steps.rb'
require './include/after_task.rb'
require './include/task_check.rb'
require './include/task_card.rb'
require './include/stage_code.rb'
require './include/person_sources.rb'
require './include/curriculum_overview.rb'
require './include/task_store.rb'
require './include/class_board.rb'
require './include/open_answers.rb'
require './include/submission_view.rb'
require './include/student_home.rb'
require './include/group_visibility.rb'
require './include/look_in.rb'
require './include/test_accounts.rb'
require './include/reference_index.rb'
require './include/extra_chapters.rb'
require './include/person_boxes.rb'
require './include/template_freshness.rb'
require './include/anaglyph_viewer.rb'
require './include/backrooms_demo.rb'
require './include/atomic_file.rb'
require './include/authentication.rb'
require './include/login_names.rb'
require './include/database_provisioning.rb'
require './include/case_server.rb'
require './include/git_server.rb'
require './include/git_browse.rb'
require './include/trusted_template.rb'
require './include/tutorial_screenshots.rb'
require './include/teacher_roles.rb'
require './include/roster.rb'
require './include/test_workspace_package.rb'
require './include/test_workspace_extensions.rb'
require './include/workspace_credentials.rb'
require './include/workspace_runtime.rb'
require './include/tic80_storage.rb'
require './include/user_files.rb'
require './include/external_links.rb'
require './include/task_html.rb'
require './include/rate_limit.rb'
require './include/tcp_console.rb'
require './include/tools.rb'
require './include/schemas.rb'
require './include/weather_mirror.rb'
require './include/privacy.rb'
require './include/privacy_documents.rb'
require './include/account_deletion.rb'
require './include/account_export.rb'
require './include/mail_texts.rb'
require './include/health.rb'
require './include/card_images.rb'
require 'base64'
require 'cgi'
require 'digest'
require 'mail'
require 'mysql2'
require 'neo4j_bolt'
require 'nokogiri'
require 'faye/websocket'
require 'open3'
require 'resolv'
require 'ipaddr'
require 'erb'
require 'redcarpet'
require 'rouge'
require './include/er_lexer.rb'
require './include/uml_lexer.rb'
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

MODULE_ORDER = [:workspace, :phpmyadmin, :neo4j, :tic80]
MODULE_LABELS = {
    :workspace  => 'Workspace',
    :phpmyadmin => 'phpMyAdmin',
    :neo4j      => 'Neo4j',
    :tic80      => 'TIC-80',
}

# Activities that were removed, with what was stored about them
# (SetupDatabase#remove_code_bites); their addresses lead to the start page.
REMOVED_ACTIVITIES = %w[uebung-zahlen uebung-arrays uebung-strings].freeze

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

# A mail's HTML in the font of the website's family, not the mail program's
# default (often Times); also what the previews on the website show.
MAIL_FONT = "font-family: 'IBM Plex Sans', -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; font-size: 15px; line-height: 1.5; color: #212529;"

def mail_html_document(html)
    "<div style=\"#{MAIL_FONT}\">#{html}</div>"
end

def mail_html_to_plain_text(s)
    s.gsub('<p>', "\n\n").gsub(/<br\s*\/?>/, "\n").gsub(/<\/?[^>]*>/, '').strip
end

# attachments: [{ filename:, content: }]. With attachments the mail is
# multipart/mixed: the text and HTML alternative, then the files (Mail would
# otherwise put a file among the alternatives, where mail programs hide it).
def deliver_mail(plain_text = nil, attachments: [], &block)
    mail = if attachments.empty?
        nil
    else
        message = nil
        built = Mail.new { message = self.instance_eval(&block) }
        alternative = Mail::Part.new { content_type 'multipart/alternative' }
        alternative.add_part(Mail::Part.new { content_type 'text/plain; charset=UTF-8'; body mail_html_to_plain_text(message) })
        alternative.add_part(Mail::Part.new { content_type 'text/html; charset=UTF-8'; body mail_html_document(message) })
        built.add_part(alternative)
        attachments.each { |a| built.add_file(:filename => a[:filename], :content => a[:content]) }
        built.content_type = built.content_type.sub(%r{\Amultipart/[a-z]+}, 'multipart/mixed')
        built
    end
    mail ||= Mail.new do
        charset = 'UTF-8'
        message = self.instance_eval(&block)
        if plain_text.nil?
            html_part do
                content_type 'text/html; charset=UTF-8'
                body mail_html_document(message)
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
        STDERR.puts "Attachments: #{mail.attachments.map(&:filename).join(', ')}" unless mail.attachments.empty?
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

    # The stored states and attempts of a task that moved to another
    # activity (Main::MOVED_TASKS) move with it. Only states still under the
    # old name match, so this runs on every start and changes something once.
    # Did someone solve the task again at its new place, that state stays.
    def migrate_moved_tasks(moved)
        moved.each do |from, to|
            rows = neo4j_query(<<~END_OF_QUERY, :from => from)
                MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState {task: $from})
                RETURN u.email AS email, s.key AS key;
            END_OF_QUERY
            rows.each do |row|
                new_key = TaskStore.state_key(row['email'], to)
                taken = neo4j_query('MATCH (s:TaskState {key: $key}) RETURN s.key AS key;', :key => new_key).first
                if taken
                    neo4j_query('MATCH (s:TaskState {key: $key}) DETACH DELETE s;', :key => row['key'])
                else
                    neo4j_query('MATCH (s:TaskState {key: $key}) SET s.key = $new_key, s.task = $to;',
                        :key => row['key'], :new_key => new_key, :to => to)
                end
            end
            neo4j_query('MATCH (a:TaskAttempt {task: $from}) SET a.task = $to;', :from => from, :to => to)
        end
    end

    # Code Bites is gone (2026-10-18), with its practice sets: what it stored
    # goes too, so the Datenschutzerklärung need not mention it. Runs on every
    # start and finds something once.
    def remove_code_bites
        %w[Submission Solved Code Task Language].each do |label|
            neo4j_query("MATCH (n:#{label}) DETACH DELETE n;")
        end
        neo4j_query(<<~END_OF_QUERY, :prefixes => REMOVED_ACTIVITIES.map { |slug| "#{slug}/" })
            MATCH (n) WHERE (n:TaskState OR n:TaskAttempt) AND any(p IN $prefixes WHERE n.task STARTS WITH p)
            DETACH DELETE n;
        END_OF_QUERY
        neo4j_query('MATCH (u:User) WHERE u.show_codebites IS NOT NULL REMOVE u.show_codebites;')
        FileUtils.rm_rf('/internal/codebites')
    end
end

class Main < Sinatra::Base
    include Neo4jBolt
    helpers Sinatra::Cookies

    # Tasks that moved to another activity, old key => new key; their stored
    # states move along at startup (SetupDatabase#migrate_moved_tasks).
    # »Einführung in SQL« was split in two (2026-10): its second part is
    # »SQL: Gruppieren und Verbinden«.
    MOVED_TASKS = %w[
        linksverkehr laender-pro-kontinent einwohner-pro-kontinent grosse-kontinente waehrung-japan
        grenze-zweimal nachbarn-deutschland viele-nachbarn kontinent-ohne-laender meiste-staedte
        sprachen-schweiz deutsch-amtssprache englisch-amtssprache naechste-hamburg brandenburg
        weiteste-vermutung weiteste-berlin
    ].to_h { |id| ["mysql/#{id}", "sql-joins/#{id}"] }.freeze

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
        # Code Bites and its practice sets are gone (2026-10-18, TODO.md).
        'codebites' => '',
        'codebites_admin' => '',
        **REMOVED_ACTIVITIES.to_h { |slug| [slug, ''] },
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
    # env: variables for the command (passwords, which must not stand in
    # the command: it goes to the log when the command fails).
    def self.shell_capture(command, timeout: nil, allow_failure: false, log_command: true, env: {})
        timeout ||= shell_timeout(:default)
        timeout = Integer(timeout)

        t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        # STDERR.puts ">>> CMD BEGIN timeout=#{timeout}s: #{command}" if log_command

        stdout_data = ''
        stderr_data = ''
        status = nil
        timed_out = false

        Open3.popen3(env, command, :pgroup => true) do |stdin, stdout, stderr, wait_thr|
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

    def self.shell_ok(command, timeout: nil, allow_failure: false, env: {})
        shell_capture(command, :timeout => timeout, :allow_failure => allow_failure, :env => env)
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
            # The details (a command line may hold passwords) are in the log.
            :error => job[:error] ? 'Der Workspace ließ sich nicht starten.' : nil,
        }
    end

    def self.server_start_status(key)
        ensure_server_start_queue!

        @@server_start_mutex.synchronize do
            server_start_snapshot(@@server_start_jobs[key] || { :key => key, :status => 'unknown' })
        end
    end

    # For »Betrieb« on /admin: when each waiting start was queued, and how
    # many run.
    def self.server_start_queue_state
        ensure_server_start_queue!
        @@server_start_mutex.synchronize do
            jobs = @@server_start_jobs.values
            [jobs.select { |j| j[:status] == 'queued' }.map { |j| j[:queued_at].to_i }, jobs.count { |j| j[:status] == 'running' }]
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
                Main.record_server_start(job, true)
                STDERR.puts ">>> Server start worker: ready #{job[:key]}"
            rescue => e
                @@server_start_mutex.synchronize do
                    job[:status] = 'failed'
                    job[:finished_at] = Time.now
                    job[:error] = "#{e.class}: #{e.message}"
                end
                Main.record_server_start(job, false)
                STDERR.puts ">>> Server start worker failed for #{job[:key]}: #{e.class}: #{e.message}"
                STDERR.puts e.backtrace.join("\n")
            ensure
                job.delete(:runner)
            end
        end
    end

    require './include/access.rb'
    require './include/live_apps.rb'
    require './include/roster_store.rb'
    require './include/groups_page.rb'
    require './include/people_page.rb'
    require './include/server_health.rb'
    require './include/my_data.rb'
    require './include/login_sessions_page.rb'
    require './include/cable_page.rb'
    require './include/kata_page.rb'

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

    # nginx maps that remove hs_server_sid and hs_watch_tag from a Cookie
    # header, one per step: $http_cookie -> $hs_cookie_for_student.
    HS_COOKIE_STEPS = 4

    def self.nginx_cookie_maps
        (1..HS_COOKIE_STEPS).map do |i|
            from = i == 1 ? '$http_cookie' : "$hs_cookie_#{i - 1}"
            to = i == HS_COOKIE_STEPS ? '$hs_cookie_for_student' : "$hs_cookie_#{i}"
            "map #{from} #{to} {\n" \
                "                default #{from};\n" \
                "                \"~^(?<hs_ck_a#{i}>(?:.*?;\\s*)?)(?:hs_server_sid|hs_watch_tag)=[^;]*(?:;\\s*)?(?<hs_ck_b#{i}>.*)$\" " \
                "\"$hs_ck_a#{i}$hs_ck_b#{i}\";\n" \
                "            }"
        end.join("\n\n            ")
    end

    # The subnets of the user network (where the Workspaces are), from the
    # networks of the nginx container whose name ends in _user; kept once
    # found (not while nginx is not up yet). Here, before the nginx config,
    # whose first one is written while this class still loads.
    @@user_subnets = nil

    def self.user_subnets
        return @@user_subnets if @@user_subnets
        found = begin
            names, = Open3.capture2('docker', 'ps', '--filter', 'label=com.docker.compose.service=nginx', '--format', '{{.Names}}')
            networks = names.split.flat_map do |name|
                out, = Open3.capture2('docker', 'inspect', '-f', '{{range $n, $c := .NetworkSettings.Networks}}{{$n}} {{end}}', name)
                out.split.select { |n| n.end_with?('_user') }
            end
            networks.uniq.flat_map do |network|
                out, = Open3.capture2('docker', 'network', 'inspect', '-f', '{{range .IPAM.Config}}{{.Subnet}}={{.Gateway}} {{end}}', network)
                out.split.filter_map do |entry|
                    cidr, gateway = entry.split('=')
                    { :net => IPAddr.new(cidr), :gateway => gateway.to_s } rescue nil
                end
            end
        end
        @@user_subnets = found unless found.empty?
        found
    rescue => e
        STDERR.puts "user subnets: #{e.class}: #{e.message}"
        []
    end

    # Limits per browser in nginx, against floods from a few addresses (a
    # flood from many is Hetzner's to filter): requests to the Ruby app per
    # second with a burst, and connections open at once (long WebSockets
    # included) everywhere. The burst is generous, since a school's classes
    # reach the server from one address: a class opening a page at once makes
    # a few hundred requests, each Workspace keeps a few WebSockets open. The
    # rate stays well below what the app serves: one Puma process renders
    # with one core (about 14 pages a second in development), so that one
    # address alone cannot keep it busy.
    NGINX_BACKEND_RATE = 10
    NGINX_BACKEND_BURST = 300
    NGINX_CONNECTIONS_PER_CLIENT = 1000

    # Which address a request counts for: through Traefik the last entry of
    # X-Forwarded-For (as client_ip), without it the peer. A Workspace
    # reaches nginx directly on the user network and could write any header,
    # so there the peer counts (its gateway is the host, as in
    # in_user_network?).
    # In development a request without the header (the browser, Playwright,
    # the screenshot service, all from the host) has no key, and nginx
    # counts nothing without one; with the header it is limited as behind
    # Traefik, which is how the limits are tried there.
    def self.nginx_client_key_config
        subnets = user_subnets.map do |s|
            "    #{s[:net]}/#{s[:net].prefix} 1;" + (s[:gateway].empty? ? '' : "\n    #{s[:gateway]}/32 0;")
        end
        <<~END_OF_STRING
            geo $hs_from_workspace {
                default 0;
            #{subnets.join("\n")}
            }
            map $http_x_forwarded_for $hs_forwarded_last {
                default "";
                "~(?:^|,)\\s*(?<a>[^,\\s]+)\\s*$" $a;
            }
            map "$hs_from_workspace:$hs_forwarded_last" $hs_client {
                default $hs_forwarded_last;
                "~^1:" $remote_addr;
                "0:" #{DEVELOPMENT ? '""' : '$remote_addr'};
            }
            limit_req_zone $hs_client zone=hs_backend:20m rate=#{NGINX_BACKEND_RATE}r/s;
            limit_conn_zone $hs_client zone=hs_conn:20m;
            limit_req_status 429;
            limit_conn_status 429;
            limit_conn hs_conn #{NGINX_CONNECTIONS_PER_CLIENT};
        END_OF_STRING
    end

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
            next unless may_use_workspace?(user[:email].to_s, user)
            if running_servers.include?(fs_tag)
                # sid_ip_pairs << "#{user[:server_sid]} #{running_servers[fs_tag][:ip]}:8443;"
                running_servers[fs_tag][:server_sid] = user[:server_sid]
                running_servers[fs_tag][:server_tag] = server_tag
                running_servers[fs_tag][:share_tag] = user[:share_tag] if user[:share_expires].nil? || user[:share_expires].to_i > Time.now.to_i
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
            # Only while its teacher still is one, with its own access
            next unless may_use_workspace?(user[:email].to_s, user) &&
                        (ADMIN_USERS.include?(user[:email].to_s) || @@teachers.include?(user[:email].to_s))

            running_servers[fs_tag][:server_sid] = user[:server_sid]
            running_servers[fs_tag][:server_tag] = "#{user[:server_tag]}#{demo_tag}"
        end
        # STDERR.puts "sid_ip_pairs:"
        # STDERR.puts sid_ip_pairs.join("\n")

        # watch_tag_ip_pairs = []

        # A watched Workspace only while the watcher still may look into it
        # (its groups and its own access as they are now); a watch it may no
        # longer have is removed.
        stale_watches = []
        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (w:User)-[r:WATCHING]->(u:User)
            RETURN r.watch_tag, u, w;
        END_OF_QUERY
            watch_tag = row['r.watch_tag']
            user = row['u']
            watcher = row['w']
            next unless may_use_workspace?(user[:email].to_s, user)
            unless may_still_look_in?(watcher[:email].to_s, user[:email].to_s, watcher)
                stale_watches << watch_tag
                next
            end
            fs_tag = fs_tag_for_email(user[:email])
            if running_servers.include?(fs_tag)
                # watch_tag_ip_pairs << "#{watch_tag} #{running_servers[fs_tag][:ip]}:8443;"
                running_servers[fs_tag][:watch_tags][watch_tag] = watcher[:server_sid]
            end
        end
        unless stale_watches.empty?
            $neo4j.neo4j_query('MATCH (:User)-[r:WATCHING]->(:User) WHERE r.watch_tag IN $tags DELETE r;', :tags => stale_watches)
            STDERR.puts ">>> Removed #{stale_watches.size} watch(es) no longer allowed"
        end

        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (u:User)-[:SHARES_LIVE_APP]->(s:LiveAppShare {active: TRUE})
            RETURN u.email, s.port, s.tag, u;
        END_OF_QUERY
            next unless may_use_workspace?(row['u.email'].to_s, row['u'])
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

            # Limits per browser (NGINX_BACKEND_RATE and the rest)
            #{nginx_client_key_config.rstrip}

            # Keep access logs useful without recording capability tokens, session
            # cookies, workspace tags or one-time login URLs.
            map $uri $hs_log_uri {
                default $uri;
                ~^/w/[a-z0-9]+(?:/|$) /w/[redacted];
                ~^/share/[a-z0-9]+(?:/|$) /share/[redacted];
                ~^/l/[a-z0-9]+(?:/|$) /l/[redacted];
            }

            log_format custom 'time=$time_iso8601 method=$request_method uri=$hs_log_uri status=$status share=$hs_is_share allowed=$hs_allowed';

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

            # Inject the browser-visible code font and workspace.css only into
            # the code-server workbench document. Internal HTML such as the web-worker extension
            # host has its own restrictive CSP and must remain untouched.
            map $request_uri $hs_code_font_head {
                default "</head>";
                ~^/(?:w/[a-z0-9]+/)?(?:\\?.*)?$ '<link rel="preload" href="/include/fonts/0xProtoNerdFontMono/0xProtoNerdFontMono-Regular.woff2" as="font" type="font/woff2" crossorigin><link rel="stylesheet" href="/include/fonts.css?#{CACHE_BUSTER}"><link rel="stylesheet" href="/include/workspace.css?#{CACHE_BUSTER}"></head>';
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

            # The Cookie header for a student's container, without the
            # domain-wide hs_server_sid and hs_watch_tag: nginx checks them
            # above, and they must never reach code a student controls
            # (code-server, apps on <tag>-<port>). The student's own cookies
            # stay. Each step removes one; a browser sends each of them at
            # most once per domain and path, so four steps leave none.
            #{Main.nginx_cookie_maps}

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
                    limit_req zone=hs_backend burst=#{NGINX_BACKEND_BURST} nodelay;
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
                    proxy_set_header Cookie $hs_cookie_for_student;
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

                # Hackschule's own styles for the workbench (terminal rows).
                location = /include/workspace.css {
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
                    proxy_set_header Cookie $hs_cookie_for_student;
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
                    proxy_set_header Cookie $hs_cookie_for_student;

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

                location /anaglyph-viewer/ {
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

        # X-Hs-Peer: whom nginx got the request from (Traefik, or a Workspace on
        # the user network calling nginx directly); client_ip uses it.
        AtomicFile.write('/nginx-snippets/proxy_ws.conf', <<~END_OF_STRING)
            proxy_http_version 1.1;
            proxy_set_header Host $http_host;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection upgrade;
            proxy_set_header Accept-Encoding gzip;
            proxy_set_header X-Hs-Peer $remote_addr;
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

    # The address people write to: invitations, the imprint, help with a
    # page. CONTACT_EMAIL in credentials.rb; empty when it is not set, and
    # pages then leave the address out.
    def self.contact_email
        defined?(CONTACT_EMAIL) ? CONTACT_EMAIL.to_s.strip : ''
    end

    # The values for ${...} in a content page: its meta block, the site's
    # contact address and its own address. The class repository's variables
    # depend on who looks at the page: in the rendered page they are tokens
    # that page_html fills in per request (ClassRepos); per_viewer: false
    # leaves the placeholders (teacher notes).
    def self.content_variables(meta, per_viewer: true)
        email = contact_email
        values = ContentParts.page_variables(meta).merge('contact_email' => email.empty? ? nil : email, 'web_root' => WEB_ROOT)
        per_viewer ? values.merge(ClassRepos::TOKENS) : values
    end

    # The features of this installation that pages may show parts for
    # (ContentParts.filter_features).
    def self.content_features
        (CaseServer.enabled? ? ['case_server'] : []) + (GitServer.enabled? ? ['git_server'] : [])
    end

    def self.convert_image(image_path)
        image_sha1 = Digest::SHA1.hexdigest(File.read(image_path))[0, 16]
        target_path = "/webcache/#{image_sha1}.webp"
        unless FileUtils.uptodate?(target_path, [image_path])
            STDERR.puts "Copying #{image_path} to cache..."
            FileUtils.cp(image_path, target_path)
        end
        image_sha1
    end

    # A drawing for an overview card (image: name.webp:contain): padded to the
    # card's 16:9 with the colour of its top left corner and a margin, so
    # the card crops nothing, in the light and in the dark variant alike.
    def self.convert_image_padded(image_path)
        image_sha1 = Digest::SHA1.hexdigest(File.read(image_path) + 'pad-16x9')[0, 16]
        target_path = "/webcache/#{image_sha1}.webp"
        unless FileUtils.uptodate?(target_path, [image_path])
            size, = Open3.capture2('identify', '-format', '%w %h', "#{image_path}[0]")
            w, h = size.split.map(&:to_i)
            corner, = Open3.capture2('convert', "#{image_path}[0]", '-format', '%[pixel:p{0,0}]', 'info:')
            margin = 1.1
            width = [w, (h * 16.0 / 9).ceil].max * margin
            height = width * 9 / 16
            ok = system('convert', "#{image_path}[0]", '-background', corner.strip, '-gravity', 'center',
                '-extent', "#{width.round}x#{height.round}", target_path)
            STDERR.puts "...padding of #{image_path} failed!" unless ok
        end
        image_sha1
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

        # The page's own heading: the first h1, else the first h2, but none
        # inside something hidden (the QR panel of the login form, say).
        hidden = ->(node) { node.ancestors.any? { |a| a.respond_to?(:[]) && (a['hidden'] || a['style'].to_s.match?(/display:\s*none/)) } }
        heading = nil
        unless homepage
            heading = body.css('h1').find { |h| !hidden.(h) } || body.css('h2').find { |h| !hidden.(h) }
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

    # The result of a full parse is kept in CONTENT_CACHE, under a key made
    # of everything it comes from: the code that renders, every file of the
    # content (path, size, time), the installation's variables in pages,
    # and whether this is development. A start with the same inputs loads it
    # instead of rendering every page again; anything changed, and the pages
    # are rendered (parse_content_locked). Only the newest result is kept.
    CONTENT_CACHE = '/webcache/content-cache'

    def self.content_cache_key
        digest = Digest::SHA256.new
        digest.update("format 1\0#{DEVELOPMENT}\0")
        files = Dir.glob('/src/ruby/**/*.rb') + Dir.glob('/src/content/**/*', File::FNM_DOTMATCH).select { |path| File.file?(path) }
        files.sort.each do |path|
            stat = File.stat(path)
            digest.update("#{path}\0#{stat.size}\0#{stat.mtime.to_f}\0")
        end
        digest.update(Main.content_variables({}, :per_viewer => false).to_a.sort.inspect)
        digest.hexdigest
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
                # an SVG stays an SVG: sharp at every size, and as WebP it
                # would be served with the wrong type
                if img.attr('data-noconvert') || File.extname(image_path).downcase == '.svg'
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
                    # a picture made from a drawing beside it (x.svg next to
                    # x.webp) has the page as its background: no shadow in
                    # dark mode, unlike a screenshot (dark.css)
                    diagram = File.exist?(image_path.sub(/\.[a-z]+\z/i, '.svg'))
                    img.wrap(diagram ? "<div class='scroll-x diagram'>" : "<div class='scroll-x'>")
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
        ExternalLinks.mark(root)
        Tools.link_targets(root)
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
    def self.insert_task_cards(root, task_slots, page_tasks, redcarpet, path, languages = [])
        render = lambda { |md| redcarpet.render(md.to_s) }
        number = 0
        root.css('div.task-slot').each do |slot|
            id = task_slots[slot['data-slot'].to_i]
            task = page_tasks && page_tasks[:by_id][id]
            html = if task && !languages.empty? && !CodeLanguages.variant_languages(task).empty?
                # A card for each language the page offers, with the same
                # number; only the one for the language shown is visible.
                number += 1
                languages.map do |language|
                    card = TaskCard.html(CodeLanguages.variant(task, language), :activity => page_tasks[:activity],
                        :number => number, :render => render, :language => language,
                        :id_suffix => language == languages.first ? '' : "-#{language}")
                    "<div class='code-language' data-language='#{language}'>#{card}</div>"
                end.join
            elsif task
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
        cache_key = partial ? nil : (content_cache_key rescue nil)
        cache_file = cache_key && File.join(CONTENT_CACHE, "#{cache_key}.marshal")
        if cache_file && File.file?(cache_file)
            begin
                cached = Marshal.load(File.binread(cache_file))
                @@section_order, @@teaching_section_order, @@reference_section_order, @@sections, @@kenney, @@content, @@content_paths = cached
                @@content_catalog_signature = content_catalog_signature
                @@parse_content_count = (@@parse_content_count ||= 0) + 1
                STDERR.puts "Loaded content from #{cache_file} in #{format('%.3f', Process.clock_gettime(Process::CLOCK_MONOTONIC) - parse_started_at)}s"
                return true
            rescue StandardError => e
                STDERR.puts "Content cache #{cache_file} not usable (#{e.class}: #{e.message}), rendering the pages"
            end
        end
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

        # the pages a person box on the Personen page may lead to (PersonBoxes)
        person_pages = paths.reject { |entry| entry[:dev_only] || entry[:extra] || %w[hidden parked].include?(entry[:section]) }
            .map { |entry| ["/#{entry[:slug]}", entry[:original_path]] }

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

        # Rendering a page: everything below for one entry of paths, into
        # parsed_content, parsed_content_paths and the catalog order
        # (parsed_sections, entries).
        render_entries = lambda do |list|
            list.each do |entry|
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
                    page_references = PageKinds.references_of(page_meta, markdown, ContentParts::INCLUDE,
                        :catalog => reference_sections.flat_map { |section| Array(section['entries']).map(&:to_s) })
                    content_warning = lambda { |message| STDERR.puts "#{path}: #{message}" }
                    page_included_parts = ContentParts.included_parts(markdown)
                    markdown = ContentParts.expand_includes(markdown,
                        :page_dir => File.dirname(path), :reference_root => REFERENCE_ROOT,
                        :variables => Main.content_variables(page_meta), :warn => content_warning)
                    markdown = ContentParts.expand_uses(markdown,
                        :references => page_meta['reference'], :reference_root => REFERENCE_ROOT,
                        :warn => content_warning)
                    markdown = ContentParts.fill_variables(markdown, Main.content_variables(page_meta))
                    markdown = ContentParts.filter_features(markdown, Main.content_features, :warn => content_warning)
                    # on the Personen page, the pages that place each person (before
                    # the part markers become anchors)
                    if path == PersonBoxes.file(REFERENCE_ROOT)
                        markdown = PersonBoxes.annotate(markdown, PersonBoxes.uses(person_pages), PersonBoxes.sources(path))
                    end
                    markdown = ContentParts.mark_parts(markdown)
                    # _person(name): a person box from the Handbuch page Personen.
                    markdown = PersonBoxes.expand(markdown, :page_dir => File.dirname(path),
                        :reference_root => REFERENCE_ROOT, :warn => content_warning)
                    # _tool(name): a box that opens a tool of the site in a new tab (Tools).
                    markdown = Tools.expand(markdown, :warn => content_warning)
                    # _schema(dataset): the tables of a dataset as schema lines (Schemas).
                    markdown = Schemas.expand(markdown, :warn => content_warning)
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
                    # Parts for one programming language (CodeLanguages), wrapped after rendering.
                    markdown = CodeLanguages.mark(markdown, :warn => content_warning)
                    # Code to write first, folded under »Lösung anzeigen« (SolutionBlocks), wrapped after rendering.
                    markdown = SolutionBlocks.mark(markdown)
                    # Tips in steps before it (HintSteps), wrapped after rendering as well.
                    markdown = HintSteps.mark(markdown)
                    # What gives away a task's answer, until it is answered (AfterTask).
                    markdown = AfterTask.mark(markdown)
                    # Parts for people with a class repository over SSH or HTTPS,
                    # or without one (<!-- if: class_repo -->, ClassRepos): the
                    # page is rendered once for each, and each request picks one
                    # (page_html).
                    page_conditions = ContentParts.conditions(markdown)
                    if !page_conditions.empty? && !ClassRepos.page?(page_meta)
                        content_warning.call("<!-- if: class_repo --> on a page without class_repo: #{ClassRepos::META_VALUE}")
                    end
                    variant_markdowns = ClassRepos.variants(page_conditions).to_h do |variant|
                        [variant, ContentParts.filter_conditions(markdown, ClassRepos::CONDITION_VALUES[variant], :warn => variant.nil? ? content_warning : nil)]
                    end
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
                variant_markdowns = { nil => markdown } if entry[:kenney]
                page_languages = CodeLanguages.of_page(page_meta)
                variants = variant_markdowns.to_h do |variant, variant_markdown|
                    variant_markdown, task_slots = TaskCard.mark_slots(variant_markdown)
                    # At each step, the whole code for teachers (StageCode), on activities.
                    stage_points = []
                    if !entry[:kenney] && parsed_sections.dig(section, :catalog) != 'reference'
                        variant_markdown, stage_points = StageCode.mark(variant_markdown, File.dirname(path))
                    end
                    root = render_content_markdown(variant_markdown, path, redcarpet) do |md|
                        if entry[:kenney]
                            gallery = StringIO.open do |io|
                                io.puts "<div class='gallery tiles'>"
                                parsed_kenney[entry[:path]].each do |model|
                                    io.puts "<figure><img alt='Das Modell #{model}' src='/cache/kenney/#{entry[:path]}/#{model}.webp'><figcaption>#{model}</figcaption></figure>"
                                end
                                io.puts "</div>"
                                io.string
                            end
                            md.sub!('__KENNEY_GALLERY__', gallery)
                        end
                        md
                    end
                    CodeLanguages.wrap(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    SolutionBlocks.wrap(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    HintSteps.wrap(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    AfterTask.wrap(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    CodeLanguages.pair(root) if page_languages.empty?
                    insert_task_cards(root, task_slots, page_tasks, redcarpet, path, page_languages) unless task_slots.empty?
                    ExternalLinks.mark(root)    # also the links in the task cards
                    StageCode.insert(root, stage_points) unless stage_points.empty?
                    # Reference pages: every h2 and h3 gets an anchor for the Handbuch index.
                    outline = parsed_sections.dig(section, :catalog) == 'reference' ? ReferenceIndex.anchor_headings(root) : nil
                    search_sections = outline ? ReferenceIndex.search_sections(root) : nil
                    # Optional chapters (<!-- extra --> before ##): badge, note, marks in the tables of contents
                    ExtraChapters.find(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    inject_autotoc(root)
                    ExtraChapters.mark(root)
                    # Code in a table that is one short word ([0-9], \\d+, --force)
                    # stays in one piece instead of breaking at its hyphen when the
                    # table squeezes its column (default.css, .table code.whole)
                    root.css('table code').each do |code|
                        text = code.text
                        next if text =~ /\s/ || text.length > 24
                        code['class'] = [code['class'], 'whole'].compact.join(' ')
                    end

                    # Scripts a page loads itself (the colour mixer) get a fresh
                    # address with each version, like default.css: nginx lets
                    # browsers keep /include/*.js forever.
                    html = root.to_html.gsub(%r{(<script src=['"]/include/[a-z_]+\.js)(['"])}) do
                        "#{$1}?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}#{$2}"
                    end
                    [variant, { :root => root, :html => html, :task_slots => task_slots, :outline => outline, :search_sections => search_sections }]
                end
                root, html, task_slots, outline, search_sections = variants[nil].values_at(:root, :html, :task_slots, :outline, :search_sections)
                # The Handbuch search is the same for everyone: placeholders for
                # the class repository's variables.
                search_sections&.each { |section_entry| section_entry.transform_values! { |value| ClassRepos.neutral(value) } }
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
                    # in a section hidden from the catalogs (sections.yaml hidden: true)
                    :hidden_section => parsed_sections.dig(section, :hidden) == true,
                    # What kind of page this is (PageKinds.header shows it).
                    :section => section,
                    :kind => page_meta['kind'],
                    :language => page_meta['language'],
                    # Programming languages the page offers (CodeLanguages), [] for one.
                    :languages => page_languages,
                    :duration => page_meta['duration'],
                    # Activities this one builds on (requires:), information
                    # for the strip above the page and the catalog card.
                    :requires => Array(page_meta['requires']).map(&:to_s).reject(&:empty?),
                    :references => page_references,
                    # Reference parts the page includes ('slug#part'), so a box
                    # can shrink once someone has been through it elsewhere.
                    :included_parts => page_included_parts,
                    # Template to clone (template:, template_name:), for the
                    # »new version« hint; nil if the page has none.
                    :template => TemplateFreshness.from_meta(page_meta),
                    :outline => outline,
                    :search_sections => search_sections,
                    # search: words that find the page in the Handbuch search
                    # without standing in its title (Regex for »Reguläre Ausdrücke«)
                    :search_words => page_meta['search'].to_s.split(',').map(&:strip).reject(&:empty?),
                    # A set of Katas (KataMap): its idea in a few words, and the
                    # line of the curriculum teachers see on the overview.
                    :idea => page_meta['idea'],
                    :curriculum => page_meta['curriculum'],
                    # its icon in the strip: bi-braces, or a picture (/images/…)
                    :icon => page_meta['icon'].to_s[%r{\A(bi-[a-z0-9-]+|/images/[\w/.-]+\.(png|webp|svg))\z}],
                }
                if page_tasks
                    parsed_content[slug][:tasks] = page_tasks.merge(:on_page => task_slots & page_tasks[:by_id].keys)
                end
                # class_repo: group (ClassRepos): the variant for people without a
                # class repository, with its own HTML and tasks.
                parsed_content[slug][:class_repo] = ClassRepos.page?(page_meta)
                (variants.keys - [nil]).each do |variant|
                    v = variants[variant]
                    (parsed_content[slug][:variants] ||= {})[variant] = {
                        :html => v[:html],
                        :on_page => page_tasks ? v[:task_slots] & page_tasks[:by_id].keys : [],
                    }
                end
                # Teacher note next to the page (docs/aufgaben.md): rendered like the
                # page, shown only to teachers by teacher_note_panel.
                note_path = File.join(File.dirname(path), ContentParts::NOTE_FILE)
                if !entry[:kenney] && File.file?(note_path)
                    note_markdown = ContentParts.fill_variables(File.read(note_path), Main.content_variables({}, :per_viewer => false))
                    note_root = render_content_markdown(note_markdown, note_path, redcarpet)
                    # Inline code that is one short word (01-karte.c, plane_x)
                    # stays in one piece, in the note's tables and lists alike,
                    # as in the tables of a page (.teacher-note-body code.whole)
                    note_root.css('code').each do |code|
                        next if code.parent&.name == 'pre' || code.text =~ /\s/ || code.text.length > 24
                        code.add_class('whole')
                    end
                    parsed_content[slug][:teacher_note] = (note_root.at_css('body') || note_root).inner_html
                end
                unless entry[:extra]
                    parsed_sections[section][:entries] << slug
                    if meta
                        meta = YAML.load(meta)
                        if meta['image']
                            parts = meta['image'].split(':')
                            # name.webp:contain for a drawing: padded to 16:9 instead of cropped
                            contain = parts[1] == 'contain'
                            parts = parts.first(1) if contain
                            convert = contain ? method(:convert_image_padded) : method(:convert_image)
                            image_path = File.join(File.dirname(path), parts[0])
                            if File.file?(image_path)
                                sha1 = convert.call(image_path)
                                parsed_content[slug][:image] = "/cache/#{sha1}.webp"

                                # The dark file as the screenshot tool names it:
                                # box-anaglyph.webp → box-dark-anaglyph.webp.
                                extension = File.extname(image_path)
                                base = image_path.delete_suffix(extension)
                                dark_image_path = base.sub(/(-anaglyph)?\z/) { "-dark#{$1}" } + extension
                                if File.exist?(dark_image_path)
                                    dark_sha1 = convert.call(dark_image_path)
                                    parsed_content[slug][:image_dark] = "/cache/#{dark_sha1}.webp"
                                end
                            else
                                # A generated screenshot may not exist on first load.
                                parsed_content[slug][:image] = '/images/keyboard.png'
                            end

                            parsed_content[slug][:image_x] = (parts[1] || '50').to_i
                            parsed_content[slug][:image_y] = (parts[2] || '50').to_i
                            parsed_content[slug][:needs_contrast] = meta['needs_contrast']
                            # The card's alt text (docs/aufgaben.md): image_alt,
                            # else the alt of the same picture on the page.
                            basename = File.basename(parts[0])
                            same = root.css('img').find { |img| File.basename(img['src'].to_s) == basename && !img['alt'].to_s.strip.empty? }
                            parsed_content[slug][:image_alt] = meta['image_alt'] || (same && same['alt'])
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
                        unless parsed_content[slug][:image]
                            parsed_content[slug][:image_alt] = root.css('img').first.attr('alt')
                        end
                        parsed_content[slug][:image] ||= root.css('img').first.attr('src')
                        parsed_content[slug][:image_x] ||= 50
                        parsed_content[slug][:image_y] ||= 50
                    rescue
                    end
                end
            end
        end

        # Rendering every page is most of a start (about ten seconds for 160
        # pages, one after another). On a full parse several processes share
        # the pages, dealt in turn: each renders its part and hands it back
        # (Marshal), and leaves with exit!, so it closes nothing it shares
        # (connections, Puma does not run yet). A part whose process fails
        # is rendered here afterwards; that only costs time.
        workers = partial || paths.size < 20 ? 1 : [Etc.nprocessors, 8].min
        if workers > 1
            children = paths.each_with_index.group_by { |_, i| i % workers }.values.map { |part| part.map(&:first) }.map do |part|
                reader, writer = IO.pipe
                pid = fork do
                    reader.close
                    begin
                        render_entries.call(part)
                        entries = parsed_sections.transform_values { |s| s[:entries] }
                        writer.write(Marshal.dump([parsed_content, parsed_content_paths, entries]))
                        writer.close
                        exit!(0)
                    rescue Exception => e
                        STDERR.puts "Rendering content in a worker failed: #{e.class}: #{e.message}"
                        exit!(1)
                    end
                end
                writer.close
                [pid, reader, part]
            end
            children.each do |pid, reader, part|
                data = reader.read
                reader.close
                Process.wait(pid)
                if $?.success? && !data.empty?
                    content, content_paths, entries = Marshal.load(data)
                    parsed_content.merge!(content)
                    parsed_content_paths.merge!(content_paths)
                    entries.each_pair { |key, slugs| parsed_sections[key][:entries].concat(slugs) }
                else
                    render_entries.call(part)
                end
            end
            # the catalog in the order of paths, as one process would make it
            order = paths.each_with_index.to_h { |entry, i| [entry[:original_path], i] }
            parsed_sections.each_value { |s| s[:entries].sort_by! { |slug| order[parsed_content_paths[slug]] || 0 } }
        else
            render_entries.call(paths)
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
            if cache_file
                begin
                    FileUtils.mkdir_p(CONTENT_CACHE)
                    data = Marshal.dump([section_order, teaching_section_order, reference_section_order, parsed_sections, parsed_kenney, parsed_content, parsed_content_paths])
                    File.binwrite("#{cache_file}.tmp", data)
                    File.rename("#{cache_file}.tmp", cache_file)
                    (Dir[File.join(CONTENT_CACHE, '*.marshal')] - [cache_file]).each { |old| File.delete(old) rescue nil }
                rescue StandardError => e
                    STDERR.puts "Content cache not written (#{e.class}: #{e.message})"
                end
            end
        end

        parse_elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - parse_started_at
        label = partial ? "content #{only_slug}" : 'content'
        STDERR.puts "Finished parsing #{label} in #{format('%.3f', parse_elapsed)}s"
        true
    end

    private_class_method :parse_content_locked

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
        git_refresh!
    end

    # --- Git repositories of groups in the Workspace (GitServer) -------------

    # Who may use a group's repositories: its members and its teachers.
    def self.git_group_emails(group)
        teachers = @@teachers.select { |_, groups| groups.include?(group) }.keys
        ((@@group_members[group] || []) + teachers).uniq
    end

    # { group => { id:, keys: } } for the groups with a folder on the server.
    def self.git_groups
        $neo4j.neo4j_query(<<~END_OF_QUERY).to_h { |row| [row['group'], { :id => row['id'], :keys => row['keys'] || GitServer::KEYS_HANDED_IN }] }
            MATCH (g:GroupSetting) WHERE g.git_id IS NOT NULL
            RETURN g.group AS group, g.git_id AS id, g.git_keys AS keys;
        END_OF_QUERY
    end

    # The access list for the server, from the groups and their people.
    def self.git_refresh_access
        map = Hash.new { |hash, key| hash[key] = [] }
        git_groups.each do |group, data|
            git_group_emails(group).each { |email| map[fs_tag_for_email(email)] << data[:id] }
        end
        GitServer.write_config('access', GitServer.access(map))
    end

    # The keys for the server: the one each person handed in, the automatic one.
    def self.git_refresh_keys
        entries = Hash.new { |hash, key| hash[key] = [] }
        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (u:User) WHERE u.git_key IS NOT NULL OR u.git_auto_key IS NOT NULL
            RETURN u.email AS email, u.git_key AS key, u.git_auto_key AS auto;
        END_OF_QUERY
            entries[fs_tag_for_email(row['email'])].concat([row['key'], row['auto']].compact)
        end
        GitServer.write_config('authorized_keys', GitServer.authorized_keys(entries))
    end

    # The people of the groups whose teachers chose automatic keys.
    def self.git_auto_emails
        git_groups.select { |_, data| data[:keys] == GitServer::KEYS_AUTO }.keys.flat_map { |group| git_group_emails(group) }.uniq
    end

    # KEYS_AUTO: the key pair in the Workspace folder of each of these
    # people that has one (anyone who has logged in), its public key on the
    # account. Returns how many got one.
    def self.git_install_auto_keys(emails)
        return 0 unless GitServer.enabled? && File.exist?(GitServer::HOST_KEY)

        host_key = File.read(GitServer::HOST_KEY)
        count = 0
        emails.each do |email|
            home = File.join('/user', fs_tag_for_email(email), 'workspace')
            next unless File.directory?(home)

            line = GitServer.install_auto_key(home, :host_key_text => host_key)
            $neo4j.neo4j_query('MATCH (u:User {email: $email}) SET u.git_auto_key = $key;', :email => email, :key => line)
            count += 1
        rescue StandardError => e
            STDERR.puts "Git: the automatic key for a Workspace failed: #{e.class}: #{e.message}"
        end
        count
    end

    # Everything the server reads, afresh: after the roster was read, a
    # repository or a key changed, an account was deleted.
    def self.git_refresh!(auto: true)
        return unless GitServer.enabled?

        git_install_auto_keys(git_auto_emails) if auto
        git_refresh_keys
        git_refresh_access
    rescue StandardError => e
        STDERR.puts "Git: refreshing the server's files failed: #{e.class}: #{e.message}"
    end


    def self.load_invitations_locked
        source = roster_source
        built = Roster.build(:groups => source[:groups], :members => source[:members],
            :designated => source[:designated], :admins => ADMIN_USERS)
        invitations = built[:invitations]
        user_groups = built[:user_groups]
        user_group_order = built[:user_group_order]
        group_members = built[:group_members] # all memberships, for the class board
        teachers = built[:teachers]
        teacher_emails = built[:teacher_emails]
        # Every teacher (administrators, + and *) has exactly one test
        # account, alone in a test group of its own that only this teacher
        # sees and administers (TestAccounts). The test groups come last.
        test_accounts = {}
        test_group_of = {}
        TestAccounts.plan(teacher_emails, invitations).each do |test_email, account|
            teacher, test_group = account[:teacher], account[:group]
            test_accounts[test_email] = teacher
            test_group_of[teacher] = test_group
            group_members[test_group] = [test_email]
            user_groups[test_group] = [test_email]
            user_group_order << test_group unless user_group_order.include?(test_group)
            invitations[test_email] = { :group => test_group, :name => account[:name] }
            (teachers[teacher] ||= Set.new) << test_group
        end

        # A group that is gone from the roster (an invitation file changed)
        # takes its settings and pins along (GroupData).
        gone = GroupData.vanished(defined?(@@group_members) ? @@group_members.keys : [], group_members.keys)
        unless gone.empty?
            begin
                GroupData.drop!($neo4j, gone)
                STDERR.puts "Groups gone from the roster, settings and pins dropped: #{gone.join(', ')}"
            rescue StandardError => e
                STDERR.puts "Dropping the settings of #{gone.join(', ')} failed: #{e.class}: #{e.message}"
            end
        end
        # Keep readers on the previous complete snapshot until parsing finishes.
        @@invitations = invitations
        @@user_groups = user_groups
        @@user_group_order = user_group_order
        @@teachers = teachers # group administration rights only
        @@teacher_emails = teacher_emails # teachers, with or without group rights
        @@group_members = group_members # all memberships, for the class board
        @@test_accounts = test_accounts # test account email => its teacher
        @@test_group_of = test_group_of # teacher => its test group
        @@test_groups = Set.new(test_group_of.values)
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
            'Database/name',
            'File/sha1',
            'Group/name',
            'Invitation/email',
            'School/key',
            'Setting/key',
            'LiveAppShare/tag',
            'LoginRequest/tag',
            'QrLoginRequest/tag',
            'Session/sid',
            'TaskState/key',
            'Pin/key',
            'Test/tag',
            'User/email',
            'User/db_login',
            'User/server_tag',
            'User/share_tag',
        ]
        INDEX_LIST = [
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
        setup.migrate_moved_tasks(MOVED_TASKS)
        setup.remove_code_bites
        self.reconcile_live_apps!(:refresh_nginx => false)
        # The invitations first: the nginx map decides who may reach a running
        # Workspace (may_use_workspace?), and without them it would allow none.
        self.load_invitations()
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

        # A restored backup brings deleted accounts back: they go again before
        # the server answers its first request (at the end of this file), and
        # then every few minutes, also when a dump is loaded while the server
        # runs (repeat_deletions_after_restore: one query and a small file).
        Thread.new do
            loop do
                sleep(DEVELOPMENT ? 60 : 300)
                begin
                    Main.new!.repeat_deletions_after_restore
                rescue => e
                    STDERR.puts "Repeating deletions after a restore failed: #{e.class}: #{e.message}"
                end
                begin
                    Main.new!.mail_deletion_log
                rescue => e
                    STDERR.puts "Mailing the deletion log failed (tried again in a few minutes): #{e.class}: #{e.message}"
                end
            end
        end

        # Announced deletions whose date has come, expired share links, the
        # disk, ended groups and »Betrieb« (include/server_health.rb), once
        # an hour (the first time a minute after start in development, to
        # see it work).
        Thread.new do
            loop do
                sleep(DEVELOPMENT ? 60 : 600)
                begin
                    Main.new!.run_scheduled_deletions
                rescue => e
                    STDERR.puts "Scheduled deletions failed: #{e.class}: #{e.message}"
                end
                begin
                    Main.expire_shares!
                rescue => e
                    STDERR.puts "Expiring share links failed: #{e.class}: #{e.message}"
                end
                begin
                    Main.new!.check_disk_space
                rescue => e
                    STDERR.puts "Disk check failed: #{e.class}: #{e.message}"
                end
                begin
                    Main.new!.remind_ended_groups
                rescue => e
                    STDERR.puts "Reminders for ended groups failed: #{e.class}: #{e.message}"
                end
                Main.hourly_jobs_ran!
                begin
                    Main.new!.check_health
                rescue => e
                    STDERR.puts "Health check failed: #{e.class}: #{e.message}"
                end
                sleep 3000
            end
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
        # abgaben.<host> serves hand-ins and the Anaglyph viewer and nothing
        # else: no session, no pages, no API (SubmissionView, AnaglyphViewer).
        if request.host == SUBMISSION_VIEW_HOST &&
            !(request.path.start_with?('/v/', '/anaglyph-viewer/') && %w[GET HEAD].include?(request.request_method))
            halt 404
        end
        origin_sensitive = %w(POST PUT PATCH DELETE).include?(request.request_method) ||
            ['/ws', '/ws_tcp_console', '/logout'].include?(request.path)
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

                Main.load_invitations()
                if session_expiry && session_expiry > DateTime.now && !Main.may_use_workspace?(results.first['u'][:email].to_s.downcase, results.first['u'])
                    # No longer invited: the session ends now, not in a year.
                    STDERR.puts "Session ended: #{results.first['u'][:email]} is no longer invited"
                    neo4j_query('MATCH (s:Session {sid: $sid}) DETACH DELETE s;', :sid => sid)
                elsif session_expiry && session_expiry > DateTime.now && session[:impersonated_by] &&
                      !Main.may_still_look_in?(session[:impersonated_by], results.first['u'][:email].to_s.downcase)
                    # A teacher imitating a student who left its groups, or who lost its
                    # own access meanwhile: the session ends, not at »Zurück«.
                    STDERR.puts "Session ended: #{session[:impersonated_by]} may no longer imitate #{results.first['u'][:email]}"
                    neo4j_query('MATCH (s:Session {sid: $sid}) DETACH DELETE s;', :sid => sid)
                elsif session_expiry && session_expiry > DateTime.now
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
                    # The day of the last visit, without a time, at most one write
                    # a day; not while someone else looks in. For the overview of
                    # who still uses the Workspace (/personen; Löschkonzept).
                    # The same for the session, for the list in the profile
                    # (login_sessions.rb): at most one write a day per session.
                    today = Date.today.to_s
                    if @impersonated_by.nil? && (@session_user[:last_seen] != today || session[:last_used_on] != today)
                        neo4j_query('MATCH (s:Session {sid: $sid})-[:FOR]->(u:User {email: $email}) SET u.last_seen = $today, s.last_used_on = $today;',
                            :sid => sid, :email => email, :today => today)
                        @session_user[:last_seen] = today
                    end

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
        cleanup_expired_sessions
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

    # Sessions past their expiry go, at most once an hour, so that a session
    # is kept no longer than the Datenschutzerklärung says (Privacy).
    @@sessions_cleaned_at = 0

    def cleanup_expired_sessions
        return if Time.now.to_i - @@sessions_cleaned_at < 3600

        @@sessions_cleaned_at = Time.now.to_i
        now = DateTime.now
        expired = neo4j_query('MATCH (s:Session) RETURN s.sid AS sid, s.expires AS expires;').map do |row|
            expires = DateTime.parse(row['expires'].to_s) rescue nil
            row['sid'] if expires.nil? || expires <= now
        end.compact
        return if expired.empty?

        neo4j_query('MATCH (s:Session) WHERE s.sid IN $sids DETACH DELETE s;', :sids => expired)
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
        unless DEVELOPMENT || QR_LOGIN_STARTS_PER_CLIENT.allow?(client_ip)
            content_type :json
            halt 429, { :error => 'rate_limited' }.to_json
        end
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
                :session_info => new_session_fields,
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
                SET s += $session_info
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
        # The name is optional and may come later than the address: the one
        # in the invitation list wins.
        name = (@@invitations[email] || {})[:name]
        if name && user[:name] != name
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

    # Limits for the login: per client generously, because all Workspaces
    # and classrooms of a school share one address; per address so tight
    # that guessing a code stays hopeless (20 codes a day, 5 tries each,
    # out of a million). Not in development, where the screenshot tool and
    # the browser tests log in all day from one machine.
    LOGIN_REQUESTS_PER_CLIENT = RateLimit.new(:limit => 120, :period => 600)
    LOGIN_TRIES_PER_CLIENT = RateLimit.new(:limit => 300, :period => 600)
    LOGIN_CODES_PER_ADDRESS = RateLimit.new(:limit => 20, :period => 24 * 3600)
    # Two more things anyone may start without a login: a QR login (qrencode
    # and an entry in the database) and a Workspace behind a share link.
    QR_LOGIN_STARTS_PER_CLIENT = RateLimit.new(:limit => 30, :period => 600)
    SHARE_STARTS_PER_CLIENT = RateLimit.new(:limit => 30, :period => 600)

    # The browser's address: Traefik appends it to X-Forwarded-For (the last
    # entry; earlier ones come from the client and prove nothing), nginx
    # passes the header on. Without Traefik (development) the peer itself.
    # The header counts only when the request comes from nginx: Workspaces
    # reach the backend directly on the user network and could otherwise set
    # any address and slip past the limits per address.
    @@nginx_addresses = nil
    @@nginx_addresses_at = 0

    # Every address of the nginx container, on each network it is in (it may
    # reach the backend over either): from docker inspect, else by name.
    def self.nginx_addresses
        if @@nginx_addresses.nil? || Time.now.to_i - @@nginx_addresses_at > 300
            names, = Open3.capture2('docker', 'ps', '--filter', 'label=com.docker.compose.service=nginx', '--format', '{{.Names}}')
            found = names.split.flat_map do |name|
                out, = Open3.capture2('docker', 'inspect', '-f', '{{range .NetworkSettings.Networks}}{{.IPAddress}} {{end}}', name)
                out.split
            end
            @@nginx_addresses = (found + (Resolv.getaddresses('nginx') rescue [])).uniq
            @@nginx_addresses_at = Time.now.to_i
        end
        @@nginx_addresses
    rescue => e
        STDERR.puts "nginx addresses: #{e.class}: #{e.message}"
        @@nginx_addresses || []
    end

    # In the user network, but not its gateway (that is the host, through
    # which browsers can arrive when ports are published).
    def self.in_user_network?(address)
        ip = (IPAddr.new(address.to_s) rescue nil)
        !ip.nil? && user_subnets.any? { |s| s[:net].include?(ip) && address.to_s != s[:gateway] }
    end

    # The peer is REMOTE_ADDR, not request.ip: Rack's request.ip already
    # follows X-Forwarded-For when the peer has a private address, which
    # every container has. Through nginx: if nginx got the request from a
    # Workspace (X-Hs-Peer in the user network), that Workspace; otherwise it
    # came through Traefik, whose entry is the last in X-Forwarded-For.
    def client_ip
        peer = request.env['REMOTE_ADDR'].to_s
        return peer unless Main.nginx_addresses.include?(peer)

        nginx_peer = request.env['HTTP_X_HS_PEER'].to_s
        return nginx_peer if Main.in_user_network?(nginx_peer)

        forwarded = request.env['HTTP_X_FORWARDED_FOR'].to_s.split(',').map(&:strip).reject(&:empty?)
        forwarded.last || (nginx_peer.empty? ? peer : nginx_peer)
    end

    post '/api/request_login' do
        unless DEVELOPMENT || LOGIN_REQUESTS_PER_CLIENT.allow?(client_ip)
            content_type :json
            halt 429, { :error => 'login_request_rate_limited' }.to_json
        end
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

        # Said plainly, so a typo is noticed at once; the limits per client
        # keep this from being a fast way to find out who is invited.
        unless neo4j_query('MATCH (u:User {email: $email}) WHERE u.suspended_at IS NOT NULL RETURN 1 AS yes;', :email => email).empty?
            content_type :json
            halt 403, { :error => 'suspended' }.to_json
        end
        announced = !@@invitations.include?(email) &&
            !neo4j_query('MATCH (u:User {email: $email}) WHERE u.deletion_at IS NOT NULL RETURN 1 AS yes;', :email => email).empty?
        unless @@invitations.include?(email) || announced
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
                next { :rate_limited => true } unless DEVELOPMENT || LOGIN_CODES_PER_ADDRESS.allow?(email)

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
                # A test account's code goes to its teacher, at the address
                # without +test: plus addresses do not reach every inbox.
                recipient = @@test_accounts[email] || email
                for_test = recipient != email
                STDERR.puts "Sending login code email for invited user #{email} to #{recipient}"
                signature = mail_signature
                deliver_mail do
                    to recipient
                    # bcc SMTP_FROM
                    from SMTP_FROM

                    subject for_test ? "Anmeldecode für dein Testkonto: #{login_request[:code]}" : "Dein Anmeldecode lautet #{login_request[:code]}"

                    StringIO.open do |io|
                        io.puts "<p>Hallo!</p>"
                        io.puts for_test ? "<p>Der Anmeldecode für dein Testkonto #{CGI.escapeHTML(email)} lautet:</p>" : "<p>Dein Anmeldecode lautet:</p>"
                        io.puts "<p style='font-size: 200%;'>#{login_request[:code]}</p>"
                        io.puts "<p>Der Code ist für zehn Minuten gültig. Nachdem du dich angemeldet hast, bleibst du für ein ganzes Jahr angemeldet (falls du dich nicht wieder abmeldest).</p>"
                        io.puts "<p>Falls du diese E-Mail nicht angefordert hast, hat jemand versucht, sich mit deiner E-Mail-Adresse auf <a href='https://#{WEBSITE_HOST}/'>https://#{WEBSITE_HOST}/</a> anzumelden. In diesem Fall musst du nichts weiter tun (es sei denn, du befürchtest, dass jemand anderes Zugriff auf dein E-Mail-Konto hat – dann solltest du dein E-Mail-Passwort ändern).</p>"
                        io.puts "<p>Viele Grüße,<br />#{CGI.escapeHTML(signature)}</p>"
                        io.string
                    end
                end
            end
        end
        respond(:ok => 'yay', :tag => login_request[:tag])
    end

    post '/api/complete_login' do
        unless DEVELOPMENT || LOGIN_TRIES_PER_CLIENT.allow?(client_ip)
            content_type :json
            halt 429, { :error => 'login_rate_limited' }.to_json
        end
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
                :session_info => new_session_fields,
            }
            neo4j_query_expect_one(<<~END_OF_QUERY, complete_params)
                MATCH (r:LoginRequest {tag: $tag, code: $code})-[:FOR]->(u:User)
                WHERE r.expires_at > $now
                  AND COALESCE(r.attempts, 0) < $max_attempts
                DETACH DELETE r
                WITH u
                REMOVE u.login_request_created_at
                CREATE (s:Session {sid: $sid, expires: $session_expires})-[:FOR]->(u)
                SET s += $session_info
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

    def terms_bar_html
        return '' unless terms_required?

        "<div class='look-in-bar terms-bar'><i class='bi bi-file-earmark-check'></i>" \
            "<span>Du nutzt den Workspace als Gast. Dafür gilt die <a href='/datenschutz/nutzungsordnung' target='_blank'>Nutzungsordnung</a>; " \
            'bitte lies sie und stimme zu, dann startet auch dein Workspace.</span>' \
            "<button type='button' class='btn btn-sm btn-light bu-accept-terms'><i class='bi bi-check-lg'></i>Ich bin einverstanden</button></div>"
    end

    post '/api/accept_terms' do
        assert(user_logged_in?)
        halt 403, { :error => 'impersonating' }.to_json if @impersonated_by
        neo4j_query('MATCH (u:User {email: $email}) SET u.terms_accepted_at = $now, u.terms_version = $version;',
            :email => @session_user[:email], :now => Time.now.to_i, :version => Privacy::CHECKED.to_s)
        STDERR.puts "Nutzungsordnung accepted by #{@session_user[:email]}"
        respond(:ok => true)
    end

    # Other browsers in which this account is logged in (sessions last a
    # year; a forgotten login on a school computer is ended from here).
    def other_sessions_count
        return 0 unless user_logged_in?

        neo4j_query('MATCH (s:Session)-[:FOR]->(:User {email: $email}) WHERE s.sid <> $sid AND s.impersonated_by IS NULL RETURN count(s) AS n;',
            :email => @session_user[:email], :sid => current_session_sid.to_s).first['n'].to_i
    end

    post '/api/sign_out_elsewhere' do
        assert(user_logged_in?)
        halt 403 if @impersonated_by
        n = other_sessions_count
        neo4j_query('MATCH (s:Session)-[:FOR]->(:User {email: $email}) WHERE s.sid <> $sid AND s.impersonated_by IS NULL DETACH DELETE s;',
            :email => @session_user[:email], :sid => current_session_sid.to_s)
        STDERR.puts "#{@session_user[:email]} signed out #{n} other sessions"
        respond(:ended => n)
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
    # include/login_names.rb: ä → ae, é → e, other characters → »-«.
    def self.sanitize_workspace_login(value)
        LoginNames.linux_login(value)
    end

    # The screenshot generator's account (development only): its Workspaces
    # start like a student's, but without the first-start walkthrough, which
    # would cover VS Code's welcome page that recipes click on.
    def self.screenshot_account?(email)
        screenshot_email = ENV['TUTORIAL_SCREENSHOT_EMAIL'].to_s
        DEVELOPMENT && !screenshot_email.empty? && email.to_s.casecmp?(screenshot_email)
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

    # A step that writes into a student's folder through UserFiles. A path
    # it refuses (a link, a FIFO the student put there) is logged and the
    # step is skipped; the Workspace still starts.
    def with_user_files(container_name, what)
        yield
    rescue UserFiles::Refused => e
        STDERR.puts ">>> #{container_name}: #{what} skipped: #{e.message}"
        nil
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

        with_user_files(container_name, '.git/info/exclude') do
            TestWorkspacePackage.write_git_exclude(workspace_path)
        end

        if git_mode == 'fresh'
            git.call('add', '-A')
            git.call('commit', '--allow-empty', '-m', 'Ausgangszustand')
        end

        FileUtils.rm_f(pending_path)
        with_user_files(container_name, '.test_git_init') do
            UserFiles.write(workspace_path, '.test_git_init', '')
        end
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

            # A group with automatic Git keys (GitServer): the key pair is in
            # the Workspace before it starts.
            if GitServer.enabled? && test_tag.nil? && Main.git_auto_emails.include?(email)
                Main.git_install_auto_keys([email])
                Main.git_refresh!(:auto => false)
            end

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

    # Who did what to whose Workspace, account or database, with the time:
    # among the program messages (docs/datenschutz.md, »Protokolle des
    # Servers«), to be found again later with ./config.rb logs.
    def log_event(message)
        STDERR.puts "#{Time.now.strftime('%Y-%m-%d %H:%M:%S')} #{message}"
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
        log_event "MySQL database reset by its owner: #{email}"
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
        log_event "Neo4j database reset by its owner: #{email}"
        respond(:yay => 'sure')
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

    def run_mysql_statements(statements)
        Open3.popen2("docker exec -i workspace_mysql_1 mysql --user=root --password=#{MYSQL_ROOT_PASSWORD}") do |stdin, _stdout, wait_thr|
            statements.each { |statement| stdin.puts statement }
            stdin.close
            raise 'MySQL statements failed' unless wait_thr.value.success?
        end
    end

    def run_neo4j_statements(statements)
        Open3.popen2("docker exec -i workspace_neo4j_1 bin/cypher-shell -u neo4j -p #{NEO4J_ROOT_PASSWORD}") do |stdin, _stdout, wait_thr|
            statements.each { |statement| stdin.puts statement }
            stdin.close
            raise 'Neo4j statements failed' unless wait_thr.value.success?
        end
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
        AccountDeletion.node_statements.each { |statement| neo4j_query(statement, :email => email) }
        # The key goes with the account; the server forgets it at once.
        Main.git_refresh!(:auto => false)
        note_deletion(email)
        Main.refresh_nginx_config()

        {
            :workspaces => 1 + instances.size,
            :databases => logins.size * 2 + extra_databases.size,
            :tasks => task_keys.size,
        }
    end

    # Who signs the mails: OPERATOR_NAME, else the address.
    def mail_signature
        defined?(OPERATOR_NAME) && !OPERATOR_NAME.to_s.strip.empty? ? OPERATOR_NAME.to_s.strip : 'Workspace'
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
        assert(admin_logged_in?)
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
        assert(admin_logged_in?)
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
        assert(admin_logged_in?)
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
        assert(admin_logged_in?)
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
        assert(admin_logged_in?)
        data = parse_request_data(:required_keys => [:email])
        neo4j_query('MATCH (u:User {email: $email}) REMOVE u.suspended_at, u.suspended_by, u.suspended_reason;', :email => data[:email].to_s.downcase)
        Main.refresh_nginx_config()
        log_event "Suspension of #{data[:email]} lifted by #{@session_user[:email]}"
        respond(:ok => true)
    end

    post '/api/admin/cancel_deletion' do
        assert(admin_logged_in?)
        data = parse_request_data(:required_keys => [:email])
        neo4j_query('MATCH (u:User {email: $email}) REMOVE u.deletion_at, u.deletion_announced_at, u.deletion_announced_by;', :email => data[:email].to_s.downcase)
        STDERR.puts "Announced deletion of #{data[:email]} cancelled by #{@session_user[:email]}"
        respond(:ok => true)
    end

    # The person keeps the account as a guest: into a group of the guests'
    # school (Nutzungsordnung), and the announced deletion is off.
    post '/api/admin/keep_as_guest' do
        assert(admin_logged_in?)
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
                next if system('docker', 'exec', 'workspace_mysql_1', 'mysqldump', '--user=root', "--password=#{MYSQL_ROOT_PASSWORD}", database,
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

    # When a person was last active, as a date and where it comes from:
    # :visit (User.last_seen, recorded since 2026-10-18), otherwise the later
    # of the last login (a session's expiry minus its fixed lifetime;
    # sessions of someone looking in do not count) and the last task answer.
    def activity_by_email
        # Every trace of use there is; the latest counts. :source says which:
        # besuch (a page of the website, User.last_seen, since 2026-10-18),
        # anmeldung (a session's start; sessions end after a year), aufgabe
        # (a task checked), code (a login code asked for and not used),
        # workspace (a browser had the Workspace open: code-server's
        # heartbeat or the launch marker), dateien (a file in the Workspace
        # changed). The last two come from housekeeping.rb.
        days = Hash.new { |hash, key| hash[key] = {} }
        note = lambda do |email, day, source|
            days[email][source] = day if day && (days[email][source].nil? || day > days[email][source])
        end
        neo4j_query('MATCH (u:User) WHERE u.last_seen IS NOT NULL RETURN u.email AS email, u.last_seen AS day;')
            .each { |r| note.(r['email'], (Date.iso8601(r['day'].to_s) rescue nil), 'besuch') }
        neo4j_query('MATCH (s:Session)-[:FOR]->(u:User) WHERE s.impersonated_by IS NULL RETURN u.email AS email, s.expires AS expires;').each do |r|
            note.(r['email'], (DateTime.parse(r['expires'].to_s).to_date - Authentication::SESSION_LIFETIME_DAYS rescue nil), 'anmeldung')
        end
        neo4j_query('MATCH (a:TaskAttempt)-[:BY]->(u:User) RETURN u.email AS email, max(a.ts) AS ts;')
            .each { |r| note.(r['email'], Time.at(r['ts'].to_i).to_date, 'aufgabe') }
        users = neo4j_query('MATCH (u:User) RETURN u.email AS email, u.login_request_created_at AS code;').to_a
        users.each { |r| note.(r['email'], r['code'] ? Time.at(r['code'].to_i).to_date : nil, 'code') }
        workspace = (JSON.parse(File.read('/internal/activity_for_fs_tag.json')) rescue {})
        users.each do |r|
            fs_tag = fs_tag_for_email(r['email'])
            { 'workspace' => 'workspace', 'files' => 'dateien' }.each do |key, source|
                ts = (workspace[key] || {})[fs_tag]
                note.(r['email'], Time.at(ts.to_i).to_date, source) if ts
            end
        end
        days.to_h do |email, by_source|
            source, day = by_source.max_by { |s, d| [d, s == 'besuch' ? 1 : 0] }
            [email, { :day => day, :source => source }]
        end
    end

    # The disk space of each Workspace folder in KB, as housekeeping.rb last
    # measured it (by fs tag), and from which size a Workspace stands out:
    # five times the median and at least 200 MB, so that it scales with the
    # installation.
    def workspace_disk_usage
        path = '/internal/du_for_fs_tag.json'
        by_tag = (JSON.parse(File.read(path)) rescue {})
        sizes = by_tag.values.map(&:to_i).sort
        median = sizes.empty? ? 0 : sizes[sizes.size / 2]
        { :by_tag => by_tag, :fat_kb => [median * 5, 200 * 1024].max, :measured_at => (File.mtime(path).to_i rescue nil) }
    end

    # Everyone invited or with an account, for the administration: address,
    # name, role, groups, schools, whether there is an account, when last
    # active, the disk space of the Workspace. query matches part of the name,
    # address, group or school.
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
            { :email => email, :name => name == email ? nil : name, :role => role, :groups => groups_of[email].sort,
              :schools => schools_of[email].sort, :account => accounts.key?(email), :invited => @@invitations.key?(email),
              :active_on => active && active[:day] ? active[:day].to_s : nil, :active_source => active && active[:day] ? active[:source] : nil,
              :disk_kb => accounts.key?(email) ? disk[fs_tag_for_email(email)] : nil, :deletion_at => deletion_at[email],
              :guest => guests.include?(email), :terms_accepted_at => terms_at[email], :suspended => suspended[email] }
        end.select do |p|
            q.empty? || p[:email].include?(q) || p[:name].to_s.downcase.include?(q) || p[:groups].any? { |g| g.downcase.include?(q) } ||
                p[:schools].any? { |g| g.downcase.include?(q) }
        end.sort_by { |p| [p[:name].to_s.downcase, p[:email]] }
    end

    # school: a school's name (or '-' for none); role: one of the roles;
    # sort: 'active' puts those inactive longest (and never) first.
    post '/api/admin/people' do
        assert(admin_logged_in?)
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
        assert(admin_logged_in?)
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

    # Free space where the Workspaces live: a full disk stops every school at
    # once. Below DISK_LOW_PERCENT or DISK_LOW_GB the administrators get a
    # mail, at most once a day, and /admin shows it.
    DISK_LOW_PERCENT = 10
    DISK_LOW_GB = 20
    DISK_ALERT_FILE = '/internal/disk-alert.json'

    def self.disk_status
        line = `df -Pk /user 2>/dev/null`.lines.last.to_s.split
        total, free = line[1].to_i, line[3].to_i
        return nil if total.zero?

        free_gb = free / 1024.0 / 1024.0
        percent = 100.0 * free / total
        { :free_gb => free_gb.round(1), :free_percent => percent.round(1), :low => percent < DISK_LOW_PERCENT || free_gb < DISK_LOW_GB }
    end

    def check_disk_space
        status = Main.disk_status
        return unless status && status[:low]

        today = Date.today.to_s
        last = (JSON.parse(File.read(DISK_ALERT_FILE))['sent'] rescue nil)
        return if last == today

        STDERR.puts "Disk space low: #{status[:free_gb]} GB (#{status[:free_percent]} %) free"
        text = "<p>Auf dem Server sind unter /user nur noch #{status[:free_gb]} GB frei (#{status[:free_percent]} %). " \
            'Ist die Platte voll, startet kein Workspace mehr, und Dateien gehen verloren. Unter »Personen und Schulen« zeigt »Speicher« die größten Workspaces.</p>'
        ADMIN_USERS.each do |admin|
            deliver_mail do
                to admin
                from SMTP_FROM
                subject "Workspace: wenig Speicherplatz (#{status[:free_gb]} GB frei)"
                text
            end
        end
        AtomicFile.write(DISK_ALERT_FILE, JSON.generate({ 'sent' => today }))
    end

    # Groups past their end date: one mail to their teachers (delete the
    # group, or set a new end), marked so that it goes only once.
    def remind_ended_groups
        return if Main.roster_imported.nil?

        rows = neo4j_query(<<~END_OF_QUERY, :today => Date.today.to_s)
            MATCH (g:Group) WHERE g.ends_on IS NOT NULL AND g.ends_on < $today AND g.end_reminded_at IS NULL
            OPTIONAL MATCH (t:Invitation)-[:TEACHES]->(g)
            RETURN g.name AS name, g.ends_on AS ends_on, collect(t.email) AS teachers;
        END_OF_QUERY
        rows.each do |r|
            recipients = r['teachers'].empty? ? ADMIN_USERS : r['teachers']
            day = (Date.iso8601(r['ends_on']).strftime('%d.%m.%Y') rescue r['ends_on'])
            text = "<p>Die Gruppe <strong>#{CGI.escapeHTML(r['name'])}</strong> im Workspace ist am #{day} zu Ende gegangen.</p>" \
                "<p>Wird sie nicht mehr gebraucht, lösche sie unter #{WEB_ROOT}/klasse (Reiter »Lernende«); wer danach in keiner Gruppe mehr steht, ist nicht mehr eingeladen, " \
                'und die Konten werden nach den Löschregeln gelöscht. Geht der Kurs weiter, trag dort ein neues Ende ein.</p>' \
                "<p>Viele Grüße<br>#{CGI.escapeHTML(mail_signature)}</p>"
            recipients.each do |email|
                deliver_mail do
                    to email
                    from SMTP_FROM
                    subject "Workspace: Die Gruppe #{r['name']} ist zu Ende"
                    text
                end
            end
            neo4j_query('MATCH (g:Group {name: $name}) SET g.end_reminded_at = $now;', :name => r['name'], :now => Time.now.to_i)
            STDERR.puts "Reminder sent for ended group #{r['name']} to #{recipients.size} teacher(s)"
        end
    end

    # Links shared before there was an expiry get SHARE_DAYS from now; expired
    # ones go, and the nginx map with them.
    def self.expire_shares!
        now = Time.now.to_i
        $neo4j.neo4j_query('MATCH (u:User) WHERE u.share_tag IS NOT NULL AND u.share_expires IS NULL SET u.share_expires = $expires;',
            :expires => now + SHARE_DAYS * 24 * 3600)
        gone = $neo4j.neo4j_query('MATCH (u:User) WHERE u.share_tag IS NOT NULL AND u.share_expires <= $now REMOVE u.share_tag, u.share_expires RETURN count(u) AS n;',
            :now => now).first['n'].to_i
        return if gone.zero?

        STDERR.puts "Expired #{gone} workspace share link(s)"
        refresh_nginx_config
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
    # Pinning the activity from its own page, for teachers with classes:
    # the same pins as on the class page (/api/class/pin), shown first on the
    # students' start page. One class: a click pins or unpins; several: a
    # list of them with ticks (page_pin.js).
    def page_pin_button(slug)
        return '' unless user_logged_in? && (teacher_account_logged_in? || admin_logged_in?)
        return '' unless pinnable_slugs.include?(slug)

        groups = class_groups
        return '' if groups.empty?

        pinned = pins_for_groups(groups).select { |p| p[:slug] == slug }.map { |p| p[:group] }
        data = groups.map { |g| { :name => g, :pinned => pinned.include?(g) } }
        "<span class='page-pin' data-slug='#{CGI.escapeHTML(slug)}' data-groups='#{CGI.escapeHTML(data.to_json)}'>" \
            "<button type='button' class='page-pin-toggle' aria-expanded='false'><i class='bi bi-pin-angle' aria-hidden='true'></i><span>Anpinnen</span></button>" \
            "<script src='/include/page_pin.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}' defer></script></span>"
    end

    def page_kind_header(slug)
        # the teacher's buttons together at the right end, wrapping together
        tools = page_pin_button(slug) + teacher_note_toggle(slug)
        toggle = tools.empty? ? '' : "<span class='page-teacher-tools'>#{tools}</span>"
        header = PageKinds.header(slug, @@content, @@sections, :development => DEVELOPMENT, :extra => toggle,
                                  :show_duration => teacher_logged_in?, :show_curriculum => teacher_logged_in?,
                                  :code_language => page_code_language(slug))
        header.empty? && !toggle.empty? ? "<div class='page-kind'>#{toggle}</div>" : header
    end

    def page_kind_class(slug)
        PageKinds.container_class(slug)
    end

    # --- Programming languages (CodeLanguages) ---------------------------------

    # The language a teacher set for each group, { group => language } in
    # the order given; nil where none is set.
    def group_code_languages(groups)
        return {} if groups.empty?

        rows = neo4j_query(<<~END_OF_QUERY, :groups => groups).to_a
            MATCH (g:GroupSetting) WHERE g.group IN $groups
            RETURN g.group AS group, g.code_language AS language;
        END_OF_QUERY
        set = rows.to_h { |row| [row['group'], row['language']] }
        groups.to_h { |group| [group, set[group]] }
    end

    # --- Copy buttons per group ---------------------------------------------------

    # By default every code block has a copy button. A teacher can choose
    # »Wo sinnvoll« for a group (class page, copy_all_code = false): then a
    # block has one only where the page gives it one (_copy: what is no use
    # to type, big blocks that go into a file), and the rest is typed. A
    # person in several groups gets every button as soon as one allows it:
    # false only when each of the groups chose »Wo sinnvoll«.
    def group_copy_all_code?(groups)
        return true if groups.empty?

        marked = neo4j_query('MATCH (g:GroupSetting) WHERE g.group IN $groups AND g.copy_all_code = false RETURN count(DISTINCT g.group) AS n;',
            :groups => groups).first['n'].to_i
        marked < groups.uniq.size
    end

    # For the page (<body data-copy-all-code>): everyone, visitors and
    # teachers too, except a student whose groups all chose »Wo sinnvoll«,
    # also while a teacher imitates that student.
    def copy_all_code?
        return true unless user_logged_in? && !teacher_logged_in?

        @copy_all_code = group_copy_all_code?(my_groups) if @copy_all_code.nil?
        @copy_all_code
    end

    # Which tips in steps were opened is kept in the browser only
    # (hint_steps.js), per account: on a shared school computer the next
    # person must not find the tips of the one before already open. A hash,
    # so the page does not carry the address; nil for visitors.
    def hint_owner
        return nil unless user_logged_in?

        Digest::SHA2.hexdigest("hint-steps:#{@session_user[:email]}")[0, 16]
    end

    # --- Class repositories per group (ClassRepos) -----------------------------

    # { group => { slug => address } } for the groups given, {} for a group
    # without any.
    def group_class_repos(groups)
        return {} if groups.empty?

        rows = neo4j_query(<<~END_OF_QUERY, :groups => groups).to_a
            MATCH (g:GroupSetting) WHERE g.group IN $groups AND g.class_repos IS NOT NULL
            RETURN g.group AS group, g.class_repos AS repos;
        END_OF_QUERY
        set = rows.to_h { |row| [row['group'], ClassRepos.parse(row['repos'])] }
        groups.to_h { |group| [group, set[group] || {}] }
    end

    # Pages with class_repo: group, in the order of the catalog (pages
    # outside it after them, by title).
    def class_repo_slugs
        slugs = @@content.select { |_, c| c[:class_repo] && (DEVELOPMENT || !c[:dev_only]) }.keys
        order = @@section_order.flat_map { |key| (@@sections[key] || {})[:entries] || [] }
        slugs.sort_by { |slug| [order.index(slug) || order.size, plain_title(slug).downcase] }
    end

    # Teachers, administrators and visitors see a page as it is with a
    # class repository, with placeholders for the address.
    def class_repo_placeholders?
        !user_logged_in? || teacher_account_logged_in? || admin_logged_in?
    end

    def my_class_repos
        @my_class_repos ||= class_repo_placeholders? ? {} : group_class_repos(my_groups)
    end

    # How this person sees a page: :variant is nil (with a class repository)
    # or ClassRepos::WITHOUT, :address the address or nil (placeholders).
    # Students see the address of the first of their groups that has one
    # for this page, and without one the page's variant without; on other
    # pages (the Handbuch) the first address of their groups.
    def class_repo_view(slug)
        @class_repo_view ||= {}
        @class_repo_view[slug] ||= ClassRepos.view(slug, :per_group => (@@content[slug] || {})[:class_repo] == true,
            :placeholders => class_repo_placeholders?, :groups => my_groups, :settings => my_class_repos,
            :local => git_local_mode)
    end

    # For ClassRepos.variant: :auto or :keys for an address on the
    # Workspace's Git server, after how its group's students get their
    # keys; nil for other addresses.
    def git_local_mode
        @git_local_mode ||= begin
            modes = GitServer.enabled? ? Main.git_groups.values.to_h { |data| [data[:id], data[:keys]] } : {}
            lambda do |address|
                parsed = GitServer.parse_address(address)
                next nil unless parsed

                modes[parsed[0]] == GitServer::KEYS_AUTO ? :auto : :keys
            end
        end
    end

    # The variant of a page this person sees (nil: the page as it is);
    # looks up the groups' addresses only on pages with class_repo: group.
    def class_repo_variant(slug)
        (@@content[slug] || {})[:class_repo] ? class_repo_view(slug)[:variant] : nil
    end

    # The variant of a page for a group (class board): the one its students
    # see, after the group's address for the page (ClassRepos.variant).
    def group_page_variant(slug, group)
        return nil unless (@@content[slug] || {})[:class_repo]

        @group_class_repos ||= {}
        @group_class_repos[group] ||= group_class_repos([group])[group] || {}
        ClassRepos.variant(@group_class_repos[group][slug], :local => git_local_mode)
    end

    # The tasks a page shows in a variant (nil: the page as it is).
    def tasks_in_variant(content, variant)
        ClassRepos.tasks(content, variant)
    end

    # For teachers on a page with class_repo: group, one line: the groups
    # that have an address, each linked to the dialog on the class page where
    # it is set. Groups without one are left out; when none has one, a quiet
    # hint says where to set it up instead of the box.
    # Administrators see the groups they teach, not every group.
    def class_repo_teacher_line(slug)
        return '' unless (@@content[slug] || {})[:class_repo] && (teacher_account_logged_in? || admin_logged_in?)

        h = ->(t) { CGI.escapeHTML(t.to_s) }
        groups = class_groups
        if admin_logged_in?
            own = @@teachers[@session_user[:email]] || Set.new
            groups = groups.select { |g| own.include?(g) || @@test_groups.include?(g) }
        end
        repos = group_class_repos(groups)
        link = ->(g) { "/klasse?group=#{CGI.escape(g)}&repos=#{CGI.escape(slug)}" }
        with = groups.select { |g| (repos[g] || {})[slug] }
        if with.empty?
            where = groups.size == 1 ? "<a href='#{h.(link.(groups.first))}'>auf der Klassenseite</a>" : "auf der <a href='/klasse'>Klassenseite</a> bei der Gruppe"
            return "<p class='class-repo-hint'><i class='bi bi-hdd-network' aria-hidden='true'></i>Lernende sehen diese Seite ohne Git. " \
                "Soll eine Klasse ihre Arbeit in ein gemeinsames Repository hochladen, trägst du dessen Adresse #{where} unter »Git« ein.</p>"
        end
        items = with.map { |g| "<a href='#{h.(link.(g))}'>#{h.(g)}</a>: <code>#{h.(repos[g][slug])}</code>" }
        others = with.size < groups.size ? ' Deine anderen Gruppen sehen die Seite ohne Git.' : ''
        "<p class='class-repo-teacher'><i class='bi bi-hdd-network' aria-hidden='true'></i><strong>Klassen-Repository</strong>: " \
            "#{items.join(' · ')}.#{others}</p>"
    end

    # The language this person sees on a page that offers more than one,
    # nil on other pages: their own choice, else their class's, else Ruby.
    def page_code_language(slug)
        languages = (@@content[slug] || {})[:languages] || []
        return nil if languages.empty?

        @page_code_language ||= {}
        @page_code_language[slug] ||= begin
            own = user_logged_in? ? @session_user[:code_language] : nil
            groups = user_logged_in? ? group_code_languages(my_groups) : {}
            # JavaScript, picked on a Kata page, is kept apart from the
            # choice between Ruby and Python, which stays for other pages
            kata = user_logged_in? ? @session_user[:kata_code_language] : nil
            CodeLanguages.resolve(kata, { :own => own }.merge(groups), languages)
        end
    end

    def code_language_attribute(slug)
        language = page_code_language(slug)
        language ? " data-code-language='#{language}'" : ''
    end

    # The switch in the page header: shows the other language at once and
    # remembers the choice, for logged-in people on the server, for others
    # in the browser.
    def code_language_setup(slug)
        return '' if page_code_language(slug).nil?

        chosen = user_logged_in? && CodeLanguages.known?(@session_user[:code_language])
        <<~END_OF_STRING
            <script>
            (function () {
                const container = document.querySelector('[data-code-language]');
                if (!container) return;
                const loggedIn = #{user_logged_in? ? 'true' : 'false'};
                // the pill behind the chosen button, sliding there
                function place() {
                    container.querySelectorAll('.code-language-switch').forEach(function (group) {
                        const thumb = group.querySelector('.code-language-thumb');
                        const pressed = group.querySelector("[aria-pressed='true']");
                        if (!thumb || !pressed || !pressed.offsetWidth) return;
                        thumb.style.left = pressed.offsetLeft + 'px';
                        thumb.style.width = pressed.offsetWidth + 'px';
                        // placed at once the first time, sliding from then on
                        if (!group.classList.contains('code-language-sliding')) {
                            requestAnimationFrame(function () { group.classList.add('code-language-sliding'); });
                        }
                    });
                }
                function show(language) {
                    container.dataset.codeLanguage = language;
                    container.querySelectorAll('[data-set-language]').forEach(function (button) {
                        const on = button.dataset.setLanguage === language;
                        button.setAttribute('aria-pressed', on ? 'true' : 'false');
                    });
                    place();
                }
                place();
                window.addEventListener('resize', place);
                if (document.fonts) document.fonts.ready.then(place);
                if (!loggedIn && !#{chosen}) {
                    try {
                        const stored = localStorage.getItem('code-language');
                        if (stored && container.querySelector("[data-set-language='" + stored + "']")) show(stored);
                    } catch (e) {}
                }
                container.querySelectorAll('[data-set-language]').forEach(function (button) {
                    button.addEventListener('click', function () {
                        const language = button.dataset.setLanguage;
                        show(language);
                        if (loggedIn) {
                            api_call('/api/set_code_language', { language: language }, function () {}, { no_please_wait: true });
                        } else {
                            try { localStorage.setItem('code-language', language); } catch (e) {}
                        }
                    });
                });
            })();
            </script>
        END_OF_STRING
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
        tasks = tasks_in_variant(@@content[slug], class_repo_variant(slug)) if tasks
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
            answers = open_answers_config(tasks, slug)
            config[:teacher] = answers if answers
            solutions = kata_solutions_config(tasks)
            config[:solutions] = solutions if solutions
            config[:revealAfter] = true if teacher_logged_in?
        end
        # Text that only stands in for the cards (an exercise list) is hidden
        # while the cards are shown.
        previews = tasks[:on_page].any? { |id| (tasks[:by_id][id] || {}).dig('check', 'preview') == 'anaglyph' }
        # SQL and Cypher answers, and the code of a Kata, in the colours of
        # the code blocks, with the keyword lists of the same lexers
        # (code_input.js).
        code = tasks[:on_page].any? { |id| %w[sql cypher function pip tests].include?((tasks[:by_id][id] || {}).dig('check', 'type')) }
        pip = tasks[:on_page].any? { |id| (tasks[:by_id][id] || {}).dig('check', 'type') == 'pip' }
        code_input = if code
            keywords = {
                :sql => Rouge::Lexers::SQL.keywords.to_a,
                :sqlTypes => Rouge::Lexers::SQL.keywords_type.to_a,
                :cypher => Rouge::Lexers::WorkspaceCypher::KEYWORDS,
            }
            "<script>window.CODE_INPUT_KEYWORDS = #{keywords.to_json};</script>" \
                "<script src='/include/code_input.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
        end
        # What gives away a task's answer waits for it (AfterTask), for
        # everyone who can answer; tasks.js shows it then. A teacher sees the
        # page as the class does and can open it without answering
        # (revealAfter: a button in the line that waits).
        waits = user_logged_in?
        "<style>.tasks-off-only { display: none; }" \
            "#{waits ? ' .after-task:not(.after-task-shown) > .after-task-body { display: none; } .after-task:not(.after-task-shown) > .after-task-wait { display: block; }' : ''}</style>" \
            "<script>window.TASK_PAGE = #{config.to_json.gsub('</', '<\/')};</script>" \
            "#{previews ? "<script src='/include/anaglyph_preview.js?#{CACHE_BUSTER}'></script>" : ''}" \
            "#{code_input}" \
            "#{pip ? "<script src='/include/pip_world.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>" : ''}" \
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
            # never a page of a hidden section (the Backrooms are found, not
            # named)
            other = @@content.find do |other_slug, c|
                other_slug != slug && c[:tasks] && (c[:included_parts] || []).include?(part) &&
                    solved.include?(c[:tasks][:activity]) && !(@@sections[c[:section]] || {})[:hidden]
            end
            seen[part] = other[1][:title].to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '') if other
        end
        return '' if seen.empty?

        "<script>window.SEEN_PARTS = #{seen.to_json.gsub('</', '<\/')};</script>" \
            "<script src='/include/include_fold.js?#{CACHE_BUSTER}'></script>"
    end

    # The picture in the front page's header (index.html): a tidy Workspace
    # from a recipe on the about page, through the image cache.
    FRONT_PICTURE = '/src/content/about/workspace.webp'

    def front_picture_html
        return '' unless File.file?(FRONT_PICTURE)

        src = "/cache/#{Main.convert_image(FRONT_PICTURE)}.webp"
        # the same picture once more, upside down: its reflection on the
        # floor (index.html), for nobody who reads the page aloud
        "<img class='front-hero-picture' src='#{src}' " \
            "alt='Der Workspace: oben im Editor ein kurzes Programm, das nach dem Namen fragt, " \
            "unten im Terminal sein Lauf: Hallo, Ada!'>" \
            "<img class='front-hero-reflection' src='#{src}' alt='' aria-hidden='true'>"
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
    # language: the card's programming language (CodeLanguages); the task
    # comes back as it is in that language, with the version of all of them.
    def find_task(key, language = nil)
        activity, id = key.to_s.split('/', 2)
        return nil unless activity.to_s =~ /\A[a-z0-9_-]+\z/ && id.to_s =~ TaskSpec::ID_PATTERN

        entry = @@content.values.find { |c| (c[:tasks] || {})[:activity] == activity }
        task = entry && entry[:tasks][:by_id][id]
        return nil unless task

        version = TaskSpec.check_version(task)
        task = CodeLanguages.variant(task, language) if CodeLanguages.known?(language)
        { :key => "#{activity}/#{id}", :task => task, :version => version }
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

    # Messages carry text from students' files: rendered and then filtered
    # to harmless HTML (TaskHtml). Feedback boxes are light and style plain
    # code themselves (.task-feedback pre code), so no highlighting here.
    def render_task_markdown(markdown, lineno: false)
        TaskHtml.render(markdown, :lineno => lineno)
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

    def store_task_result(found, status, answer, email = @session_user[:email], max: TaskStore::MAX_ANSWER)
        key = TaskStore.state_key(email, found[:key])
        now = Time.now.to_i
        previous = neo4j_query(<<~END_OF_QUERY, :email => email, :key => key).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s;
        END_OF_QUERY
        props = TaskStore.next_state(previous && previous['s'],
            :task => found[:key], :version => found[:version], :status => status,
            :answer => answer, :now => now, :max => max)
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

    # --- Answers under open tasks (docs/aufgaben.md) ---------------------------

    # Example answers are teaching material, for every teacher; the answers
    # of a group only for those who may see its class board.
    def answer_examples_visible?
        teacher_account_logged_in? || teacher_logged_in?
    end

    # For teachers on an activity page: under each open task the example
    # answers or those of the current group, as chosen last (tasks.js).
    # The example answers go only into this config, never into the cards.
    # pinned: the groups that have this activity pinned (on /klasse); tasks.js
    # gives each a button with how many have answered.
    def open_answers_config(tasks, slug)
        ids = OpenAnswers.task_ids(tasks)
        return nil if ids.empty?

        examples = answer_examples_visible?
        groups = class_groups
        return nil unless examples || !groups.empty?

        prefs = class_prefs
        pinned = pins_for_groups(groups).select { |p| p[:slug] == slug }.map { |p| p[:group] }
        {
            :view => OpenAnswers.view(prefs['answer_view'], :examples => examples, :groups => !groups.empty?),
            :groups => groups.map { |g| { :name => g, :size => (@@group_members[g] || []).size } },
            :group => groups.include?(prefs['group']) ? prefs['group'] : (groups.find { |g| pinned.include?(g) } || groups.first),
            :pinned => groups.select { |g| pinned.include?(g) },
            :examples => examples ? ids.to_h { |id| [id, (tasks[:by_id][id]['examples'] || []).map { |e| render_task_markdown(e) }] } : nil,
        }
    end

    # For teachers, beside each Kata's buttons: its solution's code in every
    # language it has, { id => { language => code } }, for a button that
    # puts it into the editor (tasks.js). Like the example answers only in
    # this config, never in the cards.
    def kata_solutions_config(tasks)
        return nil unless answer_examples_visible? || admin_logged_in?

        solutions = {}
        tasks[:on_page].each do |id|
            task = tasks[:by_id][id]
            next unless task && %w[function pip tests].include?(task.dig('check', 'type'))

            [task, *CodeLanguages.variant_languages(task).map { |l| CodeLanguages.variant(task, l) }].each do |t|
                next unless t['solution']

                code = t['solution'].scan(/^```[^\n]*\n(.*?)^```/m).map(&:first).join("\n")
                (solutions[id] ||= {})[t.dig('check', 'language')] = code unless code.empty?
            end
        end
        solutions.empty? ? nil : solutions
    end

    # --- Class board (docs/aufgaben.md, »Klassenübersicht«) ---------------------

    # Groups whose class board the logged-in person may see: administrators
    # every real group, group administrators (+) their groups, and every
    # teacher its own test group, last; never another teacher's.
    def class_groups
        return [] unless user_logged_in?

        groups = (defined?(@@group_members) ? @@group_members : {}).keys.sort_by { |g| @@user_group_order.index(g) || 0 }
        me = @session_user[:email]
        TestAccounts.visible_groups(groups, :admin => admin_logged_in?, :own => @@teachers[me] || Set.new,
            :test_groups => @@test_groups, :own_test_group => @@test_group_of[me])
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
    # variant_of: slug -> the variant whose tasks count (ClassRepos), for
    # a person or a group; nil counts the page as it is.
    def activities_with_tasks(variant_of = nil)
        @@content.select { |_, c| c[:tasks] && !(c[:tasks][:on_page] || []).empty? && (DEVELOPMENT || !c[:dev_only]) && !c[:teacher_only] }
            .map do |slug, c|
                on_page = tasks_in_variant(c, variant_of && variant_of.call(slug))[:on_page]
                tasks = on_page.map do |id|
                    t = c[:tasks][:by_id][id]
                    { :id => id, :title => t['title'], :type => t['check']['type'],
                      :version => TaskSpec.check_version(t), :prediction => t['prediction'] == true,
                      :extra => t['extra'] == true }
                end
                { :slug => slug, :activity => c[:tasks][:activity],
                  :title => CGI.unescapeHTML(c[:title].to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '')), :tasks => tasks }
            end
    end

    # Activities for the class board of a group: [[activity, title, tasks]],
    # the tasks as the group sees them.
    def class_activities(group)
        activities_with_tasks(->(slug) { group_page_variant(slug, group) }).map { |a| [a[:activity], a[:title], a[:tasks]] }.sort_by { |_, title, _| title.downcase }
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
        @my_progress ||= StudentHome.progress(activities_with_tasks(->(slug) { class_repo_variant(slug) }), my_task_states)
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

    # Activities as <option>s grouped by their catalog section, in catalog
    # order (headings in the search field, searchable_select); within a
    # section by title. items: [[value, title, slug]].
    def grouped_activity_options(items, selected = nil)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        section_of = {}
        @@section_order.each { |key| ((@@sections[key] || {})[:entries] || []).each { |slug| section_of[slug.to_s.delete_prefix('.')] ||= key } }
        groups = items.group_by { |_, _, slug| section_of[slug] }
        order = @@section_order + [nil]
        order.filter_map do |key|
            list = groups[key]
            next if list.nil? || list.empty?

            label = key ? CGI.unescapeHTML((@@sections[key] || {})[:label].to_s.gsub('&shy;', '')) : 'Weitere'
            options = list.sort_by { |_, title, _| title.downcase }.map do |value, title, slug|
                "<option value='#{h.(value)}' data-search='#{h.(activity_search_text(slug))}'#{value == selected ? ' selected' : ''}>#{h.(title)}</option>"
            end
            "<optgroup label='#{h.(label)}'>#{options.join}</optgroup>"
        end.join
    end

    # What else finds an activity in the search fields of the class page
    # (searchable_select): the label of its catalog section (»Katas« finds
    # Pip's sets too) and its description.
    def activity_search_text(slug)
        section = @@section_order.find { |key| ((@@sections[key] || {})[:entries] || []).include?(slug) }
        label = section ? CGI.unescapeHTML((@@sections[section] || {})[:label].to_s.gsub('&shy;', '')) : ''
        "#{label} #{(@@content[slug] || {})[:abstract].to_s.strip[0, 600]}".strip
    end

    # The title as text: without tags, with »&« instead of &amp; (it is
    # escaped where it goes into HTML)
    def plain_title(slug)
        CGI.unescapeHTML((@@content[slug] || {})[:title].to_s.gsub(/<[^>]*>/, '').gsub('&shy;', '')).strip
    end

    # Who gets the student's parts of the start page (StudentHome: »Weiter
    # mit«, »So geht's«, the progress on the cards): logged in, tasks on, no
    # teacher account (their start page stays as it was) and no test running.
    def student_home_visible?
        user_logged_in? && tasks_visible? && !teacher_account_logged_in? && my_running_tests.empty?
    end

    # The hue and icon of a set of Katas (KataMap: its place in the Katas
    # section, as the strip shows it), or nil for any other page.
    def kata_teaser(slug)
        entries = ((@@sections['katas'] || {})[:entries] || [])
        index = entries.index(slug)
        index && { :hue => KataMap.hue(index, entries.size), :icon => (@@content[slug] || {})[:icon] }
    end

    # Start page for logged-in students: activities a teacher pinned, where
    # to continue, how the site works (M5).
    def student_home
        return '' unless user_logged_in? && tasks_visible?
        return '' unless my_running_tests.empty?

        allowed = pinnable_slugs
        pinned = pins_for_groups(my_groups).select { |p| allowed.include?(p[:slug]) }.uniq { |p| p[:slug] }.map do |p|
            c = @@content[p[:slug]]
            # the catalog's picture, its dark variant and focus (CardImages)
            { :slug => p[:slug], :title => plain_title(p[:slug]), :abstract => c[:abstract], :group => p[:group],
              :template_folder => (c[:template] || {})[:folder], :kind => PageKinds.kind_label(c[:kind]),
              :image => c[:image] && CardImages.attributes(c[:image]), :image_dark => c[:image_dark] && CardImages.attributes(c[:image_dark]),
              :position => "#{c[:image_x] || 50}% #{c[:image_y] || 50}%",
              # a set of Katas: its band from the strip, in its hue
              :kata => kata_teaser(p[:slug]) }
        end
        html = StudentHome.html(:progress => my_progress, :pinned => pinned, :listed => allowed,
                                :guide => student_home_visible?)
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

    # Pinned activities of a group, with controls to pin and unpin: a
    # heading with what pinning does, the pinned activities as a list (title,
    # kind, »Lösen«), and a search field to pin another.
    def class_pins_html(group)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        pins = pins_for_groups([group])
        io = +"<section class='class-pins' data-group='#{h.(group)}' aria-labelledby='class-pins-title'>"
        io << "<h3 class='class-pins-title' id='class-pins-title'><i class='bi bi-pin-angle-fill' aria-hidden='true'></i>Angeheftet für #{h.(group)}</h3>"
        io << "<p class='class-pins-note'>Was du hier anheftest, sehen alle in der Gruppe als Erstes auf ihrer Startseite, mit ihrem Fortschritt; sie können es dort nicht wegklicken.</p>"
        if pins.empty?
            io << "<p class='class-pins-none'>Noch nichts angeheftet.</p>"
        else
            io << "<ul class='class-pins-list'>"
            pins.each do |p|
                title = plain_title(p[:slug])
                kind = PageKinds.kind_label((@@content[p[:slug]] || {})[:kind])
                io << "<li class='class-pin'><a href='/#{h.(p[:slug])}'>#{h.(title)}</a>"
                io << "<span class='class-pin-kind'>#{h.(kind)}</span>" if kind
                io << "<button type='button' class='btn btn-sm btn-outline-secondary class-unpin' data-slug='#{h.(p[:slug])}' aria-label='#{h.(title)} lösen'>" \
                      "<i class='bi bi-x-lg' aria-hidden='true'></i>Lösen</button></li>"
            end
            io << '</ul>'
        end
        io << "<div class='class-pins-add'><select class='form-select class-pin-select' aria-label='Aktivität suchen und anheften'><option value=''>Aktivität suchen und anheften …</option>"
        pinned = pins.map { |p| p[:slug] }
        io << grouped_activity_options(pinnable_slugs.reject { |s| pinned.include?(s) }.map { |s| [s, plain_title(s), s] })
        io << '</select></div></section>'
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
            if members.any? { |m| @@test_accounts[m[:email]] == @session_user[:email] }
                io << "<p class='class-test-reset'><button type='button' class='btn btn-sm btn-outline-danger bu-class-reset-test'>" \
                    "<i class='bi bi-arrow-counterclockwise' aria-hidden='true'></i>Testkonto zurücksetzen</button> " \
                    "<span class='class-test-reset-note'>Leert seinen Workspace und seine Workspaces in Leistungsüberprüfungen, legt seine Datenbanken neu an und löscht seine Antworten und Abgaben. Andere Konten bleiben, wie sie sind.</span></p>"
            end
        end
        # How busy the server is, so that a slow Workspace can be told from a
        # busy server, and how much room is left on the disk of the
        # Workspaces (df: the whole partition, so nothing can be saved once
        # it is full): one row as on »Betrieb«, from the teachers' websocket
        # (server_stats), see class_board.js.
        cell = ->(icon, label) { "<div class='server-load-cell'><div class='server-load-label'><i class='bi #{icon}' aria-hidden='true'></i>#{label}</div><div class='progress' role='progressbar' aria-label='#{label}'><div class='progress-bar progress-bar-striped' style='width: 0%'></div></div><p>–</p></div>" }
        io << "<div class='server-load-pair server-load-three class-server-load mb-3' aria-label='Server, jede Sekunde neu'>#{cell.('bi-cpu', 'CPU des Servers')}#{cell.('bi-memory', 'RAM des Servers')}#{cell.('bi-nvme', 'Speicherplatz')}</div>"
        io << class_group_html(group) unless test_group
        running = (workspace_runtime.running_workspaces(:timeout => shell_timeout(:docker_network_inspect)).keys rescue [])
        io << "<div class='class-people-scroll'><table class='table align-middle class-people-table'>"
        io << "<thead><tr><th>Name</th><th>Workspace</th><th class='text-end'>CPU</th><th class='text-end'>RAM</th><th></th></tr></thead><tbody>"
        members.each do |m|
            email = m[:email]
            test = @@test_accounts.key?(email)
            io << "<tr data-email='#{h.(email)}'><td class='class-person'>#{h.(m[:name])}</td>"
            # Right from the start, not »aus« until the websocket says otherwise.
            on = running.include?(fs_tag_for_email(email))
            io << "<td class='class-ws-status#{on ? ' class-ws-running' : ''}' data-fs-tag='#{h.(fs_tag_for_email(email))}'><span class='class-ws-dot' aria-hidden='true'></span><span class='class-ws-text'>#{on ? 'läuft' : 'aus'}</span></td>"
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
        store_class_prefs(prefs)
    end

    def store_class_prefs(prefs)
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

    # »Neue Gruppe« on the class page: for teachers with a school (their
    # groups belong to it) and the administration.
    def may_create_group?
        admin_logged_in? || (teacher_account_logged_in? && !school_of_teacher(my_email).nil?)
    end

    def new_group_button_html
        return '' unless may_create_group? && !Main.roster_imported.nil?

        "<button type='button' class='btn btn-outline-secondary class-group-new'><i class='bi bi-plus-lg' aria-hidden='true'></i>Neue Gruppe</button>"
    end

    # In the tab »Lernende«: the group's card from include/groups.js
    # (members, co-teachers, consent, welcome mails, end), filled by
    # class_board.js, which moves each person's Workspace into its row.
    def class_group_html(group)
        "<section class='class-group-manage' data-group='#{CGI.escapeHTML(group.to_s)}'><p class='text-body-secondary'>Einen Moment …</p></section>"
    end

    # The class page (/klasse): the group up top, waiting login codes right
    # under it (never missed), and three tabs, each with one job: what the
    # group did in an activity, the people and their Workspaces, and what
    # is pinned on their start page.
    def class_board_page
        groups = class_groups
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        if groups.empty?
            return "<p>Du betreust noch keine Gruppe.</p><p>#{new_group_button_html}</p>" unless new_group_button_html.empty?

            return "<p>Du betreust noch keine Gruppe. Dir ist noch keine Schule zugeordnet; das macht die Administration. Danach legst du hier deine Gruppen an.</p>"
        end

        prefs = class_prefs
        group = [params['group'], prefs['group']].find { |g| groups.include?(g) } || groups.first
        # »gruppe«: the tab that managed the group is part of »Lernende« now.
        wanted = params['tab'] == 'gruppe' ? 'lernende' : params['tab']
        tab = CLASS_TABS.any? { |key, _, _| key == wanted } ? wanted : 'fortschritt'
        io = +"<form class='class-board-select' method='get' action='/klasse'>"
        # a search field (class_board.js), with the schools as headings
        io << "<label>Gruppe <select class='form-select class-group-choice' name='group'>"
        option = ->(g) { "<option value='#{h.(g)}'#{g == group ? ' selected' : ''}>#{h.(g)} (#{(@@group_members[g] || []).size})</option>" }
        # With more than one school (partner schools, guests), the groups are
        # nested under their school; test groups and groups without a school
        # come last, on their own.
        schools = school_names_of_groups
        by_school = groups.group_by { |g| schools[g] }
        if (by_school.keys - [nil]).size > 1
            by_school.each do |school, names|
                next if school.nil?

                io << "<optgroup label='#{h.(school)}'>#{names.map(&option).join}</optgroup>"
            end
            (by_school[nil] || []).each { |g| io << option.call(g) }
        else
            groups.each { |g| io << option.call(g) }
        end
        io << "</select></label><input type='hidden' name='tab' value='#{h.(tab)}'>"
        # The programming language of the group's activities (CodeLanguages),
        # where an activity offers more than one; saved at once, not sent with
        # the form.
        current = group_code_languages([group])[group]
        io << "<label title='Für Aktivitäten, die es in mehreren Sprachen gibt. Lernende können auf der Seite selbst umschalten.'>Sprache <select class='form-select class-code-language' data-group='#{h.(group)}'>"
        CodeLanguages::GROUP_LANGUAGES.each do |language|
            selected = (current || CodeLanguages::DEFAULT) == language ? ' selected' : ''
            io << "<option value='#{language}'#{selected}>#{CodeLanguages::NAMES[language]}</option>"
        end
        io << "</select></label>"
        io << "<script>document.querySelector('.class-code-language').addEventListener('change', function (e) {" \
            "api_call('/api/set_group_code_language', { group: e.target.dataset.group, language: e.target.value }, function () {}, { no_please_wait: true });" \
            "});</script>"
        # Copy buttons on every code block for the group's students, or only
        # where the page has them (group_copy_all_code?); saved at once.
        copy_all = group_copy_all_code?([group])
        io << "<label title='An jedem Codeblock (Voreinstellung): überall ein Kopierknopf. Wo sinnvoll: nur dort, wo Abtippen nichts lehrt (Adressen, Befehle, große Dateien); alles andere tippen die Lernenden ab.'>" \
            "Kopierknöpfe <select class='form-select class-copy-all-code' data-group='#{h.(group)}'>" \
            "<option value='all'#{copy_all ? ' selected' : ''}>An jedem Codeblock</option>" \
            "<option value='marked'#{copy_all ? '' : ' selected'}>Wo sinnvoll</option></select></label>"
        io << "<script>document.querySelector('.class-copy-all-code').addEventListener('change', function (e) {" \
            "api_call('/api/set_group_copy_all_code', { group: e.target.dataset.group, on: e.target.value === 'all' }, function () {}, { no_please_wait: true });" \
            "});</script>"
        io << class_repos_button_html(group)
        io << "<noscript><button class='btn btn-primary'>Anzeigen</button></noscript>"
        # For the projector: the group's codes, big, names only (class_board.js).
        io << "<span class='class-board-actions'>#{new_group_button_html}" \
            "<button type='button' class='btn btn-outline-secondary class-codes-show'><i class='bi bi-projector' aria-hidden='true'></i>Anmelde-Codes zeigen</button></span></form>"
        io << "<div class='class-login-codes' data-group='#{h.(group)}' data-school='#{h.(school_names_of_groups[group])}' hidden><h2><i class='bi bi-key' aria-hidden='true'></i>Anmelde-Codes</h2>" \
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

    # The class repositories of the group (ClassRepos): a button beside the
    # language that opens a dialog with one field per activity that clones
    # one (class_board.js); its count says how many are set.
    def class_repos_button_html(group)
        slugs = class_repo_slugs
        return '' if slugs.empty?

        h = ->(t) { CGI.escapeHTML(t.to_s) }
        set = group_class_repos([group])[group] || {}
        entries = slugs.map do |slug|
            { :slug => slug, :title => plain_title(slug), :address => set[slug],
              :folder => ((@@content[slug] || {})[:template] || {})[:folder] || slug,
              # with the Workspace's own Git server: a starter folder fills a new repository
              :starter => File.directory?(File.join('/src/content', slug, 'starter')) }
        end
        count = entries.count { |e| e[:address] }
        state = "Klassen-Repositorys: #{count} von #{entries.size} eingetragen"
        git = GitServer.enabled? ? " data-git-server='1' data-git-keys='#{h.((Main.git_groups[group] || {})[:keys] || GitServer::KEYS_HANDED_IN)}'" : ''
        "<button type='button' class='btn btn-outline-secondary class-repos-open#{count > 0 ? ' has-repos' : ''}' data-group='#{h.(group)}' " \
            "data-repos='#{h.(entries.to_json)}'#{git} aria-label='#{h.(state)}' title='#{h.(state)}'>" \
            "<i class='bi bi-hdd-network' aria-hidden='true'></i>Git<span class='class-repos-count'>#{count > 0 ? "· #{count}" : ''}</span></button>"
    end

    # The tab »Fortschritt«: the activity, a toolbar (look into the
    # Workspaces now, is the template current) and the board.
    def class_progress_html(group)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        activities = class_activities(group)
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
        # searchable (class_board.js): the list of all activities is too long
        # to scroll, and a native list that tall picked an entry by itself
        io << "<label>Aktivität <select class='form-select class-activity-choice' name='activity'>"
        slugs = @@content.each_with_object({}) { |(s, c), all| all[c[:tasks][:activity]] ||= s if c[:tasks] }
        io << grouped_activity_options(activities.map { |a, t, _| [a, t, slugs[a]] }, activity_key)
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
        io << class_kata_hand_ins_html(activity_key, tasks, members)
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
                              :outline => content[:outline], :sections => content[:search_sections], :search_words => content[:search_words],
                              :section_label => section[:label], :section_icon => section[:icon] }
                        end
                    end
                    io.puts ReferenceIndex.html(pages)
                    section_order = []
                end
                card_index = 0
                # pinned for one of the person's groups: a pin on the card, as on the Kata strip
                catalog_pins = user_logged_in? ? pins_for_groups((my_groups + class_groups).uniq).map { |p| p[:slug] }.to_set : Set.new
                section_order.each do |section_key|
                    section = @@sections[section_key]
                    # hidden: true in sections.yaml: the pages exist, the
                    # catalog does not show them.
                    next if section[:hidden]
                    next if section[:entries].reject do |entry|
                        (!DEVELOPMENT) && @@content[entry][:dev_only]
                    end.empty?
                    # The icon next to the heading is decoration (alt='').
                    io.puts "<h2><div class='squircle'><img alt='' src='#{section[:icon]}'></div> #{section[:label]}</h2>"
                    if section[:description]
                        io.puts "<p style='margin-top: -1em; margin-bottom: 1em;'>#{section[:description]}</p>"
                    end
                    entries = section[:entries].reject do |entry|
                        (!DEVELOPMENT) && @@content[entry][:dev_only]
                    end
                    # carousel: true (Katas): one strip of the sets with the
                    # person's progress instead of cards (KataMap)
                    if section[:carousel]
                        io.puts kata_carousel_html(section)
                        next
                    end
                    # tiles: true (programming languages): small tiles with
                    # logo, name and year instead of cards, all visible.
                    if section[:tiles]
                        io.puts "<div class='lang-tiles'>"
                        entries.each do |slug|
                            content = @@content[slug]
                            title = content[:title].to_s
                            name = title.sub(%r{<span[^>]*>.*?</span>}, '').strip
                            year = title[/\((\d{4})\)/, 1]
                            image = content[:image] || '/images/white.webp'
                            dark = content[:image_dark]
                            # Small variants, loaded when near the screen (CardImages).
                            tile_image = ->(url) { "#{CardImages.attributes(url, :sizes => CardImages::TILE_SIZES)} loading='lazy' decoding='async'" }
                            # A logo with a dark variant (pascal-logo-dark.png) shows it in dark mode.
                            logo = if dark
                                "<img alt='' class='theme-image-light' #{tile_image.(image)}>" \
                                    "<img alt='' class='theme-image-dark' #{tile_image.(dark)}>"
                            else
                                "<img alt='' #{tile_image.(image)}>"
                            end
                            io.puts "<a class='lang-tile' href='/#{slug}'>" \
                                "<span class='lang-tile-logo'>#{logo}</span>" \
                                "<span class='lang-tile-name'>#{name}</span>" \
                                "#{year ? "<span class='lang-tile-year'>#{year}</span>" : ''}</a>"
                        end
                        io.puts "</div>"
                        next
                    end
                    io.puts "<div class='row'>"
                    # Every card the same size; a section ends where it ends.
                    entries.each do |slug|
                        content = @@content[slug]
                        io.puts "<div class='hscol'>"
                        pinned_card = catalog == :student && catalog_pins.include?(slug)
                        io.puts "<a href='/#{slug}' class='tutorial_card3#{pinned_card ? ' card-pinned' : ''}'>"
                        io.puts "<span class='card-pin' title='Angepinnt'><i class='bi bi-pin-angle-fill' aria-hidden='true'></i><span class='visually-hidden'>Angepinnt</span></span>" if pinned_card
                        io.puts "<h4>#{content[:title]}</h4>"
                        io.puts "<div>"
                        additional_classes = []
                        if content[:needs_contrast] == 'light'
                            additional_classes << 'dark-only-bg-contrast-light'
                        end
                        # Small variants (CardImages). Only the first row loads
                        # at once; the rest when it comes near the screen, and
                        # of a light and a dark picture only the one shown.
                        card_index += 1
                        loading = card_index <= 3 && !content[:image_dark] ? '' : " loading='lazy'"
                        card_image = ->(url) { "#{CardImages.attributes(url)}#{loading} decoding='async'" }
                        image_style = "object-position: #{content[:image_x]}% #{content[:image_y]}%;"
                        # What the picture shows (image_alt); the title next to
                        # it is no alt text, so without one it stays empty.
                        card_alt = CGI.escapeHTML(content[:image_alt].to_s)
                        if content[:image_dark]
                            io.puts "<img alt='#{card_alt}' class='#{(additional_classes + ['theme-image-light']).join(' ')}' #{card_image.(content[:image])} style='#{image_style}'>"
                            io.puts "<img alt='#{card_alt}' class='#{(additional_classes + ['theme-image-dark']).join(' ')}' #{card_image.(content[:image_dark])} style='#{image_style}'>"
                        else
                            io.puts "<img alt='#{card_alt}' class='#{additional_classes.join(' ')}' #{card_image.(content[:image] || '/images/white.webp')} style='#{image_style}'>"
                        end
                        io.puts "<div class='shade'></div>"
                        io.puts "<div class='card-content'>"
                        # The kind on its own line above the title (default.css).
                        # With it, for a student who solved a task here, how many (»3/7«).
                        io.puts StudentHome.card_badges(content[:kind], content[:language]) if catalog == :student
                        io.puts "<div class='card-title-row'>"
                        io.puts "#{content[:dev_only] ? '<span class="badge badge-sm bg-danger">dev</span> ' : ''}<h4>#{content[:title]}</h4>"
                        io.puts "</div>"
                        io.puts "<div class='abstract'>#{content[:abstract]}</div>"
                        if catalog == :student
                            io.puts StudentHome.card_requires(PageKinds.requires_of(slug, @@content, :development => DEVELOPMENT).map(&:last))
                        end
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

    def stub
        write = contact_email.empty? ? 'Melde dich' : "Schreib eine E-Mail an #{contact_email_link}"
        "<em>Dieser Artikel ist noch nicht vollständig. #{write}, wenn du dabei helfen möchtest, ihn fertigzustellen.</em>\n"
    end

    def contact_email
        Main.contact_email
    end

    def contact_email_link
        email = CGI.escapeHTML(contact_email)
        email.empty? ? '' : "<a href='mailto:#{email}'>#{email}</a>"
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
                # Whose Workspace is whose. A Workspace opened for someone
                # without an account yet (the account comes with it) is not
                # in the list: an unknown one reloads it, at most every 5 s.
                email_for_tag = {}
                email_for_tag_loaded_at = Time.at(0)
                load_email_for_tag = lambda do
                    email_for_tag = neo4j_query('MATCH (u:User) RETURN u.email;').to_h { |row| [fs_tag_for_email(row['u.email']), row['u.email']] }
                    email_for_tag_loaded_at = Time.now
                end
                load_email_for_tag.call
                # For admins also the disk space of each Workspace, as the
                # hourly housekeeping measured it; read again every minute.
                show_disk = admin_logged_in?
                disk = nil
                disk_loaded_at = Time.at(0)
                docker_stats_thread = Thread.new do
                    loop do
                        if show_disk && Time.now - disk_loaded_at > 60
                            disk = workspace_disk_usage
                            disk_loaded_at = Time.now
                        end
                        lines = {}
                        workspace_runtime.workspace_stats.each_pair do |fs_tag, stat_line|
                            load_email_for_tag.call if !email_for_tag.key?(fs_tag) && Time.now - email_for_tag_loaded_at > 5
                            email = email_for_tag[fs_tag]
                            next unless workspace_user_visible_to?(email, viewer_email)
                            lines[fs_tag] = {
                                :email => email,
                                :name => (@@invitations[email] || {})[:name],
                                :group => (@@invitations[email] || {})[:group],
                                :stats => stat_line
                            }
                            if disk && disk[:by_tag].key?(fs_tag)
                                lines[fs_tag][:disk_kb] = disk[:by_tag][fs_tag].to_i
                                lines[fs_tag][:disk_fat] = lines[fs_tag][:disk_kb] >= disk[:fat_kb]
                            end
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

        # the page changes the button and the menu itself, without reloading
        respond(:module => key, :shown => new_flag, :label => MODULE_LABELS[key])
    end

    # Checks an answer and, if it counts, remembers it (docs/aufgaben.md).
    post '/api/task/check' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task, :answer], :optional_keys => [:language],
            :max_body_length => 16384,
            :max_value_lengths => {:task => 120, :answer => TaskStore::MAX_ANSWER})
        found = find_task(data[:task], data[:language])
        halt 404 if found.nil?

        result = TaskCheck.check(found[:task], data[:answer],
            :variables => task_variables(found[:task], found[:key]),
            :workspace => task_workspace_files,
            :git => lambda { |repo| task_git_state(repo) },
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
            :solution => render_task_markdown(result.solution, :lineno => true),
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
            :submitted_at => File.mtime(path).to_i,
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

    # The Anaglyph viewer (AnaglyphViewer, check option preview: anaglyph):
    # pictures of handed-in scenes on the task card and in the class gallery.
    # A scene's expressions are JavaScript, so the viewer runs only on
    # abgaben.<host>, sandboxed, never on the site: there a scene cannot use
    # the session of whoever looks at it. The page asks here for a scene's
    # address and gets a signed one there. vorschau/ is the file someone is
    # about to hand in, read from their Workspace; abgaben/ a handed-in scene
    # (ich: one's own), for that person and the teachers of their groups.
    get '/api/anaglyph/address' do
        assert(user_logged_in?)
        rel = params['scene'].to_s
        data = if (m = rel.match(%r{\Avorschau/([a-z0-9_-]+)/([^/]+)/([^/]+\.ini)\z}))
            found = anaglyph_preview_task("#{m[1]}/#{m[2]}")
            ws = task_workspace_files
            halt 404 if ws.nil?
            pick = found[:task]['check']['pick']
            dir = File.dirname(pick)
            path = dir == '.' ? m[3] : "#{dir}/#{m[3]}"
            halt 404 unless SubmissionStore.candidates(ws, pick).include?(path)
            { 'e' => @session_user[:email], 'f' => path }
        elsif (m = rel.match(%r{\Aabgaben/([a-z0-9_-]+)/([^/]+)/(ich|[0-9a-f]{32})\.ini\z}))
            found = anaglyph_preview_task("#{m[1]}/#{m[2]}")
            email = anaglyph_submission_owner(found[:key], m[3])
            halt 404 if email.nil?
            { 's' => TaskStore.state_key(email, found[:key]) }
        else
            halt 404
        end
        token = AnaglyphViewer.scene_token(data, :secret => SUBMISSION_VIEW_SECRET)
        response.headers['Cache-Control'] = 'no-store'
        respond(:url => "#{SUBMISSION_VIEW_ROOT}/anaglyph-viewer/index.html?scene=#{CGI.escape("szene/#{token}.ini")}")
    end

    # The demo level of the Backrooms as a website (BackroomsDemo): the
    # folder export.sh builds, copied to /internal/nachtschicht. Streamed,
    # since its light alone is about 20 MB.
    get "/#{BackroomsDemo::PREFIX}" do
        redirect "/#{BackroomsDemo::PREFIX}/"
    end

    get "/#{BackroomsDemo::PREFIX}/*" do
        path = BackroomsDemo.file(params['splat'].first)
        halt 404 if path.nil?
        @respond_streaming = true
        if BackroomsDemo.revalidate?(path)
            cache_control :public, :no_cache
        else
            cache_control :public, :max_age => 600
        end
        send_file path, :type => BackroomsDemo.mime(path)
    end

    # abgaben.<host>/anaglyph-viewer/: the viewer, the Kenney kits it loads
    # and, under szene/<token>.ini, the scene the token names. No session;
    # the token is the only permission.
    get '/anaglyph-viewer/*' do
        halt 404 unless request.host == SUBMISSION_VIEW_HOST
        rel = params['splat'].first.to_s
        if (m = rel.match(%r{\Aszene/([A-Za-z0-9_.-]+)\.ini\z}))
            data = AnaglyphViewer.verify_scene_token(m[1], :secret => SUBMISSION_VIEW_SECRET)
            halt 404 if data.nil?
            text = begin
                if data['s'].is_a?(String) && data['s'].match?(/\A[0-9a-f]{32}\z/)
                    AnaglyphViewer.scene_from_submission(File.join(SUBMISSIONS_ROOT, "#{data['s']}.tar.gz"))
                elsif data['e'].is_a?(String) && data['f'].is_a?(String)
                    path = "/user/#{fs_tag_for_email(data['e'])}/workspace"
                    File.directory?(path) ? WorkspaceFiles.new(path).read_text(data['f'], max: 1_000_000) : nil
                end
            rescue StandardError
                nil
            end
            halt 404 if text.nil?
            SubmissionView.headers('szene.ini', WEB_ROOT).each { |k, v| response.headers[k] = v }
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
            bytes = AnaglyphViewer.for_sandbox(path, File.binread(path))
            SubmissionView.headers(path, WEB_ROOT).each { |k, v| response.headers[k] = v }
            response.headers['Cache-Control'] = 'public, max-age=3600'
            respond_raw_with_mimetype(bytes, AnaglyphViewer.mime(path))
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
            # a Kata handed in to be shown in class (KataHandIns)
            states[id][:handed_in] = File.file?(submission_path(stored[:task].to_s)) if Main::KATA_TYPES.include?(task['check']['type'])
        end
        respond(:states => states)
    end

    # The answers of a group to the open tasks of an activity, for the
    # teacher's view under the cards (tasks.js).
    post '/api/task/group_answers' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:activity, :group],
            :max_value_lengths => {:activity => 64, :group => 256})
        group = data[:group]
        assert(class_groups.include?(group))
        activity = data[:activity]
        assert(activity =~ /\A[a-z0-9_-]+\z/)
        slug, entry = @@content.find { |_, c| (c[:tasks] || {})[:activity] == activity }
        halt 404 if entry.nil?

        tasks = OpenAnswers.task_ids(tasks_in_variant(entry, group_page_variant(slug, group))).to_h { |id| [id, { :version => TaskSpec.check_version(entry[:tasks][:by_id][id]) }] }
        members = class_members(group)
        states = class_board_states(members.map { |m| m[:email] }, activity)
        respond(:group => group, :members => members.size, :answers => OpenAnswers.collect(members, tasks, states))
    end

    # What a teacher last chose under the open tasks: example answers, the
    # answers of a group (which becomes the current group, as on /klasse),
    # or nothing.
    post '/api/task/answer_view' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:view], :optional_keys => [:group],
            :max_value_lengths => {:view => 16, :group => 256})
        assert(OpenAnswers::VIEWS.include?(data[:view]))
        prefs = class_prefs
        prefs['answer_view'] = data[:view]
        if data[:group]
            assert(class_groups.include?(data[:group]))
            prefs['group'] = data[:group]
        end
        store_class_prefs(prefs)
        respond(:view => data[:view])
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

    # The groups and people a teacher can assign to an exam (profile): the
    # groups of its class page, so its own test account is among them.
    def my_user_group_order
        teacher_logged_in? ? class_groups : []
    end

    def my_user_groups
        my_user_group_order.to_h { |g| [g, @@group_members[g] || []] }
    end

    def my_invitations
        my_user_groups.values.flatten.uniq.to_h { |email| [email, { :name => (@@invitations[email] || {})[:name] || email }] }
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
            log_event "MySQL database #{data[:database]} deleted by its owner: #{@session_user[:email]}"
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

    # The programming language someone wants to see on pages that offer
    # more than one (CodeLanguages); overrides the class's.
    post '/api/set_code_language' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:language])
        assert(CodeLanguages.known?(data[:language]))
        # JavaScript, Dart and Java only where a page offers them
        # (CodeLanguages): they do not replace the choice between Ruby and
        # Python, and picking one of those again leaves them behind.
        if CodeLanguages::GROUP_LANGUAGES.include?(data[:language])
            neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :language => data[:language]})
                MATCH (u:User {email: $email})
                SET u.code_language = $language
                REMOVE u.kata_code_language
                RETURN u;
            END_OF_STRING
        else
            neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :language => data[:language]})
                MATCH (u:User {email: $email})
                SET u.kata_code_language = $language
                RETURN u;
            END_OF_STRING
        end
        respond(:yay => 'sure')
    end

    # The language a teacher sets for a group they administer; '' removes
    # it, and the group sees Ruby again.
    post '/api/set_group_code_language' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:group, :language])
        group = data[:group]
        assert(class_groups.include?(group))
        language = data[:language].to_s
        assert(language.empty? || CodeLanguages::GROUP_LANGUAGES.include?(language))
        if language.empty?
            neo4j_query(<<~END_OF_STRING, {:group => group})
                MATCH (g:GroupSetting {group: $group})
                REMOVE g.code_language;
            END_OF_STRING
        else
            neo4j_query(<<~END_OF_STRING, {:group => group, :language => language})
                MERGE (g:GroupSetting {group: $group})
                SET g.code_language = $language;
            END_OF_STRING
        end
        respond(:yay => 'sure')
    end

    # Every code block copyable for a group's students (the default), or only
    # those the page marks (group_copy_all_code?); stored as given, also
    # true, so that the choice stays when the default changes.
    post '/api/set_group_copy_all_code' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:group, :on], :types => { :on => Object })
        group = data[:group]
        assert(class_groups.include?(group))
        neo4j_query('MERGE (g:GroupSetting {group: $group}) SET g.copy_all_code = $on;', :group => group, :on => data[:on] == true)
        respond(:on => data[:on] == true)
    end

    # The class repositories of a group (ClassRepos), from the dialog on the
    # class page: { slug => address } for pages with class_repo: group, an
    # empty address removes one. All or nothing: with one invalid address
    # nothing is saved, and the answer names it.
    post '/api/set_group_class_repos' do
        teacher_or_admin!
        data = parse_request_data(:required_keys => [:group, :repos], :types => { :repos => Hash },
            :max_body_length => 16384, :max_value_lengths => { :group => 256 })
        group = data[:group]
        halt 403 unless class_groups.include?(group)
        slugs = class_repo_slugs
        changes = {}
        errors = {}
        data[:repos].each do |slug, address|
            halt 400 unless slugs.include?(slug) && address.is_a?(String) && address.size <= 1000
            text = address.strip
            if text.empty? || ClassRepos.valid?(text)
                changes[slug] = text
            else
                errors[slug] = 'invalid'
            end
        end
        return respond(:ok => false, :errors => errors) unless errors.empty?

        row = neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.class_repos AS repos;', :group => group).first
        json = row && row['repos']
        changes.each { |slug, address| json = ClassRepos.update(json, slug, address) }
        if json
            neo4j_query(<<~END_OF_QUERY, :group => group, :json => json)
                MERGE (g:GroupSetting {group: $group})
                SET g.class_repos = $json;
            END_OF_QUERY
        elsif row
            neo4j_query('MATCH (g:GroupSetting {group: $group}) REMOVE g.class_repos;', :group => group)
        end
        STDERR.puts "Class repositories of #{group} set by #{@session_user[:email]}: #{json || '{}'}"
        respond(:ok => true, :repos => ClassRepos.parse(json))
    end

    # --- Repositories of groups in the Workspace (GitServer) -----------------

    def git_teacher_of!(group)
        teacher_or_admin!
        halt 404 unless GitServer.enabled?
        halt 403 unless class_groups.include?(group)
    end

    # The folder of a group on the server; with create, a new one if it has none.
    def git_id_for_group(group, create: false)
        row = neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.git_id AS id;', :group => group).first
        return row['id'] if row && row['id']
        return nil unless create

        neo4j_query('MERGE (g:GroupSetting {group: $group}) SET g.git_id = coalesce(g.git_id, $id);',
            :group => group, :id => GitServer.new_git_id)
        neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.git_id AS id;', :group => group).first['id']
    end

    # Sets (or with nil removes) one class repository of a group; returns the JSON.
    def git_set_class_repo(group, slug, address)
        row = neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.class_repos AS repos;', :group => group).first
        json = ClassRepos.update(row && row['repos'], slug, address)
        if json
            neo4j_query('MERGE (g:GroupSetting {group: $group}) SET g.class_repos = $json;', :group => group, :json => json)
        elsif row
            neo4j_query('MATCH (g:GroupSetting {group: $group}) REMOVE g.class_repos;', :group => group)
        end
        json
    end

    # A teacher creates the repository of a page for the group, from the
    # page's starter/ folder if it has one; its address becomes the group's
    # class repository of that page.
    post '/api/git/create_repo' do
        data = parse_request_data(:required_keys => [:group, :slug], :max_value_lengths => { :group => 256, :slug => 100 })
        group, slug = data[:group], data[:slug]
        git_teacher_of!(group)
        halt 400 unless class_repo_slugs.include?(slug) && slug.match?(GitServer::NAME)
        id = git_id_for_group(group, :create => true)
        unless File.exist?(GitServer.repo_path(id, slug))
            starter = File.join('/src/content', slug, 'starter')
            GitServer.create_repo(id, slug, :starter => File.directory?(starter) ? starter : nil)
        end
        address = GitServer.address(id, slug)
        json = git_set_class_repo(group, slug, address)
        Main.git_refresh!
        STDERR.puts "Git: repository #{id}/#{slug} of #{group} created by #{@session_user[:email]}"
        respond(:ok => true, :address => address, :repos => ClassRepos.parse(json))
    end

    # Deletes the group's repository of a page, with everything in it.
    post '/api/git/delete_repo' do
        data = parse_request_data(:required_keys => [:group, :slug], :max_value_lengths => { :group => 256, :slug => 100 })
        group, slug = data[:group], data[:slug]
        git_teacher_of!(group)
        halt 400 unless slug.match?(GitServer::NAME)
        id = git_id_for_group(group)
        halt 404 unless id
        GitServer.delete_repo(id, slug)
        row = neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.class_repos AS repos;', :group => group).first
        json = row && row['repos']
        # The page's address goes too, if it is this repository's.
        json = git_set_class_repo(group, slug, nil) if GitServer.parse_address(ClassRepos.parse(json)[slug]) == [id, slug]
        STDERR.puts "Git: repository #{id}/#{slug} of #{group} deleted by #{@session_user[:email]}"
        respond(:ok => true, :repos => ClassRepos.parse(json))
    end

    # How the group's students get their keys: handed in, or automatic.
    post '/api/git/set_keys' do
        data = parse_request_data(:required_keys => [:group, :mode], :max_value_lengths => { :group => 256, :mode => 20 })
        group, mode = data[:group], data[:mode]
        git_teacher_of!(group)
        halt 400 unless GitServer::KEY_MODES.include?(mode)
        neo4j_query('MERGE (g:GroupSetting {group: $group}) SET g.git_keys = $mode;', :group => group, :mode => mode)
        count = mode == GitServer::KEYS_AUTO ? Main.git_install_auto_keys(Main.git_group_emails(group)) : 0
        Main.git_refresh!(:auto => false)
        STDERR.puts "Git: keys of #{group} set to #{mode} by #{@session_user[:email]}"
        respond(:ok => true, :mode => mode, :installed => count)
    end

    # The person's public key in their Workspace, if there is one.
    def git_workspace_key(email)
        ssh = File.join('/user', fs_tag_for_email(email), 'workspace', '.ssh')
        %w[id_ed25519.pub id_ecdsa.pub id_rsa.pub].each do |name|
            path = File.join(ssh, name)
            next unless File.file?(path) && File.size(path) < 16_384

            key = GitServer.parse_key(File.read(path))
            return { :file => "~/.ssh/#{name}", :line => key[:line] } if key
        end
        nil
    end

    # For the box on a page: what the server knows of this person's key.
    post '/api/git/key_state' do
        assert(user_logged_in?)
        halt 404 unless GitServer.enabled?
        email = @session_user[:email]
        row = neo4j_query('MATCH (u:User {email: $email}) RETURN u.git_key AS key, u.git_auto_key AS auto;', :email => email).first || {}
        workspace = git_workspace_key(email)
        host = File.exist?(GitServer::HOST_KEY) ? GitServer.fingerprint(File.read(GitServer::HOST_KEY)) : nil
        respond(:handed_in => row['key'] && GitServer.fingerprint(row['key']),
                :workspace => workspace && { :file => workspace[:file], :fingerprint => GitServer.fingerprint(workspace[:line]) },
                :auto => Main.git_auto_emails.include?(email) && !row['auto'].nil?,
                :host => host, :host_name => GitServer::HOST)
    end

    # The person hands in the public key of their Workspace.
    post '/api/git/hand_in_key' do
        assert(user_logged_in?)
        halt 404 unless GitServer.enabled?
        email = @session_user[:email]
        workspace = git_workspace_key(email)
        return respond(:ok => false, :error => 'missing') unless workspace

        neo4j_query('MATCH (u:User {email: $email}) SET u.git_key = $key;', :email => email, :key => workspace[:line])
        Main.git_refresh!(:auto => false)
        respond(:ok => true, :fingerprint => GitServer.fingerprint(workspace[:line]), :file => workspace[:file])
    end

    post '/api/git/remove_key' do
        assert(user_logged_in?)
        halt 404 unless GitServer.enabled?
        neo4j_query('MATCH (u:User {email: $email}) REMOVE u.git_key;', :email => @session_user[:email])
        Main.git_refresh!(:auto => false)
        respond(:ok => true)
    end

    # --- Reading a group's repository in the browser (GitBrowse) -------------
    #
    # /git/<git id>/<name>                        the files of the default branch, README below
    # /git/<git id>/<name>/tree/<ref>/<path>      a folder
    # /git/<git id>/<name>/blob/<ref>/<path>      a file
    # /git/<git id>/<name>/commits/<ref>[/<path>] the history, of one file too
    # /git/<git id>/<name>/commit/<sha>           a commit with its diff
    # /git/<git id>/<name>/archive/<ref>          the files as a zip
    # /git/<git id>/<name>/raw/<ref>/<path>       a picture, nothing else
    #
    # For the members and teachers of the group the folder belongs to.
    # Sinatra (Mustermann) anchors the pattern itself.
    GIT_BROWSE_PATH = %r{/git/([a-z0-9]{10})/([a-z0-9][a-z0-9-]{0,62})(?:/(tree|blob|commits|commit|archive|raw)(?:/(.*))?)?/?}
    GIT_PICTURES = { '.png' => 'image/png', '.jpg' => 'image/jpeg', '.jpeg' => 'image/jpeg', '.gif' => 'image/gif', '.webp' => 'image/webp' }.freeze
    GIT_LANGUAGES = {
        '.rb' => 'ruby', '.py' => 'python', '.js' => 'javascript', '.mjs' => 'javascript', '.json' => 'json',
        '.md' => 'markdown', '.html' => 'html', '.htm' => 'html', '.css' => 'css', '.yaml' => 'yaml', '.yml' => 'yaml',
        '.sh' => 'bash', '.c' => 'c', '.h' => 'c', '.cpp' => 'cpp', '.hpp' => 'cpp', '.cc' => 'cpp', '.java' => 'java',
        '.dart' => 'dart', '.sql' => 'sql', '.go' => 'go', '.rs' => 'rust', '.lua' => 'lua', '.uml' => 'uml', '.er' => 'er',
        '.tex' => 'tex', '.cs' => 'cs', '.php' => 'php', '.svelte' => 'svelte', '.glsl' => 'glsl', '.frag' => 'glsl', '.vert' => 'glsl',
    }.freeze

    def git_group_of_id(id)
        row = neo4j_query('MATCH (g:GroupSetting {git_id: $id}) RETURN g.group AS group;', :id => id).first
        row && row['group']
    end

    def git_may_browse?(group)
        admin_logged_in? || my_groups.include?(group) || class_groups.include?(group)
    end

    def git_url(*parts)
        base = "#{WEB_ROOT}/git/#{@git[:id]}/#{@git[:name]}"
        rest = parts.flatten.compact.map(&:to_s).reject(&:empty?).flat_map { |p| p.split('/') }.map { |p| ERB::Util.url_encode(p) }
        rest.empty? ? base : "#{base}/#{rest.join('/')}"
    end

    def git_time(time)
        time.localtime.strftime('%d.%m.%Y, %H:%M')
    end

    def git_size(bytes)
        return '' unless bytes
        return "#{bytes} Bytes" if bytes < 1024
        return format('%.1f KB', bytes / 1024.0).tr('.', ',') if bytes < 1024 * 1024

        format('%.1f MB', bytes / 1024.0 / 1024.0).tr('.', ',')
    end

    # Code of a file, highlighted like the site's code blocks.
    def git_code_html(text, path)
        language = GIT_LANGUAGES[File.extname(path).downcase] || 'text'
        fragment = Nokogiri::HTML::DocumentFragment.parse("<pre><code class='#{language}_lineno'></code></pre>")
        fragment.at('code').content = text
        CodeHighlight.apply(fragment)
        fragment.to_html
    end

    # A README: Markdown without HTML of its own and without pictures, code highlighted.
    def git_markdown_html(text)
        renderer = Redcarpet::Render::HTML.new(:filter_html => true, :no_images => true, :safe_links_only => true, :no_styles => true)
        html = Redcarpet::Markdown.new(renderer, :fenced_code_blocks => true, :tables => true, :autolink => true).render(text)
        fragment = Nokogiri::HTML::DocumentFragment.parse(html)
        fragment.css('pre > code').each { |code| code['class'] = 'text' unless code['class'].to_s.match?(/\A[a-z]+\z/) }
        CodeHighlight.apply(fragment)
        fragment.to_html
    end

    def git_diff_html(patch)
        lines = patch.each_line.map do |line|
            cls = if line.start_with?('@@') then 'git-hunk'
                  elsif line.start_with?('+') then 'git-add'
                  elsif line.start_with?('-') then 'git-del'
                  end
            text = CGI.escapeHTML(line.chomp)
            cls ? "<span class='#{cls}'>#{text}</span>" : text
        end
        "<pre class='git-diff no-copy'>#{lines.join("\n")}</pre>"
    end

    def git_header_html(ref, tab)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        branches = GitBrowse.branches(@git[:repo])
        address = GitServer.address(@git[:id], @git[:name])
        io = +''
        io << "<nav class='git-crumbs' aria-label='Repository'><i class='bi bi-git' aria-hidden='true'></i> "
        io << "<span>#{h.(@git[:group])}</span> / <a href='#{h.(git_url)}'><strong>#{h.(@git[:name])}</strong></a></nav>"
        io << "<div class='git-bar'>"
        io << "<div class='btn-group git-tabs' role='group'>"
        io << "<a class='btn btn-sm #{tab == :files ? 'btn-secondary' : 'btn-outline-secondary'}' href='#{h.(git_url('tree', ref))}'><i class='bi bi-folder2-open' aria-hidden='true'></i>Dateien</a>"
        io << "<a class='btn btn-sm #{tab == :history ? 'btn-secondary' : 'btn-outline-secondary'}' href='#{h.(git_url('commits', ref))}'><i class='bi bi-clock-history' aria-hidden='true'></i>Verlauf</a>"
        io << "</div>"
        if branches.size > 1
            io << "<div class='dropdown git-branches'><button class='btn btn-sm btn-outline-secondary dropdown-toggle' type='button' data-bs-toggle='dropdown' aria-expanded='false'>"
            io << "<i class='bi bi-diagram-2' aria-hidden='true'></i>#{h.(ref)}</button><ul class='dropdown-menu'>"
            branches.each { |b| io << "<li><a class='dropdown-item#{b == ref ? ' active' : ''}' href='#{h.(git_url('tree', b))}'>#{h.(b)}</a></li>" }
            io << "</ul></div>"
        elsif ref
            io << "<span class='git-branch'><i class='bi bi-diagram-2' aria-hidden='true'></i>#{h.(ref)}</span>"
        end
        io << "<a class='btn btn-sm btn-outline-secondary' href='#{h.(git_url('archive', ref))}'><i class='bi bi-file-earmark-zip' aria-hidden='true'></i>Als ZIP</a>" if ref
        io << "</div>"
        io << "<div class='git-clone'><span>Klonen:</span> <code>git clone #{h.(address)}</code></div>"
        io
    end

    def git_path_html(ref, path)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        parts = path.split('/')
        links = ["<a href='#{h.(git_url('tree', ref))}'>#{h.(@git[:name])}</a>"]
        parts.each_with_index do |part, i|
            sub = parts[0..i].join('/')
            links << (i == parts.size - 1 ? "<span>#{h.(part)}</span>" : "<a href='#{h.(git_url('tree', ref, sub))}'>#{h.(part)}</a>")
        end
        "<div class='git-path'>#{links.join(' / ')}</div>"
    end

    def git_page_html(view, rest)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        repo = @git[:repo]
        io = +''
        if view == 'commit'
            sha = rest.to_s.split('/').first.to_s
            halt 404 unless sha.match?(GitBrowse::SHA)
            commit = GitBrowse.commit(repo, sha)
            halt 404 unless commit
            io << git_header_html(GitBrowse.default_branch(repo), :history)
            subject, body = commit[:message].split("\n", 2)
            io << "<div class='git-commit-head'><h2>#{h.(subject)}</h2>"
            io << "<div class='git-message'>#{h.(body.strip)}</div>" if body && !body.strip.empty?
            io << "<p class='git-meta'><i class='bi bi-person' aria-hidden='true'></i>#{h.(commit[:author])} · #{h.(git_time(commit[:time]))} · <code>#{h.(commit[:short])}</code>"
            commit[:parents].each { |parent| io << " · davor <a href='#{h.(git_url('commit', parent))}'><code>#{h.(parent[0, 7])}</code></a>" }
            io << " · <a href='#{h.(git_url('tree', commit[:sha]))}'>Dateien zu diesem Stand</a></p></div>"
            labels = { :added => 'neu', :deleted => 'gelöscht', :renamed => 'umbenannt', :modified => 'geändert' }
            commit[:files].each do |file|
                io << "<section class='git-file'><h3><span class='git-status git-status-#{file[:status]}'>#{labels[file[:status]]}</span> "
                io << "#{file[:old_path] ? "#{h.(file[:old_path])} → " : ''}#{h.(file[:path])}</h3>"
                io << (file[:patch].empty? ? "<p class='git-note'>Ohne Änderung am Inhalt.</p>" : git_diff_html(file[:patch]))
                io << "</section>"
            end
            io << "<p class='git-note'>Der Rest dieses Commits ist zu groß, um ihn hier zu zeigen.</p>" if commit[:cut]
            io << "<p class='git-note'>Dieser Commit ändert keine Datei.</p>" if commit[:files].empty? && !commit[:cut]
            return io
        end

        ref, path = rest.to_s.split('/', 2)
        ref = GitBrowse.default_branch(repo) if ref.nil? || ref.empty?
        if ref.nil?
            io << git_header_html(nil, :files)
            io << "<div class='git-empty'><p>Das Repository ist noch leer.</p><p>So kommt der erste Commit hinein:</p>"
            io << "<pre><code>git clone #{h.(GitServer.address(@git[:id], @git[:name]))}\n# Dateien anlegen, dann\ngit add -A\ngit commit -m \"Der Anfang\"\ngit push</code></pre></div>"
            return io
        end
        sha = GitBrowse.resolve(repo, ref)
        halt 404 unless sha
        path = GitBrowse.clean_path(path.to_s)
        halt 404 unless path

        if view == 'commits'
            page = [params['seite'].to_i, 1].max
            log = GitBrowse.log(repo, sha, path, :skip => (page - 1) * GitBrowse::PAGE, :max => GitBrowse::PAGE + 1)
            io << git_header_html(ref, :history)
            io << git_path_html(ref, path) unless path.empty?
            io << "<ol class='git-log'>"
            log.first(GitBrowse::PAGE).each do |c|
                io << "<li><a class='git-log-subject' href='#{h.(git_url('commit', c[:sha]))}'>#{h.(c[:subject])}</a>"
                io << "<span class='git-meta'>#{h.(c[:author])} · #{h.(git_time(c[:time]))} · <code>#{h.(c[:short])}</code></span></li>"
            end
            io << "</ol>"
            io << "<p class='git-note'>Noch keine Commits.</p>" if log.empty?
            if log.size > GitBrowse::PAGE
                io << "<p><a class='btn btn-sm btn-outline-secondary' href='#{h.(git_url('commits', ref, path))}?seite=#{page + 1}'>Ältere Commits</a></p>"
            end
            return io
        end

        kind = GitBrowse.kind(repo, sha, path)
        halt 404 unless kind
        io << git_header_html(ref, :files)
        io << git_path_html(ref, path) unless path.empty?
        if kind == :tree
            last = GitBrowse.log(repo, sha, path, :max => 1).first
            if last
                io << "<p class='git-last'><a href='#{h.(git_url('commit', last[:sha]))}'>#{h.(last[:subject])}</a> "
                io << "<span class='git-meta'>#{h.(last[:author])} · #{h.(git_time(last[:time]))}</span></p>"
            end
            io << "<ul class='git-tree'>"
            GitBrowse.tree(repo, sha, path).each do |entry|
                icon = entry[:type] == :tree ? 'bi-folder-fill' : 'bi-file-earmark'
                link = git_url(entry[:type] == :tree ? 'tree' : 'blob', ref, entry[:path])
                io << "<li><i class='bi #{icon}' aria-hidden='true'></i><a href='#{h.(link)}'>#{h.(entry[:name])}</a>"
                io << "<span class='git-size'>#{h.(git_size(entry[:size]))}</span></li>"
            end
            io << "</ul>"
            readme = GitBrowse.tree(repo, sha, path).find { |e| e[:type] == :blob && e[:name].match?(/\A(readme|liesmich)\.md\z/i) }
            if readme
                file = GitBrowse.blob(repo, sha, readme[:path])
                io << "<article class='git-readme'>#{git_markdown_html(file[:text])}</article>" if file && file[:text]
            end
        else
            file = GitBrowse.blob(repo, sha, path)
            halt 404 unless file
            io << "<div class='git-file-bar'><span class='git-meta'>#{h.(git_size(file[:size]))}</span> "
            io << "<a href='#{h.(git_url('commits', ref, path))}'><i class='bi bi-clock-history' aria-hidden='true'></i>Verlauf dieser Datei</a></div>"
            type = GIT_PICTURES[File.extname(path).downcase]
            if type
                io << "<p class='git-picture'><img src='#{h.(git_url('raw', ref, path))}' alt='#{h.(File.basename(path))}'></p>"
            elsif file[:binary]
                io << "<p class='git-note'>Keine Textdatei; hier gibt es nichts zu lesen. Im ZIP ist sie dabei.</p>"
            elsif File.extname(path).downcase == '.md'
                io << "<article class='git-readme'>#{git_markdown_html(file[:text])}</article>"
                io << "<details class='git-source'><summary>Als Text</summary>#{git_code_html(file[:text], path)}</details>"
            else
                io << git_code_html(file[:text], path)
            end
            io << "<p class='git-note'>Die Datei ist länger; hier stehen die ersten #{GitBrowse::MAX_BLOB / 1024} KB.</p>" if file[:cut] && !file[:binary]
        end
        io
    end

    get GIT_BROWSE_PATH do
        this_is_a_page_for_logged_in_users!
        halt 404 unless GitServer.enabled?
        id, name, view, rest = params['captures']
        rest = rest.to_s.split('/').map { |part| URI.decode_www_form_component(part.gsub('+', '%2B')) }.join('/')
        group = git_group_of_id(id)
        halt_forbidden_page! unless group && git_may_browse?(group)
        repo = GitServer.repo_path(id, name)
        halt 404 unless File.directory?(repo)
        @git = { :id => id, :name => name, :group => group, :repo => repo }

        if view == 'raw'
            ref, path = rest.split('/', 2)
            sha = GitBrowse.resolve(repo, ref.to_s)
            path = GitBrowse.clean_path(path.to_s)
            type = path && GIT_PICTURES[File.extname(path).downcase]
            halt 404 unless sha && type
            file = GitBrowse.blob(repo, sha, path)
            halt 404 unless file
            # A picture, shown in an <img>; as a page of its own nothing in it runs.
            headers 'X-Content-Type-Options' => 'nosniff', 'Content-Security-Policy' => "sandbox; default-src 'none'",
                    'Cache-Control' => 'private, max-age=60'
            return respond_raw_with_mimetype(file[:data], type)
        end
        if view == 'archive'
            ref = rest.split('/').first.to_s
            sha = GitBrowse.resolve(repo, ref)
            halt 404 unless sha
            zip = GitBrowse.git(repo, 'archive', '--format=zip', "--prefix=#{name}/", '--end-of-options', sha, :binary => true)
            halt 404 unless zip
            return respond_raw_with_mimetype_and_filename(zip, 'application/zip', "#{name}.zip")
        end

        @git_page = "<div class='container git-browse'>#{git_page_html(view, rest)}</div>"
        # what the template reads, as in get '/*'
        running_tests = my_running_tests()
        path = request.path
        slug = nil
        template = File.read(File.join(@@static_dir, '_template.html'))
        # The page is the literal below; the repository's text comes in as the
        # value of @git_page and is never read as a template (TrustedTemplate).
        html = Main.inject_page_metadata(TrustedTemplate.render_page(template, '#{@git_page}', binding), request.path)
        respond_raw_with_mimetype(html, 'text/html')
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

    # The school's mirror of Open-Meteo (WeatherMirror), for when Open-Meteo
    # refuses the school's address. Open without a login: the weather page
    # runs on the student's own subdomain, and Ruby in the Workspace has no
    # login either. It forwards only two endpoints with a fixed list of
    # parameters, keeps answers for everyone, and asks Open-Meteo at most
    # WeatherMirror::UPSTREAM_PER_HOUR times an hour. Many people share the
    # school network's address, so the limit per client is generous.
    WEATHER_MIRROR = WeatherMirror::Mirror.new('/internal/weather-mirror')
    WEATHER_MIRROR_PER_CLIENT = RateLimit.new(:limit => 3000, :period => 600)

    get %r{/open-meteo/(api/v1/forecast|geocoding/v1/search)} do |endpoint|
        content_type :json
        headers 'Access-Control-Allow-Origin' => '*', 'Access-Control-Expose-Headers' => 'X-Hackschule-Cache',
                'Cache-Control' => 'no-store'
        unless DEVELOPMENT || WEATHER_MIRROR_PER_CLIENT.allow?(client_ip)
            halt 429, { :error => true, :reason => 'Zu viele Anfragen an den Spiegel. Warte ein paar Minuten.' }.to_json
        end
        begin
            code, data, cache = WEATHER_MIRROR.get(endpoint, request.GET)
        rescue WeatherMirror::Refused => e
            halt 400, { :error => true, :reason => e.message }.to_json
        end
        headers 'X-Hackschule-Cache' => cache if cache
        halt code, data unless code == 200
        # A 200 gets its body from the after filter (helper.rb).
        respond_raw_with_mimetype(data, 'application/json')
    end

    # TCP-Konsole (/tcp-console, TcpConsole): one connection per person at a
    # time, started from their own Workspace, at most TCP_CONSOLE_CONNECTS
    # per person and ten minutes.
    TCP_CONSOLE_CONNECTS = RateLimit.new(:limit => 30, :period => 600)
    @@tcp_console_sessions = {}
    @@tcp_console_mutex = Mutex.new

    get '/ws_tcp_console' do
        halt 403 unless user_logged_in?
        halt 400 unless Faye::WebSocket.websocket?(request.env)

        ws = Faye::WebSocket.new(request.env)
        email = @session_user[:email]
        fs_tag = fs_tag_for_email(email)
        session = nil
        say = ->(event) { ws.send(event.to_json) rescue nil }

        close_session = lambda do
            @@tcp_console_mutex.synchronize do
                @@tcp_console_sessions.delete(email) if @@tcp_console_sessions[email].equal?(session)
            end
            session&.close
            session = nil
        end

        ws.on(:open) { |_| say.({ :hello => true, :idle => TcpConsole::IDLE_SECONDS, :max_seconds => TcpConsole::MAX_SECONDS }) }

        ws.on(:message) do |message|
            data = JSON.parse(message.data.to_s) rescue {}
            data = {} unless data.is_a?(Hash)
            case data['action']
            when 'open'
                host = data['host'].to_s.strip
                port = TcpConsole.port(data['port'])
                tls = data['tls'] == true
                if !TcpConsole.valid_host?(host) || port.nil?
                    say.({ :error => 'invalid' })
                    next
                end
                unless DEVELOPMENT || TCP_CONSOLE_CONNECTS.allow?(email)
                    say.({ :error => 'rate_limited' })
                    next
                end
                close_session.call
                Thread.new do
                    unless workspace_runtime.workspace_state(fs_tag, :timeout => 10)[:running]
                        say.({ :error => 'not_running' })
                        next
                    end
                    started = TcpConsole::Session.new(TcpConsole.command(fs_tag, host, port, tls, :uid => GUEST_UID)) { |event| say.(event) }
                    session = started
                    # One connection per person: one in another tab ends.
                    previous = @@tcp_console_mutex.synchronize do
                        old = @@tcp_console_sessions[email]
                        @@tcp_console_sessions[email] = started
                        old
                    end
                    previous&.close
                rescue StandardError => e
                    STDERR.puts "TCP-Konsole: #{e.class}: #{e.message}"
                    say.({ :error => 'other' })
                end
            when 'send'
                session&.send_line(data['line'])
            when 'close'
                close_session.call
            when 'ping'
                # nginx ends a websocket that stays silent for a minute.
                say.({ :pong => true })
            end
        end

        ws.on(:close) { |_| close_session.call }
        ws.rack_response
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
            unless markdown_path && File.file?(markdown_path)
                # A page without recipes, such as /automatron: nothing to watch.
                content_type :json
                return respond(:monitor => false)
            end

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

        # The progress page (/screenshot-progress): the pass, its pages and
        # the files captured last, from the generator.
        get '/api/tutorial_screenshots/overview' do
            assert(admin_logged_in?)
            headers 'Cache-Control' => 'no-store'
            begin
                respond(TutorialScreenshots.overview)
            rescue => e
                status 503
                content_type :json
                { :error => e.message }.to_json
            end
        end

        # A captured file by its tutorial (rust/rust.md) and target, for the
        # progress page's stream: inside that tutorial's folder only.
        get '/api/tutorial_screenshots/capture' do
            assert(admin_logged_in?)
            relative = params['markdown'].to_s
            target = params['target'].to_s
            halt 400 unless TutorialScreenshots.valid_relative_markdown_path?(relative)
            mime = { '.webp' => 'image/webp', '.png' => 'image/png', '.mp4' => 'video/mp4', '.webm' => 'video/webm' }[File.extname(target)]
            halt 400 unless mime && !target.start_with?('/') && !target.split('/').include?('..')
            directory = File.expand_path(File.dirname(File.join('/src/content', relative)))
            file = File.expand_path(target, directory)
            halt 400 unless file.start_with?("#{directory}/")
            halt 404 unless File.file?(file)
            headers 'Cache-Control' => 'no-store'
            respond_raw_with_mimetype(File.binread(file), mime)
        end

        get '/api/tutorial_screenshots/image' do
            slug = params['slug'].to_s
            target = params['target'].to_s
            halt 400 unless slug.match?(/\A[a-zA-Z0-9_-]+\z/)
            # pictures and the Git graphs (SVG)
            mimetype = { '.webp' => 'image/webp', '.svg' => 'image/svg+xml', '.png' => 'image/png' }[File.extname(target)]
            halt 400 unless mimetype &&
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
            respond_raw_with_mimetype(File.binread(image_path), mimetype)
        end
    end

    # The case files of »Ein Krimi im Terminal«, with a note of one person's
    # access to the case server: whose, the key in the address says (the
    # page gives each logged-in person their own, CaseServer.archive_key),
    # or else the Workspace the request comes from. Otherwise without.
    get '/terminal-mystery/bitstein.tar.gz' do
        case_archive_response(CaseServer.enabled? ? workspace_email_for_ip(client_ip) : nil)
    end

    get '/terminal-mystery/akten/:key/bitstein.tar.gz' do
        email = nil
        if CaseServer.enabled?
            Main.load_invitations()
            email = CaseServer.email_for_key(CaseServer.secret(LOGIN_CODE_SALT), params[:key], @@invitations.keys) ||
                workspace_email_for_ip(client_ip)
        end
        case_archive_response(email)
    end

    def case_archive_response(email)
        archive = File.binread('/src/content/terminal-mystery/beispiele/bitstein.tar.gz')
        if email
            archive = CaseServer.archive_with_note(archive, CaseServer.note(CaseServer.secret(LOGIN_CODE_SALT), email))
        end
        headers 'Cache-Control' => 'no-store'
        respond_raw_with_mimetype(archive, 'application/gzip')
    end

    # A page's HTML for the person viewing it: the case files' address
    # carries their key (CaseServer.personal_archive_path).
    # On a page with a class repository (ClassRepos), the variant for this
    # person and the address of their group.
    def page_html(slug)
        content = @@content[slug]
        html = content[:html]
        # Only pages that use the class repository's variables look it up.
        if content[:class_repo] || html.match?(ClassRepos::TOKEN)
            view = class_repo_view(slug)
            html = ((view[:variant] && (content[:variants] || {})[view[:variant]]) || content)[:html]
            html = ClassRepos.fill(html, ClassRepos.values(view[:address]))
        end
        # the whole code at each step only for teachers, not while they see
        # the page as a student; after the variant is chosen, which has the
        # buttons too
        html = StageCode.strip(html) unless teacher_logged_in? && @impersonated_by.nil?
        return html unless slug == 'terminal-mystery' && CaseServer.enabled?

        # the note with the person's own access to the case server, written
        # into the page for them (CaseServer.note_values); a visitor sees an
        # example and how to get theirs
        html = CaseServer.fill_note(html, @session_user && CaseServer.note_values(CaseServer.secret(LOGIN_CODE_SALT), @session_user[:email]))
        return html unless @session_user

        html.gsub(CaseServer::ARCHIVE_PATH, CaseServer.personal_archive_path(CaseServer.secret(LOGIN_CODE_SALT), @session_user[:email]))
    end

    # The Katas of a catalog section (carousel: true) for the person viewing
    # the front page (KataMap): the sets with their icons and progress, the curriculum lines for teachers, and the sets pinned for
    # the person's groups, the latest first.
    def kata_carousel_html(section)
        slugs = section[:entries].select do |slug|
            @@content[slug] && @@content[slug][:tasks] && (DEVELOPMENT || !@@content[slug][:dev_only])
        end
        stops = slugs.map { |slug| KataMap.stop(slug, @@content[slug]) }
        # a picture as an icon with its file's time, so a new one is loaded
        stops.each do |stop|
            path = "/src/static#{stop.icon}"
            stop.icon = "#{stop.icon}?#{File.mtime(path).to_i}" if stop.icon.to_s.start_with?('/') && File.file?(path)
        end
        progress = user_logged_in? ? my_progress.to_h { |p| [p[:slug], p] } : nil
        pinned = user_logged_in? ? pins_for_groups((my_groups + class_groups).uniq).map { |p| p[:slug] }.uniq & slugs : []
        KataMap.carousel_html(stops, :progress => progress, :teacher => teacher_logged_in?, :pinned => pinned)
    end

    # Whose Workspace has this address: exactly one running Workspace of an
    # invited person, or nil (an exam Workspace has its own tag and no match).
    def workspace_email_for_ip(ip)
        return nil if ip.to_s.empty?

        tags = workspace_runtime.running_workspaces(:timeout => shell_timeout(:docker_network_inspect))
            .select { |_, w| w[:ip] == ip }.keys
        return nil unless tags.size == 1

        Main.load_invitations()
        @@invitations.keys.find { |email| fs_tag_for_email(email) == tags.first }
    end

    # The documents for the school (PrivacyDocuments): public, like the
    # Datenschutzerklärung they are made from, so that a link can go to the
    # Schulleitung.
    #
    # ?schule=KEY renders them for a partner school (Privacy.for_school): that
    # school as controller, its teachers as contact; the teacher who is logged
    # in signs the Elternbrief. The operator's own school and the guests get
    # the plain package.
    def privacy_settings_for(key)
        s = Privacy.settings
        return s if key.to_s.empty? || Main.roster_imported.nil?

        row = neo4j_query('MATCH (s:School {key: $key}) RETURN s;', :key => key.to_s).first
        halt 404 unless row
        teachers = neo4j_query(<<~END_OF_QUERY, :key => key.to_s).map { |r| { :name => r['name'], :email => r['email'] } }
            MATCH (i:Invitation)-[:AT]->(:School {key: $key})
            RETURN i.name AS name, i.email AS email
            ORDER BY toLower(coalesce(i.name, i.email));
        END_OF_QUERY
        me = user_logged_in? ? teachers.find { |x| x[:email] == @session_user[:email] } : nil
        Privacy.for_school(s, row['s'], :teachers => teachers, :signer => me && me[:name])
    end

    # The approved partner schools, for the list on /impressum.
    def partner_schools
        return [] if Main.roster_imported.nil?

        neo4j_query(<<~END_OF_QUERY).map { |r| { :key => r['key'], :name => r['name'] } }
            MATCH (s:School)
            WHERE s.approved_at IS NOT NULL AND coalesce(s.home, false) = false AND coalesce(s.guests, false) = false
            RETURN s.key AS key, s.name AS name
            ORDER BY toLower(s.name);
        END_OF_QUERY
    end

    # Who may open a document (the maintainer's decision: only /impressum is
    # public; the school's documents go to the Schulleitung as files or on
    # paper). Datenschutzerklärung, Nutzungsordnung and the webhosting
    # agreement: everyone logged in, a partner school's version only its
    # people. Everything else: teachers of that school and administrators.
    def document_access!(slug, key)
        this_is_a_page_for_logged_in_users!
        return if admin_logged_in?

        key = key.to_s
        roster = !Main.roster_imported.nil?
        mine = roster ? (school_of_teacher(@session_user[:email]) || {})[:key] : nil
        if PrivacyDocuments::FOR_EVERYONE.include?(slug)
            return if key.empty? || mine == key
            return if roster && !neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :key => key).empty?
                MATCH (:Invitation {email: $email})-[:MEMBER_OF]->(:Group)-[:AT]->(:School {key: $key}) RETURN 1 AS yes LIMIT 1;
            END_OF_QUERY
        else
            halt 403 unless teacher_account_logged_in?
            return if key.empty? ? (!roster || mine.nil? || mine == Main::HOME_SCHOOL) : mine == key
        end
        halt 403
    end

    # A document as PDF (weasyprint, in the Ruby image): the print view, with
    # the page's own stylesheet and fonts read from the static folder.
    #
    # fonts.css defines every font of the site (48 faces); loading them all
    # costs weasyprint nine seconds per document. The documents use IBM Plex
    # Sans only, so the PDF gets just its faces, with file paths.
    @@pdf_font_css = nil

    def self.pdf_font_css
        @@pdf_font_css ||= begin
            css = File.read(File.join(@@static_dir, 'include', 'fonts.css'))
            css.scan(/@font-face\s*\{[^}]*\}/m).select { |face| face.match?(/font-family:\s*['"]?IBM Plex Sans['"]?\s*;/) }
                .map { |face| face.gsub(/url\((['"]?)fonts\//, "url(\\1file://#{@@static_dir}/include/fonts/").gsub(/\s*font-display:[^;]*;/, '') }
                .join("\n")
        end
    end

    def document_pdf(slug, s)
        html = PrivacyDocuments.page(slug, s).sub(%r{<link href='/include/fonts.css' rel='stylesheet'>}, "<style>#{Main.pdf_font_css}</style>")
        pdf, error, status = Open3.capture3('weasyprint', '--quiet', '-', '-', :stdin_data => html, :binmode => true)
        raise "weasyprint failed: #{error[0, 300]}" unless status.success?

        pdf
    end

    def document_settings(slug)
        s = privacy_settings_for(params['schule'])
        s = s.merge(:parental_consent_required => true) if slug == 'elternbrief' && params['einverstaendnis'] == '1'
        s
    end

    get '/datenschutz' do
        document_access!('schulleitung', params['schule'])
        respond_raw_with_mimetype(PrivacyDocuments.page(nil, privacy_settings_for(params['schule'])), 'text/html')
    end

    # ?einverstaendnis=1 on the Elternbrief adds the consent form: for a
    # student whose school has not agreed yet (/gruppen, »Einverständnis«).
    # NAME.pdf is the document as a PDF, unterlagen.zip all of the package.
    get '/datenschutz/:document' do
        document = params['document'].to_s
        if document == 'unterlagen.zip'
            document_access!('schulleitung', params['schule'])
            s = privacy_settings_for(params['schule'])
            Dir.mktmpdir('unterlagen') do |dir|
                PrivacyDocuments::PACKAGE.each_with_index do |slug, i|
                    File.binwrite(File.join(dir, format('%02d %s', i + 1, PrivacyDocuments.pdf_name(slug, s))), document_pdf(slug, s))
                end
                File.binwrite(File.join(dir, format('%02d %s', PrivacyDocuments::PACKAGE.size + 1, PrivacyDocuments.pdf_name('elternbrief', s, :consent => true))),
                    document_pdf('elternbrief', s.merge(:parental_consent_required => true)))
                zip = File.join(dir, 'unterlagen.zip')
                ok = system('python3', '-I', '-c', 'import sys, zipfile, os; z = zipfile.ZipFile(sys.argv[1], "w", zipfile.ZIP_DEFLATED); [z.write(os.path.join(sys.argv[2], f), f) for f in sorted(os.listdir(sys.argv[2])) if f.endswith(".pdf")]; z.close()',
                    zip, dir)
                raise 'zip failed' unless ok

                @respond_filename = PrivacyDocuments.ascii_name("Unterlagen Workspace#{s[:school_name] ? " - #{s[:school_name]}" : ''}") + '.zip'
                respond_raw_with_mimetype(File.binread(zip), 'application/zip')
            end
            return
        end
        pdf = document.end_with?('.pdf')
        slug = document.delete_suffix('.pdf')
        halt 404 unless PrivacyDocuments::DOCUMENTS.key?(slug)
        document_access!(slug, params['schule'])
        s = document_settings(slug)
        if pdf
            @respond_filename = PrivacyDocuments.pdf_name(slug, s, :consent => s[:parental_consent_required] && slug == 'elternbrief')
            respond_raw_with_mimetype(document_pdf(slug, s), 'application/pdf')
        else
            respond_raw_with_mimetype(PrivacyDocuments.page(slug, s), 'text/html')
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
            halt_forbidden_page! unless teacher_account_logged_in?
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
        if path.start_with?('/codebites/')
            redirect "#{WEB_ROOT}/", 301
            return
        end
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
            halt_forbidden_page! unless admin_logged_in?
        end
        # How a screenshot pass is going (recreate-screenshots.rb), with the
        # files as they are captured: development only.
        if path == '/screenshot-progress'
            halt 404 unless DEVELOPMENT
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt 403 unless admin_logged_in?
        end
        # The person boxes with their sources (person_sources.rb), for
        # fact-checking: development only.
        if path == '/person-sources'
            halt 404 unless DEVELOPMENT
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt_forbidden_page! unless admin_logged_in?
        end
        # The activities by the names of the curricula (curriculum_overview.rb),
        # like the curriculum lines only for teachers.
        if path == '/lehrplan'
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt_forbidden_page! unless teacher_logged_in?
        end
        # The profile is about the logged-in person; without one it would
        # fail while drawing its buttons.
        if path == '/profil'
            this_is_a_page_for_logged_in_users!
        end
        # The tools (Tools: TCP-Konsole, Automatron) are for logged-in users only.
        this_is_a_page_for_logged_in_users! if Tools::PATHS.include?(path)
        # Managing groups (groups_page.rb) is for every teacher, also one
        # without a group yet.
        if path == '/personen'
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt_forbidden_page! unless admin_logged_in?
        end
        if path == '/gruppen'
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt_forbidden_page! unless teacher_account_logged_in? || admin_logged_in?
            # Teachers manage their groups on the class page (tab »Gruppe«);
            # /gruppen is the administration's overview of all groups.
            redirect "#{WEB_ROOT}/klasse?tab=lernende", 302 unless admin_logged_in?
        end
        if path == '/klasse'
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            # Every teacher, also one without a group yet: »Neue Gruppe«.
            halt_forbidden_page! if class_groups.empty? && !teacher_account_logged_in? && !admin_logged_in?
        end
        if path == '/tic80/'
            s = File.read(File.join(@@static_dir, '/tic80/index.html'))
            s = TrustedTemplate.render(s, binding)
            respond_raw_with_mimetype(s, 'text/html')
            return
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
            # A new Workspace credential, so one that leaked stops working;
            # other devices of this person get the new one on their next
            # request (before filter).
            if @session_user && @impersonated_by.nil?
                neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :server_sid => gen_server_sid())
                    MATCH (u:User {email: $email})
                    SET u.server_sid = $server_sid;
                END_OF_QUERY
                Main.refresh_nginx_config()
            end
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
        # A page that does not exist gets the 404 page in the frame of the
        # site (_not_found.html); a missing picture, script or stylesheet
        # stays an empty 404. The after filter only writes bodies of a 200,
        # so the page goes out with halt.
        if path.end_with?('.html') && !File.exist?(path)
            template = File.read(File.join(@@static_dir, '_template.html'))
            content = File.read(File.join(@@static_dir, '_not_found.html'))
            html = Main.inject_page_metadata(TrustedTemplate.render_page(template, content, binding), request.path)
            halt 404, { 'Content-Type' => 'text/html; charset=utf-8' }, html
        end
        respond_with_file(path) do |content, mime_type|
            if mime_type == 'text/html'
                template = File.read(File.join(@@static_dir, '_template.html'))
                rendered_page = TrustedTemplate.render_page(template, content, binding)
                Main.inject_page_metadata(rendered_page, request.path)
            end
        end
    end
end

# Before the first request (config.ru starts the server only after this
# file): accounts a restored backup brought back go again. Here, not in
# configure, which runs while the class is still being read. It fails
# closed: when it cannot finish (Neo4j still starting: a few tries), the
# server does not start, rather than serve accounts that should be gone.
# Normally there is nothing to do, and what it needs (Neo4j, the deletion
# log) the rest of the site needs too. The way out for the operator, until
# the cause is fixed: the file SKIP_START_REPLAY (docs/datenschutz.md,
# »Wenn der Workspace nicht startet«).
START_REPLAY_TRIES = 5
SKIP_START_REPLAY = '/internal/skip-deletion-replay'
if File.exist?(SKIP_START_REPLAY)
    STDERR.puts "!!! Deletions after a restore NOT repeated at the start: #{SKIP_START_REPLAY} exists. Restored accounts may be usable. " \
        'Fix the cause, remove the file, restart (docs/datenschutz.md, »Wenn der Workspace nicht startet«).'
else
    1.upto(START_REPLAY_TRIES) do |attempt|
        Main.new!.repeat_deletions_after_restore
        break
    rescue => e
        STDERR.puts "Repeating deletions after a restore failed at the start (#{attempt}/#{START_REPLAY_TRIES}): #{e.class}: #{e.message}"
        if attempt == START_REPLAY_TRIES
            abort "Not starting: the deletions after a restore could not be repeated (cause above). What to do: " \
                "docs/datenschutz.md, »Wenn der Workspace nicht startet«. To start anyway: create internal/skip-deletion-replay " \
                "in the data folder and restart."
        end
        sleep 3 * attempt
    end
end
