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
require './include/card_usage.rb'
require './include/task_check.rb'
require './include/task_card.rb'
require './include/stage_code.rb'
require './include/command_redaction.rb'
require './include/person_sources.rb'
require './include/picture_rights.rb'
require './include/curriculum_overview.rb'
require './include/curriculum_graph.rb'
require './include/task_store.rb'
require './include/class_board.rb'
require './include/meeting_point.rb'
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
require './include/git_overview.rb'
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
require './include/activity_feedback.rb'
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

MODULE_ORDER = [:workspace, :phpmyadmin, :neo4j, :tic80, :git]
MODULE_LABELS = {
    :workspace  => 'Workspace',
    :phpmyadmin => 'phpMyAdmin',
    :neo4j      => 'Neo4j',
    :tic80      => 'TIC-80',
    # only with the Workspace's own Git server (GIT_SERVER)
    :git        => 'Git',
}

# Activities that were removed, with what was stored about them: results,
# attempts and hand-ins (SetupDatabase#remove_code_bites); their addresses
# lead to the start page.
REMOVED_ACTIVITIES = %w[uebung-zahlen uebung-arrays uebung-strings feuerwerk].freeze

# Hand-ins, one archive per person and task (Main#submission_path)
SUBMISSIONS_ROOT = '/internal/submissions'

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

    # The site no longer makes Git keys for a group (GitServer); a group set
    # to them hands in keys from now on. Keys made before stay valid until
    # their owner takes them back. Runs on every start and finds something
    # once.
    def no_automatic_git_keys
        neo4j_query("MATCH (g:GroupSetting {git_keys: 'auto'}) SET g.git_keys = 'handed_in';")
    end

    # A Session stores the hash of the sid in the cookie
    # (Authentication.session_key) since 2026-10-30; before, the sid itself.
    # A sid has 24 characters, a hash 64: the first start after the change
    # hashes the old ones, so nobody is logged out. It runs before the first
    # request, and on every start after that finds nothing.
    def hash_session_sids
        sids = neo4j_query('MATCH (s:Session) WHERE size(s.sid) <> 64 RETURN s.sid AS sid;').map { |row| row['sid'] }
        sids.each_slice(500) do |slice|
            pairs = slice.map { |sid| { 'from' => sid, 'to' => Authentication.session_key(sid) } }
            neo4j_query('UNWIND $pairs AS p MATCH (s:Session {sid: p.from}) SET s.sid = p.to;', :pairs => pairs)
        end
        STDERR.puts "Hashed the sids of #{sids.size} sessions" unless sids.empty?
    end

    # Code Bites is gone (2026-10-18), with its practice sets: what it stored
    # goes too, so the Datenschutzerklärung need not mention it. Runs on every
    # start and finds something once.
    def remove_code_bites
        %w[Submission Solved Code Task Language].each do |label|
            neo4j_query("MATCH (n:#{label}) DETACH DELETE n;")
        end
        prefixes = REMOVED_ACTIVITIES.map { |slug| "#{slug}/" }
        neo4j_query(<<~END_OF_QUERY, :prefixes => prefixes).each do |row|
            MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState) WHERE any(p IN $prefixes WHERE s.task STARTS WITH p)
            RETURN u.email AS email, s.task AS task;
        END_OF_QUERY
            FileUtils.rm_f(File.join(SUBMISSIONS_ROOT, "#{TaskStore.state_key(row['email'], row['task'])}.tar.gz"))
        end
        neo4j_query(<<~END_OF_QUERY, :prefixes => prefixes)
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
        # »Datenübertragung per Kabel« became »per Funk« (2026-10)
        'cable' => 'radio',
        # the guide to the Hackschule subdomain became a Handbuch page (2026-10)
        'custom-subdomain' => 'reference/webspace',
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
            # the command as it may stand in the log: secrets replaced (CommandRedaction)
            shown = CommandRedaction.redact(command)
            STDERR.puts ">>> CMD TIMEOUT after #{format('%.3f', dt)}s: #{shown}"
            raise "Command timed out after #{timeout}s: #{shown}"
        end

        unless allow_failure || (status && status.success?)
            shown = CommandRedaction.redact(command)
            STDERR.puts ">>> CMD ERROR after #{format('%.3f', dt)}s status=#{status&.exitstatus}: #{shown}"
            raise "Command failed with status #{status&.exitstatus}: #{shown}\n#{stderr_data}"
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

    # the parts of the site that live in files of their own: they reopen
    # this class, so they come before any route (Sinatra matches routes in
    # the order they are defined, and routes further down catch everything)
    require './include/access.rb'
    require './include/accounts_page.rb'
    require './include/activity_feedback_page.rb'
    require './include/cable_page.rb'
    require './include/class_page.rb'
    require './include/databases_page.rb'
    require './include/exams_page.rb'
    require './include/git_page.rb'
    require './include/groups_page.rb'
    require './include/kata_page.rb'
    require './include/live_apps.rb'
    require './include/login_page.rb'
    require './include/login_sessions_page.rb'
    require './include/look_ins_page.rb'
    require './include/my_data.rb'
    require './include/nginx_config.rb'
    require './include/people_page.rb'
    require './include/roster_store.rb'
    require './include/server_health.rb'
    require './include/server_start_queue.rb'
    require './include/task_api.rb'
    require './include/turtle_page.rb'
    require './include/workspace_routes.rb'

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
            "#{heading_text} | Hackschule Workspace"
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
        PictureRights.withhold(root, path, development: DEVELOPMENT)

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
                # a screenshot of the site (class shot, taken at twice the
                # pixels): at most half its width, so that its text is never
                # bigger than the page's around it
                if img.classes.include?('shot') && (width = webp_width(image_path))
                    img['style'] = ["max-width: #{width / 2}px; min-width: min(1000px, #{width / 2}px)", img['style']].compact.join('; ')
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

    # The width of a WebP in pixels, from its header (lossy VP8, lossless
    # VP8L, extended VP8X); nil for anything else
    def self.webp_width(path)
        head = File.binread(path, 30)
        return nil unless head && head.size >= 30 && head[0, 4] == 'RIFF' && head[8, 4] == 'WEBP'

        case head[12, 4]
        when 'VP8 ' then head[26, 2].unpack1('v') & 0x3fff
        when 'VP8L' then (head[21, 4].unpack1('V') & 0x3fff) + 1
        when 'VP8X' then (head[24, 3] + "\0").unpack1('V') + 1
        end
    rescue StandardError
        nil
    end

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
                    ClassRepos.mark_browse(root) if variant == ClassRepos::LOCAL_KEYS
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
                    # its topics (src/content/curricula/themen.yaml), from which
                    # the plan of another state derives its line (Curriculum)
                    :topics => Array(page_meta['topics']).map(&:to_s),
                    # whom the activity is for, where its curriculum line
                    # names both plans but it is meant for the younger
                    # (CurriculumGraph.age_of): Sek I, Sek I und Oberstufe,
                    # Oberstufe
                    :age => page_meta['age'],
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
                            # name.webp:contain for a drawing: padded to 16:9 instead of cropped;
                            # name.webp:contain:x:y also sets what a phone's narrower box keeps
                            contain = parts[1] == 'contain'
                            parts = [parts[0], *parts[2..]] if contain
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
            # dealt in alphabetical order, so that the »Added content« lines
            # of the workers come roughly in that order and show the progress;
            # the catalog keeps the order of paths (sorted below)
            sorted = paths.sort_by { |entry| entry[:slug] || entry[:path] }
            children = sorted.each_with_index.group_by { |_, i| i % workers }.values.map { |part| part.map(&:first) }.map do |part|
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

    # The access list for the server, from the groups and their people, and
    # each person's own folder for that person alone (GitServer.access_map).
    def self.git_refresh_access
        group_people = git_groups.to_h { |group, data| [data[:id], git_group_emails(group).map { |email| fs_tag_for_email(email) }] }
        personal = $neo4j.neo4j_query('MATCH (u:User) WHERE u.git_id IS NOT NULL RETURN u.email AS email, u.git_id AS id;')
            .to_h { |row| [row['id'], fs_tag_for_email(row['email'])] }
        GitServer.write_config('access', GitServer.access(GitServer.access_map(:group_people => group_people, :personal => personal)))
    end

    # The keys for the server: the one each person handed in, and one the
    # site had made in someone's Workspace before it stopped doing so
    # (git_auto_key), until that person takes it back.
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

    # Everything the server reads, afresh: after the roster was read, a
    # repository or a key changed, an account was deleted.
    def self.git_refresh!
        return unless GitServer.enabled?

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
        setup.no_automatic_git_keys
        setup.hash_session_sids
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
            ['/ws', '/ws_tcp_console', '/ws_meeting', '/logout'].include?(request.path)
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
            key = Authentication.session_key(sid)
            results = neo4j_query(<<~END_OF_QUERY, :sid => key).to_a
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
                    neo4j_query('MATCH (s:Session {sid: $sid}) DETACH DELETE s;', :sid => key)
                elsif session_expiry && session_expiry > DateTime.now && session[:impersonated_by] &&
                      !Main.may_still_look_in?(session[:impersonated_by], results.first['u'][:email].to_s.downcase)
                    # A teacher imitating a student who left its groups, or who lost its
                    # own access meanwhile: the session ends, not at »Zurück«.
                    STDERR.puts "Session ended: #{session[:impersonated_by]} may no longer imitate #{results.first['u'][:email]}"
                    neo4j_query('MATCH (s:Session {sid: $sid}) DETACH DELETE s;', :sid => key)
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
                            :sid => key, :email => email, :today => today)
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
                    neo4j_query(<<~END_OF_QUERY, :sid => key)
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

    # The root clients of MySQL and Neo4j, for statements on standard input.
    # The password goes in the environment (docker exec -e NAME takes the
    # value from here): on a command line every ps on the host and Docker's
    # own events (exec_create) would show it.
    MYSQL_ROOT_CLIENT = 'docker exec -i -e MYSQL_PWD workspace_mysql_1 mysql --user=root'
    MYSQL_ROOT_ENV = { 'MYSQL_PWD' => MYSQL_ROOT_PASSWORD }.freeze
    NEO4J_ROOT_CLIENT = 'docker exec -i -e NEO4J_PASSWORD workspace_neo4j_1 bin/cypher-shell -u neo4j'
    NEO4J_ROOT_ENV = { 'NEO4J_PASSWORD' => NEO4J_ROOT_PASSWORD }.freeze

    def self.init_mysql(email, db_login)
        DatabaseIdentity.validate!(db_login)
        mysql_password = Main.gen_password_for_email(email, MYSQL_PASSWORD_SALT)
        STDERR.puts "Setting up MySQL user #{db_login} with database #{db_login}"
        Open3.popen2(MYSQL_ROOT_ENV, MYSQL_ROOT_CLIENT) do |stdin, stdout, wait_thr|
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
        Open3.popen2(MYSQL_ROOT_ENV, MYSQL_ROOT_CLIENT) do |stdin, stdout, wait_thr|
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

        Open3.popen2(NEO4J_ROOT_ENV, NEO4J_ROOT_CLIENT) do |stdin, stdout, wait_thr|
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
        Open3.popen2(NEO4J_ROOT_ENV, NEO4J_ROOT_CLIENT) do |stdin, stdout, wait_thr|
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

        Open3.popen2(MYSQL_ROOT_ENV, MYSQL_ROOT_CLIENT) do |stdin, stdout, wait_thr|
            stdin.puts "DROP DATABASE IF EXISTS `#{db_login}`;"
            stdin.close
            status = wait_thr.value
            raise "MySQL test demo reset failed for #{db_login}" unless status.success?
        end

        Open3.popen2(NEO4J_ROOT_ENV, NEO4J_ROOT_CLIENT) do |stdin, stdout, wait_thr|
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

    # Who did what to whose Workspace, account or database, with the time:
    # among the program messages (docs/datenschutz.md, »Protokolle des
    # Servers«), to be found again later with ./config.rb logs.
    def log_event(message)
        STDERR.puts "#{Time.now.strftime('%Y-%m-%d %H:%M:%S')} #{message}"
    end

    # Who signs the mails: OPERATOR_NAME, else the address.
    def mail_signature
        defined?(OPERATOR_NAME) && !OPERATOR_NAME.to_s.strip.empty? ? OPERATOR_NAME.to_s.strip : 'Workspace'
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

    # The state whose curricula a teacher sees (profile, /lehrplan;
    # Curriculum, docs/plan-bundeslaender.md); Berlin without a choice.
    def curriculum_state
        # a teacher's is stored; a guest chooses on /lehrplan (?land=)
        state = @session_user ? @session_user[:curriculum_state].to_s : params['land'].to_s
        Curriculum.states.any? { |key, _, _| key == state } ? state : Curriculum::DEFAULT_STATE
    end

    def curriculum_plan
        Curriculum.plan_for(curriculum_state)
    end

    # The choice of the state, for the profile and /lehrplan: a select that
    # stores it (curriculum_state.js) and reloads the page.
    def curriculum_state_select(id)
        options = Curriculum.states.map do |key, name, plan|
            label = Curriculum.draft?(plan) ? "#{name} (Entwurf)" : name
            "<option value='#{CGI.escapeHTML(key)}'#{key == curriculum_state ? ' selected' : ''}>#{CGI.escapeHTML(label)}</option>"
        end
        "<select id='#{id}' class='form-select form-select-sm curriculum-state' aria-label='Bundesland für die Lehrpläne'>#{options.join}</select>"
    end

    def page_kind_header(slug)
        # the teacher's buttons together at the right end, wrapping together
        tools = page_pin_button(slug) + teacher_note_toggle(slug)
        toggle = tools.empty? ? '' : "<span class='page-teacher-tools'>#{tools}</span>"
        header = PageKinds.header(slug, @@content, @@sections, :development => DEVELOPMENT, :extra => toggle,
                                  :show_duration => teacher_logged_in?, :show_curriculum => teacher_logged_in?,
                                  :code_language => page_code_language(slug),
                                  :curriculum_plan => teacher_logged_in? ? curriculum_plan : nil)
        header.empty? && !toggle.empty? ? "<div class='page-kind'>#{toggle}</div>" : header
    end

    def page_kind_class(slug)
        PageKinds.container_class(slug)
    end

    # --- Programming languages (CodeLanguages) ---------------------------------

    # The languages a teacher ranked for each group, { group => [languages] }
    # in the order given; nil where none is set. A group set before the
    # list has one language (g.code_language), read as a list of one.
    def group_code_languages(groups)
        return {} if groups.empty?

        rows = neo4j_query(<<~END_OF_QUERY, :groups => groups).to_a
            MATCH (g:GroupSetting) WHERE g.group IN $groups
            RETURN g.group AS group, g.code_languages AS languages, g.code_language AS language;
        END_OF_QUERY
        set = rows.to_h { |row| [row['group'], CodeLanguages.ranked(row['languages'] || row['language'])] }
        groups.to_h { |group| [group, set[group]] }
    end

    # The activities of the catalog that offer more than one language, for
    # the class's ranking: [{ title, languages }] in catalog order
    def multi_language_activities
        pinnable_slugs.filter_map do |slug|
            languages = (@@content[slug] || {})[:languages] || []
            next if languages.size < 2

            { :title => plain_title(slug), :languages => languages }
        end
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

    # For ClassRepos.variant: true for an address on the Workspace's Git
    # server, where the students hand in a key of their own.
    def git_local_mode
        @git_local_mode ||= lambda { |address| GitServer.parse_address(address) ? true : nil }
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
                "Soll eine Klasse ihre Arbeit in ein gemeinsames Repository hochladen, trägst du dessen Adresse #{where} im Reiter »Einstellungen« ein.</p>"
        end
        items = with.map do |g|
            browse = git_browse_url(repos[g][slug])
            "<a href='#{h.(link.(g))}'>#{h.(g)}</a>: <code>#{h.(repos[g][slug])}</code>" \
                "#{browse ? " (<a href='#{h.(browse)}'><i class='bi bi-eye' aria-hidden='true'></i>ansehen</a>)" : ''}"
        end
        others = with.size < groups.size ? ' Deine anderen Gruppen sehen die Seite ohne Git.' : ''
        "<p class='class-repo-teacher'><i class='bi bi-hdd-network' aria-hidden='true'></i><strong>Klassen-Repository</strong>: " \
            "#{items.join(', ')}.#{others}</p>"
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
            CodeLanguages.resolve([kata, own], groups, languages)
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
            meeting = meeting_page_config(tasks)
            config[:meeting] = meeting if meeting
        end
        # Text that only stands in for the cards (an exercise list) is hidden
        # while the cards are shown.
        previews = tasks[:on_page].any? { |id| (tasks[:by_id][id] || {}).dig('check', 'preview') == 'anaglyph' }
        # SQL and Cypher answers, and the code of a Kata, in the colours of
        # the code blocks, with the keyword lists of the same lexers
        # (code_input.js).
        code = tasks[:on_page].any? { |id| %w[sql cypher function pip tests turtle].include?((tasks[:by_id][id] || {}).dig('check', 'type')) }
        pip = tasks[:on_page].any? { |id| (tasks[:by_id][id] || {}).dig('check', 'type') == 'pip' }
        turtle = tasks[:on_page].any? { |id| (tasks[:by_id][id] || {}).dig('check', 'type') == 'turtle' }
        code_input = if code
            keywords = {
                :sql => Rouge::Lexers::SQL.keywords.to_a,
                :sqlTypes => Rouge::Lexers::SQL.keywords_type.to_a,
                :cypher => Rouge::Lexers::WorkspaceCypher::KEYWORDS,
            }
            "<script>window.CODE_INPUT_KEYWORDS = #{keywords.to_json};</script>" \
                "<script src='/include/code_input.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
        end
        # What gives away a task's answer waits for it (AfterTask), also for
        # a guest: otherwise a student could read the answers in a private
        # window before guessing. tasks.js shows it once answered. A teacher
        # sees the page as the class does and can open it without answering
        # (revealAfter: a button in the line that waits).
        waits = true
        "<style>.tasks-off-only { display: none; }" \
            "#{waits ? ' .after-task:not(.after-task-shown) > .after-task-body { display: none; } .after-task:not(.after-task-shown) > .after-task-wait { display: block; }' : ''}</style>" \
            "<script>window.TASK_PAGE = #{config.to_json.gsub('</', '<\/')};</script>" \
            "#{previews ? "<script src='/include/anaglyph_preview.js?#{CACHE_BUSTER}'></script>" : ''}" \
            "#{code_input}" \
            "#{pip ? "<script src='/include/pip_world.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>" : ''}" \
            "#{turtle ? "<script src='/include/turtle/turtle_canvas.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>" : ''}" \
            "<script src='/include/tasks.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>" \
            "#{config[:meeting] && config[:meeting][:teacher] ? "<script src='/include/meeting_status.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>" : ''}" \
            "#{config[:meeting] ? "<script src='/include/meeting_point.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>" : ''}"
    end

    # What meeting_point.js needs on an activity page: the points of the
    # person's groups (a teacher's: of the group chosen on the class page)
    # and, for a student who has arrived, the next set of Katas. nil
    # without a group: then no point can concern the page.
    def meeting_page_config(tasks)
        teacher = teacher_logged_in?
        # a teacher: the group chosen on the class page, or the only one
        groups = if teacher
            chosen = [class_prefs['group']].compact & class_groups
            chosen.empty? && class_groups.size == 1 ? class_groups : chosen
        else
            my_groups
        end
        return nil if groups.empty?

        config = { :points => meeting_points(groups, tasks[:activity]), :groups => groups, :teacher => teacher }
        if teacher
            config[:classUrl] = "/klasse?group=#{CGI.escape(groups.first)}&tab=fortschritt&activity=#{CGI.escape(tasks[:activity])}"
        else
            config[:kata] = next_kata_set
        end
        config
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

    # The teacher note of a page (lehrkraft.md), for teachers only: a small
    # button at the right end of the page strip, so the page can be shown on
    # a projector as it is, and the note itself below it, closed; on a wide
    # screen it opens beside the text (teacher_note.js).
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

        # where the class can go next: the activities that build on this one
        # (the arrows out of it on /lehrplan), always current with requires:
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        after = PageKinds.builds_on(slug, @@content, pinnable_slugs, :development => DEVELOPMENT)
        show = "<a href='/lehrplan?zeige=#{h.(slug)}#wege'>im Lehrplan zeigen</a>"
        next_html = "<div class='teacher-note-next'><i class='bi bi-signpost-split' aria-hidden='true'></i><div><strong>Wie es weitergehen kann:</strong> " +
            (after.empty? ? "Keine andere Aktivität baut auf dieser auf. Wie die Aktivitäten aufeinander aufbauen, zeigt der <a href='/lehrplan?zeige=#{h.(slug)}#wege'>Lehrplan</a>." :
                "Auf dieser Aktivität #{after.size == 1 ? 'baut' : 'bauen'} #{PageKinds.join_and(after.map { |s, t| "<a href='/#{h.(s)}'>#{h.(CGI.unescapeHTML(t))}</a>" })} auf (#{show}).") +
            '</div></div>'
        # opened above the text or, on a wide screen, beside it (teacher_note.js)
        "<div class='teacher-note' id='teacher-note' hidden><div class='teacher-note-body'>#{@@content[slug][:teacher_note]}#{next_html}</div></div>" \
            "<script src='/include/teacher_note.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
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
              # centred: the card shows nearly all of the 16:9 picture, a
              # little narrower once its text is taller (default.css); the
              # focus (image: x y) is for the phone's square crop on the
              # catalog and would push the little that is cut to one side
              :position => '50% 50%',
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

    # --- The meeting point of an activity (MeetingPoint, docs/plan-treffpunkt.md)

    # { group => task id } of the activity, for these groups.
    def meeting_points(groups, activity)
        return {} if groups.empty?

        neo4j_query(<<~END_OF_QUERY, :groups => groups, :activity => activity).to_h { |row| [row['group'], row['task']] }
            MATCH (m:MeetingPoint) WHERE m.group IN $groups AND m.activity = $activity
            RETURN m.group AS group, m.task AS task;
        END_OF_QUERY
    end

    # Open pages of an activity, each with the groups whose point it shows
    # (/ws_meeting): { id => { ws:, activity:, groups:, teacher: } }
    @@meeting_ws = {}
    @@meeting_ws_mutex = Mutex.new

    # A student's task changed: the teachers' open pages of the activity
    # (the box on the page, the line on the class board) count again.
    def notify_meeting_progress(email, activity)
        return unless defined?(@@group_members)

        groups = @@group_members.select { |_, members| members.include?(email) }.keys
        return if groups.empty?

        clients = @@meeting_ws_mutex.synchronize do
            @@meeting_ws.values.select { |c| c[:teacher] && c[:activity] == activity && (c[:groups] & groups).any? }
        end
        clients.each do |c|
            (c[:groups] & groups).each { |group| c[:ws].send({ :action => 'progress', :group => group }.to_json) rescue nil }
        end
    end

    def broadcast_meeting(group, activity, task)
        clients = @@meeting_ws_mutex.synchronize do
            @@meeting_ws.values.select { |c| c[:activity] == activity && c[:groups].include?(group) }
        end
        message = { :action => 'meeting', :group => group, :task => task }.to_json
        clients.each { |c| c[:ws].send(message) rescue nil }
    end

    # A teacher sets the point after a task (not an Extra), moves it by
    # setting it elsewhere, or lifts it (task empty); every open page of the
    # group shows the change.
    post '/api/class/meeting' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :activity, :task],
            :max_value_lengths => {:group => 200, :activity => 64, :task => 100})
        halt 403 unless class_groups.include?(data[:group])
        _, _, tasks = class_activities(data[:group]).find { |a, _, _| a == data[:activity] }
        halt 404 if tasks.nil?
        key = "#{data[:group]}|#{data[:activity]}"
        task = data[:task].to_s
        if task.empty?
            neo4j_query('MATCH (m:MeetingPoint {key: $key}) DETACH DELETE m;', :key => key)
        else
            halt 400 unless tasks.any? { |t| t[:id] == task && !t[:extra] }
            neo4j_query(<<~END_OF_QUERY, :key => key, :group => data[:group], :activity => data[:activity], :task => task, :ts => Time.now.to_i)
                MERGE (m:MeetingPoint {key: $key})
                SET m.group = $group, m.activity = $activity, m.task = $task, m.ts = $ts;
            END_OF_QUERY
        end
        broadcast_meeting(data[:group], data[:activity], task.empty? ? nil : task)
        respond(:task => task.empty? ? nil : task)
    end

    # The line above the class board, again and again while it is open
    # (class_meeting.js): who has arrived.
    post '/api/class/meeting_status' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :activity], :max_value_lengths => {:group => 200, :activity => 64})
        halt 403 unless class_groups.include?(data[:group])
        respond(:status => meeting_status(data[:group], data[:activity]))
    end

    # MeetingPoint.status of a group's point, or nil without one
    def meeting_status(group, activity)
        task = meeting_points([group], activity)[group]
        return nil if task.nil?

        _, _, tasks = class_activities(group).find { |a, _, _| a == activity }
        return nil if tasks.nil?

        members = class_members(group)
        states = class_board_states(members.map { |m| m[:email] }, activity)
        MeetingPoint.status(tasks, task, members, states)
    end

    # The choice on the class board: no point, or after one of the tasks.
    def meeting_control_html(group, activity, tasks)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        current = meeting_points([group], activity)[group]
        options = ["<option value=''#{current ? '' : ' selected'}>keiner</option>"]
        tasks.each_with_index do |t, i|
            next if t[:extra]

            options << "<option value='#{h.(t[:id])}'#{t[:id] == current ? ' selected' : ''}>nach #{i + 1}. #{h.(t[:title])}</option>"
        end
        "<label class='class-meeting-control' title='Wo sich die Klasse trifft, bevor es weitergeht; die Seiten zeigen es sofort'>" \
            "<i class='bi bi-flag' aria-hidden='true'></i>Treffpunkt " \
            "<select class='form-select form-select-sm class-meeting-choice' data-group='#{h.(group)}' data-activity='#{h.(activity)}'>#{options.join}</select></label>"
    end

    # Open pages of an activity hear when a point of their groups is set,
    # moved or lifted (meeting_point.js, class_meeting.js), and teachers when
    # a student of the group solved a task; a ping every half minute keeps
    # the connection (nginx ends a silent one after a minute).
    get '/ws_meeting' do
        halt 403 unless user_logged_in?
        halt 400 unless Faye::WebSocket.websocket?(request.env)
        activity = params['activity'].to_s
        halt 400 unless activity =~ /\A[a-z0-9_-]+\z/
        Main.load_invitations()
        teacher = teacher_logged_in?
        groups = (my_groups + (teacher ? class_groups : [])).uniq
        ws = Faye::WebSocket.new(request.env)
        id = ws.object_id
        ws.on(:open) do |_|
            @@meeting_ws_mutex.synchronize { @@meeting_ws[id] = { :ws => ws, :activity => activity, :groups => groups, :teacher => teacher } }
            # what is set now: a page that lost its connection catches up
            ws.send({ :action => 'points', :points => meeting_points(groups, activity) }.to_json) rescue nil
        end
        ws.on(:message) { |_| ws.send({ :pong => true }.to_json) rescue nil }
        ws.on(:close) { |_| @@meeting_ws_mutex.synchronize { @@meeting_ws.delete(id) } }
        ws.rack_response
    end

    # What a student who has arrived could do next: the first set of Katas
    # with tasks still open for them, { slug:, title: }, or nil.
    def next_kata_set
        ((@@sections['katas'] || {})[:entries] || []).each do |entry|
            slug = entry.to_s.delete_prefix('.')
            c = @@content[slug]
            next if c.nil? || c[:tasks].nil? || (c[:dev_only] && !DEVELOPMENT)

            ids = c[:tasks][:on_page] || []
            next if ids.empty?

            done = ids.count { |id| StudentHome::DONE.include?((my_task_states["#{c[:tasks][:activity]}/#{id}"] || {})[:status].to_s) }
            return { :slug => slug, :title => plain_title(slug) } if done < ids.size
        end
        nil
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
        # it is full): one quiet line, from the teachers' websocket
        # (server_stats), see class_board.js.
        io << "<p class='class-server-line' hidden><i class='bi bi-hdd-rack' aria-hidden='true'></i>Server: " \
            "<span class='class-server-cpu'></span>, <span class='class-server-ram'></span>, <span class='class-server-disk'></span></p>"
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
        ['ueberblick', 'Überblick', 'bi-bar-chart-steps'],
        ['lernende', 'Lernende', 'bi-people'],
        ['startseite', 'Startseite', 'bi-pin-angle'],
        ['einstellungen', 'Einstellungen', 'bi-sliders'],
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
        "<section class='class-group-manage' data-group='#{CGI.escapeHTML(group.to_s)}' data-contact='#{CGI.escapeHTML(contact_email)}'><p class='text-body-secondary'>Einen Moment …</p></section>"
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

            return "<p>Du betreust noch keine Gruppe. Dir ist noch keine Schule zugeordnet; das macht die Administration. Danach legst du hier deine Gruppen an. " \
                "Schreib dafür #{class_contact_html}, an welcher Schule du unterrichtest.</p>"
        end

        prefs = class_prefs
        group = [params['group'], prefs['group']].find { |g| groups.include?(g) } || groups.first
        # »gruppe«: the tab that managed the group is part of »Lernende« now.
        wanted = params['tab'] == 'gruppe' ? 'lernende' : params['tab']
        # the link of an activity page to its class repository (/klasse?repos=SLUG)
        wanted = 'einstellungen' if params['repos'].to_s != ''
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
        io << "<noscript><button class='btn btn-primary'>Anzeigen</button></noscript>"
        # For the projector: the group's codes, big, names only (class_board.js).
        io << "<span class='class-board-actions'>#{new_group_button_html}" \
            "<button type='button' class='btn btn-outline-secondary class-codes-show'><i class='bi bi-projector' aria-hidden='true'></i>Anmelde-Codes zeigen</button></span></form>"
        io << new_group_note_html
        io << "<div class='class-login-codes' data-group='#{h.(group)}' data-school='#{h.(school_names_of_groups[group])}' hidden><h2><i class='bi bi-key' aria-hidden='true'></i>Anmelde-Codes</h2>" \
            "<p>Diese Personen aus #{h.(group)} warten gerade auf ihren Code:</p>" \
            "<table class='table table-sm'><tbody></tbody></table></div>"
        io << "<div class='class-tabs' role='tablist' aria-label='Ansicht'>"
        CLASS_TABS.each do |key, label, icon|
            count = key == 'lernende' ? " <span class='class-tab-count'>#{class_members(group).size}</span>" : ''
            io << "<button type='button' role='tab' class='class-tab' id='class-tab-#{key}' data-tab='#{key}' aria-controls='class-panel-#{key}' " \
                "aria-selected='#{key == tab}'#{key == tab ? '' : " tabindex='-1'"} title='#{label}'><i class='bi #{icon}' aria-hidden='true'></i><span class='class-tab-label'>#{label}</span>#{count}</button>"
        end
        io << "</div>"
        panel = ->(key, html) { "<section class='class-panel' role='tabpanel' id='class-panel-#{key}' aria-labelledby='class-tab-#{key}'#{key == tab ? '' : ' hidden'}>#{html}</section>" }
        io << panel.('fortschritt', class_progress_html(group))
        io << panel.('ueberblick', class_overview_html(group))
        io << panel.('lernende', class_people_html(group))
        io << panel.('startseite', class_pins_html(group))
        io << panel.('einstellungen', class_settings_html(group))
        io
    end

    # The tab »Einstellungen«: what holds for the whole group, each saved at
    # once, with what it does in a sentence instead of a tooltip.
    def class_settings_html(group)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        io = +"<div class='class-settings'>"
        # The programming languages of the group's activities, ranked
        # (CodeLanguages): drawn by class_languages.js from the data, the
        # languages of the activities with it for the counts and the preview.
        ranked = group_code_languages([group])[group] || [CodeLanguages::DEFAULT]
        activities = multi_language_activities
        data = {
            :group => group, :ranked => ranked, :default => CodeLanguages::DEFAULT,
            :languages => CodeLanguages::GROUP_LANGUAGES.map { |l| { :id => l, :name => CodeLanguages::NAMES[l] } },
            :activities => activities,
        }
        io << "<section class='class-setting class-setting-languages'><h3 class='class-setting-title' id='class-languages-title'>Sprachen der Klasse</h3>" \
            "<p class='class-setting-hint'>Jede Aktivität, die es in mehreren Sprachen gibt, zeigt die erste dieser Liste, die sie anbietet. " \
            "Zieh eine Sprache an ihren Platz oder hol eine aus »Weitere Sprachen« dazu. Lernende können auf der Seite selbst umschalten.</p>" \
            "<div class='class-languages' data-languages='#{h.(data.to_json)}'></div></section>"
        io << "<script src='/include/class_languages.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
        # Copy buttons on every code block for the group's students, or only
        # where the page has them (group_copy_all_code?).
        copy_all = group_copy_all_code?([group])
        io << "<section class='class-setting'><h3 class='class-setting-title'><label for='class-copy-all-code'>Kopierknöpfe</label></h3>" \
            "<select class='form-select class-copy-all-code' id='class-copy-all-code' data-group='#{h.(group)}' aria-describedby='class-copy-all-code-hint'>" \
            "<option value='all'#{copy_all ? ' selected' : ''}>An jedem Codeblock</option>" \
            "<option value='marked'#{copy_all ? '' : ' selected'}>Wo sinnvoll</option></select>" \
            "<p class='class-setting-hint' id='class-copy-all-code-hint'>»An jedem Codeblock« (Voreinstellung): überall ein Kopierknopf. " \
            "»Wo sinnvoll«: nur dort, wo Abtippen nichts lehrt (Adressen, Befehle, große Dateien); alles andere tippen die Lernenden ab.</p></section>"
        io << "<script>document.querySelector('.class-copy-all-code').addEventListener('change', function (e) {" \
            "api_call('/api/set_group_copy_all_code', { group: e.target.dataset.group, on: e.target.value === 'all' }, function () { show_success_message('Gespeichert.'); }, { no_please_wait: true });" \
            "});</script>"
        io << class_repos_html(group)
        io << class_school_html(group)
        io << "</div>"
    end

    # The class repositories of the group (ClassRepos), in the tab
    # »Einstellungen«: one field per activity that clones one, drawn by
    # class_board.js from data-repos.
    def class_repos_html(group)
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
        git = GitServer.enabled? ? " data-git-server='1'" : ''
        # every repository in the teacher's groups at once: /repos (GitOverview)
        overview = GitServer.enabled? ? "<p class='class-repos-overview'><i class='bi bi-git' aria-hidden='true'></i>" \
            "Alle Repositorys deiner Gruppen im Workspace: <a href='#{WEB_ROOT}/repos'>Übersicht</a></p>" : ''
        "<section class='class-setting class-setting-repos'><h3 class='class-setting-title'>Git-Repositorys</h3>" \
            "<div class='class-repos' data-group='#{h.(group)}' data-repos='#{h.(entries.to_json)}'#{git}></div>#{overview}</section>"
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
        # the group's repository of the activity, on the Workspace's Git server
        browse = slug && (@@content[slug] || {})[:class_repo] && git_browse_url((group_class_repos([group])[group] || {})[slug])
        io << "<a class='class-activity-link' href='#{h.(browse)}' title='Das Repository der Gruppe zu dieser Aktivität'><i class='bi bi-git' aria-hidden='true'></i>Repository</a>" if browse
        io << activity_feedback_button(group, activity_key)
        live = tasks.select { |t| TaskCheck::LIVE_TYPES.include?(t[:type]) }
        unless live.empty?
            io << "<div class='dropdown class-live' data-group='#{h.(group)}'>"
            io << "<button type='button' class='btn btn-sm btn-outline-secondary dropdown-toggle' data-bs-toggle='dropdown' aria-expanded='false'>"
            io << "<i class='bi bi-search' aria-hidden='true'></i>Jetzt nachsehen</button><ul class='dropdown-menu class-live-menu'>"
            io << "<li><span class='dropdown-item-text class-live-note'>Liest die Workspaces der Gruppe so, wie die Karte es tut, und ändert nichts.</span></li>"
            live.each do |t|
                n = tasks.index(t) + 1
                io << "<li><button type='button' class='dropdown-item class-live-check' data-task='#{h.(activity_key)}/#{h.(t[:id])}'>#{n}. #{h.(t[:title])}</button></li>"
            end
            io << "</ul></div>"
        end
        io << meeting_control_html(group, activity_key, tasks)
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
        # How did it go? Once the group looks finished (ActivityFeedback)
        io << activity_feedback_html(group, activity_key, states)
        board = ClassBoard.build(members, tasks, states, :viewer => viewer)
        # The meeting point: who has arrived (class_meeting.js asks again
        # while the page is open), and the line on the board after its task
        point = meeting_points([group], activity_key)[group]
        io << "<div class='class-meeting-line' data-group='#{h.(group)}' data-activity='#{h.(activity_key)}' aria-live='polite'>" \
            "#{point ? MeetingPoint.status_html(MeetingPoint.status(tasks, point, members, states)) : ''}</div>"
        io << ClassBoard.html(board, :activity => activity_key, :meeting => point,
            :submission_url => lambda { |email, id| "/api/task/submission/#{activity_key}/#{id}?email=#{CGI.escape(email)}" })
        io << class_scene_galleries_html(activity_key, tasks, board[:own] ? members + [viewer] : members)
        io << class_kata_hand_ins_html(activity_key, tasks, members)
        io
    end

    # For admins on the front page: how often each task was solved (correct
    # or handed in, StudentHome::DONE), and by how many people per activity,
    # without admins, teachers and test accounts. Two queries over all
    # TaskStates, at most every USAGE_SECONDS; nothing new is stored.
    # { :tasks => { 'activity/id' => solved }, :people => { activity => n } }
    USAGE_SECONDS = 600
    def activity_usage
        @@activity_usage = nil unless defined?(@@activity_usage)
        return @@activity_usage[:counts] if @@activity_usage && Time.now - @@activity_usage[:at] < USAGE_SECONDS

        excluded = (ADMIN_USERS.to_a + @@teachers.keys + @@test_accounts.keys).uniq
        options = { :done => StudentHome::DONE, :excluded => excluded }
        tasks = neo4j_query(<<~END_OF_QUERY, options).to_h { |row| [row['task'], row['solved'].to_i] }
            MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.status IN $done AND NOT u.email IN $excluded
            RETURN s.task AS task, count(s) AS solved;
        END_OF_QUERY
        people = neo4j_query(<<~END_OF_QUERY, options).to_h { |row| [row['activity'], row['people'].to_i] }
            MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.status IN $done AND NOT u.email IN $excluded
            RETURN split(s.task, '/')[0] AS activity, count(DISTINCT u) AS people;
        END_OF_QUERY
        counts = { :tasks => tasks, :people => people }
        @@activity_usage = { :at => Time.now, :counts => counts }
        counts
    end

    # For a teacher the same counts as activity_usage, over the students of
    # the groups the teacher teaches (CardUsage.students_of), with :of, how
    # many they are. Kept only until a task result changes (stored or
    # deleted, @@task_results_changed_at) or the groups change: during a
    # lesson the band is as current as the class page, and a quiet evening
    # costs no queries.
    def teacher_usage(email)
        @@teacher_usage = {} unless defined?(@@teacher_usage)
        excluded = (ADMIN_USERS.to_a + @@teachers.keys + @@test_accounts.keys).uniq
        students = CardUsage.students_of(email, :teachers => @@teachers, :group_members => @@group_members,
                                         :skip => @@test_groups, :excluded => excluded).to_a.sort
        cached = @@teacher_usage[email]
        changed = defined?(@@task_results_changed_at) ? @@task_results_changed_at : Time.at(0)
        return cached[:counts] if cached && cached[:students] == students && cached[:at] > changed

        options = { :done => StudentHome::DONE, :students => students }
        tasks = students.empty? ? {} : neo4j_query(<<~END_OF_QUERY, options).to_h { |row| [row['task'], row['solved'].to_i] }
            MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.status IN $done AND u.email IN $students
            RETURN s.task AS task, count(s) AS solved;
        END_OF_QUERY
        people = students.empty? ? {} : neo4j_query(<<~END_OF_QUERY, options).to_h { |row| [row['activity'], row['people'].to_i] }
            MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.status IN $done AND u.email IN $students
            RETURN split(s.task, '/')[0] AS activity, count(DISTINCT u) AS people;
        END_OF_QUERY
        counts = { :tasks => tasks, :people => people, :of => students.size }
        @@teacher_usage[email] = { :at => Time.now, :students => students, :counts => counts }
        counts
    end

    # A student's own tasks on the cards, for the activities begun:
    # { slug => { :titles => [core task titles], :marks => [...] } }, as the
    # pinned card counts them (StudentHome.progress: extras do not count).
    def my_card_progress
        @my_card_progress ||= begin
            begun = my_progress.to_h { |p| [p[:slug], p] }
            activities_with_tasks(->(slug) { class_repo_variant(slug) }).filter_map do |a|
                entry = begun[a[:slug]]
                next unless entry

                [a[:slug], { :titles => a[:tasks].reject { |t| t[:extra] }.map { |t| t[:title] }, :marks => entry[:marks] }]
            end.to_h
        end
    end

    # Which band the catalog shows at the foot of each card (usage_html):
    # admins everybody's (an admin who also teaches too: the admin view is
    # the wider one and already holds the class), a teacher their own
    # students', a student their own tasks; nil for anyone else.
    def card_usage
        if admin_logged_in?
            { :mode => :admin, :counts => activity_usage }
        elsif teacher_logged_in?
            { :mode => :teacher, :counts => teacher_usage(@session_user[:email]) }
        elsif student_home_visible?
            { :mode => :own, :progress => my_card_progress }
        end
    end

    # The band at the foot of a card, a tile or a Kata set (CardUsage): one
    # row, a thin band with a segment for every task of the page, in its
    # order, and beside it what it adds up to; '' where there is none (no
    # tasks, or a student who has not begun).
    def usage_html(usage, slug, css_class)
        content = @@content[slug] || {}
        return '' unless usage && content[:tasks]

        if usage[:mode] == :own
            mine = usage[:progress][slug]
            return mine ? CardUsage.own_html(mine[:titles], mine[:marks], :css_class => css_class) : ''
        end
        counts = usage[:counts]
        activity = content[:tasks][:activity]
        ids = content[:tasks][:on_page] || []
        per_task = ids.map { |id| counts[:tasks]["#{activity}/#{id}"].to_i }
        titles = ids.map { |id| content[:tasks][:by_id][id]['title'].to_s }
        CardUsage.band_html(titles, per_task, :solved => per_task.sum, :people => counts[:people][activity].to_i,
                            :css_class => css_class, :of => usage[:mode] == :teacher ? counts[:of] : nil)
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
                # the band at the foot of the cards (card_usage): admins, teachers, students
                usage = catalog == :student ? card_usage : nil
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
                    # short: in sections.yaml is the sentence a phone shows
                    # instead of the description, so the cards come sooner.
                    if section[:description]
                        if section[:short]
                            io.puts "<p class='section-description'><span class='section-long'>#{section[:description]}</span><span class='section-short'>#{section[:short]}</span></p>"
                        else
                            io.puts "<p class='section-description'>#{section[:description]}</p>"
                        end
                    end
                    entries = section[:entries].reject do |entry|
                        (!DEVELOPMENT) && @@content[entry][:dev_only]
                    end
                    # carousel: true (Katas): one strip of the sets with the
                    # person's progress instead of cards (KataMap)
                    if section[:carousel]
                        # a student's own progress is the strip's already
                        io.puts kata_carousel_html(section, usage && usage[:mode] != :own ? usage : nil)
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
                                "#{year ? "<span class='lang-tile-year'>#{year}</span>" : ''}#{usage_html(usage, slug, 'lang-tile-usage')}</a>"
                        end
                        io.puts "</div>"
                        next
                    end
                    io.puts "<div class='row'>"
                    # Every card the same size, three in a row. A last row
                    # that would not be full (4, 5 or 7 cards) is not left
                    # half empty: the last cards go in pairs instead, each a
                    # compact row like on a phone (default.css, .hscol-pair),
                    # the last two (5, 8 …) or the last four (4, 7 …).
                    rest = entries.size % 3
                    paired = rest == 0 ? 0 : (rest == 1 ? [4, entries.size].min : 2)
                    entries.each_with_index do |slug, index|
                        content = @@content[slug]
                        io.puts "<div class='hscol#{index >= entries.size - paired ? ' hscol-pair' : ''}'>"
                        pinned_card = catalog == :student && catalog_pins.include?(slug)
                        band = usage_html(usage, slug, 'card-usage')
                        used_card = band.empty? ? '' : ' card-with-usage'
                        io.puts "<a href='/#{slug}' class='tutorial_card3#{pinned_card ? ' card-pinned' : ''}#{used_card}'>"
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
                            io.puts StudentHome.card_requires(PageKinds.requires_of(slug, @@content, :development => DEVELOPMENT))
                        end
                        io.puts band
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
        assert(key != :git || GitServer.enabled?)

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

    # The menu's button to /repos, for those who chose it on the profile
    # (show_git); none without the Workspace's own Git server.
    def git_menu_button_html
        return '' unless GitServer.enabled? && @session_user

        "<a style='#{@session_user[:show_git] ? '' : 'display: none;'}' href='/repos' data-menu-module='git' class='btn btn-menu'>" \
            "<i class='bi bi-git'></i>Git</a>"
    end

    # The menu (_template.html): one strong button (open the Workspace or
    # log in), then the tools. What the visitor may not use is left out of
    # the page, not hidden, so an anonymous page names no tool, class or
    # admin page. A tool switched off in the profile stays in the page,
    # hidden: the profile switches it on and off in place (profil.html).
    def menu_tool_buttons_html(path)
        if @session_user.nil?
            return "<a class=\"btn btn-success #{path == '/login.html' ? 'active' : ''}\" href=\"/login\"><i class='bi bi-box-arrow-in-right'></i>Anmelden</a>"
        end
        hidden = ->(key) { @session_user[key] ? '' : " style='display: none;'" }
        io = []
        io << "<button id='bu_launch' class=\"btn btn-success bu-launch-workspace\"><i class='bi bi-code-slash'></i>Workspace öffnen</button>" if @session_user[:show_workspace]
        io << "<button#{hidden.(:show_phpmyadmin)} id='bu_phpmyadmin' data-menu-module='phpmyadmin' class=\"btn btn-menu\" target=\"_blank\"><i class='bi bi-database'></i>phpMyAdmin</button>"
        io << "<button#{hidden.(:show_neo4j)} id='bu_neo4j' data-menu-module='neo4j' class=\"btn btn-menu\" target=\"_blank\"><i class='bi bi-diagram-3'></i>Neo4j</button>"
        io << "<a#{hidden.(:show_tic80)} href=\"/tic80/\" data-menu-module='tic80' class=\"btn btn-menu\" target=\"_blank\"><svg class='bi bi-tic80' width='16' height='16' viewBox='0 0 16 16' fill='currentColor' aria-hidden='true'><path fill-rule='evenodd' d='M2 .5h11A1.5 1.5 0 0 1 14.5 2v9a1.5 1.5 0 0 1-1.5 1.5H2A1.5 1.5 0 0 1 .5 11V2A1.5 1.5 0 0 1 2 .5Zm0 1a.5.5 0 0 0-.5.5v1h12V2a.5.5 0 0 0-.5-.5H2Zm11.5 2.5h-12v7a.5.5 0 0 0 .5.5h11a.5.5 0 0 0 .5-.5V4Z'/><path d='M5 5.5h1V8H5zM9 5.5h1V8H9zM4.5 9h1v.5h4V9h1v1.5h-6z'/><path d='M3 12.5h2V15H3zM10 12.5h2V15h-2z'/><path d='M14.5 5H16v3h-1.5V7h.5V6h-.5z'/></svg>TIC-80</a>"
        io << git_menu_button_html
        io.join("\n")
    end

    # The menu after »Handbuch«: »Klassen« for teachers, »Administration«
    # for administrators, the profile for everyone logged in, and
    # »Abmelden« only on the profile page, beside the name, with its label:
    # a login lasts a year, and the profile is where one looks for it (and
    # signs out elsewhere). Teachers manage their groups on the class page
    # (tab »Gruppe«); /gruppen is the administration's overview.
    def menu_account_buttons_html
        return '' if @session_user.nil?
        here = ->(p) { request.path == p ? " aria-current='page'" : '' }
        active = ->(p) { request.path == p ? 'active' : '' }
        io = []
        if teacher_logged_in? || teacher_account_logged_in?
            io << "<a href='/klasse' class=\"btn btn-menu #{active.('/klasse')}\"#{here.('/klasse')}><i class='bi bi-people'></i>Klassen</a>"
        end
        if admin_logged_in?
            # what the site shows: picture rights (picture_rights.rb), the
            # teachers' feedback on activities (activity_feedback.rb) and,
            # in development only, the sources of the person boxes
            item = ->(p, icon, label) { "<li><a class=\"dropdown-item #{active.(p)}\" href='#{p}'#{here.(p)}><i class='bi bi-#{icon} me-2'></i>#{label}</a></li>" }
            open = ['/admin', '/gruppen', '/personen', '/picture-rights', '/person-sources', '/rueckmeldungen'].include?(request.path)
            io << "<div class='btn-group'>"
            io << "<button type='button' class=\"btn btn-menu dropdown-toggle #{open ? 'active' : ''}\" data-bs-toggle='dropdown' aria-expanded='false'><i class='bi bi-wrench-adjustable'></i>Administration</button>"
            io << "<ul class='dropdown-menu'>"
            io << item.('/admin', 'speedometer2', 'Betrieb')
            io << item.('/gruppen', 'person-lines-fill', 'Gruppen')
            io << item.('/personen', 'building', 'Personen und Schulen')
            io << "<li><hr class='dropdown-divider'></li><li><h6 class='dropdown-header'>Inhalte</h6></li>"
            io << item.('/picture-rights', 'image', 'Bildrechte')
            io << item.('/rueckmeldungen', 'chat-square-text', 'Rückmeldungen')
            io << item.('/person-sources', 'patch-check', 'Belege der Personen') if DEVELOPMENT
            io << "</ul></div>"
        end
        name = (@@invitations[@session_user[:email]] || {})[:name].to_s.strip
        io << "<a href='/profil' class=\"btn btn-menu btn-menu-profile #{active.('/profil')}\" title='Profil'><i class='bi bi-person-badge'></i>#{CGI.escapeHTML(name.empty? ? @session_user[:email].to_s : name)}</a>"
        if request.path == '/profil'
            io << "<a href='/logout' class=\"btn btn-menu btn-menu-logout\"><i class='bi bi-box-arrow-right' aria-hidden='true'></i>Abmelden</a>"
        end
        io.join("\n")
    end

    # The profile's »Git« (under »Werkzeuge«, a tool like the TIC-80): the
    # key with the Workspace's Git server, one's own repositories with
    # »Neues Repository«, the way to the groups' ones and to the Handbuch.
    def git_profile_html
        return '' unless GitServer.enabled? && user_logged_in?

        h = ->(t) { CGI.escapeHTML(t.to_s) }
        email = @session_user[:email]
        id = git_personal_id(email)
        mine = id ? GitServer.repos(id) : []
        git_groups = Main.git_groups
        groups = (teacher_account_logged_in? || admin_logged_in? ? class_groups : my_groups).select { |g| git_groups.key?(g) }
        shared = groups.sum { |g| git_names(git_groups[g][:id]).size }
        full = mine.size >= GitServer::PERSONAL_MAX ? "Du hast schon #{GitServer::PERSONAL_MAX} Repositorys." : nil
        io = +"<div class='profile-subsection' id='git'><div class='profile-subsection-heading'><h3>Git</h3>" \
            "<div class='btn-module-control'>#{print_module_button(:git)}</div></div>"
        io << '<p>Der Git-Server des Workspace hält Repositorys für deine Gruppen und eigene zum Üben, die nur du klonen, pushen und ansehen kannst. ' \
            "Erreichbar ist er aus deinem Workspace. Wie Git geht, steht im Handbuch unter <a href='/reference/git'>Git</a>.</p>"
        io << git_my_key_html(email)
        io << "<div class='git-profile-head'><h4>Meine Repositorys <span class='git-profile-count'>#{mine.size} von #{GitServer::PERSONAL_MAX}</span></h4>" \
            "#{GitOverview.new_form_html(:disabled => full)}</div>"
        if mine.empty?
            io << "<p class='git-overview-intro'>Noch keine.</p>"
        else
            io << "<ul class='git-profile-list'>"
            mine.each do |r|
                io << "<li><i class='bi bi-journal-code' aria-hidden='true'></i><a href='#{h.("#{WEB_ROOT}/git/#{id}/#{r[:name]}")}'>#{h.(r[:name])}</a>"
                io << "<span class='git-overview-description'>#{h.(r[:description])}</span>" unless r[:description].empty?
                io << '</li>'
            end
            io << '</ul>'
        end
        io << "<p class='git-profile-groups'><i class='bi bi-people' aria-hidden='true'></i>" \
            "#{shared.zero? ? 'Deine Gruppen haben noch keine Repositorys im Workspace.' : "In deinen Gruppen: #{shared == 1 ? 'ein Repository' : "#{shared} Repositorys"}."} " \
            "<a href='/repos'>Alle Repositorys</a></p>"
        cache = "#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}"
        io << "<script src='/include/git_repos.js?#{cache}' defer></script><script src='/include/git_key.js?#{cache}' defer></script></div>"
    end

    def print_module_button(key)
        assert(user_logged_in?)
        active = @session_user["show_#{key}".to_sym]
        "<button class='btn btn-md #{active ? 'btn-success' : 'btn-outline-secondary'} bu-toggle-module' data-module='#{key}'><i class='bi #{active ? 'bi-check-lg' : 'bi-x-lg'}'></i>#{MODULE_LABELS[key]} im Menü #{active ? '' : 'nicht'} anzeigen</button>"
    end

    # The state whose curricula a teacher sees (Curriculum): one of the
    # states of the plans in src/content/curricula/.
    post '/api/set_curriculum_state/:state' do
        assert(teacher_logged_in?)
        state = params['state']
        assert(Curriculum.states.any? { |key, _, _| key == state })
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :state => state})
            MATCH (u:User {email: $email})
            SET u.curriculum_state = $state
            RETURN u;
        END_OF_STRING
        respond(:ok => true)
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

    # SELECT
    #     table_name AS `table`,
    #     table_rows AS `rows`,
    #     data_length + index_length AS size
    # FROM
    #     information_schema.tables
    # WHERE
    #     table_schema = 'specht';

    # show create table specht.crew;

    # The programming language someone wants to see on pages that offer
    # more than one (CodeLanguages); overrides the class's.
    post '/api/set_code_language' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:language])
        assert(CodeLanguages.known?(data[:language]))
        # JavaScript, Dart and Java only where a page offers them
        # (CodeLanguages): they do not replace the choice between Ruby and
        # Python, and picking one of those again leaves them behind.
        if CodeLanguages::OWN_LANGUAGES.include?(data[:language])
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

    # The languages a teacher ranks for a group they administer
    # (»Sprachen der Klasse«): known ones, each once, at least one; the list
    # replaces the one language stored before it.
    post '/api/set_group_code_languages' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:group, :languages], :types => { :languages => Array })
        group = data[:group]
        assert(class_groups.include?(group))
        languages = data[:languages]
        assert(!languages.empty? && languages.all? { |l| l.is_a?(String) && CodeLanguages::GROUP_LANGUAGES.include?(l) } && languages.uniq.size == languages.size)
        neo4j_query(<<~END_OF_STRING, {:group => group, :languages => languages})
            MERGE (g:GroupSetting {group: $group})
            SET g.code_languages = $languages
            REMOVE g.code_language;
        END_OF_STRING
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
            admins_only!
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
            admins_only!
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
            if html.include?(ClassRepos::BROWSE_TOKEN)
                browse = git_browse_url(view[:address])
                html = browse ? html.gsub(ClassRepos::BROWSE_TOKEN, CGI.escapeHTML(browse)) : html.gsub(%r{<p class=["']class-repo-browse["']>.*?</p>}m, '')
            end
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
    def kata_carousel_html(section, usage = nil)
        slugs = section[:entries].select do |slug|
            @@content[slug] && @@content[slug][:tasks] && (DEVELOPMENT || !@@content[slug][:dev_only])
        end
        # the curriculum line through the teacher's state (Curriculum)
        plan = teacher_logged_in? ? curriculum_plan : nil
        stops = slugs.map { |slug| KataMap.stop(slug, plan ? @@content[slug].merge(:curriculum => Curriculum.line(plan, slug, @@content[slug])) : @@content[slug]) }
        # a picture as an icon with its file's time, so a new one is loaded
        stops.each do |stop|
            path = "/src/static#{stop.icon}"
            stop.icon = "#{stop.icon}?#{File.mtime(path).to_i}" if stop.icon.to_s.start_with?('/') && File.file?(path)
        end
        progress = user_logged_in? ? my_progress.to_h { |p| [p[:slug], p] } : nil
        pinned = user_logged_in? ? pins_for_groups((my_groups + class_groups).uniq).map { |p| p[:slug] }.uniq & slugs : []
        KataMap.carousel_html(stops, :progress => progress, :teacher => teacher_logged_in?, :pinned => pinned,
            :usage => usage ? slugs.to_h { |slug| [slug, usage_html(usage, slug, 'kata-set-usage')] } : {})
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

    # The starter of an activity, its folder starter/ (the class quiz): the
    # files a page fetches with wget when the group has no class repository,
    # the same that GitServer puts into a new one. Only that folder of an
    # activity the person may see; anything else in the activity's folder
    # (tasks.yaml, the solutions) stays unreachable.
    get %r{/([a-z0-9-]+)/starter/([A-Za-z0-9_\-./]+)} do |slug, path|
        content = @@content[slug]
        halt 404 unless content && !content[:teacher_only] && (DEVELOPMENT || !content[:dev_only])
        root = File.join('/src/content', slug, 'starter')
        file = File.expand_path(path, root)
        halt 404 unless file.start_with?("#{root}/") && File.file?(file)
        respond_raw_with_mimetype(File.binread(file), 'text/plain; charset=utf-8')
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
        # The pictures that need permission, with the letters
        # (picture_rights.rb): administrators only.
        if path == '/picture-rights'
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt_forbidden_page! unless admin_logged_in?
        end
        # Feedback of the teachers on the activities (activity_feedback.rb):
        # administrators only.
        if path == '/rueckmeldungen'
            this_is_a_page_for_logged_in_users!
            Main.load_invitations()
            halt_forbidden_page! unless admin_logged_in?
        end
        # The activities by the names of the curricula (curriculum_overview.rb)
        # are public: a teacher or a school can see what the Workspace offers
        # before asking for an account. The curriculum line on each activity
        # stays for teachers.
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
            sid = current_session_key
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
