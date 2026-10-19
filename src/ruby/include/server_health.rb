require 'open3'
require 'socket'
require 'time'
require 'timeout'

# What /admin shows under »Betrieb« and what the administration gets a mail
# about: the facts are gathered here, judged by Health (include/health.rb).
class Main < Sinatra::Base
    # The backup reports itself by touching this file (borgmatic, after a
    # successful run); BACKUP_MAX_AGE_HOURS in credentials.rb, default 26.
    BACKUP_STAMP_FILE = '/internal/backup-done'
    HEALTH_ALERT_FILE = '/internal/health-alerts.json'
    PROBE_SECONDS = 3

    @@process_started_at = Time.now
    @@start_history = []
    @@start_history_mutex = Mutex.new
    @@hourly_ran_at = nil

    # Called by the start worker for every finished start.
    def self.record_server_start(job, ok)
        return unless job[:queued_at]

        @@start_history_mutex.synchronize do
            @@start_history << { :at => Time.now.to_i, :ok => ok, :seconds => Time.now - job[:queued_at] }
            cutoff = Time.now.to_i - 86_400
            @@start_history.shift while @@start_history.first && @@start_history.first[:at] < cutoff
        end
    end

    def self.hourly_jobs_ran!
        @@hourly_ran_at = Time.now.to_i
    end

    def self.backup_max_hours
        defined?(BACKUP_MAX_AGE_HOURS) ? BACKUP_MAX_AGE_HOURS.to_i : 26
    end

    # nil when it answered, else what went wrong (no details that could
    # hold a password).
    def self.probe_error
        Timeout.timeout(PROBE_SECONDS) { yield }
        nil
    rescue Timeout::Error
        "keine Antwort in #{PROBE_SECONDS} s"
    rescue StandardError => e
        e.class.name.split('::').last
    end

    def self.docker_ping
        socket = ENV['DOCKER_HOST'].to_s.start_with?('unix://') ? ENV['DOCKER_HOST'].delete_prefix('unix://') : '/var/run/docker.sock'
        UNIXSocket.open(socket) do |s|
            s.write("GET /_ping HTTP/1.1\r\nHost: docker\r\nConnection: close\r\n\r\n")
            answer = s.read.to_s
            raise "HTTP #{answer[/\AHTTP\/1\.\d (\d+)/, 1] || '?'}" unless answer.start_with?('HTTP/1.1 200', 'HTTP/1.0 200')
        end
    end

    GIT_DIR = '/git'

    # The checked-out commit (config.rb mounts .git read-only), or nil.
    def self.running_commit
        return nil unless File.directory?(GIT_DIR)

        git = ['git', '-c', 'safe.directory=*', "--git-dir=#{GIT_DIR}"]
        out, status = Open3.capture2(*git, 'log', '-1', '--format=%H%x09%h%x09%ct%x09%s')
        return nil unless status.success?

        full, short, time, subject = out.strip.split("\t", 4)
        branch, = Open3.capture2(*git, 'symbolic-ref', '--short', '-q', 'HEAD')
        origin, = Open3.capture2(*git, 'config', '--get', 'remote.origin.url')
        repo = origin.strip[%r{github\.com[:/](.+?)(?:\.git)?\z}, 1]
        { :short => short, :time => time.to_i, :subject => subject, :branch => branch.strip,
          :url => repo ? "https://github.com/#{repo}/commit/#{full}" : nil }
    rescue StandardError
        nil
    end

    def self.workspace_image
        out, status = Open3.capture2('docker', 'image', 'inspect', '--format',
            '{{.Created}}|{{index .Config.Labels "de.hackschule.file-capabilities"}}', 'hs_code_server')
        return nil unless status.success?

        created, label = out.strip.split('|', 2)
        { :created => Time.parse(created).to_i, :hardened => label == 'none' }
    rescue StandardError
        nil
    end

    def health_checks
        now = Time.now
        queued, running = Main.server_start_queue_state
        history = @@start_history_mutex.synchronize { @@start_history.dup }
        probes = {
            'docker' => [ENV['DOCKER_HOST'].to_s.empty? ? 'Docker' : 'Docker (über den Wächter)', -> { Main.docker_ping }],
            'mysql' => ['MySQL (Lernende)', -> {
                client = Mysql2::Client.new(:host => 'mysql', :username => 'root', :password => MYSQL_ROOT_PASSWORD,
                    :connect_timeout => PROBE_SECONDS, :read_timeout => PROBE_SECONDS)
                client.query('SELECT 1;')
                client.close
            }],
            'neo4j' => ['Neo4j (Lernende)', -> { Socket.tcp('neo4j', 7687, :connect_timeout => PROBE_SECONDS).close }],
            'app_db' => ['Neo4j (Workspace)', -> { neo4j_query('RETURN 1 AS one;') }],
        }
        # At the same time, so that a hanging service costs PROBE_SECONDS once.
        results = probes.transform_values { |(_, probe)| Thread.new { Main.probe_error { probe.call } } }
            .transform_values { |thread| thread.join(PROBE_SECONDS + 1) ? thread.value : "keine Antwort in #{PROBE_SECONDS} s" }
        [
            Health.version(Main.running_commit),
            Health.image(Main.workspace_image),
            Health.backup((File.mtime(BACKUP_STAMP_FILE) rescue nil), :now => now, :max_hours => Main.backup_max_hours),
            Health.starts(history, :now => now, :since => @@process_started_at),
            Health.queue(queued, :running => running, :now => now),
            *probes.map { |key, (label, _)| Health.probe(key, label, results[key]) },
            Health.disk(Main.disk_status),
            Health.jobs(@@hourly_ran_at, workspace_disk_usage[:measured_at], :now => now, :since => @@process_started_at),
        ]
    end

    def health_html
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        icons = {
            :ok => "<i class='bi bi-check-circle-fill text-success' title='in Ordnung'></i>",
            :warn => "<i class='bi bi-exclamation-triangle-fill text-warning' title='ansehen'></i>",
            :bad => "<i class='bi bi-x-octagon-fill text-danger' title='gestört'></i>",
            :unknown => "<i class='bi bi-question-circle text-body-secondary' title='nicht eingerichtet'></i>",
        }
        checks = health_checks
        bad = checks.select { |c| c.state == :bad }
        alert = bad.empty? ? '' : "<div class='alert alert-danger'><i class='bi bi-x-octagon'></i> Im Betrieb ist etwas gestört: #{bad.map { |c| h.(c.label) }.join(', ')}.</div>"
        alert + "<table class='table table-sm people-table health-table'><tbody>" +
            checks.map do |c|
                link = c.link ? " <a href='#{h.(c.link)}' target='_blank' rel='noopener'>Details</a>" : ''
                "<tr><td>#{icons[c.state]}</td><th scope='row'>#{h.(c.label)}</th><td>#{h.(c.text)}#{link}</td></tr>"
            end.join +
            '</tbody></table>'
    end

    # Hourly: a mail to the administration for what stops lessons or loses
    # data, once a day for each.
    def check_health
        today = Date.today.to_s
        sent = (JSON.parse(File.read(HEALTH_ALERT_FILE)) rescue {})
        due = Health.to_mail(health_checks, sent, :today => today)
        return if due.empty?

        # One paragraph each: the plain-text part keeps paragraphs, not lists.
        items = due.map { |c| "<p><strong>#{CGI.escapeHTML(c.label)}:</strong> #{CGI.escapeHTML(c.text)}</p>" }.join
        text = "<p>Im Workspace ist etwas gestört.</p>#{items}<p>Den Stand zeigt #{WEB_ROOT}/admin#betrieb.</p>"
        ADMIN_USERS.each do |admin|
            deliver_mail do
                to admin
                from SMTP_FROM
                subject "Workspace: #{due.map(&:label).join(', ')} gestört"
                text
            end
        end
        due.each { |c| sent[c.key] = today }
        AtomicFile.write(HEALTH_ALERT_FILE, JSON.generate(sent))
        STDERR.puts "Health alert sent: #{due.map(&:key).join(', ')}"
    end
end
