# How the installation is doing, for /admin (»Betrieb«) and the mails to
# the administration: the backup, Workspace starts and their queue, Docker
# (through the guard), the databases, the disk, the hourly jobs. Plain Ruby:
# the server gathers the facts (include/server_health.rb), this module
# judges them, tested on its own (test/health_test.rb).
module Health
    # state: :ok, :warn (look at it soon), :bad (something does not work),
    # :unknown (not set up). text: one German sentence for the page.
    # link: a page with the details (the commit on GitHub).
    Check = Struct.new(:key, :label, :state, :text, :link, keyword_init: true)

    STATES = %i[ok unknown warn bad].freeze

    def self.worst(checks)
        checks.map(&:state).max_by { |s| STATES.index(s) || 0 } || :ok
    end

    # »3 Minuten«, »5 Stunden«, »2 Tagen« (after »vor«).
    def self.age_text(seconds)
        seconds = seconds.to_i
        if seconds < 120
            "#{seconds} Sekunden"
        elsif seconds < 2 * 3600
            "#{seconds / 60} Minuten"
        elsif seconds < 2 * 86_400
            "#{seconds / 3600} Stunden"
        else
            "#{seconds / 86_400} Tagen"
        end
    end

    # A start takes from under a second to minutes.
    def self.seconds_text(seconds)
        format(seconds < 10 ? '%.1f s' : '%.0f s', seconds)
    end

    def self.clock(time)
        Time.at(time.to_i).strftime('%d.%m.%Y %H:%M')
    end

    # The backup reports itself: after a successful run, borgmatic touches a
    # file (docs/entwicklung.md, »Betrieb überwachen«). mtime: its time or
    # nil; max_hours: from when on it is too old.
    def self.backup(mtime, now:, max_hours:)
        label = 'Sicherung'
        unless mtime
            return Check.new(:key => 'backup', :label => label, :state => :unknown,
                :text => 'Keine Meldung der Sicherung: Nach jeder Sicherung soll sie /internal/backup-done anlegen (docs/entwicklung.md, »Betrieb überwachen«).')
        end

        age = now.to_i - mtime.to_i
        state = age < max_hours * 3600 ? :ok : :bad
        text = "Letzte Sicherung vor #{age_text(age)} (#{clock(mtime)})."
        text += " Älter als #{max_hours} Stunden: Läuft die Sicherung noch?" if state == :bad
        Check.new(:key => 'backup', :label => label, :state => state, :text => text)
    end

    # history: finished starts, each { at:, ok:, seconds: } (queued until
    # done); since: when the server started (the history is kept in memory).
    def self.starts(history, now:, since:)
        label = 'Workspace-Starts'
        from = now.to_i - 86_400
        recent = history.select { |h| h[:at].to_i >= from }
        span = since.to_i > from ? "seit dem Neustart um #{Time.at(since.to_i).strftime('%H:%M')}" : 'in 24 Stunden'
        return Check.new(:key => 'starts', :label => label, :state => :ok, :text => "Keine Starts #{span}.") if recent.empty?

        failed = recent.reject { |h| h[:ok] }
        times = recent.select { |h| h[:ok] }.map { |h| h[:seconds].to_f }.sort
        state = if failed.empty? then :ok
            elsif failed.size >= 3 && failed.size * 5 >= recent.size then :bad
            else :warn
            end
        text = "#{recent.size} #{recent.size == 1 ? 'Start' : 'Starts'} #{span}"
        text += failed.empty? ? ', keiner fehlgeschlagen' : ", #{failed.size} fehlgeschlagen (zuletzt #{clock(failed.map { |h| h[:at] }.max)}, Details im Log)"
        text += "; bis gestartet im Median #{seconds_text(times[times.size / 2])}, höchstens #{seconds_text(times.last)}" unless times.empty?
        Check.new(:key => 'starts', :label => label, :state => state, :text => "#{text}.")
    end

    # queued: when each waiting start was queued; running: how many run.
    def self.queue(queued, running:, now:)
        label = 'Warteschlange'
        if queued.empty?
            return Check.new(:key => 'queue', :label => label, :state => :ok,
                :text => running.zero? ? 'Leer.' : "Leer; #{running} Start läuft.")
        end

        wait = now.to_i - queued.map(&:to_i).min
        state = wait > 300 ? :bad : wait > 60 ? :warn : :ok
        Check.new(:key => 'queue', :label => label, :state => state,
            :text => "#{queued.size} #{queued.size == 1 ? 'Start wartet' : 'Starts warten'}, der älteste seit #{age_text(wait)}; #{running} läuft.")
    end

    # A service that answers or not: error is nil when it answered.
    def self.probe(key, label, error, ok_text: 'Antwortet.')
        if error
            Check.new(:key => key, :label => label, :state => :bad, :text => "Antwortet nicht: #{error}")
        else
            Check.new(:key => key, :label => label, :state => :ok, :text => ok_text)
        end
    end

    # status: Main.disk_status (nil when df gave nothing).
    def self.disk(status)
        label = 'Speicherplatz'
        return Check.new(:key => 'disk', :label => label, :state => :unknown, :text => 'Nicht messbar (df /user).') unless status

        text = "Unter /user sind #{status[:free_gb]} GB frei (#{status[:free_percent]} %)."
        Check.new(:key => 'disk', :label => label, :state => status[:low] ? :bad : :ok, :text => text)
    end

    # last_run: when the hourly jobs (deletions, share links, disk, groups)
    # last finished; measured: when housekeeping.rb last measured the
    # Workspaces; since: the server's start.
    def self.jobs(last_run, measured, now:, since:)
        label = 'Stündliche Aufgaben'
        now = now.to_i
        late = ->(time) { time ? now - time.to_i > 2 * 3600 : now - since.to_i > 2 * 3600 }
        problems = []
        problems << "Löschungen, Links und Erinnerungen #{last_run ? "zuletzt vor #{age_text(now - last_run.to_i)}" : 'noch nie gelaufen'}" if late.call(last_run)
        problems << "Speichermessung #{measured ? "zuletzt vor #{age_text(now - measured.to_i)}" : 'fehlt'}" if late.call(measured)
        unless problems.empty?
            return Check.new(:key => 'jobs', :label => label, :state => :bad, :text => "#{problems.join('; ')}: im Log nachsehen.")
        end

        text = last_run ? "Zuletzt vor #{age_text(now - last_run.to_i)}." : 'Laufen kurz nach dem Start zum ersten Mal.'
        Check.new(:key => 'jobs', :label => label, :state => :ok, :text => text)
    end

    # commit: { short:, time:, subject:, branch:, url: } from git, or nil
    # when the Ruby service does not see .git (config.rb mounts it).
    def self.version(commit)
        label = 'Version'
        unless commit
            return Check.new(:key => 'version', :label => label, :state => :unknown,
                :text => 'Nicht lesbar: Der Ruby-Dienst sieht ./.git nicht (config.rb, dann ./config.rb up -d).')
        end

        text = "#{commit[:short]} vom #{clock(commit[:time])}"
        text += " auf #{commit[:branch]}" unless commit[:branch].to_s.empty?
        subject = commit[:subject].to_s
        text += ": #{subject.size > 100 ? "#{subject[0, 99]}…" : subject}" unless subject.empty?
        Check.new(:key => 'version', :label => label, :state => :ok, :text => text, :link => commit[:url])
    end

    # image: { created:, hardened: } for hs_code_server, or nil. hardened:
    # built with docker/code/Dockerfile's label for no file capabilities.
    def self.image(image)
        label = 'Workspace-Image'
        return Check.new(:key => 'image', :label => label, :state => :bad, :text => 'Nicht gefunden: ./build-image.sh') unless image

        text = "Gebaut am #{clock(image[:created])}."
        return Check.new(:key => 'image', :label => label, :state => :ok, :text => text) if image[:hardened]

        Check.new(:key => 'image', :label => label, :state => :warn,
            :text => "#{text} Älter als die Härtung (Oktober 2026): ./build-image.sh, sonst startet ping in neuen Workspaces nicht.")
    end

    # What gets a mail (once a day each): what stops lessons or loses data.
    # The disk has its own mail (check_disk_space), the queue clears itself.
    MAIL_KEYS = %w[backup starts docker mysql neo4j app_db jobs].freeze

    # The checks to mail about today: bad, mailed for, not yet today.
    # sent: key => the day of the last mail (»YYYY-MM-DD«).
    def self.to_mail(checks, sent, today:)
        checks.select { |c| c.state == :bad && MAIL_KEYS.include?(c.key) && sent[c.key] != today.to_s }
    end
end
