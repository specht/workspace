require 'json'
require 'open3'

# »Meine Daten« in the profile and the administration's tools for big
# Workspaces: how big the download is before it starts, how many run at a
# time, folders a tool makes again (»Platz schaffen«, tools/rebuildable.py),
# and stopping a Workspace that runs away. Whatever the administration does
# in someone's Workspace, the person learns about it: a notice on the
# website (LookIn: stop, cleanup), and an e-mail when ticked.
class Main < Sinatra::Base
    # A download keeps one of Puma's threads for its whole length (minutes
    # for a big Workspace): at most this many at once, one per person. A
    # claim nobody released (a stream that never started) ends by itself.
    EXPORT_LIMIT = 2
    EXPORT_STALE_SECONDS = 3 * 3600
    @@exports_running = {}
    @@exports_mutex = Mutex.new

    # :ok, :mine (this person's download is already running) or :busy.
    def self.claim_export(email, now = Time.now)
        @@exports_mutex.synchronize do
            @@exports_running.delete_if { |_, since| now - since > EXPORT_STALE_SECONDS }
            return :mine if @@exports_running.key?(email)
            return :busy if @@exports_running.size >= EXPORT_LIMIT

            @@exports_running[email] = now
            :ok
        end
    end

    def self.release_export(email)
        @@exports_mutex.synchronize { @@exports_running.delete(email) }
    end

    # »1,2 GB«, »640 MB«, »12 kB«.
    def self.human_size(bytes)
        units = [['GB', 1024**3], ['MB', 1024**2], ['kB', 1024]]
        unit, size = units.find { |_, s| bytes.to_i >= s } || ['kB', 1024]
        value = bytes.to_f / size
        text = value >= 10 || unit == 'kB' ? value.round.to_s : format('%.1f', value).sub('.', ',')
        "#{text} #{unit}"
    end

    def workspace_folder_of(email)
        File.join('/user', fs_tag_for_email(email), 'workspace')
    end

    # tools/rebuildable.py as the Workspace user, never as root: the folder
    # is the student's, who may put a link anywhere.
    def run_rebuildable(*args)
        out, status = Open3.capture2('setpriv', "--reuid=#{WORKSPACE_UID}", "--regid=#{WORKSPACE_GID}", '--clear-groups',
            'python3', '-I', File.join(__dir__, '..', 'tools', 'rebuildable.py'), *args, :err => File::NULL)
        raise 'rebuildable.py failed' unless status.success?

        JSON.parse(out)
    end

    def rebuildable_of(email)
        folder = workspace_folder_of(email)
        File.directory?(folder) ? run_rebuildable('scan', folder) : []
    end

    # What the profile shows before the download: the Workspace's size (the
    # hourly measurement) and the folders that stay out unless asked.
    def data_overview(email)
        usage = workspace_disk_usage
        kb = usage[:by_tag][fs_tag_for_email(email)]
        folders = rebuildable_of(email)
        {
            :workspace_bytes => kb ? kb.to_i * 1024 : nil,
            :measured_at => usage[:measured_at],
            :rebuildable => folders,
            :rebuildable_bytes => folders.sum { |f| f['bytes'].to_i },
        }
    end

    post '/api/my_data_overview' do
        assert(user_logged_in?)
        respond(data_overview(@session_user[:email]))
    end

    # The person's own »Platz schaffen«. Not while someone imitates them:
    # the administration has its own way, which tells the person.
    post '/api/clean_rebuildable' do
        assert(user_logged_in?)
        halt 403, { :error => 'impersonating' }.to_json if @impersonated_by
        data = parse_request_data(:required_keys => [:paths], :types => { :paths => Array }, :max_body_length => 16_000)
        email = @session_user[:email]
        deleted = run_rebuildable('delete', workspace_folder_of(email), *data[:paths].map(&:to_s))
        STDERR.puts "#{email} made room: #{deleted.join(', ')}"
        respond(:deleted => deleted)
    end

    post '/api/admin/rebuildable' do
        admins_only!
        data = parse_request_data(:required_keys => [:email])
        respond(:folders => rebuildable_of(data[:email].to_s.downcase))
    end

    # The notice the person gets, and the mail when ticked (MailTexts).
    def tell_about(email, kind, reason, details: nil, notify: false)
        record_look_in(email, kind, reason, :details => details)
        return unless notify

        by = @session_user[:email]
        subject_line, html = MailTexts.workspace_action(:kind => kind, :by_name => ((@@invitations[by] || {})[:name] || by),
            :reason => LookIn.reason(reason), :details => details, :web_root => WEB_ROOT, :signature => mail_signature)
        deliver_mail do
            to email
            from SMTP_FROM
            reply_to by
            subject subject_line
            html
        end
    end

    # »Platz schaffen« for someone else, from /personen: with a reason the
    # person reads, in the audit log, and by mail when ticked.
    post '/api/admin/clean_rebuildable' do
        admins_only!
        data = parse_request_data(:required_keys => [:email, :paths, :reason], :optional_keys => [:notify],
            :types => { :paths => Array, :notify => Object }, :max_body_length => 16_000)
        email = data[:email].to_s.downcase
        halt 400, { :error => 'reason_missing' }.to_json unless LookIn.reason(data[:reason])
        before = rebuildable_of(email).to_h { |f| [f['path'], f] }
        deleted = run_rebuildable('delete', workspace_folder_of(email), *data[:paths].map(&:to_s))
        halt 400, { :error => 'nothing_deleted' }.to_json if deleted.empty?

        details = deleted.map { |p| "#{p} (#{Main.human_size(before[p]['bytes'])}, wieder mit: #{before[p]['command']})" }.join('; ')
        tell_about(email, 'cleanup', data[:reason], :details => details, :notify => data[:notify] == true)
        STDERR.puts "#{@session_user[:email]} made room in the Workspace of #{email}: #{deleted.join(', ')}"
        respond(:deleted => deleted, :bytes => deleted.sum { |p| before[p]['bytes'].to_i })
    end

    # A Workspace that runs away (»Betrieb«): the processes end, the files
    # stay; the person reads why and can start it again at once.
    post '/api/admin/stop_workspace' do
        admins_only!
        data = parse_request_data(:required_keys => [:email, :reason], :optional_keys => [:notify], :types => { :notify => Object })
        email = data[:email].to_s.downcase
        halt 400, { :error => 'own_account' }.to_json if email == @session_user[:email]
        halt 400, { :error => 'reason_missing' }.to_json unless LookIn.reason(data[:reason])
        stop_server(email)
        tell_about(email, 'stop', data[:reason], :notify => data[:notify] == true)
        log_event "#{@session_user[:email]} stopped the Workspace of #{email}"
        respond(:stopped => email)
    end
end
