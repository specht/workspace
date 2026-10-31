# Starting Workspaces: the runtime that talks to Docker, and the queue that
# starts them in the background, one worker, so that long Docker, chown,
# tar and nginx work stays out of the requests and a second click on
# »Starten« waits for the first. Moved out of main.rb, which loads it.
class Main < Sinatra::Base
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
end
