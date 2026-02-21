require "json"
require "open3"
require "timeout"

module Judge
  class DockerExecutor
    DEFAULT_TIMEOUT = 3 # seconds

    def initialize(image:, timeout: DEFAULT_TIMEOUT)
      @image = image
      @timeout = timeout
    end

    # Interactive session: keep one docker process alive and exchange NDJSON over stdin/stdout.
    # This is required for true live streaming of student stdout/stderr while cases run.
    class Session
      def initialize(stdin:, stdout:, stderr:, wait_thr:, timeout:)
        @stdin = stdin
        @stdout = stdout
        @stderr = stderr
        @wait_thr = wait_thr
        @deadline = Time.now + timeout
        @stderr_buf = +""

        @stderr_thread = Thread.new do
          begin
            @stderr.each_line { |line| @stderr_buf << line }
          rescue StandardError
            # ignore
          end
        end
      end

      def send(obj)
        @stdin.write(JSON.generate(obj))
        @stdin.write("\n")
        @stdin.flush
      end

      # Read next NDJSON object from executor stdout.
      # Returns a parsed Hash, or nil on EOF.
      def recv
        remaining = @deadline - Time.now
        raise Timeout::Error if remaining <= 0

        ready = IO.select([@stdout], nil, nil, remaining)
        raise Timeout::Error unless ready

        line = @stdout.gets
        return nil if line.nil?
        line = line.strip
        return nil if line.empty?
        JSON.parse(line)
      end

      def close
        @stdin.close unless @stdin.closed?
      rescue StandardError
        # ignore
      ensure
        begin
          @stderr_thread.join(0.05)
        rescue StandardError
          # ignore
        end
      end

      def wait
        @wait_thr.value
      end

      def stderr_text
        @stderr_buf.to_s
      end
    end

    # Non-streaming (backwards compatible): returns a Hash parsed from stdout JSON.
    def run(request)
      cmd = docker_cmd

      stdout = ""
      stderr = ""
      status = nil

      begin
        Timeout.timeout(@timeout) do
          stdout, stderr, status = Open3.capture3(*cmd, stdin_data: JSON.generate(request))
        end
      rescue Timeout::Error
        return timeout_error
      end

      unless status&.success?
        return executor_failed_error(stderr)
      end

      begin
        JSON.parse(stdout)
      rescue JSON::ParserError
        invalid_output_error
      end
    end

    # Streaming: expects NDJSON (one JSON object per line) on stdout.
    #
    # Yields each parsed object to the provided block.
    # Returns the last parsed object (commonly {"event":"final", ...}) if present.
    #
    # If the executor still returns a single JSON blob (legacy), this will yield it once.
    def run_stream(request)
      cmd = docker_cmd
      stderr_buf = +""
      last_obj = nil
      stdout_buf = +""

      begin
        Timeout.timeout(@timeout) do
          Open3.popen3(*cmd) do |stdin, stdout, stderr, wait_thr|
            stdin.write(JSON.generate(request))
            stdin.close

            stderr_thread = Thread.new do
              begin
                stderr.each_line { |line| stderr_buf << line }
              rescue StandardError
                # ignore
              end
            end

            stdout.each_line do |line|
              stdout_buf << line
              line = line.strip
              next if line.empty?

              begin
                obj = JSON.parse(line)
                last_obj = obj
                yield obj if block_given?
              rescue JSON::ParserError
                # Not an NDJSON line; keep buffering and try parse whole output later.
              end
            end

            stderr_thread.join

            status = wait_thr.value
            unless status.success?
              msg = stderr_buf.strip
              msg = stdout_buf.strip if msg.empty?
              yield executor_failed_error(msg)["error"] if block_given?
              return executor_failed_error(msg)
            end
          end
        end
      rescue Timeout::Error
        yield timeout_error["error"] if block_given?
        return timeout_error
      end

      return last_obj if last_obj

      # Legacy single JSON blob fallback
      begin
        obj = JSON.parse(stdout_buf)
        yield obj if block_given?
        obj
      rescue JSON::ParserError
        invalid_output_error
      end
    end

    # Start an interactive executor session.
    # Returns a Session instance or an error hash like {"error"=>{...}}.
    def open_session
      cmd = docker_cmd

      begin
        Timeout.timeout(@timeout) do
          stdin, stdout, stderr, wait_thr = Open3.popen3(*cmd)
          return Session.new(stdin: stdin, stdout: stdout, stderr: stderr, wait_thr: wait_thr, timeout: @timeout)
        end
      rescue Timeout::Error
        return timeout_error
      end
    end

    private

    def docker_cmd
      [
        "docker", "run", "--rm", "-i",
        "--network=none",
        "--memory=256m",
        "--cpus=1",
        "--pids-limit=128",
        "--read-only",
        "--tmpfs", "/workspace:rw,size=16m,mode=1777",
        @image
      ]
    end

    def timeout_error
      {
        "error" => {
          "type" => "Timeout",
          "message" => "Execution exceeded #{@timeout} seconds",
          "location" => nil
        }
      }
    end

    def executor_failed_error(stderr)
      {
        "error" => {
          "type" => "ExecutorFailed",
          "message" => (stderr || "").strip,
          "location" => nil
        }
      }
    end

    def invalid_output_error
      {
        "error" => {
          "type" => "InvalidExecutorOutput",
          "message" => "Executor did not return valid JSON/NDJSON",
          "location" => nil
        }
      }
    end
  end
end