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

        def run(request)
            cmd = [
                "docker", "run", "--rm", "-i",
                "--network=none",
                "--memory=256m",
                "--cpus=1",
                "--pids-limit=128",
                "--read-only",
                "--tmpfs", "/workspace:rw,size=16m,mode=1777",
                @image
            ]

            stdout = ""
            stderr = ""
            status = nil

            begin
                Timeout.timeout(@timeout) do
                    stdout, stderr, status = Open3.capture3(
                        *cmd,
                        stdin_data: JSON.generate(request)
                    )
                end
            rescue Timeout::Error
                return {
                    "error" => {
                        "type" => "Timeout",
                        "message" => "Execution exceeded #{@timeout} seconds",
                        "location" => nil
                    }
                }
            end

            unless status.success?
                return {
                    "error" => {
                        "type" => "ExecutorFailed",
                        "message" => stderr.strip,
                        "location" => nil
                    }
                }
            end

            begin
                JSON.parse(stdout)
            rescue JSON::ParserError
                {
                    "error" => {
                        "type" => "InvalidExecutorOutput",
                        "message" => "Executor did not return valid JSON",
                        "location" => nil
                    }
                }
            end
        end
    end
end