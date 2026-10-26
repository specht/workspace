require 'json'
require 'open3'
require 'securerandom'

# One run of a Kata (docs/plan-katas.md) in a container of its own, never in
# a Workspace and never in this container: from the Workspace image, so Ruby,
# Python and Node are those of the Workspace, but without code-server, without
# network, with a read-only image and a small tmpfs as /tmp, as nobody, without
# capabilities and with limits on CPU, memory and processes. Named
# hs_code_kata_*, such a container is one the Docker guard already lets
# through (docker_policy.rb), like the offline reader of workspace_runtime.rb.
#
# The code goes in on stdin, never as a file on the host: RUNNER reads the
# first line ({files, argv, limit}), writes the files into /tmp/kata and
# starts the program there; after that line comes what the program reads. A
# run ends when the program ends, after timeout seconds or when it writes
# more than MAX_OUTPUT bytes; then the container is killed, and docker run
# --rm removes it. The alarm in RUNNER (limit) ends the program even if this
# process is gone; --init keeps the program from being process 1, which
# would ignore it.
module KataRunner
    IMAGE = 'hs_code_server'
    NAME_PREFIX = 'hs_code_kata_'
    CPUS = '0.5'
    MEMORY = '192m'
    PIDS = 64
    TMPFS = '16m'
    MAX_OUTPUT = 256 * 1024
    USER = '65534:65534'
    # seconds after which the program ends itself (RUNNER's alarm)
    HARD_LIMIT = 60

    # Started as python3 -I -c RUNNER: the first line on stdin is the setup,
    # read byte by byte, so nothing after it is read away from the program.
    # The files go into /tmp/kata, the program replaces this script there,
    # and the alarm stays set across that exec.
    RUNNER = <<~'PY'
        import json
        import os
        import signal
        import sys

        MAX_SETUP = 1_000_000

        line = bytearray()
        while True:
            byte = os.read(0, 1)
            if not byte or byte == b'\n':
                break
            line += byte
            if len(line) > MAX_SETUP:
                sys.exit('kata runner: setup line too long')

        setup = json.loads(line)
        os.makedirs('/tmp/kata', exist_ok=True)
        for name, content in setup.get('files', {}).items():
            if '/' in name or name.startswith('.') or not name:
                sys.exit(f'kata runner: bad file name {name!r}')
            with open(os.path.join('/tmp/kata', name), 'w', encoding='utf-8') as file:
                file.write(content)
        os.chdir('/tmp/kata')
        signal.alarm(int(setup.get('limit', 60)))
        argv = setup['argv']
        os.execvp(argv[0], argv)
    PY

    # status: the program's exit status; timed_out, too_long: stopped for
    # that; cancelled: stopped from outside (a newer run of the same person).
    Result = Struct.new(:stdout, :stderr, :status, :timed_out, :too_long, :cancelled, keyword_init: true) do
        def ok?
            !timed_out && !too_long && !cancelled && status == 0
        end
    end

    def self.container_name
        "#{NAME_PREFIX}#{SecureRandom.hex(8)}"
    end

    # The docker command for one run; every option is one the guard checks.
    def self.docker_args(name, image: IMAGE)
        ['docker', 'run', '--rm', '-i', '--init', '--name', name,
         '--network', 'none', '--read-only',
         '--tmpfs', "/tmp:rw,nosuid,nodev,size=#{TMPFS},mode=1777",
         '--user', USER, '--cap-drop', 'ALL', '--security-opt', 'no-new-privileges',
         '--cpus', CPUS, '--memory', MEMORY, '--memory-swap', MEMORY, '--pids-limit', PIDS.to_s,
         '--log-driver', 'none', '-e', 'HOME=/tmp', '-e', 'LANG=C.UTF-8', '-e', 'PYTHONDONTWRITEBYTECODE=1',
         '--entrypoint', 'python3', image, '-I', '-c', RUNNER]
    end

    # Runs files (name => text) with argv in a new container. input: what
    # the program reads after the setup line. cancel: a lambda that, given
    # a lambda, keeps it to stop this run from outside (KataQueue).
    # on_output(stream, text) gets the output as it comes.
    def self.run(files:, argv:, input: '', timeout: 10, image: IMAGE, cancel: nil, on_output: nil)
        name = container_name
        stdout = +''
        stderr = +''
        timed_out = false
        too_long = false
        stopped = false
        # in the background: while the docker client still writes into a pipe
        # nobody reads, docker kill would wait for it
        kill = lambda do
            stopped = true
            Thread.new { system('docker', 'kill', name, :out => File::NULL, :err => File::NULL) }
        end
        cancel&.call(kill)
        status = nil
        Open3.popen3(*docker_args(name, image: image)) do |stdin, out, err, wait|
            begin
                stdin.write(JSON.generate({ 'files' => files, 'argv' => argv, 'limit' => [timeout + 5, HARD_LIMIT].min }) + "\n")
                stdin.write(input.to_s)
            rescue Errno::EPIPE
                # the container ended (or never started) before it read everything
            end
            stdin.close
            deadline = Time.now + timeout
            streams = { out => [:stdout, stdout], err => [:stderr, stderr] }
            until streams.empty?
                left = deadline - Time.now
                if left <= 0
                    timed_out = true
                    kill.call
                    break
                end
                ready, = IO.select(streams.keys, nil, nil, left)
                next unless ready

                ready.each do |io|
                    chunk = io.read_nonblock(16 * 1024, exception: false)
                    next if chunk == :wait_readable

                    if chunk.nil?
                        streams.delete(io)
                        next
                    end
                    kind, buffer = streams[io]
                    text = chunk.force_encoding('UTF-8').scrub
                    buffer << text
                    on_output&.call(kind, text)
                    if stdout.bytesize + stderr.bytesize > MAX_OUTPUT
                        too_long = true
                        kill.call
                        streams.clear
                        break
                    end
                end
            end
            # After a stop nothing more is read: closed pipes let the docker
            # client end even if it was writing into a full one.
            [out, err].each { |io| io.close unless io.closed? } if stopped
            unless wait.join(5)
                Process.kill('KILL', wait.pid) rescue nil
            end
            status = wait.value.exitstatus
        end
        Result.new(:stdout => stdout, :stderr => stderr, :status => status, :timed_out => timed_out,
                   :too_long => too_long, :cancelled => stopped && !timed_out && !too_long)
    end
end
