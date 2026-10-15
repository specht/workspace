require 'json'
require 'open3'

# The TCP-Konsole (/tcp-console): a TCP connection, optionally with TLS, that
# students type into line by line, like nc -C, and that shows what comes back.
#
# The connection does not start from the web server but from the student's
# own Workspace: a small relay (RELAY) runs there with docker exec, as the
# guest user. It reaches exactly what nc in the Workspace reaches (no
# internal services of the site, nothing in an exam without a network),
# and localhost is the student's own Workspace, where their web server runs.
#
# The relay writes what the server sends to stdout and one JSON line per
# event to stderr: connected, error (could not connect), closed (by the
# server, after IDLE_SECONDS without traffic, after MAX_BYTES). The whole
# connection lasts at most MAX_SECONDS.
#
# The relay reads at most BYTES_PER_SECOND from the server, in small pieces,
# so an answer arrives bit by bit, as over a slow line, and the times in the
# console are true. The server cannot send faster than that: its data waits
# in the buffers (TCP flow control). It also caps what one connection costs.
module TcpConsole
    MAX_SECONDS = 600
    IDLE_SECONDS = 120
    MAX_BYTES = 512 * 1024
    BYTES_PER_SECOND = 1024
    MAX_LINE = 4096

    HOST = /\A(?=.{1,253}\z)[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?(?:\.[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?)*\.?\z/
    IPV6 = /\A[0-9A-Fa-f:.]{2,45}\z/

    RELAY = <<~PY
        import json, os, select, socket, ssl, sys, time
        host, port, use_tls = sys.argv[1], int(sys.argv[2]), sys.argv[3] == '1'
        idle, max_bytes, rate = float(sys.argv[4]), int(sys.argv[5]), float(sys.argv[6])
        piece = max(16, int(rate / 16))

        def status(**fields):
            sys.stderr.write(json.dumps(fields) + '\\n')
            sys.stderr.flush()

        def kind(error):
            if isinstance(error, socket.gaierror): return 'dns'
            if isinstance(error, ssl.SSLCertVerificationError): return 'certificate'
            if isinstance(error, ssl.SSLError): return 'tls'
            if isinstance(error, ConnectionRefusedError): return 'refused'
            if isinstance(error, (socket.timeout, TimeoutError)): return 'timeout'
            return 'other'

        try:
            sock = socket.create_connection((host, port), timeout=10)
            if use_tls:
                sock = ssl.create_default_context().wrap_socket(sock, server_hostname=host)
            sock.settimeout(None)
        except Exception as error:
            status(error=kind(error), detail=str(error)[:200])
            sys.exit(0)
        status(connected=sock.getpeername()[0], tls=sock.version() if use_tls else None)

        received = 0
        last = time.monotonic()
        next_read = last
        while True:
            now = time.monotonic()
            left = idle - (now - last)
            if left <= 0:
                status(closed='idle')
                break
            # Read from the server only when the rate allows it; TLS may hold
            # decrypted bytes that select does not see.
            may_read = now >= next_read
            buffered = may_read and use_tls and sock.pending() > 0
            wait = 0 if buffered else (left if may_read else min(left, next_read - now))
            ready, _, _ = select.select([sock, 0] if may_read else [0], [], [], wait)
            if may_read and (buffered or sock in ready):
                try:
                    data = sock.recv(piece)
                except ssl.SSLWantReadError:
                    continue
                except OSError as error:
                    status(closed='error', detail=str(error)[:200])
                    break
                if not data:
                    status(closed='server')
                    break
                os.write(1, data)
                received += len(data)
                last = time.monotonic()
                next_read = max(next_read, now) + len(data) / rate
                if received >= max_bytes:
                    status(closed='limit')
                    break
            if 0 in ready:
                data = os.read(0, 65536)
                if not data:
                    break
                sock.sendall(data)
                last = time.monotonic()
        sock.close()
    PY

    def self.valid_host?(host)
        host.is_a?(String) && (host.match?(HOST) || (host.count(':') >= 2 && host.match?(IPV6)))
    end

    # The port as a number, or nil.
    def self.port(value)
        number = Integer(value.to_s, 10) rescue nil
        number && number.between?(1, 65535) ? number : nil
    end

    # docker exec for the relay in the Workspace hs_code_<fs_tag>.
    def self.command(fs_tag, host, port, tls, uid:)
        ['docker', 'exec', '-i', '-u', uid, '-e', 'HOME=/tmp', '-w', '/tmp', "hs_code_#{fs_tag}",
         'timeout', '-k', '2', MAX_SECONDS.to_s,
         'python3', '-I', '-c', RELAY, host, port.to_s, tls ? '1' : '0',
         IDLE_SECONDS.to_s, MAX_BYTES.to_s, BYTES_PER_SECOND.to_s]
    end

    # Bytes from the server as UTF-8 text. A character split between two
    # reads is kept back for the next one; what is not UTF-8 becomes �.
    class Decoder
        def initialize
            @rest = ''.b
        end

        def call(bytes)
            data = @rest + bytes.b
            cut = data.bytesize
            # At most three bytes of an unfinished character at the end.
            (1..[3, data.bytesize].min).each do |back|
                byte = data.getbyte(data.bytesize - back)
                next if byte & 0xC0 == 0x80

                needed = byte >= 0xF0 ? 4 : byte >= 0xE0 ? 3 : byte >= 0xC0 ? 2 : 1
                cut = data.bytesize - back if needed > back
                break
            end
            @rest = data.byteslice(cut..)
            data.byteslice(0, cut).force_encoding(Encoding::UTF_8).scrub("�")
        end
    end

    # One running relay. on_event gets hashes: { data: text }, the relay's
    # events ({ connected:, tls: }, { error:, detail: }, { closed: }), and
    # { ended: true } once at the end.
    class Session
        def initialize(argv, &on_event)
            @on_event = on_event
            @stdin, @stdout, @stderr, @wait = Open3.popen3(*argv)
            @stdin.sync = true
            readers = [Thread.new { pump_stdout }, Thread.new { pump_stderr }]
            Thread.new do
                readers.each(&:join)
                @wait.value
                @on_event.call(:ended => true)
            end
        end

        # Sends a line with CRLF at the end, like nc -C.
        def send_line(line)
            @stdin.write(line.to_s.delete("\r\n")[0, MAX_LINE] + "\r\n")
        rescue IOError, SystemCallError
            nil
        end

        def close
            @stdin.close unless @stdin.closed?
            Thread.new do
                sleep 3
                Process.kill('TERM', @wait.pid) if @wait.alive?
            rescue SystemCallError
                nil
            end
        rescue IOError
            nil
        end

        private

        def pump_stdout
            decoder = Decoder.new
            loop do
                text = decoder.call(@stdout.readpartial(16384))
                @on_event.call(:data => text) unless text.empty?
            end
        rescue EOFError, IOError
            nil
        end

        def pump_stderr
            @stderr.each_line do |line|
                event = JSON.parse(line) rescue nil
                @on_event.call(event.transform_keys(&:to_sym)) if event.is_a?(Hash)
            end
        rescue IOError
            nil
        end
    end
end
