require 'base64'
require 'json'
require 'open3'
require 'securerandom'
require_relative 'kata_runner'

# Java and Dart compiled by a compiler that stays loaded (docs/katas.md,
# »Java and Dart compiled hot«). Started cold, javac takes about 1.1 s in a Kata
# container and Dart about 0.8 s, while the program itself then runs in a
# tenth of a second; a compiler that stays loaded takes 0.1 to 0.3 s. So
# one container per language stays (hs_code_compile_*, as isolated as a
# Kata run: no network, read-only, nobody, limits) and only compiles; the
# program still runs in a container of its own (KataRunner), never in the
# service, since Java 25 has no sandbox for code in a shared JVM.
#
# The scripts of KataCompiled, TestsCompiled and PipCompiled stay as they
# were and the one place that decides: their compile step (HotCompile.step)
# is skipped when the files of the run already hold what the service made
# (hot_TAG.ok and the classes or .dill files), or takes the service's
# errors (hot_TAG.err); otherwise the compiler runs as before. Output and
# errors are those of the compiler run cold: the service compiles in
# /tmp/kata as the run does, with the same names and options.
#
# A service that is not ready yet is started in the background and the
# run compiles cold meanwhile, so no run waits for a service; one that
# fails or takes longer than REQUEST_SECONDS is killed and started again
# at the next run. Off in tests and where credentials.rb sets
# HOT_COMPILE = false (Main enables it).
#
# Between this process and a service, lines:
#
#   REQUEST <files> <entries>         RESPONSE <entries> <outputs>
#   <name> <base64>   (per file)       ENTRY <entry> <1|0> <base64 errors>
#   <entry>           (per entry)      <name> <base64>   (per output)
#
# Java has one entry, javac (every .java file together, classes under
# out/); Dart one entry per program to compile (run.dart -> run.dill).
module HotCompile
    LANGUAGES = %w[java dart].freeze
    NAME_PREFIX = 'hs_code_compile_'
    REQUEST_SECONDS = 20
    START_SECONDS = 120
    LIMITS = {
        'java' => { :cpus => '1', :memory => '512m', :tmpfs => '64m' },
        'dart' => { :cpus => '1', :memory => '512m', :tmpfs => '64m' },
    }.freeze

    JAVA_SERVER = <<~'JAVA'
        import javax.tools.*;
        import java.io.*;
        import java.nio.charset.StandardCharsets;
        import java.nio.file.*;
        import java.util.*;
        import java.util.stream.*;

        // HotCompile's Java service: javac through javax.tools, which prints
        // what the command prints, in /tmp/kata (the working directory).
        public class HsCompileServer {
            static final Path WORK = Path.of("").toAbsolutePath();

            static String[] compile(JavaCompiler compiler, List<String> sources) throws IOException {
                Files.createDirectories(WORK.resolve("out"));
                List<String> args = new ArrayList<>(List.of("-nowarn", "-encoding", "UTF-8", "-proc:none", "-d", "out"));
                args.addAll(sources);
                ByteArrayOutputStream err = new ByteArrayOutputStream();
                int status = compiler.run(null, OutputStream.nullOutputStream(), err, args.toArray(new String[0]));
                return new String[] { status == 0 ? "1" : "0", err.toString(StandardCharsets.UTF_8) };
            }

            static void clean() throws IOException {
                try (Stream<Path> paths = Files.walk(WORK)) {
                    for (Path p : paths.sorted(Comparator.reverseOrder()).toList()) {
                        if (!p.equals(WORK)) Files.delete(p);
                    }
                }
            }

            static String b64(byte[] bytes) {
                return Base64.getEncoder().encodeToString(bytes);
            }

            public static void main(String[] args) throws Exception {
                JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
                BufferedReader in = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
                PrintStream out = new PrintStream(new FileOutputStream(FileDescriptor.out), false, StandardCharsets.UTF_8);
                // warm: the first compiles are the slow ones
                for (int i = 0; i < 6; i++) {
                    Files.writeString(WORK.resolve("HsWarm.java"), "import java.util.*;\npublic class HsWarm {\n    static int f(List<Integer> l) { return l.stream().mapToInt(x -> x * " + i + ").sum(); }\n    public static void main(String[] a) { System.out.println(new ArrayList<>(Map.of(\"a\", f(List.of(1, 2))).keySet())); }\n}\n");
                    compile(compiler, List.of("HsWarm.java"));
                    clean();
                }
                out.println("READY");
                out.flush();
                String line;
                while ((line = in.readLine()) != null) {
                    String[] head = line.trim().split(" ");
                    if (head.length != 3 || !head[0].equals("REQUEST")) System.exit(2);
                    int files = Integer.parseInt(head[1]);
                    int entries = Integer.parseInt(head[2]);
                    List<String> sources = new ArrayList<>();
                    for (int i = 0; i < files; i++) {
                        String[] f = in.readLine().split(" ", 2);
                        if (f[0].contains("/") || f[0].startsWith(".")) System.exit(2);
                        Files.write(WORK.resolve(f[0]), Base64.getDecoder().decode(f[1].trim()));
                        if (f[0].endsWith(".java")) sources.add(f[0]);
                    }
                    for (int i = 0; i < entries; i++) in.readLine();
                    String[] result = compile(compiler, sources);
                    List<String> outputs = new ArrayList<>();
                    if (result[0].equals("1")) {
                        Path dir = WORK.resolve("out");
                        try (Stream<Path> paths = Files.walk(dir)) {
                            for (Path p : paths.filter(Files::isRegularFile).sorted().toList()) {
                                outputs.add("out/" + dir.relativize(p) + " " + b64(Files.readAllBytes(p)));
                            }
                        }
                    }
                    out.println("RESPONSE 1 " + outputs.size());
                    out.println("ENTRY javac " + result[0] + " " + b64(result[1].getBytes(StandardCharsets.UTF_8)));
                    for (String o : outputs) out.println(o);
                    out.flush();
                    clean();
                }
            }
        }
    JAVA

    # Dart's frontend_server in its incremental mode, as Flutter's tools
    # drive it on stdin: »compile FILE« answers »result KEY«, the errors, a
    # line KEY, the files it read (+file …) and »KEY DILL COUNT«. Then accept
    # (or reject after errors, which answers once more), and reset, so that
    # the next answer is a whole program again; the platform stays loaded,
    # which is what takes long. Every compile after the first is »recompile
    # FILE« with every file name a request had, since compile would keep
    # the files it read before (the same kata.dart with new code). Its output is buffered: after errors, the last line
    # comes only with the next command. So the errors decide (a line
    # »…: Error: …«), not COUNT (which also adds up over compiles in the
    # other mode), every compile skips what is left of the one before until
    # its own »result«, and a good compile waits for its last line, after
    # which the .dill is complete.
    DART_DRIVER = <<~'PY'
        import base64
        import os
        import re
        import select
        import shutil
        import subprocess
        import sys
        import time

        SDK = '/opt/dart-sdk'
        WORK = '/tmp/kata'
        OUT = '/tmp/svc/o.dill'
        ERROR = re.compile(r'^(?:\S+?:\d+:\d+: )?Error: ')

        server = subprocess.Popen(
            [SDK + '/bin/dartaotruntime', SDK + '/bin/snapshots/frontend_server_aot.dart.snapshot',
             '--sdk-root=' + SDK + '/', '--platform=' + SDK + '/lib/_internal/vm_platform.dill',
             '--target=vm', '--incremental', '--output-dill=' + OUT],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, cwd=WORK)
        pending = b''

        def send(command):
            server.stdin.write((command + '\n').encode())
            server.stdin.flush()

        # the next line of the compiler, or None after timeout seconds
        def line(timeout):
            global pending
            end = time.time() + timeout
            while b'\n' not in pending:
                left = end - time.time()
                if left <= 0 or not select.select([server.stdout], [], [], left)[0]:
                    return None
                chunk = os.read(server.stdout.fileno(), 65536)
                if not chunk:
                    sys.exit('frontend_server ended')
                pending += chunk
            text, pending = pending.split(b'\n', 1)
            return text.decode('utf-8', 'replace')

        def expect(timeout, done):
            while True:
                text = line(timeout)
                if text is None:
                    sys.exit('frontend_server did not answer')
                if done(text):
                    return text

        # every file a request ever had: recompile names them as changed,
        # since compile after reset would keep files it read before
        seen = set()
        started = [False]

        # [failed, errors as the compiler run cold prints them]
        def compile(path):
            if not started[0]:
                send('compile ' + path)
                started[0] = True
            else:
                send('recompile ' + path + ' HS_FILES')
                for name in sorted(seen):
                    send('file://' + os.path.join(WORK, name))
                send('HS_FILES')
            key = expect(30, lambda t: t.startswith('result '))[len('result '):]
            errors = []
            expect(30, lambda t: t == key or errors.append(t))
            failed = any(ERROR.search(e) for e in errors)
            if failed:
                send('reject')
                again = line(10)
                while again is not None and not again.startswith('result '):
                    again = line(10)
                if again is not None:
                    other = again[len('result '):]
                    while again is not None and again != other:
                        again = line(10)
            else:
                send('accept')
                expect(30, lambda t: t.startswith(key + ' '))
            send('reset')
            text = '\n'.join(e.replace('file://' + WORK + '/', '').replace(WORK + '/', '') for e in errors)
            return failed, text + ('\n' if errors else '')

        def clean():
            for name in os.listdir(WORK):
                path = os.path.join(WORK, name)
                shutil.rmtree(path) if os.path.isdir(path) else os.remove(path)

        def b64(data):
            return base64.b64encode(data).decode('ascii')

        # warm: the first compiles are the slow ones, also those with errors
        for i in range(4):
            seen.add('hs_warm.dart')
            with open(os.path.join(WORK, 'hs_warm.dart'), 'w') as f:
                f.write(('int sq(int x) => x * x;\nvoid main() { print([for (var i = 0; i < 5; i++) sq(i) + %d].map((x) => "$x").join(", ")); }\n' % i)
                        if i != 1 else 'void main() { print(nope); }\n')
            compile(os.path.join(WORK, 'hs_warm.dart'))
            clean()
        print('READY', flush=True)

        for request in sys.stdin:
            head = request.split()
            if len(head) != 3 or head[0] != 'REQUEST':
                sys.exit('bad request')
            for _ in range(int(head[1])):
                name, data = sys.stdin.readline().split(' ', 1)
                if '/' in name or name.startswith('.'):
                    sys.exit('bad file name')
                seen.add(name)
                with open(os.path.join(WORK, name), 'wb') as f:
                    f.write(base64.b64decode(data))
            entries = [sys.stdin.readline().strip() for _ in range(int(head[2]))]
            answers = []
            outputs = []
            for entry in entries:
                if os.path.exists(OUT):
                    os.remove(OUT)
                failed, errors = compile(os.path.join(WORK, entry))
                ok = not failed and os.path.exists(OUT)
                answers.append('ENTRY %s %d %s' % (entry, 1 if ok else 0, b64(errors.encode('utf-8'))))
                if ok:
                    with open(OUT, 'rb') as f:
                        outputs.append('%s %s' % (entry[:-len('.dart')] + '.dill', b64(f.read())))
            print('RESPONSE %d %d' % (len(answers), len(outputs)))
            for text in answers + outputs:
                print(text)
            sys.stdout.flush()
            clean()
    PY

    # [files, argv] that start the service in a Kata container: the files go
    # to /tmp/kata, which becomes the service's working directory, so its
    # own files move to /tmp/svc first.
    def self.service(language)
        if language == 'java'
            [{ 'HsCompileServer.java' => JAVA_SERVER },
             ['sh', '-c', 'mkdir -p /tmp/svc && mv HsCompileServer.java /tmp/svc/ && ' \
                          'exec java -XX:+UseSerialGC -Xmx256m /tmp/svc/HsCompileServer.java']]
        else
            [{ 'hs_dart_driver.py' => DART_DRIVER },
             ['sh', '-c', 'mkdir -p /tmp/svc && mv hs_dart_driver.py /tmp/svc/ && exec python3 -I /tmp/svc/hs_dart_driver.py']]
        end
    end

    # The shell of a compile step: skipped when the service compiled
    # already, the service's errors when it found some, else the compiler
    # as before. command writes its errors to the file errors; failed is
    # what the script does then.
    def self.step(tag, command, failed)
        "if [ -e hot_#{tag}.ok ]; then\n:\n" \
            "elif [ -e hot_#{tag}.err ]; then\ncp hot_#{tag}.err errors\n#{failed.chomp}\n" \
            "elif ! #{command}; then\n#{failed.chomp}\nfi\n"
    end

    # the tag of an entry: javac, or the file without .dart
    def self.tag(entry)
        entry.sub(/\.dart\z/, '')
    end

    class << self
        attr_writer :enabled

        def enabled?
            @enabled == true
        end
    end

    @servers = {}
    @servers_mutex = Mutex.new

    def self.server(language)
        @servers_mutex.synchronize { @servers[language] ||= Server.new(language) }
    end

    # The files of a program (KataCompiled, TestsCompiled, PipCompiled:
    # { files:, hot: { language:, entries: } }) with what the service made,
    # or as they are when the service is off, not ready or failed.
    def self.files(program)
        hot = program[:hot]
        files = program[:files]
        return files unless enabled? && hot && LANGUAGES.include?(hot[:language])

        extension = hot[:language] == 'java' ? '.java' : '.dart'
        sources = files.select { |name, _| name.end_with?(extension) }
        answer = server(hot[:language]).compile(sources, hot[:entries])
        return files unless answer

        extra = {}
        answer[:entries].each do |entry, (ok, errors)|
            ok ? extra["hot_#{tag(entry)}.ok"] = '' : extra["hot_#{tag(entry)}.err"] = errors
        end
        answer[:outputs].each { |name, bytes| extra[name] = { 'base64' => Base64.strict_encode64(bytes) } }
        files.merge(extra)
    end

    # Stops every service (at exit; a service also ends by itself when this
    # process is gone, since its stdin closes).
    def self.stop_all
        @servers_mutex.synchronize { @servers.each_value(&:stop) }
    end

    # One service: :stopped, :starting or :ready.
    class Server
        attr_reader :state, :language

        def initialize(language)
            @language = language
            @state = :stopped
            @mutex = Mutex.new
            @buffer = +''
        end

        # { entries: { entry => [ok, errors] }, outputs: { name => bytes } },
        # or nil when the service is not ready (then it starts) or fails.
        def compile(files, entries)
            unless @state == :ready
                start_in_background
                return nil
            end
            @mutex.synchronize do
                return nil unless @state == :ready

                request(files, entries)
            end
        rescue StandardError => e
            STDERR.puts "HotCompile #{@language}: #{e.class}: #{e.message}"
            stop
            nil
        end

        def start_in_background
            @mutex.synchronize do
                return unless @state == :stopped

                @state = :starting
            end
            Thread.new do
                start
            rescue StandardError => e
                STDERR.puts "HotCompile #{@language}: not started: #{e.class}: #{e.message}"
                stop
            end
        end

        def start
            @name = "#{NAME_PREFIX}#{@language}_#{SecureRandom.hex(4)}"
            @stdin, @stdout, @stderr, @wait = Open3.popen3(*KataRunner.docker_args(@name, **LIMITS.fetch(@language)))
            Thread.new do
                @stderr.each_line { |l| STDERR.puts "HotCompile #{@language}: #{l.chomp}" }
            rescue IOError
                nil
            end
            files, argv = HotCompile.service(@language)
            @stdin.write(JSON.generate({ 'files' => files, 'argv' => argv, 'limit' => 0 }) + "\n")
            @stdin.flush
            @buffer = +''
            started = Time.now
            line = read_line(started + START_SECONDS)
            raise "unexpected first line #{line.inspect}" unless line == 'READY'

            STDERR.puts "HotCompile #{@language}: ready after #{(Time.now - started).round(1)} s (#{@name})"
            @state = :ready
        end

        def stop
            name = @name
            @state = :stopped
            [@stdin, @stdout, @stderr].each { |io| io&.close rescue nil }
            @stdin = @stdout = @stderr = nil
            system('docker', 'kill', name, :out => File::NULL, :err => File::NULL) if name
            @name = nil
        end

        private

        def request(files, entries)
            deadline = Time.now + REQUEST_SECONDS
            text = +"REQUEST #{files.size} #{entries.size}\n"
            files.each { |name, content| text << "#{name} #{Base64.strict_encode64(content.to_s)}\n" }
            entries.each { |e| text << "#{e}\n" }
            @stdin.write(text)
            @stdin.flush
            head = read_line(deadline).split(' ')
            raise "unexpected answer #{head.inspect}" unless head.size == 3 && head[0] == 'RESPONSE'

            answers = {}
            head[1].to_i.times do
                _, entry, ok, errors = read_line(deadline).split(' ', 4)
                answers[entry] = [ok == '1', Base64.decode64(errors.to_s).force_encoding('UTF-8')]
            end
            outputs = {}
            head[2].to_i.times do
                name, data = read_line(deadline).split(' ', 2)
                outputs[name] = Base64.decode64(data.to_s)
            end
            { :entries => answers, :outputs => outputs }
        end

        def read_line(deadline)
            loop do
                if (at = @buffer.index("\n"))
                    return @buffer.slice!(0..at).chomp
                end
                left = deadline - Time.now
                raise "no answer within the time limit" if left <= 0

                ready, = IO.select([@stdout], nil, nil, left)
                next unless ready

                chunk = @stdout.read_nonblock(256 * 1024, exception: false)
                next if chunk == :wait_readable
                raise 'the service ended' if chunk.nil?

                @buffer << chunk
            end
        end
    end
end
