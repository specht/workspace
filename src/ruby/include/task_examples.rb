require 'fileutils'
require 'open3'
require 'timeout'
require 'tmpdir'
require 'yaml'
require_relative 'code_languages'
require_relative 'task_check'
require_relative 'task_spec'
require_relative 'workspace_files'
require_relative 'kata_runner'

# Examples for the checks of an activity (docs/aufgaben.md, »Beispiele für
# die Prüfungen«): what each check must accept and what it must reject.
# They live in task-examples.yaml next to tasks.yaml:
#
#   format: 1
#   activity: pixelwand
#   examples:
#     lock:
#       pass:
#         - files: { pixelwand/server.rb: stufen/server5.rb }
#       fail:
#         - files: { pixelwand/server.rb: stufen/server4.rb }
#           say: Mutex.new
#       python:
#         pass: …
#         fail: …
#
# A file is copied from the activity's folder, or given as { text: … };
# a folder (textadventure: loesung/java) puts every file in it below the
# target, for a Java project with one file per class;
# { from: …, executable: true } or { text: …, executable: true } also sets
# its executable bit. archive: a .tar.gz, relative to the activity's
# folder, unpacked into the example's /workspace before the files, for a
# case with many files (the folder of »Ein Krimi im Terminal«).
# answer: is the answer a student types (for command: the command, run with
# sh in the example's /workspace); say: a piece of the message a
# rejected example must get. variables: the values of ${…} a check
# compares with ({ workspace.mac: …}), which the server knows per person
# (TaskSpec::VARIABLES). skip: names why a check cannot be tested here,
# for example because it reads what a build produces. Choice and number tasks need no examples: their
# correct answer must pass and every other option (or a number outside the
# tolerance) must fail.
#
# A Kata (function) has no files: answer: is the code, and the task's
# starter must fail by itself (automatic_cases).
#
# Only checks that run without a Workspace container or a database are run
# here (FAST_TYPES); ruby, python and function checks run with the local
# interpreters, c checks with the local cc, command checks with the local sh.
module TaskExamples
    FILE = 'task-examples.yaml'
    FAST_TYPES = %w[files html css references ruby python prolog javascript c java dart command choice number answer function pip tests circuit turtle].freeze
    # Checks that need examples to be tested at all.
    NEEDS_EXAMPLES = %w[files html css references ruby python prolog javascript c java dart command function pip tests circuit turtle].freeze
    # answer with compare: regex needs examples as well (see missing).
    CASE_KEYS = %w[archive files answer say variables].freeze

    Outcome = Struct.new(:activity, :task, :language, :kind, :index, :ok, :detail, keyword_init: true) do
        def label
            "#{activity}/#{task}#{language ? " (#{language})" : ''} #{kind} ##{index + 1}"
        end
    end

    # Runs every example of one activity. Yields nothing; returns outcomes
    # and a list of problems with the examples file itself.
    def self.run(dir)
        activity = File.basename(dir)
        tasks = TaskSpec.load_file(File.join(dir, 'tasks.yaml'))['tasks'].to_h { |t| [t['id'], t] }
        path = File.join(dir, FILE)
        data = File.exist?(path) ? YAML.safe_load(File.read(path)) : { 'examples' => {} }
        problems = []
        problems << "#{FILE}: activity must be #{activity}" if File.exist?(path) && data['activity'] != activity
        examples = data['examples'] || {}
        (examples.keys - tasks.keys).each { |id| problems << "#{FILE}: no task #{id}" }

        outcomes = []
        tasks.each do |id, task|
            ([nil] + checked_languages(task)).each do |language|
                variant = CodeLanguages.variant(task, language)
                type = variant['check']['type']
                next unless FAST_TYPES.include?(type)
                # A Kata in JavaScript runs with the local node; where there is
                # none (the Ruby container), its examples wait for a machine
                # that has it.
                next if %w[function pip tests turtle].include?(type) && variant['check']['language'] == 'javascript' && !node?
                # Java and Dart likewise: their examples wait for a machine with javac or dart
                next if type == 'javascript' && !node?
                next if type == 'java' && !tool?('javac')
                next if type == 'prolog' && !tool?('swipl')
                next if type == 'dart' && !(File.executable?(DartCheck::DART) || tool?('dart'))
                next if %w[function pip tests].include?(type) && variant['check']['language'] == 'java' && !tool?('javac')
                next if %w[function pip tests].include?(type) && variant['check']['language'] == 'dart' && !(File.executable?(DartCheck::DART) || tool?('dart'))

                given = examples[id] || {}
                given = given[language] || {} if language
                cases = automatic_cases(variant)
                if given['skip']
                    problems << "#{FILE}: #{id}: skip needs a reason" unless given['skip'].is_a?(String) && !given['skip'].strip.empty?
                    given = {}
                end
                %w[pass fail].each do |kind|
                    list = given[kind] || []
                    unless list.is_a?(Array)
                        problems << "#{FILE}: #{id}#{language ? ".#{language}" : ''}.#{kind} must be a list"
                        next
                    end
                    list.each { |c| cases << [kind, c] }
                end
                numbers = Hash.new(0)
                cases.each do |kind, example|
                    outcome = run_case(dir, activity, id, language, variant, kind, example, numbers[kind], problems)
                    outcomes << outcome if outcome
                    numbers[kind] += 1
                end
            end
        end
        [outcomes, problems]
    end

    # The languages with a check of their own; a variant that only words the
    # task differently is tested with the task's own examples.
    def self.checked_languages(task)
        CodeLanguages.variant_languages(task).select { |l| task[l].key?('check') }
    end

    # Choice: the correct option passes, every other one fails. Number: the
    # expected value passes, one well outside the tolerance fails. Answer:
    # the first expected answer passes.
    def self.automatic_cases(task)
        check = task['check']
        case check['type']
        when 'choice'
            correct = Array(check['correct'])
            singles = (0...check['options'].size).map { |i| [correct == [i] ? 'pass' : 'fail', { 'answer' => i.to_s }] }
            correct.size > 1 ? [['pass', { 'answer' => correct.join(',') }]] + singles : singles
        when 'number'
            expect = check['expect']
            return [] unless expect.is_a?(Numeric)
            off = (check['tolerance'] || 0).to_f * 2 + expect.abs * 0.5 + 1
            [['pass', { 'answer' => expect.to_s }], ['fail', { 'answer' => (expect + off).to_s }]]
        when 'answer'
            return [] if check['compare'] == 'regex'
            first = Array(check['expect']).first
            first.is_a?(String) && !first.include?('${') ? [['pass', { 'answer' => first }]] : []
        when 'function', 'pip', 'tests', 'turtle'
            task['starter'].to_s.strip.empty? ? [] : [['fail', { 'answer' => task['starter'] }]]
        when 'circuit'
            # the editor's start: the switches and lamps, nothing between them
            [['fail', { 'answer' => CircuitCheck.starter(check) }]]
        else
            []
        end
    end

    def self.run_case(dir, activity, id, language, task, kind, example, index, problems)
        outcome = Outcome.new(:activity => activity, :task => id, :language => language, :kind => kind, :index => index)
        unless example.is_a?(Hash) && (example.keys - CASE_KEYS).empty?
            problems << "#{FILE}: #{id} #{kind} ##{index + 1}: only #{CASE_KEYS.join(', ')}"
            outcome.ok = false
            outcome.detail = 'malformed example'
            return outcome
        end
        Dir.mktmpdir('task-example') do |root|
            if (archive = example['archive'])
                file = File.expand_path(archive.to_s, dir)
                unless File.file?(file) && system('tar', '-xzf', file, '-C', root)
                    problems << "#{FILE}: #{id} #{kind} ##{index + 1}: cannot unpack #{archive}"
                    outcome.ok = false
                    outcome.detail = "cannot unpack #{archive}"
                    return outcome
                end
            end
            (example['files'] || {}).each do |target, source|
                spec = source.is_a?(Hash) ? source : { 'from' => source.to_s }
                # a folder: every file in it, below target (the class
                # files of a Java project)
                folder = !spec.key?('text') && File.directory?(File.join(dir, spec['from'].to_s)) ? File.join(dir, spec['from'].to_s) : nil
                if folder
                    Dir[File.join(folder, '**', '*')].select { |f| File.file?(f) }.each do |f|
                        file = File.join(root, WorkspaceFiles.clean(File.join(target, f.delete_prefix(folder + '/'))))
                        FileUtils.mkdir_p(File.dirname(file))
                        File.binwrite(file, File.binread(f))
                    end
                    next
                end
                unless spec.key?('text') || File.file?(File.join(dir, spec['from'].to_s))
                    problems << "#{FILE}: #{id} #{kind} ##{index + 1}: no file #{spec['from']}"
                    outcome.ok = false
                    outcome.detail = "no file #{spec['from']}"
                    return outcome
                end
                text = spec.key?('text') ? spec['text'].to_s : File.binread(File.join(dir, spec['from'].to_s))
                file = File.join(root, WorkspaceFiles.clean(target))
                FileUtils.mkdir_p(File.dirname(file))
                File.binwrite(file, text)
                File.chmod(0o755, file) if spec['executable']
            end
            # A Python file that imports a module this machine does not have
            # (websockets in the Ruby container, which the Workspace has):
            # like JavaScript without Node, its example waits for a machine
            # that has it.
            checked = task['check']['type'] == 'python' && task['check']['file'] && File.join(root, WorkspaceFiles.clean(task['check']['file']))
            beside = ->(name) { File.exist?(File.join(File.dirname(checked), "#{name}.py")) || File.directory?(File.join(File.dirname(checked), name)) }
            return nil if checked && File.file?(checked) && python_modules(File.read(checked)).any? { |name| !beside.(name) && !python_module?(name) }
            # a turtle program runs here as the browser would run it
            turtle = nil
            if task['check']['type'] == 'turtle' && !example['answer'].to_s.strip.empty?
                runs, error = TurtleCheck.record(task['check']['language'], example['answer'], TurtleCheck.runs(task['check']))
                turtle = { 'runs' => runs, 'error' => error }
            end
            result = TaskCheck.check(task, example['answer'], :workspace => WorkspaceFiles.new(root),
                :runner => local_runner(root), :guest_runner => local_runner(root), :kata => local_kata(root),
                :variables => example['variables'] || {}, :turtle => turtle)
            expected = kind == 'pass' ? %i[correct] : %i[wrong invalid]
            outcome.ok = expected.include?(result.status)
            if outcome.ok && kind == 'fail' && example['say'] && !result.message.to_s.include?(example['say'].to_s)
                outcome.ok = false
                outcome.detail = "rejected, but the message does not say »#{example['say']}«: #{result.message}"
            elsif !outcome.ok
                outcome.detail = "#{result.status}: #{result.message}"
            end
        end
        outcome
    end

    # Every local run goes through timeout(1), as RUN_SCRIPT does in a
    # Workspace: the program ends a little after its time even if the test
    # that started it is gone (a stopped run left a harness computing
    # fast_fibonacci(70) for 32 hours in the Ruby container). The group kill
    # below ends it on time while the test is there.
    def self.bounded(argv, seconds)
        ['timeout', '-k', '2', (seconds.to_i + 2).to_s, *argv]
    end

    # Runs a check's command in the example folder, the way the guest runs
    # it in /workspace.
    def self.local_runner(root)
        lambda do |command, seconds|
            output = +''
            timed_out = false
            Open3.popen2e(*bounded(['sh', '-c', command], seconds), :chdir => root, :pgroup => true) do |_stdin, out, wait|
                begin
                    Timeout.timeout(seconds) { output << out.read }
                rescue Timeout::Error
                    timed_out = true
                    Process.kill('KILL', -wait.pid) rescue nil
                end
                { :output => output, :status => wait.value.exitstatus, :timed_out => timed_out }
            end
        end
    end

    # the top-level modules a Python file imports
    def self.python_modules(source)
        source.scan(/^\s*(?:import|from)\s+([A-Za-z_]\w*)/).flatten.uniq
    end

    def self.python_module?(name)
        @python_modules ||= {}
        return @python_modules[name] if @python_modules.key?(name)
        @python_modules[name] = system('python3', '-c', 'import importlib.util, sys; sys.exit(importlib.util.find_spec(sys.argv[1]) is None)', name,
            :out => File::NULL, :err => File::NULL)
    end

    # whether this machine has a program, for the examples of Java and Dart
    def self.tool?(name)
        @tools ||= {}
        @tools[name] = system('sh', '-c', "command -v #{name}", :out => File::NULL, :err => File::NULL) unless @tools.key?(name)
        @tools[name]
    end

    def self.node?
        @node = system('node', '--version', :out => File::NULL, :err => File::NULL) if @node.nil?
        @node
    end

    # A Kata's run, here with the local interpreters in the example folder
    # (KataRunner does the same in a container of its own).
    def self.local_kata(root)
        lambda do |files:, argv:, timeout:, dialog: nil|
            files.each { |name, text| File.write(File.join(root, name), text) }
            next local_dialog(root, argv, timeout, dialog) if dialog
            stdout = +''
            stderr = +''
            timed_out = false
            status = nil
            Open3.popen3(*bounded(argv, timeout), :chdir => root, :pgroup => true) do |stdin, out, err, wait|
                stdin.close
                reader = Thread.new { stderr << err.read.to_s }
                begin
                    Timeout.timeout(timeout) { stdout << out.read.to_s }
                rescue Timeout::Error
                    timed_out = true
                    Process.kill('KILL', -wait.pid) rescue nil
                end
                reader.join(1)
                status = wait.value.exitstatus
            end
            KataRunner::Result.new(:stdout => stdout, :stderr => stderr, :status => status, :timed_out => timed_out,
                :too_long => false, :cancelled => false)
        end
    end

    # A run that asks the world as it goes (Pip, PipCheck): each line that
    # starts with KataRunner::DIALOG_MARK gets dialog's answer on stdin.
    def self.local_dialog(root, argv, timeout, dialog)
        stdout = +''
        stderr = +''
        timed_out = false
        status = nil
        Open3.popen3(*bounded(argv, timeout), :chdir => root, :pgroup => true) do |stdin, out, err, wait|
            reader = Thread.new { stderr << err.read.to_s }
            begin
                Timeout.timeout(timeout) do
                    while (line = out.gets)
                        if line.start_with?(KataRunner::DIALOG_MARK)
                            stdin.write("#{dialog.call(line.delete_prefix(KataRunner::DIALOG_MARK).chomp)}\n")
                            stdin.flush
                        else
                            stdout << line
                        end
                    end
                end
            rescue Timeout::Error
                timed_out = true
                Process.kill('KILL', -wait.pid) rescue nil
            rescue Errno::EPIPE
                nil
            end
            stdin.close unless stdin.closed?
            reader.join(1)
            status = wait.value.exitstatus
        end
        KataRunner::Result.new(:stdout => stdout, :stderr => stderr, :status => status, :timed_out => timed_out,
            :too_long => false, :cancelled => false)
    end

    # Tasks whose check can only be tested with examples, but has none.
    def self.missing(dir)
        tasks = TaskSpec.load_file(File.join(dir, 'tasks.yaml'))['tasks']
        path = File.join(dir, FILE)
        examples = File.exist?(path) ? (YAML.safe_load(File.read(path))['examples'] || {}) : {}
        tasks.flat_map do |task|
            ([nil] + checked_languages(task)).filter_map do |language|
                variant = CodeLanguages.variant(task, language)
                check = variant['check']
                next unless NEEDS_EXAMPLES.include?(check['type']) || (check['type'] == 'answer' && check['compare'] == 'regex')
                given = examples[task['id']] || {}
                given = given[language] || {} if language
                next if given['skip'] || (given['pass'].is_a?(Array) && !given['pass'].empty?)
                "#{File.basename(dir)}/#{task['id']}#{language ? " (#{language})" : ''}"
            end
        end
    end
end
