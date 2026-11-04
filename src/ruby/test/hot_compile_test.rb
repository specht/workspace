require 'minitest/autorun'
require 'open3'
require 'tmpdir'
require_relative '../include/hot_compile'

# The compile service (HotCompile): the shell of a compile step and the
# files of a run here; the services themselves in real containers, skipped
# where there is no Docker image (src/scripts/compare-hot-compile.rb
# compares every Java and Dart card hot and cold).
class HotCompileTest < Minitest::Test
    FAILED = "echo failed; cat errors\n"

    def step_output(files)
        Dir.mktmpdir do |dir|
            files.each { |name, text| File.write(File.join(dir, name), text) }
            script = HotCompile.step('javac', "sh -c 'echo cold > errors; exit 1'", FAILED)
            out, = Open3.capture2('sh', '-c', script, :chdir => dir)
            out
        end
    end

    def test_the_step_is_skipped_when_the_service_compiled
        assert_equal '', step_output('hot_javac.ok' => '')
    end

    def test_the_step_shows_the_errors_the_service_found
        assert_equal "failed\nfrom the service\n", step_output('hot_javac.err' => "from the service\n")
    end

    def test_the_step_compiles_as_before_without_the_service
        assert_equal "failed\ncold\n", step_output({})
    end

    def test_files_stay_as_they_are_when_the_service_is_off
        HotCompile.enabled = false
        program = { :files => { 'HsKata.java' => 'class HsKata {}' }, :hot => { :language => 'java', :entries => ['javac'] } }
        assert_same program[:files], HotCompile.files(program)
    end

    class FakeServer
        def compile(files, entries)
            { :entries => { 'run.dart' => [true, ''], 'other.dart' => [false, "kata.dart:1:1: Error: x\n"] },
              :outputs => { 'run.dill' => "\x00\x01binary".b } }
        end
    end

    def test_files_get_what_the_service_made
        HotCompile.enabled = true
        HotCompile.instance_variable_get(:@servers)['dart'] = FakeServer.new
        files = HotCompile.files(:files => { 'run.dart' => 'void main() {}' }, :hot => { :language => 'dart', :entries => %w[run.dart other.dart] })
        assert_equal '', files['hot_run.ok']
        assert_equal "kata.dart:1:1: Error: x\n", files['hot_other.err']
        assert_equal "\x00\x01binary".b, Base64.strict_decode64(files['run.dill']['base64'])
        assert_equal 'void main() {}', files['run.dart']
    ensure
        HotCompile.enabled = false
        HotCompile.instance_variable_get(:@servers).delete('dart')
    end

    # ----- in containers -----

    def docker?
        system('docker', 'image', 'inspect', KataRunner::IMAGE, :out => File::NULL, :err => File::NULL)
    end

    def test_the_runner_writes_bytes_and_one_folder
        skip "no Docker image #{KataRunner::IMAGE}" unless docker?
        files = { 'out/a.bin' => { 'base64' => Base64.strict_encode64("\x00\xff".b) }, 'b.txt' => 'text' }
        result = KataRunner.run(:files => files, :argv => ['sh', '-c', 'od -An -tx1 out/a.bin; cat b.txt'], :timeout => 20)
        assert_equal '00 ff text', result.stdout.split.join(' ')
        bad = KataRunner.run(:files => { 'a/b/c' => 'x' }, :argv => ['true'], :timeout => 20)
        refute bad.ok?
    end

    def test_java_compiles_hot_with_the_errors_of_javac
        skip "no Docker image #{KataRunner::IMAGE}" unless docker?
        server = HotCompile::Server.new('java')
        server.start
        good = server.compile({ 'Hello.java' => "public class Hello {\n    public static void main(String[] a) { System.out.println(6 * 7); }\n}\n" }, ['javac'])
        assert_equal [true, ''], good[:entries]['javac']
        assert_includes good[:outputs].keys, 'out/Hello.class'
        run = KataRunner.run(:files => { 'out/Hello.class' => { 'base64' => Base64.strict_encode64(good[:outputs]['out/Hello.class']) } },
                             :argv => ['java', '-cp', 'out', 'Hello'], :timeout => 20)
        assert_equal "42\n", run.stdout
        bad = server.compile({ 'Hello.java' => "public class Hello {\n    int f() { return nope; }\n}\n" }, ['javac'])
        ok, errors = bad[:entries]['javac']
        refute ok
        assert_match(/\AHello\.java:2: error: cannot find symbol/, errors)
        assert_empty bad[:outputs]
    ensure
        server&.stop
    end

    def test_dart_compiles_hot_also_after_errors_and_with_new_code_in_the_same_file
        skip "no Docker image #{KataRunner::IMAGE}" unless docker?
        server = HotCompile::Server.new('dart')
        server.start
        main = "import 'kata.dart';\nvoid main() { print(f(21)); }\n"
        results = ['int f(int x) => 0;', "int f(int x) => 0;\nthis is not dart (", 'int f(int x) => x * 2;'].map do |kata|
            server.compile({ 'run.dart' => main, 'kata.dart' => "#{kata}\n" }, ['run.dart'])
        end
        refute results[1][:entries]['run.dart'][0]
        assert_match(/\Akata\.dart:2:\d+: Error: /, results[1][:entries]['run.dart'][1])
        outputs = [results[0], results[2]].map do |r|
            assert r[:entries]['run.dart'][0]
            dill = { 'base64' => Base64.strict_encode64(r[:outputs]['run.dill']) }
            KataRunner.run(:files => { 'run.dill' => dill }, :argv => ['/opt/dart-sdk/bin/dart', 'run.dill'], :timeout => 20).stdout
        end
        assert_equal ["0\n", "42\n"], outputs
    ensure
        server&.stop
    end
end
