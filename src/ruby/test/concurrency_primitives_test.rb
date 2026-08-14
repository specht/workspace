require 'minitest/autorun'
require 'timeout'
require 'tmpdir'
require_relative '../include/atomic_file'
require_relative '../include/serialized_neo4j'

class ConcurrencyPrimitivesTest < Minitest::Test
    class QueryProbe
        attr_reader :entered, :release

        def initialize
            @entered = Queue.new
            @release = Queue.new
        end

        def neo4j_query(value)
            @entered << value
            @release.pop
            value
        end
    end

    class SerializedQueryProbe < QueryProbe
        prepend SerializedNeo4j
    end

    def test_serialized_neo4j_allows_only_one_query_into_the_client
        probe = SerializedQueryProbe.new
        first = Thread.new { probe.neo4j_query(:first) }
        assert_equal :first, probe.entered.pop

        second = Thread.new { probe.neo4j_query(:second) }
        assert_raises(Timeout::Error) do
            Timeout.timeout(0.1) { probe.entered.pop }
        end

        probe.release << true
        assert_equal :second, Timeout.timeout(1) { probe.entered.pop }
        probe.release << true

        assert_equal :first, first.value
        assert_equal :second, second.value
    ensure
        probe&.release&.close
        first&.kill
        second&.kill
    end

    def test_atomic_file_never_exposes_a_partial_concurrent_write
        Dir.mktmpdir do |directory|
            path = File.join(directory, 'state.json')
            payloads = 8.times.map { |index| "#{index}:#{'x' * 128_000}" }
            AtomicFile.write(path, 'initial')

            ready = Queue.new
            start = Queue.new
            writers = payloads.map do |payload|
                Thread.new do
                    ready << true
                    start.pop
                    AtomicFile.write(path, payload)
                end
            end
            writers.size.times { ready.pop }

            observed = []
            reader = Thread.new do
                observed << File.binread(path) while writers.any?(&:alive?)
            end
            writers.size.times { start << true }
            writers.each(&:join)
            reader.join

            allowed = payloads + ['initial']
            assert observed.all? { |contents| allowed.include?(contents) }
            assert_includes payloads, File.binread(path)
            assert_empty Dir.glob(File.join(directory, '.*.tmp'))
        end
    end

    def test_atomic_file_preserves_existing_file_metadata
        Dir.mktmpdir do |directory|
            path = File.join(directory, 'state.json')
            File.binwrite(path, 'old contents')
            File.chmod(0640, path)
            before = File.stat(path)

            AtomicFile.write(path, 'new contents')

            after = File.stat(path)
            assert_equal 'new contents', File.binread(path)
            refute_equal before.ino, after.ino
            assert_equal before.uid, after.uid
            assert_equal before.gid, after.gid
            assert_equal before.mode & 07777, after.mode & 07777
        end
    end

    def test_atomic_file_inherits_directory_ownership_for_a_new_file
        Dir.mktmpdir do |directory|
            path = File.join(directory, 'state.json')
            directory_stat = File.stat(directory)

            AtomicFile.write(path, 'contents')

            file_stat = File.stat(path)
            assert_equal directory_stat.uid, file_stat.uid
            assert_equal directory_stat.gid, file_stat.gid
        end
    end
end
