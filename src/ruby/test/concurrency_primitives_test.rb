require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/atomic_file'

class ConcurrencyPrimitivesTest < Minitest::Test
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
