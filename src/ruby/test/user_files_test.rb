require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require 'timeout'
require_relative '../include/user_files'

class UserFilesTest < Minitest::Test
    def setup
        @tmp = Dir.mktmpdir
        @ws = File.join(@tmp, 'workspace')
        @outside = File.join(@tmp, 'outside')
        FileUtils.mkdir_p(@ws)
        FileUtils.mkdir_p(@outside)
        @target = File.join(@outside, 'important.txt')
        File.write(@target, 'do not touch')
    end

    def teardown
        FileUtils.rm_rf(@tmp)
    end

    def test_writes_appends_and_creates_only_once
        assert UserFiles.write(@ws, '.hackschule', "one\n")
        assert_equal "one\n", File.read(File.join(@ws, '.hackschule'))
        assert UserFiles.write(@ws, '.hackschule', "two\n")
        assert_equal "two\n", File.read(File.join(@ws, '.hackschule'))
        UserFiles.write(@ws, '.hackschule', "three\n", mode: :append)
        assert_equal "two\nthree\n", File.read(File.join(@ws, '.hackschule'))
        refute UserFiles.write(@ws, '.hackschule', "four\n", create_only: true)
        assert_equal "two\nthree\n", File.read(File.join(@ws, '.hackschule'))
    end

    def test_creates_missing_folders
        UserFiles.write(@ws, '.local/share/code-server/User/settings.json', '{}')
        assert_equal '{}', File.read(File.join(@ws, '.local/share/code-server/User/settings.json'))
        UserFiles.mkdir_p(@ws, '.git/info')
        assert File.directory?(File.join(@ws, '.git/info'))
    end

    def test_reads_and_reports_missing_files
        File.write(File.join(@ws, '.my.cnf'), 'cnf')
        assert_equal 'cnf', UserFiles.read(@ws, '.my.cnf')
        assert_nil UserFiles.read(@ws, 'missing')
        assert_nil UserFiles.read(@ws, 'missing-folder/file')
        refute UserFiles.exist?(@ws, 'missing')
        assert UserFiles.exist?(@ws, '.my.cnf')
    end

    def test_a_link_as_the_file_is_refused
        File.symlink(@target, File.join(@ws, '.hackschule'))
        assert_raises(UserFiles::Refused) { UserFiles.write(@ws, '.hackschule', 'x') }
        assert_raises(UserFiles::Refused) { UserFiles.write(@ws, '.hackschule', 'x', mode: :append) }
        assert_raises(UserFiles::Refused) { UserFiles.read(@ws, '.hackschule') }
        assert_equal 'do not touch', File.read(@target)
    end

    def test_a_dangling_link_is_not_created_through
        missing = File.join(@outside, 'new.txt')
        File.symlink(missing, File.join(@ws, '.gitconfig'))
        refute UserFiles.write(@ws, '.gitconfig', 'x', create_only: true)
        assert_raises(UserFiles::Refused) { UserFiles.write(@ws, '.gitconfig', 'x') }
        refute File.exist?(missing)
    end

    def test_a_link_on_the_way_is_refused
        File.symlink(@outside, File.join(@ws, '.local'))
        assert_raises(UserFiles::Refused) { UserFiles.write(@ws, '.local/important.txt', 'x') }
        assert_raises(UserFiles::Refused) { UserFiles.read(@ws, '.local/important.txt') }
        assert_raises(UserFiles::Refused) { UserFiles.mkdir_p(@ws, '.local/share') }
        assert_equal 'do not touch', File.read(@target)
        refute File.exist?(File.join(@outside, 'share'))
    end

    def test_a_fifo_does_not_block
        File.mkfifo(File.join(@ws, '.my.cnf'))
        Timeout.timeout(2) do
            assert_raises(UserFiles::Refused) { UserFiles.read(@ws, '.my.cnf') }
            assert_raises(UserFiles::Refused) { UserFiles.write(@ws, '.my.cnf', 'x') }
        end
    end

    def test_dot_dot_is_refused
        assert_raises(UserFiles::Refused) { UserFiles.write(@ws, '../outside/important.txt', 'x') }
        assert_equal 'do not touch', File.read(@target)
    end

    def test_walk_lists_folders_and_files_but_no_links
        UserFiles.write(@ws, 'game/main.lua', 'print(1)')
        UserFiles.write(@ws, 'game/sprites/a.png', 'png')
        File.symlink(@outside, File.join(@ws, 'game', 'escape'))
        File.symlink(@target, File.join(@ws, 'game', 'file-link'))
        File.mkfifo(File.join(@ws, 'game', 'fifo'))
        seen = []
        UserFiles.walk(@ws, 'game') { |rel, stat, f| seen << [rel, stat.directory? ? :dir : f.read] }
        assert_equal [['main.lua', 'print(1)'], ['sprites', :dir], ['sprites/a.png', 'png']], seen
    end

    def test_kind_delete_rmdir
        UserFiles.write(@ws, 'a/b.txt', 'x')
        File.symlink(@target, File.join(@ws, 'a', 'link'))
        assert_equal :file, UserFiles.kind(@ws, 'a/b.txt')
        assert_equal :directory, UserFiles.kind(@ws, 'a')
        assert_equal :other, UserFiles.kind(@ws, 'a/link')
        assert_nil UserFiles.kind(@ws, 'a/missing')
        refute UserFiles.rmdir(@ws, 'a')
        assert UserFiles.delete(@ws, 'a/b.txt')
        assert UserFiles.delete(@ws, 'a/link')
        assert_equal 'do not touch', File.read(@target)
        assert UserFiles.rmdir(@ws, 'a')
        refute UserFiles.delete(@ws, 'a/b.txt')
    end

    def test_rm_r_does_not_follow_links
        UserFiles.write(@ws, 'old/inner/file.txt', 'x')
        File.symlink(@outside, File.join(@ws, 'old', 'inner', 'escape'))
        UserFiles.rm_r(@ws, 'old')
        refute File.exist?(File.join(@ws, 'old'))
        assert_equal 'do not touch', File.read(@target)
    end

    def test_write_sets_mtime
        time = Time.at(1_700_000_000)
        UserFiles.write(@ws, 'cart.tic', 'x', mtime: time)
        assert_equal time.to_i, File.mtime(File.join(@ws, 'cart.tic')).to_i
    end

    def test_chown_leaves_links_alone
        File.symlink(@target, File.join(@ws, 'link'))
        UserFiles.chown(@ws, 'link')
        UserFiles.chown(@ws, 'missing')
        File.write(File.join(@ws, 'file'), 'x')
        UserFiles.chown(@ws, 'file')
        assert_equal UserFiles::UID, File.stat(File.join(@ws, 'file')).uid
    end
end
