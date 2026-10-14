require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/tic80_storage'

class Tic80StorageTest < Minitest::Test
    def setup
        @tmp = Dir.mktmpdir
        @root = File.join(@tmp, 'TIC-80')
        @uid = Process.uid
        @gid = Process.gid
        Tic80Storage.prepare_root(@root, :uid => @uid, :gid => @gid)
    end

    def teardown
        FileUtils.rm_rf(@tmp)
    end

    def b64(text)
        Base64.strict_encode64(text)
    end

    def sha(text)
        Tic80Storage.sha256(text)
    end

    def write(relative, text)
        path = File.join(@root, relative)
        FileUtils.mkdir_p(File.dirname(path))
        File.binwrite(path, text)
    end

    def apply(changes)
        Tic80Storage.apply(@root, changes, :uid => @uid, :gid => @gid)
    end

    def test_new_file_and_folder
        result = apply([{ :path => 'games', :dir => true },
                        { :path => 'games/snake.lua', :base => nil, :contents => b64('-- snake') }])
        assert_equal({ 'games/snake.lua' => sha('-- snake') }, result[:written])
        assert_empty result[:conflicts]
        assert_equal '-- snake', File.read(File.join(@root, 'games/snake.lua'))
        tree = Tic80Storage.tree(@root)
        assert_equal ['games'], tree[:dirs]
        assert_equal ['games/snake.lua'], tree[:files].map { |f| f[:path] }
    end

    def test_change_from_the_version_on_disk
        write('snake.lua', 'v1')
        result = apply([{ :path => 'snake.lua', :base => sha('v1'), :contents => b64('v2') }])
        assert_equal 'v2', File.read(File.join(@root, 'snake.lua'))
        assert_empty result[:conflicts]
    end

    # The file changed on disk since the browser read it (VS Code, git pull):
    # it stays, and the browser's version lands next to it.
    def test_changed_elsewhere_keeps_both
        write('snake.lua', 'from vs code')
        result = apply([{ :path => 'snake.lua', :base => sha('v1'), :contents => b64('from tic-80') }])
        assert_equal 'from vs code', File.read(File.join(@root, 'snake.lua'))
        assert_equal 'from tic-80', File.read(File.join(@root, 'snake (TIC-80).lua'))
        assert_equal [{ :path => 'snake.lua', :saved_as => 'snake (TIC-80).lua' }], result[:conflicts]

        # a second clash gets a new name, the first copy stays
        apply([{ :path => 'snake.lua', :base => sha('v1'), :contents => b64('again') }])
        assert_equal 'again', File.read(File.join(@root, 'snake (TIC-80 2).lua'))
        assert_equal 'from tic-80', File.read(File.join(@root, 'snake (TIC-80).lua'))
    end

    # A file created elsewhere under the same name as a new browser file.
    def test_new_in_both_places
        write('snake.lua', 'mine')
        result = apply([{ :path => 'snake.lua', :base => nil, :contents => b64('theirs') }])
        assert_equal 'mine', File.read(File.join(@root, 'snake.lua'))
        assert_equal 'snake (TIC-80).lua', result[:conflicts].first[:saved_as]
    end

    # Other files changed on disk do not matter at all (that used to block
    # every save).
    def test_unrelated_changes_do_not_block
        write('notiz.txt', 'hallo')
        result = apply([{ :path => 'snake.lua', :base => nil, :contents => b64('x') }])
        assert_empty result[:conflicts]
        assert File.exist?(File.join(@root, 'notiz.txt'))
    end

    # A retry after a lost response finds its own version on disk.
    def test_retry_is_harmless
        write('snake.lua', 'v2')
        result = apply([{ :path => 'snake.lua', :base => sha('v1'), :contents => b64('v2') }])
        assert_empty result[:conflicts]
        assert_equal({ 'snake.lua' => sha('v2') }, result[:written])
    end

    def test_delete
        write('snake.lua', 'v1')
        result = apply([{ :path => 'snake.lua', :base => sha('v1'), :deleted => true }])
        assert_equal ['snake.lua'], result[:deleted]
        refute File.exist?(File.join(@root, 'snake.lua'))
    end

    # Deleted in TIC-80, changed elsewhere: the file stays.
    def test_delete_of_a_changed_file_keeps_it
        write('snake.lua', 'changed')
        result = apply([{ :path => 'snake.lua', :base => sha('v1'), :deleted => true }])
        assert_equal [{ :path => 'snake.lua', :kept => true }], result[:conflicts]
        assert File.exist?(File.join(@root, 'snake.lua'))
    end

    def test_folder_with_foreign_files_is_not_removed
        write('games/a.lua', 'a')
        write('games/sprite.png', 'png')
        apply([{ :path => 'games/a.lua', :base => sha('a'), :deleted => true },
               { :path => 'games', :dir => true, :deleted => true }])
        assert File.exist?(File.join(@root, 'games/sprite.png'))
    end

    def test_refuses_unsafe_paths
        ['../escape.lua', '/etc/passwd', 'a/../../b', ''].each do |bad|
            assert_raises(Tic80Storage::Invalid) { apply([{ :path => bad, :base => nil, :contents => b64('x') }]) }
        end
    end

    def test_refuses_to_write_through_a_symlink
        File.symlink(@tmp, File.join(@root, 'out'))
        assert_raises(Tic80Storage::Invalid) do
            apply([{ :path => 'out/x.lua', :base => nil, :contents => b64('x') }])
        end
        refute File.exist?(File.join(@tmp, 'x.lua'))
        # and the tree leaves the link out instead of failing
        assert_equal [], Tic80Storage.tree(@root)[:files]
    end

    def test_tree_contents_only_for_requested_paths
        write('a.lua', 'a')
        write('b.lua', 'b')
        files = Tic80Storage.tree(@root, :contents => true, :only => Set['b.lua'])[:files]
        assert_nil files.find { |f| f[:path] == 'a.lua' }[:contents]
        assert_equal b64('b'), files.find { |f| f[:path] == 'b.lua' }[:contents]
    end

    def test_prepare_root_recovers_an_interrupted_swap
        FileUtils.rm_rf(@root)
        stage = "#{@root}.sync-abc"
        FileUtils.mkdir_p(stage)
        File.write(File.join(stage, 'snake.lua'), 'saved')
        Tic80Storage.prepare_root(@root, :uid => @uid, :gid => @gid)
        assert_equal 'saved', File.read(File.join(@root, 'snake.lua'))
        refute File.exist?(stage)
    end
end
