require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/workspace_files'

class WorkspaceFilesTest < Minitest::Test
    def setup
        @tmp = Dir.mktmpdir
        @ws = File.join(@tmp, 'workspace')
        @other = File.join(@tmp, 'other-student')
        FileUtils.mkdir_p(File.join(@ws, 'shower.js', 'pictures'))
        FileUtils.mkdir_p(@other)
        File.write(File.join(@other, 'secret.txt'), 'geheim')
        File.write(File.join(@ws, 'shower.js', 'index.html'), '<h1>Hallo</h1>')
        File.write(File.join(@ws, 'shower.js', 'pictures', 'Fuji.jpg'), "\xFF\xD8\xFF".b)
        File.symlink(File.join(@other, 'secret.txt'), File.join(@ws, 'shower.js', 'link.txt'))
        File.symlink(@other, File.join(@ws, 'shower.js', 'linkdir'))
        @files = WorkspaceFiles.new(@ws)
    end

    def teardown
        FileUtils.rm_rf(@tmp)
    end

    def test_reads_files_inside
        assert_equal '<h1>Hallo</h1>', @files.read_text('shower.js/index.html')
        assert_equal "\xFF\xD8".b, @files.head('shower.js/pictures/Fuji.jpg', 2)
        assert_equal :dir, @files.entry('shower.js').fetch(:type)
        assert_equal :missing, @files.entry('shower.js/nope.html').fetch(:type)
    end

    def test_refuses_paths_that_leave_the_workspace
        ['/etc/passwd', '../other-student/secret.txt', 'shower.js/../../other-student/secret.txt', '~/x', 'a//b'].each do |path|
            error = assert_raises(WorkspaceFiles::Refused, path) { @files.read(path) }
            assert_equal :bad_path, error.problem
        end
    end

    def test_refuses_symlinks_to_files_and_folders
        error = assert_raises(WorkspaceFiles::Refused) { @files.read('shower.js/link.txt') }
        assert_equal :symlink, error.problem
        error = assert_raises(WorkspaceFiles::Refused) { @files.read('shower.js/linkdir/secret.txt') }
        assert_equal :symlink, error.problem
        assert_equal 'shower.js/linkdir', @files.entry('shower.js/linkdir/secret.txt')[:link]
        error = assert_raises(WorkspaceFiles::Refused) { @files.list('shower.js/linkdir') }
        assert_equal :symlink, error.problem
    end

    def test_workspace_root_itself_may_not_be_swapped
        # Even if a folder is replaced by a link after the check, the opened
        # file must lie inside the Workspace.
        assert_raises(WorkspaceFiles::Refused) do
            @files.send(:verify_fd!, File.open(File.join(@other, 'secret.txt')).fileno, 'x')
        end
    end

    def test_list_marks_links_and_sorts
        names = @files.list('shower.js').map { |e| [e[:name], e[:type]] }
        assert_equal [['index.html', :file], ['link.txt', :symlink], ['linkdir', :symlink], ['pictures', :dir]], names
    end

    def test_other_case
        assert_equal 'shower.js/pictures/Fuji.jpg', @files.other_case('shower.js/pictures/fuji.jpg')
        assert_nil @files.other_case('shower.js/pictures/Fuji.jpg')
        assert_nil @files.other_case('shower.js/pictures/monkey.jpg')
    end

    def test_walk_skips_links_and_excludes
        FileUtils.mkdir_p(File.join(@ws, 'shower.js', '.git'))
        File.write(File.join(@ws, 'shower.js', '.git', 'HEAD'), 'ref')
        files, skipped = @files.walk('shower.js', exclude: ['shower.js/.git'])
        assert_equal ['shower.js/index.html', 'shower.js/pictures/Fuji.jpg'], files.map { |f| f[:path] }
        assert_equal ['shower.js/link.txt', 'shower.js/linkdir'], skipped
    end

    def test_size_limit_and_special_files
        File.write(File.join(@ws, 'big.txt'), 'x' * 2000)
        error = assert_raises(WorkspaceFiles::Refused) { @files.read('big.txt', max: 1000) }
        assert_equal :too_big, error.problem
        File.mkfifo(File.join(@ws, 'pipe'))
        error = assert_raises(WorkspaceFiles::Refused) { @files.read('pipe') }
        assert_equal :wrong_type, error.problem
    end
end
