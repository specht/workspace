require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/git_browse'

# Reading a repository for the pages under /git (GitBrowse), on a small
# bare repository with two commits.
class GitBrowseTest < Minitest::Test
    ENV_GIT = { 'GIT_AUTHOR_NAME' => 'Ida Beispiel', 'GIT_AUTHOR_EMAIL' => 'ida@example.org',
                'GIT_COMMITTER_NAME' => 'Ida Beispiel', 'GIT_COMMITTER_EMAIL' => 'ida@example.org' }.freeze

    def run_git(dir, *args)
        out, status = Open3.capture2e(ENV_GIT, 'git', *args, :chdir => dir)
        raise out unless status.success?

        out
    end

    def setup
        @root = Dir.mktmpdir
        work = File.join(@root, 'work')
        @repo = File.join(@root, 'quiz.git')
        run_git(@root, 'init', '-q', '--bare', '-b', 'main', @repo)
        run_git(@root, 'clone', '-q', @repo, work)
        File.write(File.join(work, 'README.md'), "# Quiz\n")
        FileUtils.mkdir_p(File.join(work, 'questions'))
        File.write(File.join(work, 'questions', 'ida.txt'), "? a\n= b\n")
        File.binwrite(File.join(work, 'logo.png'), "\x89PNG\r\n\x1a\n\0\0\0".b)
        run_git(work, 'add', '-A')
        run_git(work, 'commit', '-q', '-m', 'Der Anfang')
        File.write(File.join(work, 'questions', 'ida.txt'), "? a\n= c\n")
        run_git(work, 'mv', 'README.md', 'LIESMICH.md')
        run_git(work, 'commit', '-q', '-am', "Antwort korrigiert\n\nMit Begründung.")
        run_git(work, 'push', '-q', 'origin', 'main')
    end

    def teardown
        FileUtils.rm_rf(@root)
    end

    def test_refs_and_paths
        sha = GitBrowse.resolve(@repo, 'main')
        assert_match GitBrowse::SHA, sha
        assert_equal sha, GitBrowse.resolve(@repo, sha[0, 10])
        assert_nil GitBrowse.resolve(@repo, '--output=/tmp/x')
        assert_nil GitBrowse.resolve(@repo, 'main..x')
        assert_nil GitBrowse.resolve(@repo, 'nope')
        assert_equal ['main'], GitBrowse.branches(@repo)
        assert_equal 'main', GitBrowse.default_branch(@repo)
        assert_equal 'questions/ida.txt', GitBrowse.clean_path('/questions//ida.txt')
        assert_equal '', GitBrowse.clean_path('')
        assert_nil GitBrowse.clean_path('questions/../../etc')
        assert_nil GitBrowse.clean_path('-x')
    end

    def test_folders_and_files
        sha = GitBrowse.resolve(@repo, 'main')
        root = GitBrowse.tree(@repo, sha, '')
        assert_equal %w[questions LIESMICH.md logo.png], root.map { |e| e[:name] }
        assert_equal :tree, GitBrowse.kind(@repo, sha, 'questions')
        assert_equal :blob, GitBrowse.kind(@repo, sha, 'questions/ida.txt')
        assert_nil GitBrowse.kind(@repo, sha, 'questions/tom.txt')
        file = GitBrowse.blob(@repo, sha, 'questions/ida.txt')
        assert_equal "? a\n= c\n", file[:text]
        refute file[:binary]
        assert GitBrowse.blob(@repo, sha, 'logo.png')[:binary]
        assert_nil GitBrowse.blob(@repo, sha, 'nope.txt')
    end

    def test_history_and_a_commit
        sha = GitBrowse.resolve(@repo, 'main')
        log = GitBrowse.log(@repo, sha)
        assert_equal ['Antwort korrigiert', 'Der Anfang'], log.map { |c| c[:subject] }
        assert_equal 'Ida Beispiel', log.first[:author]
        refute log.first.key?(:email)
        assert_equal ['Antwort korrigiert', 'Der Anfang'], GitBrowse.log(@repo, sha, 'questions/ida.txt').map { |c| c[:subject] }
        assert_equal ['Der Anfang'], GitBrowse.log(@repo, sha, '', :skip => 1).map { |c| c[:subject] }
        commit = GitBrowse.commit(@repo, sha)
        assert_equal "Antwort korrigiert\n\nMit Begründung.", commit[:message]
        files = commit[:files].to_h { |f| [f[:path], f] }
        assert_equal :renamed, files['LIESMICH.md'][:status]
        assert_equal 'README.md', files['LIESMICH.md'][:old_path]
        assert_equal :modified, files['questions/ida.txt'][:status]
        assert_includes files['questions/ida.txt'][:patch], "-= b\n+= c"
        first = GitBrowse.commit(@repo, log.last[:sha])
        assert_empty first[:parents]
        assert_equal :added, first[:files].find { |f| f[:path] == 'questions/ida.txt' }[:status]
    end
end
