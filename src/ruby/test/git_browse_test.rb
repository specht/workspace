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

    # A branch with a slash, an annotated and a lightweight tag.
    def more_refs
        work = File.join(@root, 'more')
        run_git(@root, 'clone', '-q', @repo, work)
        run_git(work, 'tag', '-a', 'v1.0', '-m', "Erste Version\n\nFür die Klasse.")
        run_git(work, 'tag', 'stand')
        run_git(work, 'checkout', '-q', '-b', 'feature/runde')
        File.write(File.join(work, 'questions', 'tom.txt'), "? x\n= y\n")
        run_git(work, 'add', '-A')
        run_git(work, 'commit', '-q', '-m', 'Runde von Tom')
        run_git(work, 'push', '-q', 'origin', 'feature/runde', '--tags')
    end

    def test_branches_and_tags_with_one_call
        more_refs
        refs = GitBrowse.refs(@repo)
        main = GitBrowse.resolve(@repo, 'main')
        assert_equal %w[feature/runde main], refs[:branches].map { |b| b[:name] }
        assert_equal main, refs[:branches].find { |b| b[:name] == 'main' }[:sha]
        tags = refs[:tags].to_h { |t| [t[:name], t] }
        assert_equal %w[stand v1.0].sort, tags.keys.sort
        # an annotated tag: its commit (peeled), its message and who set it, no e-mail
        assert_equal main, tags['v1.0'][:sha]
        assert_equal 'Erste Version', tags['v1.0'][:subject]
        assert_equal 'Für die Klasse.', tags['v1.0'][:body]
        assert_equal 'Ida Beispiel', tags['v1.0'][:tagger]
        refute_includes tags['v1.0'].values.join(' '), 'ida@example.org'
        # a lightweight tag: the commit, no message
        assert_equal main, tags['stand'][:sha]
        assert_equal '', tags['stand'][:subject]
        assert_equal 'main', GitBrowse.default_branch(@repo, refs)
    end

    def test_a_ref_with_a_slash_in_the_address
        more_refs
        refs = GitBrowse.refs(@repo)
        assert_equal ['feature/runde', 'questions/tom.txt', :branch], GitBrowse.split_ref('feature/runde/questions/tom.txt', refs)
        assert_equal ['feature/runde', '', :branch], GitBrowse.split_ref('feature/runde', refs)
        assert_equal ['v1.0', 'LIESMICH.md', :tag], GitBrowse.split_ref('v1.0/LIESMICH.md', refs)
        sha = GitBrowse.resolve(@repo, 'main')
        assert_equal [sha[0, 10], 'questions', nil], GitBrowse.split_ref("#{sha[0, 10]}/questions", refs)
        assert_equal sha, GitBrowse.commit_of(@repo, sha[0, 10], nil, refs)
        assert_equal sha, GitBrowse.commit_of(@repo, 'v1.0', :tag, refs)
        refute_equal sha, GitBrowse.commit_of(@repo, 'feature/runde', :branch, refs)
        # what the address may not carry stays out of git
        assert_nil GitBrowse.commit_of(@repo, '--upload-pack=x', nil, refs)
        assert_nil GitBrowse.commit_of(@repo, 'main..main', nil, refs)
    end

    def test_the_whole_tree_and_the_last_commit_of_each_entry
        sha = GitBrowse.resolve(@repo, 'main')
        folders = GitBrowse.all_folders(@repo, sha)
        assert_equal %w[questions LIESMICH.md logo.png], folders[''].map { |e| e[:name] }
        assert_equal [{ :name => 'ida.txt', :path => 'questions/ida.txt', :type => :blob }], folders['questions']
        last = GitBrowse.last_commits(@repo, sha, '')
        assert_equal 'Antwort korrigiert', last['questions'][:subject]
        assert_equal 'Antwort korrigiert', last['LIESMICH.md'][:subject]
        assert_equal 'Der Anfang', last['logo.png'][:subject]
        refute last['logo.png'].key?(:email)
        assert_equal({ 'ida.txt' => 'Antwort korrigiert' }, GitBrowse.last_commits(@repo, sha, 'questions').transform_values { |c| c[:subject] })
        assert_equal 2, GitBrowse.count(@repo, sha)
        assert_equal 1, GitBrowse.count(@repo, sha, 'logo.png')
    end

    def test_a_tree_too_big_for_the_sidebar
        sha = GitBrowse.resolve(@repo, 'main')
        assert_nil GitBrowse.all_folders(@repo, sha, :max => 3)
        refute_nil GitBrowse.all_folders(@repo, sha, :max => 4)
    end

    def test_calls_are_bounded
        sha = GitBrowse.resolve(@repo, 'main')
        cut = GitBrowse.run(@repo, 'cat-file', 'blob', "#{sha}:questions/ida.txt", :max => 3)
        assert cut[:ok]
        assert cut[:cut]
        assert_equal '? a', cut[:out]
        assert_nil GitBrowse.git(@repo, 'cat-file', 'blob', "#{sha}:questions/ida.txt", :max => 3)
        refute GitBrowse.run(@repo, 'cat-file', 'blob', "#{sha}:nope")[:ok]
        file = GitBrowse.blob(@repo, sha, 'questions/ida.txt', :max => 4)
        assert file[:cut]
        assert_equal 8, file[:size]
        # a character cut in two at the end is no binary file
        File.write(File.join(@root, 'umlaut.txt'), 'ä' * 10)
        blob = Open3.capture2('git', '--git-dir', @repo, 'hash-object', '-w', File.join(@root, 'umlaut.txt'))[0].strip
        part = GitBrowse.run(@repo, 'cat-file', 'blob', blob, :max => 5, :binary => true)[:out]
        assert_equal 5, part.bytesize
        # no time left: stopped, nothing comes back
        refute GitBrowse.run(@repo, 'log', :seconds => 0)[:ok]
        assert_nil GitBrowse.git(@repo, 'log', :seconds => 0)
    end

    def test_pictures_in_markdown_stay_in_the_repository
        assert_equal 'images/logo.png', GitBrowse.picture_path('', 'images/logo.png')
        assert_equal 'images/logo.png', GitBrowse.picture_path('docs', '../images/logo.png')
        assert_equal 'docs/a.svg', GitBrowse.picture_path('docs', './a.svg?raw=1')
        assert_equal 'images/logo.png', GitBrowse.picture_path('docs/deep', '/images/logo.png')
        assert_nil GitBrowse.picture_path('docs', '../../geheim.png')
        assert_nil GitBrowse.picture_path('', 'https://example.org/x.png')
        assert_nil GitBrowse.picture_path('', '//example.org/x.png')
        assert_nil GitBrowse.picture_path('', 'data:image/png;base64,AAAA')
        assert_nil GitBrowse.picture_path('', 'quiz.rb')
        assert_equal 'quiz.rb', GitBrowse.repo_path('', 'quiz.rb')
        assert_nil GitBrowse.repo_path('', 'javascript:alert(1)')
    end

    def test_a_file_on_its_own_runs_nothing
        svg = GitBrowse.raw_headers('images/plan.svg', false, false)
        assert_equal 'image/svg+xml', svg['Content-Type']
        assert_equal "sandbox; default-src 'none'", svg['Content-Security-Policy']
        assert_equal 'nosniff', svg['X-Content-Type-Options']
        assert_equal "inline; filename*=UTF-8''plan.svg", svg['Content-Disposition']
        html = GitBrowse.raw_headers('seite.html', false, false)
        assert_equal 'text/plain; charset=utf-8', html['Content-Type']
        assert_equal "sandbox; default-src 'none'", html['Content-Security-Policy']
        bin = GitBrowse.raw_headers('punkte.dat', true, false)
        assert_equal 'application/octet-stream', bin['Content-Type']
        assert bin['Content-Disposition'].start_with?('attachment')
        assert GitBrowse.raw_headers('Übung 1.txt', false, true)['Content-Disposition'].end_with?("''%C3%9Cbung%201.txt")
    end
end
