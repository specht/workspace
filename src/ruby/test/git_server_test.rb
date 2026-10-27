require 'minitest/autorun'
require 'tmpdir'
require 'open3'
require_relative '../include/git_server'

# The repositories of groups in the Workspace (GitServer): keys, the files
# for the server, the automatic key and creating repositories.
class GitServerTest < Minitest::Test
    KEY = 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl Lena Beispiel'

    def test_addresses
        address = GitServer.address('a1b2c3d4e5', 'class-quiz')
        assert_equal 'git@git.workspace:a1b2c3d4e5/class-quiz.git', address
        assert_equal %w[a1b2c3d4e5 class-quiz], GitServer.parse_address(address)
        assert GitServer.local?(address)
        refute GitServer.local?('git@git.example.org:7b/quiz.git')
        assert_nil GitServer.parse_address('git@git.workspace:../x/quiz.git')
        assert_nil GitServer.parse_address('git@git.workspace:a1b2c3d4e5/Quiz.git')
        assert_match GitServer::GIT_ID, GitServer.new_git_id
    end

    def test_keys_lose_their_comment
        key = GitServer.parse_key(KEY)
        assert_equal 'ssh-ed25519', key[:type]
        refute_includes key[:line], 'Lena'
        assert_nil GitServer.parse_key('ssh-ed25519 kein-base64')
        assert_nil GitServer.parse_key('ssh-dss AAAAB3NzaC1kc3MAAACBAP')
        # a blob of another type under this name
        assert_nil GitServer.parse_key('ssh-ed25519 AAAAB3NzaC1yc2EAAAADAQABAAABAQ==')
        assert_equal 'SHA256:', GitServer.fingerprint(KEY)[0, 7]
        assert_nil GitServer.fingerprint('nonsense')
    end

    def test_fingerprint_matches_ssh_keygen
        Dir.mktmpdir do |dir|
            _, status = Open3.capture2e('ssh-keygen', '-q', '-t', 'ed25519', '-N', '', '-f', File.join(dir, 'k'))
            skip 'no ssh-keygen' unless status.success?
            out, = Open3.capture2e('ssh-keygen', '-l', '-f', File.join(dir, 'k.pub'))
            assert_equal out.split[1], GitServer.fingerprint(File.read(File.join(dir, 'k.pub')))
        end
    rescue Errno::ENOENT
        skip 'no ssh-keygen'
    end

    def test_files_for_the_server
        keys = GitServer.authorized_keys('abc123' => KEY, 'Bad Account' => KEY, 'def456' => 'nonsense')
        assert_equal 1, keys.lines.size
        two = GitServer.authorized_keys('abc123' => [KEY, KEY.sub('Lena Beispiel', 'auto'), 'nonsense'])
        assert_equal 1, two.lines.size, 'the same key once'
        assert_match(/\Acommand="\/usr\/local\/bin\/git-gate abc123",restrict ssh-ed25519 AAAA\S+\n\z/, keys)
        access = GitServer.access('abc123' => %w[a1b2c3d4e5 a1b2c3d4e5 ../etc], 'zz' => %w[0000000000])
        assert_equal "abc123 a1b2c3d4e5\nzz 0000000000\n", access
        Dir.mktmpdir do |dir|
            GitServer.write_config('access', access, :dir => dir)
            assert_equal access, File.read(File.join(dir, 'access'))
            assert_equal ['access'], Dir.children(dir)
        end
    end

    def test_blocks_in_ssh_files
        mine = "Host github.com\n    User git\n"
        once = GitServer.replace_block(mine, ['Host git.workspace'])
        assert_includes once, mine
        assert_includes once, GitServer::BEGIN_MARK
        assert_equal once, GitServer.replace_block(once, ['Host git.workspace'])
        assert_equal mine, GitServer.replace_block(once, nil)
        assert_equal "#{GitServer::BEGIN_MARK}\nx\n#{GitServer::END_MARK}\n", GitServer.replace_block('', ['x'])
    end

    def test_auto_key_in_a_workspace
        Dir.mktmpdir do |home|
            File.write(File.join(home, 'keep'), '')
            FileUtils.mkdir_p(File.join(home, '.ssh'))
            File.write(File.join(home, '.ssh', 'config'), "Host github.com\n    User git\n")
            line = GitServer.install_auto_key(home, :host_key_text => KEY, :uid => Process.uid)
            assert GitServer.parse_key(line)
            assert_equal line, GitServer.install_auto_key(home, :host_key_text => KEY, :uid => Process.uid), 'an existing key stays'
            config = File.read(File.join(home, '.ssh', 'config'))
            assert_includes config, 'Host github.com'
            assert_includes config, "IdentityFile ~/.ssh/#{GitServer::AUTO_KEY}"
            assert_equal 1, config.scan(GitServer::BEGIN_MARK).size
            assert_includes File.read(File.join(home, '.ssh', 'known_hosts')), 'git.workspace ssh-ed25519 AAAA'
            assert_equal '600', format('%o', File.stat(File.join(home, '.ssh', GitServer::AUTO_KEY)).mode & 0o777)
        end
    rescue Errno::ENOENT
        skip 'no ssh-keygen'
    end

    def test_repositories
        Dir.mktmpdir do |root|
            Dir.mktmpdir do |starter|
                File.write(File.join(starter, 'quiz.rb'), "puts 1\n")
                FileUtils.mkdir_p(File.join(starter, 'questions'))
                File.write(File.join(starter, 'questions', 'beispiel.txt'), "? a\n= b\n")
                path = GitServer.create_repo('a1b2c3d4e5', 'class-quiz', :starter => starter, :root => root, :uid => Process.uid)
                assert_equal "questions/beispiel.txt\nquiz.rb\n", GitServer.git('--git-dir', path, 'ls-tree', '-r', '--name-only', 'main')
                assert_equal 'true', GitServer.git('--git-dir', path, 'config', 'receive.denyNonFastForwards').strip
                assert_empty Dir.children(File.join(path, 'hooks'))
                assert_raises(RuntimeError) { GitServer.create_repo('a1b2c3d4e5', 'class-quiz', :root => root, :uid => Process.uid) }
            end
            GitServer.create_repo('a1b2c3d4e5', 'sandbox', :root => root, :uid => Process.uid)
            repos = GitServer.repos('a1b2c3d4e5', :root => root)
            assert_equal %w[class-quiz sandbox], repos.map { |r| r[:name] }
            assert_kind_of Time, repos[0][:last]
            assert_nil repos[1][:last]
            assert_raises(ArgumentError) { GitServer.repo_path('a1b2c3d4e5', '../x', :root => root) }
            GitServer.delete_repo('a1b2c3d4e5', 'sandbox', :root => root)
            assert_equal %w[class-quiz], GitServer.repos('a1b2c3d4e5', :root => root).map { |r| r[:name] }
        end
    end

    def test_force_push_is_refused
        Dir.mktmpdir do |root|
            Dir.mktmpdir do |starter|
                File.write(File.join(starter, 'a.txt'), "a\n")
                path = GitServer.create_repo('a1b2c3d4e5', 'quiz', :starter => starter, :root => root, :uid => Process.uid)
                work = File.join(root, 'work')
                env = { 'GIT_AUTHOR_NAME' => 'L', 'GIT_AUTHOR_EMAIL' => 'l@x', 'GIT_COMMITTER_NAME' => 'L', 'GIT_COMMITTER_EMAIL' => 'l@x' }
                GitServer.git('clone', '-q', path, work)
                File.write(File.join(work, 'a.txt'), "b\n")
                GitServer.git('commit', '-qam', 'b', :chdir => work, :env => env)
                GitServer.git('commit', '-q', '--amend', '-m', 'c', :chdir => work, :env => env)
                GitServer.git('push', '-q', 'origin', 'main', :chdir => work)
                File.write(File.join(work, 'a.txt'), "d\n")
                GitServer.git('commit', '-q', '--amend', '-am', 'd', :chdir => work, :env => env)
                error = assert_raises(RuntimeError) { GitServer.git('push', '-q', '--force', 'origin', 'main', :chdir => work) }
                assert_match(/denying non-fast-forward|rejected/, error.message)
                error = assert_raises(RuntimeError) { GitServer.git('push', '-q', 'origin', ':main', :chdir => work) }
                assert_match(/deletion prohibited|rejected/, error.message)
            end
        end
    end
end
