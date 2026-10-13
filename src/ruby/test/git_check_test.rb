require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require 'open3'
require_relative '../include/git_check'

class GitCheckTest < Minitest::Test
    EXPECT = { 'has_remote' => true, 'committed' => true, 'pushed' => true }

    def parse(lines)
        GitCheck.parse(lines.map { |l| "#{l}\n" }.join)
    end

    def test_branch_lines
        assert_equal({ :branch_line => 'main...origin/main [ahead 2, behind 1]', :branch => 'main', :upstream => 'origin/main', :ahead => 2, :behind => 1 },
            GitCheck.parse_branch('main...origin/main [ahead 2, behind 1]'))
        assert_equal 'main', GitCheck.parse_branch('No commits yet on main')[:branch]
        assert GitCheck.parse_branch('HEAD (no branch)')[:detached]
        assert_nil GitCheck.parse_branch('main')[:upstream]
        assert GitCheck.parse_branch('main...origin/main [gone]')[:upstream_gone]
    end

    def test_missing_and_not_git
        assert_equal :no_repo, GitCheck.evaluate('sandbox', EXPECT, GitCheck.parse("NOREPO\n"))[1]
        assert_equal :not_git, GitCheck.evaluate('sandbox', EXPECT, GitCheck.parse("NOGIT\n"))[1]
    end

    def test_uncommitted_names_files_and_next_steps
        state = parse(['TOP /workspace/sandbox', 'COUNT 4', 'REMOTE origin', 'STATUS ## main...origin/main',
                       'STATUS  M structures/lena/stack.py', 'STATUS ?? structures/lena/queue.py'])
        ok, problem, message = GitCheck.evaluate('sandbox', EXPECT, state)
        refute ok
        assert_equal :uncommitted, problem
        assert_includes message, '2 Dateien sind geändert, aber nicht committet: `structures/lena/stack.py`, `structures/lena/queue.py`'
        assert_includes message, "cd /workspace/sandbox\ngit pull\ngit add structures/\ngit commit"
        assert_includes message, "\ngit push\n"
        assert_equal 'lena/', GitCheck.add_target(['lena/a.py', 'lena/b/c.py'])
        assert_equal '-A', GitCheck.add_target(['lena/a.py', 'README.md'])
        assert_equal '-A', GitCheck.add_target(['lena/a.py', 'tom/b.py'])
        assert_includes GitCheck.describe('sandbox', state), ' M structures/lena/stack.py'
    end

    def test_not_pushed_and_no_upstream
        state = parse(['COUNT 5', 'REMOTE origin', 'STATUS ## main...origin/main [ahead 2]'])
        assert_equal :not_pushed, GitCheck.evaluate('sandbox', EXPECT, state)[1]
        assert_includes GitCheck.evaluate('sandbox', EXPECT, state)[2], '2 Commits sind noch nicht gepusht'
        state = parse(['COUNT 5', 'REMOTE origin', 'STATUS ## main...origin/main [ahead 1, behind 3]'])
        message = GitCheck.evaluate('sandbox', EXPECT, state)[2]
        assert_includes message, 'auseinandergelaufen: 1 Commit von dir fehlen dort, 3 Commits von anderen'
        assert_includes message, "git pull\ngit push"
        state = parse(['COUNT 1', 'REMOTE origin', 'STATUS ## feature'])
        assert_includes GitCheck.evaluate('sandbox', EXPECT, state)[2], '`git push -u origin feature`'
        state = parse(['COUNT 1', 'STATUS ## main'])
        assert_equal :no_remote, GitCheck.evaluate('sandbox', EXPECT, state)[1]
    end

    def test_all_good
        state = parse(['COUNT 9', 'EMAIL lena@example.org', 'MINE 3', 'REMOTE origin', 'STATUS ## main...origin/main'])
        assert GitCheck.evaluate('sandbox', EXPECT.merge('min_commits' => 3), state)[0]
        problem, message = GitCheck.evaluate('sandbox', { 'min_commits' => 4 }, state)[1, 2]
        assert_equal :too_few_commits, problem
        assert_includes message, '3 Commits von `lena@example.org`'
        assert_equal :no_identity, GitCheck.evaluate('sandbox', { 'min_commits' => 1 }, parse(['COUNT 9', 'REMOTE origin']))[1]
    end

    def test_identity_without_repository
        expect = { 'identity' => true }
        ok, problem, message = GitCheck.evaluate('', expect, parse(['NAME Lena Muster']))
        refute ok
        assert_equal :no_identity, problem
        assert_includes message, 'Git kennt deine E-Mail-Adresse noch nicht'
        refute_includes message, 'deinen Namen'
        assert_includes GitCheck.evaluate('', expect, parse([]))[2], 'deinen Namen und deine E-Mail-Adresse'
        problem, message = GitCheck.evaluate('', expect, parse(['NAME Vorname Nachname', 'EMAIL lena@example.org']))[1, 2]
        assert_equal :placeholder, problem
        assert_includes message, '`Vorname Nachname`'
        assert GitCheck.evaluate('', expect, parse(['NAME Lena Muster', 'EMAIL lena@example.org']))[0]
        assert_equal "`git config user.name`: `Lena Muster`\n\n`git config user.email`: nicht gesetzt",
            GitCheck.describe('', parse(['NAME Lena Muster']))
        # With a repository, identity is checked after the repository itself.
        state = parse(['COUNT 1', 'REMOTE origin', 'STATUS ## main...origin/main'])
        assert_equal :no_identity, GitCheck.evaluate('sandbox', { 'has_remote' => true, 'identity' => true }, state)[1]
        assert_equal :no_repo, GitCheck.evaluate('sandbox', { 'identity' => true }, parse(['NOREPO']))[1]
    end

    # Runs the real script against real repositories (needs git and sh).
    def test_script_against_real_repositories
        skip 'git not installed' unless system('git --version > /dev/null 2>&1')
        Dir.mktmpdir do |tmp|
            env = { 'GIT_AUTHOR_NAME' => 'T', 'GIT_AUTHOR_EMAIL' => 't@x', 'GIT_COMMITTER_NAME' => 'T', 'GIT_COMMITTER_EMAIL' => 't@x', 'HOME' => tmp }
            system(env, 'git', 'config', '--global', 'user.email', 't@x')
            remote = File.join(tmp, 'remote.git')
            work = File.join(tmp, 'work')
            system(env, 'git', 'init', '-q', '--bare', '-b', 'main', remote)
            system(env, 'git', 'clone', '-q', remote, work, err: File::NULL)
            File.write(File.join(work, 'a.txt'), 'a')
            run = lambda do |path|
                out, = Open3.capture2(env, 'sh', '-c', GitCheck::STATUS_SCRIPT, 'sh', path)
                GitCheck.parse(out)
            end
            state = run.(work)
            assert_equal :uncommitted, GitCheck.evaluate('work', EXPECT, state, root: tmp)[1]
            # A plain folder inside a repository is not a repository of its own.
            FileUtils.mkdir_p(File.join(work, 'structures'))
            inner = run.(File.join(work, 'structures'))
            ok, problem, message = GitCheck.evaluate('work/structures', EXPECT, inner, root: tmp)
            assert_equal :not_git, problem
            assert_includes message, 'Ordner im Repository `work`'
            system(env, 'git', '-C', work, 'add', '-A')
            system(env, 'git', '-C', work, 'commit', '-q', '-m', 'erster')
            state = run.(work)
            assert_equal :no_upstream, GitCheck.evaluate('work', EXPECT, state, root: tmp)[1]
            system(env, 'git', '-C', work, 'push', '-q', '-u', 'origin', 'main', err: File::NULL)
            state = run.(work)
            assert GitCheck.evaluate('work', EXPECT, state, root: tmp)[0], GitCheck.describe('work', state)
            File.write(File.join(work, 'a.txt'), 'b')
            system(env, 'git', '-C', work, 'commit', '-q', '-am', 'zweiter')
            state = run.(work)
            assert_equal :not_pushed, GitCheck.evaluate('work', EXPECT, state, root: tmp)[1]
            assert_equal 2, state[:commits]
            assert_equal 't@x', state[:email]
            assert_equal 2, state[:mine]
            assert_equal({ :repo => false, :git => false }, run.(File.join(tmp, 'nope')))
            FileUtils.mkdir_p(File.join(tmp, 'plain'))
            assert_equal({ :repo => true, :git => false, :email => 't@x' }, run.(File.join(tmp, 'plain')))
            # Identity is read outside any repository, from git config --global.
            assert_equal :no_identity, GitCheck.evaluate('', { 'identity' => true }, run.(tmp))[1]
            system(env, 'git', 'config', '--global', 'user.name', 'Tara Test')
            assert GitCheck.evaluate('', { 'identity' => true }, run.(tmp))[0]
            assert_includes GitCheck.describe('', run.(tmp)), '`Tara Test`'
            # Reading the status must not rewrite the index.
            index = File.join(work, '.git', 'index')
            sleep 1.1
            File.write(File.join(work, 'a.txt'), 'b') # same content, new mtime
            before = File.mtime(index)
            run.(work)
            assert_equal before, File.mtime(index)
        end
    end
end
