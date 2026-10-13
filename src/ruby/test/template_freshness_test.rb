require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require 'open3'
require_relative '../include/template_freshness'

class TemplateFreshnessTest < Minitest::Test
    TF = TemplateFreshness

    def test_from_meta
        t = TF.from_meta('template' => ' https://github.com/specht/cpu-sandbox.git ', 'template_name' => 'der CPU Sandbox')
        assert_equal({ :url => 'https://github.com/specht/cpu-sandbox.git', :folder => 'cpu-sandbox', :name => 'der CPU Sandbox' }, t)
        assert_equal 'shower.js', TF.from_meta('template' => 'https://git.nhcham.org/specht/shower.js.git')[:folder]
        assert_nil TF.from_meta('template' => 'https://github.com/specht/anaglyph')[:name]
        # Only public HTTPS addresses; never an SSH address or a local path.
        assert_nil TF.from_meta('template' => 'git@github.com:specht/anaglyph.git')
        assert_nil TF.from_meta('template' => 'http://github.com/specht/anaglyph.git')
        assert_nil TF.from_meta('template' => 'https://github.com/specht/..')
        assert_nil TF.from_meta({})
    end

    def test_same_repo
        a = 'https://github.com/specht/anaglyph.git'
        assert TF.same_repo?('https://github.com/specht/anaglyph', a)
        assert TF.same_repo?('https://GitHub.com/specht/anaglyph/', a)
        assert TF.same_repo?('git@github.com:specht/anaglyph.git', a)
        assert TF.same_repo?('ssh://git@github.com/specht/anaglyph.git', a)
        assert TF.same_repo?('https://anna@github.com/specht/anaglyph.git', a)
        refute TF.same_repo?('https://github.com/anna/anaglyph.git', a)
        refute TF.same_repo?('git@git.nhcham.org:specht/cpu-sandbox-programs-2026.git', 'https://github.com/specht/cpu-sandbox.git')
        refute TF.same_repo?('', a)
    end

    def pkt(line)
        format('%04x', line.bytesize + 4) + line
    end

    def test_head_from_advertisement
        sha = 'ec080795f88b538b382398fb91a8e234df3e0c0c'
        body = pkt("# service=git-upload-pack\n") + '0000' +
            pkt("#{sha} HEAD\0multi_ack symref=HEAD:refs/heads/main agent=git/github\n") +
            pkt("#{sha} refs/heads/main\n") + pkt("1111111111111111111111111111111111111111 refs/tags/v1\n") + '0000'
        assert_equal sha, TF.head_from_advertisement(body)
        # No HEAD advertised: the first ref.
        assert_equal sha, TF.head_from_advertisement(pkt("#{sha} refs/heads/main\0caps\n") + '0000')
        assert_nil TF.head_from_advertisement('<html>Not Found</html>')
        assert_nil TF.head_from_advertisement('')
    end

    def test_cache_asks_once_and_retries_failures_sooner
        now = 1000
        calls = []
        answer = 'a' * 40
        cache = TF::LatestCache.new(:ttl => 600, :failure_ttl => 120, :clock => -> { now }, :fetcher => lambda { |url| calls << url; answer })
        assert_equal answer, cache.get('u')
        assert_equal answer, cache.get('u')
        assert_equal 1, calls.size
        now += 601
        cache.get('u')
        assert_equal 2, calls.size

        answer = nil
        cache.get('v')
        now += 60
        cache.get('v')
        assert_equal 3, calls.size
        now += 61
        cache.get('v')
        assert_equal 4, calls.size
    end

    def test_refresh_asks_at_once_and_later_views_see_the_answer
        now = 1000
        calls = []
        answer = 'a' * 40
        cache = TF::LatestCache.new(:clock => -> { now }, :fetcher => lambda { |url| calls << url; answer })
        assert_equal answer, cache.get('u')
        answer = 'b' * 40
        assert_equal 'a' * 40, cache.get('u'), 'remembered for an hour'
        assert_equal 'b' * 40, cache.refresh('u')
        assert_equal 'b' * 40, cache.get('u')
        assert_equal 2, calls.size
        now += 3599
        cache.get('u')
        assert_equal 2, calls.size
    end

    # --- the script, against real repositories --------------------------------

    def git(dir, *args)
        out, status = Open3.capture2e({ 'HOME' => @home }, 'git', *args, :chdir => dir)
        raise "git #{args.join(' ')}: #{out}" unless status.success?

        out.strip
    end

    def run_script(dir, sha)
        out, = Open3.capture2e({ 'HOME' => @home }, 'sh', '-c', TF::SCRIPT, 'sh', dir, sha)
        TF.parse(out)
    end

    def commit(dir, file, text)
        File.write(File.join(dir, file), text)
        git(dir, 'add', file)
        git(dir, 'commit', '-q', '-m', "change #{file}")
        git(dir, 'rev-parse', 'HEAD')
    end

    def test_script_reads_the_clone
        skip 'git not installed' unless system('git --version > /dev/null 2>&1')
        Dir.mktmpdir do |tmp|
            @home = File.join(tmp, 'home')
            FileUtils.mkdir_p(@home)
            git(@home, 'config', '--global', 'user.email', 'test@example.com')
            git(@home, 'config', '--global', 'user.name', 'Test')
            git(@home, 'config', '--global', 'init.defaultBranch', 'main')

            upstream = File.join(tmp, 'upstream')
            FileUtils.mkdir_p(upstream)
            git(upstream, 'init', '-q')
            first = commit(upstream, 'scene.ini', "shape = box\n")
            workspace = File.join(tmp, 'workspace')
            FileUtils.mkdir_p(workspace)
            git(workspace, 'clone', '-q', upstream, 'anaglyph')
            clone = File.join(workspace, 'anaglyph')
            template = { :url => upstream, :folder => 'anaglyph', :name => nil }

            r = run_script(clone, first)
            assert_equal :current, r[:state]
            assert_equal upstream, r[:origin]
            assert_equal :current, TF.status(template, first, r)

            second = commit(upstream, 'README.md', "neu\n")
            r = run_script(clone, second)
            assert_equal :behind, r[:state]
            assert_equal :outdated, TF.status(template, second, r)

            # Fetched (for example by VS Code) but not merged: still outdated.
            git(clone, 'fetch', '-q')
            assert_equal :fetched, run_script(clone, second)[:state]
            assert_equal :outdated, TF.status(template, second, run_script(clone, second))

            # Nothing the script did changed the clone.
            assert_equal first, git(clone, 'rev-parse', 'HEAD')
            assert_equal '', git(clone, 'status', '--porcelain')

            git(clone, 'pull', '-q')
            assert_equal :current, run_script(clone, second)[:state]
            # Own commits on top still count as current.
            commit(clone, 'mine.ini', "shape = sphere\n")
            assert_equal :current, run_script(clone, second)[:state]

            assert_equal :absent, run_script(File.join(workspace, 'bif'), second)[:state]
            FileUtils.mkdir_p(File.join(workspace, 'plain'))
            assert_equal :norepo, run_script(File.join(workspace, 'plain'), second)[:state]
            FileUtils.mkdir_p(File.join(clone, 'sub'))
            assert_equal :norepo, run_script(File.join(clone, 'sub'), second)[:state]

            # A clone of something else in the same folder: no hint.
            assert_equal :other, TF.status({ :url => 'https://github.com/specht/anaglyph.git', :folder => 'anaglyph' }, second, run_script(clone, second))
        end
    end

    def test_status_without_answers
        t = { :url => 'https://github.com/specht/anaglyph.git', :folder => 'anaglyph' }
        assert_equal :unavailable, TF.status(t, nil, { :state => :behind, :origin => t[:url] })
        assert_equal :unavailable, TF.status(t, 'a' * 40, nil)
        assert_nil TF.parse(nil)
        assert_nil TF.parse("fatal: something\n")
    end

    def test_notice
        text = TF.notice({ :folder => 'cpu-sandbox', :name => 'der CPU Sandbox' })
        assert_includes text, '**Es gibt eine neue Version der CPU Sandbox!** Du solltest `git pull` ausführen:'
        assert_includes text, "```bash\ncd /workspace/cpu-sandbox\ngit pull\n```"
        assert_includes text, 'nichts verändert'
        assert_includes TF.notice({ :folder => 'bif', :name => nil }), 'neue Version von `bif`!'
    end

    # Every page with a template gets a usable one.
    def test_content_templates
        dir = File.expand_path('../../content', __dir__)
        found = Dir[File.join(dir, '*', '*.md')].filter_map do |path|
            text = File.read(path, :encoding => 'UTF-8')
            meta = text[/<div class='meta'>\n(.*?)<\/div>/m, 1]
            next unless meta && meta =~ /^template:/

            require 'yaml'
            [path, YAML.safe_load(meta)]
        end
        refute_empty found
        found.each do |path, meta|
            t = TF.from_meta(meta)
            refute_nil t, "#{path}: template must be an https:// address"
            refute_nil t[:name], "#{path}: template_name fehlt"
        end
    end
end
