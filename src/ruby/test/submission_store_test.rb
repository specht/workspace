require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/submission_store'

class SubmissionStoreTest < Minitest::Test
    def setup
        @tmp = Dir.mktmpdir
        @root = File.join(@tmp, 'workspace')
        FileUtils.mkdir_p(File.join(@root, 'shower.js', 'pictures'))
        FileUtils.mkdir_p(File.join(@root, 'shower.js', '.git', 'objects'))
        FileUtils.mkdir_p(File.join(@root, 'anaglyph'))
        File.write(File.join(@root, 'shower.js', 'index.html'), "<section class='slide'><h2>A</h2></section>\n")
        File.write(File.join(@root, 'shower.js', 'styles.css'), 'h2 { color: red; }')
        File.binwrite(File.join(@root, 'shower.js', 'pictures', 'fuji.jpg'), "\xFF\xD8\xFF".b + 'x' * 1000)
        File.write(File.join(@root, 'shower.js', '.git', 'objects', 'big'), 'x' * 5000)
        File.write(File.join(@root, 'anaglyph', 'kugel.ini'), "sphere\n")
        File.write(File.join(@root, 'anaglyph', 'stern.ini'), "box\n")
        File.write(File.join(@root, 'anaglyph', 'notizen.txt'), "x\n")
        File.symlink('/etc/hostname', File.join(@root, 'shower.js', 'host'))
        @ws = WorkspaceFiles.new(@root)
        @check = { 'type' => 'submit', 'paths' => ['shower.js'], 'exclude' => ['shower.js/.git'], 'max_size' => '20MB', 'requires' => { 'html_parses' => true } }
    end

    def teardown
        FileUtils.rm_rf(@tmp)
    end

    def test_plan_copies_folder_without_git_and_links
        plan = SubmissionStore.plan(@ws, @check)
        assert_equal %w[shower.js/index.html shower.js/pictures/fuji.jpg shower.js/styles.css], plan.files.map { |f| f[:path] }
        assert_equal ['shower.js/host'], plan.skipped
        assert_match(/symbolische Links sind: `shower.js\/host`/, plan.notes.join)
    end

    def test_write_and_read_back
        plan = SubmissionStore.plan(@ws, @check)
        target = File.join(@tmp, 'store', 'abc.tar.gz')
        sha = SubmissionStore.write(@ws, plan, target)
        assert_equal 64, sha.size
        assert_equal plan.files.map { |f| [f[:path], f[:size]] }, SubmissionStore.contents(target).map { |f| [f[:path], f[:size]] }
        assert_empty Dir[File.join(@tmp, 'store', '*.tmp')]
        # A new submission replaces the old one.
        File.write(File.join(@root, 'shower.js', 'neu.txt'), 'neu')
        SubmissionStore.write(@ws, SubmissionStore.plan(@ws, @check), target)
        assert_includes SubmissionStore.contents(target).map { |f| f[:path] }, 'shower.js/neu.txt'
    end

    def test_refusals
        error = assert_raises(SubmissionStore::Refused) { SubmissionStore.plan(@ws, @check.merge('max_size' => '1kB')) }
        assert_equal :too_big, error.problem
        assert_includes error.message, '`shower.js/pictures/fuji.jpg`'

        File.write(File.join(@root, 'shower.js', 'index.html'), "<section>\n<div>\n</section>\n")
        error = assert_raises(SubmissionStore::Refused) { SubmissionStore.plan(@ws, @check) }
        assert_equal :parse_error, error.problem
        assert_includes error.message, 'Zeile 3'

        error = assert_raises(SubmissionStore::Refused) { SubmissionStore.plan(@ws, @check.merge('paths' => ['praesentation'])) }
        assert_equal :missing, error.problem
    end

    def test_pick
        check = { 'type' => 'submit', 'pick' => 'anaglyph/*.ini', 'max_size' => '1MB', 'requires' => { 'scene_loads' => true } }
        assert_equal %w[anaglyph/kugel.ini anaglyph/stern.ini], SubmissionStore.candidates(@ws, check['pick'])
        assert_raises(SubmissionStore::Refused) { SubmissionStore.plan(@ws, check, picked: 'anaglyph/notizen.txt') }
        assert_raises(SubmissionStore::Refused) { SubmissionStore.plan(@ws, check, picked: '../../etc/passwd') }
        plan = SubmissionStore.plan(@ws, check, picked: 'anaglyph/stern.ini')
        assert_equal ['anaglyph/stern.ini'], plan.files.map { |f| f[:path] }
        assert_match(/prüft der Workspace noch nicht/, plan.notes.join)
    end
end
