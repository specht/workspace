require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/case_store'

# Cases for the development (CaseStore, cases_page.rb): what is copied from
# a Workspace, what a case holds, and when it goes.
class CaseStoreTest < Minitest::Test
    def setup
        @dir = Dir.mktmpdir
        @workspace = File.join(@dir, 'workspace')
        @root = File.join(@dir, 'cases')
        FileUtils.mkdir_p(File.join(@workspace, 'routenplaner-starter', 'dart', 'lib'))
        File.write(File.join(@workspace, 'routenplaner-starter', 'dart', 'routes.dart'), 'List<String>? walk() => null;')
        File.write(File.join(@workspace, 'routenplaner-starter', 'dart', 'lib', 'graph.dart'), 'class Graph {}')
        File.write(File.join(@workspace, 'routenplaner-starter', 'dart', '.hidden'), 'x')
        File.write(File.join(@workspace, 'routenplaner-starter', 'README.md'), 'not in the folder of the file')
        File.write(File.join(@dir, 'secret.txt'), 'outside the Workspace')
        File.symlink(File.join(@dir, 'secret.txt'), File.join(@workspace, 'routenplaner-starter', 'dart', 'link.txt'))
    end

    def teardown
        FileUtils.rm_rf(@dir)
    end

    # the folder of the checked file and below it; no hidden files, no links
    # (not even one pointing out of the Workspace), nothing above
    def test_collect_takes_the_folder_of_the_file_only
        files = CaseStore.collect(@workspace, ['routenplaner-starter/dart/routes.dart'])
        assert_equal ['routenplaner-starter/dart/lib/graph.dart', 'routenplaner-starter/dart/routes.dart'], files.keys.sort
        assert_empty CaseStore.collect(@workspace, ['../secret.txt'])
        assert_empty CaseStore.collect(@workspace, ['gibt-es-nicht/x.rb'])
    end

    def test_collect_keeps_to_the_limits
        big = File.join(@workspace, 'routenplaner-starter', 'dart', 'big.bin')
        File.binwrite(big, 'x' * (CaseStore::MAX_FILE + 1))
        refute_includes CaseStore.collect(@workspace, ['routenplaner-starter/dart/routes.dart']).keys, 'routenplaner-starter/dart/big.bin'
    end

    # a case holds the files and its record, no name; it goes after
    # KEEP_DAYS, with the account (owner) or by hand
    def test_save_list_and_delete
        files = CaseStore.collect(@workspace, ['routenplaner-starter/dart/routes.dart'])
        now = Time.at(1_800_000_000)
        id = CaseStore.save({ 'task' => 'routenplaner/walk', 'owner' => CaseStore.owner('ada@schule.de'), 'note' => 'sieht richtig aus' },
            :files => files, :blobs => { 'abgabe.tar.gz' => 'gz' }, :root => @root, :now => now)
        assert_match CaseStore::ID, id
        assert File.file?(File.join(@root, id, 'files', 'routenplaner-starter', 'dart', 'routes.dart'))
        assert File.file?(File.join(@root, id, 'abgabe.tar.gz'))
        record = File.read(File.join(@root, id, 'case.json'))
        refute_includes record, 'ada'
        listed = CaseStore.list(:root => @root)
        assert_equal [id], listed.map { |c| c['id'] }
        assert_equal ['abgabe.tar.gz'], listed.first['blobs']

        assert_equal 0, CaseStore.purge(:root => @root, :now => now + 24 * 3600)
        assert_equal 1, CaseStore.purge(:root => @root, :now => now + (CaseStore::KEEP_DAYS + 1) * 24 * 3600)
        assert_empty CaseStore.list(:root => @root)

        id = CaseStore.save({ 'owner' => CaseStore.owner('Ada@schule.de') }, :root => @root)
        CaseStore.save({ 'owner' => CaseStore.owner('ben@schule.de') }, :root => @root)
        assert_equal 1, CaseStore.delete_for('ada@schule.de', :root => @root)
        refute_includes CaseStore.list(:root => @root).map { |c| c['id'] }, id
        refute CaseStore.delete('../etc', :root => @root)
    end
end
