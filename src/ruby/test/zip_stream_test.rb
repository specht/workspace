require 'minitest/autorun'
require 'open3'
require 'tmpdir'
require 'fileutils'
require_relative '../include/zip_stream'

# The streaming ZIP (tools/zip_stream.py): its path is there (it was wrong
# for a week, ZipStream), it writes a ZIP of folders under prefixes, and
# --skip-hidden leaves out code-server's folders but keeps .git and
# .workspace (the results of a Leistungsüberprüfung).
class ZipStreamTest < Minitest::Test
    def test_the_tool_is_where_the_pages_look_for_it
        assert File.file?(ZipStream::PATH), ZipStream::PATH
    end

    def entries(*args)
        Dir.mktmpdir do |dir|
            work = File.join(dir, 'workspace')
            { 'main.dart' => 'void main() {}', 'test/main_test.dart' => 'x', '.gitignore' => 'build/',
              '.git/HEAD' => 'ref', '.workspace/config.yaml' => 'workspace_package: 1',
              '.local/share/code-server/state' => 'x', '.cache/a' => 'x' }.each do |name, text|
                FileUtils.mkdir_p(File.dirname(File.join(work, name)))
                File.write(File.join(work, name), text)
            end
            zip, status = Open3.capture2('python3', '-I', ZipStream::PATH, *args, "Ada Lovelace=#{work}", :binmode => true)
            assert status.success?
            File.binwrite(File.join(dir, 'out.zip'), zip)
            list, = Open3.capture2('python3', '-c', 'import sys, zipfile; print("\n".join(zipfile.ZipFile(sys.argv[1]).namelist()))', File.join(dir, 'out.zip'))
            list.split("\n").reject { |e| e.end_with?('/') }.sort
        end
    end

    def test_hidden_folders_of_code_server_stay_out
        assert_equal ['Ada Lovelace/.git/HEAD', 'Ada Lovelace/.gitignore', 'Ada Lovelace/.workspace/config.yaml',
                      'Ada Lovelace/main.dart', 'Ada Lovelace/test/main_test.dart'], entries('--skip-hidden')
        assert_includes entries, 'Ada Lovelace/.local/share/code-server/state'
    end

    def test_folder_names_are_safe_and_unique
        assert_equal ['Ada Lovelace', 'ada lovelace (2)', 'a b', 'ohne Namen', 'x'], ZipStream.folder_names(['Ada Lovelace', 'ada lovelace', 'a/b', '..', 'x'])
    end
end
