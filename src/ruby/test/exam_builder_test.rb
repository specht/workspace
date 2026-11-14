require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/exam_builder'
require_relative '../include/test_workspace_package'

# A Leistungsüberprüfung from a folder of the Workspace (ExamBuilder): the
# folders, the tree with what starts unticked, links never followed or
# packed, the config.yaml from the dialog, the package as tar would unpack it.
class ExamBuilderTest < Minitest::Test
    def setup
        @root = Dir.mktmpdir
        @dir = File.join(@root, 'pruefung')
        FileUtils.mkdir_p(File.join(@dir, 'tests'))
        FileUtils.mkdir_p(File.join(@dir, '__pycache__'))
        FileUtils.mkdir_p(File.join(@dir, '.workspace'))
        File.write(File.join(@dir, 'AUFGABE.md'), "# Aufgabe\n")
        File.write(File.join(@dir, 'main.py'), "print(1)\n")
        File.write(File.join(@dir, 'tests', 'test_main.py'), "assert True\n")
        File.write(File.join(@dir, '__pycache__', 'main.pyc'), "x")
        File.write(File.join(@dir, '.notes'), "privat\n")
        File.write(File.join(@dir, '.workspace', 'config.yaml'), "workspace_package: 1\nextensions:\n  - ms-python.python\nprint:\n  include: ['*.py']\n")
        @secret = File.join(@root, 'secret.txt')
        File.write(@secret, "geheim\n")
        File.symlink(@secret, File.join(@dir, 'link.txt'))
        FileUtils.mkdir_p(File.join(@root, '.hidden'))
        File.symlink(@dir, File.join(@root, 'verweis'))
    end

    def teardown
        FileUtils.rm_rf(@root)
    end

    def test_folders_are_real_and_visible_ones
        assert_equal ['pruefung'], ExamBuilder.folders(@root)
        assert_raises(ExamBuilder::Refused) { ExamBuilder.folder_path(@root, '../etc') }
        assert_raises(ExamBuilder::Refused) { ExamBuilder.folder_path(@root, 'verweis') }
    end

    def test_the_tree_says_what_starts_unticked
        tree = ExamBuilder.tree(@dir)[:entries].to_h { |e| [e[:p], e] }
        assert_nil tree['AUFGABE.md'][:r]
        assert_nil tree['tests'][:r]
        assert tree.key?('tests/test_main.py')
        assert_equal 'artifact', tree['__pycache__'][:r]
        refute tree.key?('__pycache__/main.pyc'), 'a folder that starts unticked is not opened'
        assert_equal 'hidden', tree['.notes'][:r]
        File.write(File.join(@dir, '.gitignore'), "*.pyc\n")
        assert_nil ExamBuilder.tree(@dir)[:entries].find { |e| e[:p] == '.gitignore' }[:r], '.gitignore belongs to a package'
        assert_equal 'link', tree['link.txt'][:r]
        refute tree.key?('.workspace'), 'the dialog writes the config'
        assert_equal ['tests', '__pycache__'], tree.values.select { |e| e[:d] }.map { |e| e[:p] }.sort.reverse
        # a repository in a subfolder: a Git task, unticked, not opened
        FileUtils.mkdir_p(File.join(@dir, 'tests', '.git', 'objects'))
        tree = ExamBuilder.tree(@dir)[:entries].to_h { |e| [e[:p], e] }
        assert_equal 'git', tree['tests/.git'][:r]
        refute tree.key?('tests/.git/objects')
    end

    def test_expand_refuses_links_and_paths_out_of_the_folder
        files, bytes = ExamBuilder.expand(@dir, ['AUFGABE.md', 'tests'])
        assert_equal ['AUFGABE.md', 'tests/test_main.py'], files
        assert_equal 22, bytes
        assert_raises(ExamBuilder::Refused) { ExamBuilder.expand(@dir, ['link.txt']) }
        assert_raises(ExamBuilder::Refused) { ExamBuilder.expand(@dir, ['../secret.txt']) }
        assert_raises(ExamBuilder::Refused) { ExamBuilder.expand(@dir, ['/etc/passwd']) }
        assert_raises(ExamBuilder::Refused) { ExamBuilder.expand(@dir, ['.workspace/config.yaml']) }
        # a link inside a chosen folder is left out, not followed
        File.symlink(@secret, File.join(@dir, 'tests', 'geheim.txt'))
        assert_equal ['tests/test_main.py'], ExamBuilder.expand(@dir, ['tests']).first
    end

    def test_the_config_keeps_what_was_there_and_takes_the_dialog
        existing = ExamBuilder.existing_config(@dir)
        File.write(File.join(@dir, 'a[1].txt'), "x\n")
        patterns = ExamBuilder.print_patterns(@dir, ['tests', 'a[1].txt'], :before => ['**/runner.*', 'main.py'], :listed => ['main.py', 'tests/test_main.py'])
        assert_equal ['**/runner.*', 'tests/**', 'a\\[1\\].txt'], patterns, 'main.py was matched: now the choice of the dialog'
        assert ExamBuilder.matches?(patterns[2], 'a[1].txt')
        refute ExamBuilder.matches?(patterns[2], 'a1.txt')
        assert ExamBuilder.matches?('tests/**', 'tests/sub/x.py')
        yaml = YAML.safe_load(ExamBuilder.config_yaml(existing, :color => 'yellow', :look => 'frame', :git_mode => 'fresh',
                                                     :print_exclude => patterns))
        assert_equal ['ms-python.python'], yaml['extensions']
        assert_equal({ 'color' => 'yellow', 'look' => 'frame' }, yaml['exam'])
        assert_equal ['*.py'], yaml['print']['include']
        assert_equal patterns, yaml['print']['exclude']
        assert_nil YAML.safe_load(ExamBuilder.config_yaml({}, :color => 'blue', :look => 'theme', :git_mode => 'none', :print_exclude => []))['print']
    end

    def test_the_package_holds_the_files_and_the_config_and_passes_the_check
        files, = ExamBuilder.expand(@dir, ['AUFGABE.md', 'main.py', 'tests'])
        blob = ExamBuilder.pack(@dir, files, ExamBuilder.config_yaml({}, :color => 'green', :look => 'frame', :git_mode => 'fresh', :print_exclude => ['tests/test_main.py']))
        Dir.mktmpdir do |out|
            archive = File.join(out, 'p.tar.gz')
            File.binwrite(archive, blob)
            listed = `tar -tzf #{archive}`.lines.map(&:chomp).sort
            assert_equal ['.workspace/', '.workspace/config.yaml', 'AUFGABE.md', 'main.py', 'tests/test_main.py'], listed
            unpacked = File.join(out, 'x')
            FileUtils.mkdir_p(unpacked)
            system('tar', '-xzf', archive, '-C', unpacked)
            assert_equal '#119c4a', TestWorkspacePackage.vscode_config(unpacked)['workbench.colorCustomizations']['statusBar.background']
            assert_equal ['AUFGABE.md', 'main.py'], TestWorkspacePackage.printable_files(unpacked).map(&:first)
        end
    end

    # ---- dropped from the computer

    def test_files_from_a_folder_with_their_paths
        Dir.mktmpdir do |draft|
            src = File.join(@root, 'readme')
            File.write(src, "x\n")
            ExamBuilder.stage_files(draft, [['README.md', src], ['.workspace/config.yaml', src], ['__MACOSX/x', src]])
            assert File.file?(File.join(draft, 'README.md'))
            assert File.file?(File.join(draft, '.workspace/config.yaml'))
            refute File.exist?(File.join(draft, '__MACOSX'))
            assert_raises(ExamBuilder::Refused) { ExamBuilder.stage_files(draft, [['../x', src]]) }
            assert_raises(ExamBuilder::Refused) { ExamBuilder.stage_files(draft, []) }
        end
    end

    def zip(path, script)
        system('python3', '-I', '-c', "import zipfile, sys\nz = zipfile.ZipFile(sys.argv[1], 'w')\n#{script}\nz.close()", path, exception: true)
    end

    def test_a_zip_of_a_folder_becomes_the_folder
        Dir.mktmpdir do |draft|
            archive = File.join(@root, 'k.zip')
            zip(archive, "z.writestr('workspace-180/README.md', 'x')\nz.writestr('workspace-180/.workspace/config.yaml', 'workspace_package: 1')\nz.writestr('__MACOSX/._x', 'x')")
            assert_equal 'workspace-180', ExamBuilder.stage_archive(draft, archive, 'Klausur.zip')
            assert_equal ['.workspace', 'README.md'], Dir.children(draft).sort
        end
    end

    def test_an_archive_with_a_link_or_a_path_out_is_refused
        Dir.mktmpdir do |draft|
            archive = File.join(@root, 'l.zip')
            zip(archive, "i = zipfile.ZipInfo('link')\ni.external_attr = (0o120777 << 16)\nz.writestr(i, '/etc/passwd')")
            error = assert_raises(ExamBuilder::Refused) { ExamBuilder.stage_archive(draft, archive, 'l.zip') }
            assert_includes error.message, 'Links'
            zip(archive, "z.writestr('../out.txt', 'x')")
            assert_raises(ExamBuilder::Refused) { ExamBuilder.stage_archive(draft, archive, 'l.zip') }
            assert_empty Dir.children(draft)
            assert_raises(ExamBuilder::Refused) { ExamBuilder.stage_archive(draft, archive, 'l.docx') }
        end
    end

    def test_a_tar_gz_is_unpacked
        Dir.mktmpdir do |draft|
            archive = File.join(@root, 'p.tar.gz')
            system('tar', '-czf', archive, '-C', @dir, 'AUFGABE.md', 'main.py', exception: true)
            assert_equal 'p', ExamBuilder.stage_archive(draft, archive, 'p.tar.gz')
            assert_equal ['AUFGABE.md', 'main.py'], Dir.children(draft).sort
        end
    end

    def test_the_dart_extension_is_offered_and_added
        offers = ExamBuilder.offers(['linked_list.dart', 'README.md'], {})
        assert_equal [{ :id => 'Dart-Code.dart-code', :name => 'Dart', :why => ExamBuilder::LANGUAGE_EXTENSIONS['.dart'][2], :included => false }], offers
        assert ExamBuilder.offers(['a.dart'], { 'extensions' => ['dart-code.dart-code'] }).first[:included]
        assert_empty ExamBuilder.offers(['main.py'], {})
        yaml = YAML.safe_load(ExamBuilder.config_yaml({ 'extensions' => [{ 'id' => 'Dart-Code.dart-code', 'version' => '3.1' }] },
                                                      :color => 'blue', :look => 'theme', :git_mode => 'fresh', :print_exclude => [],
                                                      :extensions => ['Dart-Code.dart-code']))
        assert_equal [{ 'id' => 'Dart-Code.dart-code', 'version' => '3.1' }], yaml['extensions'], 'already there, with its version'
        yaml = YAML.safe_load(ExamBuilder.config_yaml({}, :color => 'blue', :look => 'theme', :git_mode => 'fresh', :print_exclude => [],
                                                      :extensions => ['Dart-Code.dart-code']))
        assert_equal ['Dart-Code.dart-code'], yaml['extensions']
    end
end
