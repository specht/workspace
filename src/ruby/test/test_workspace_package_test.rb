require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/test_workspace_package'

class TestWorkspacePackageTest < Minitest::Test
    def test_default_exam_color_is_blue_and_package_settings_are_merged
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), <<~YAML)
                workspace_package: 1
                vscode_config:
                  editor.minimap.enabled: false
                  workbench.colorCustomizations:
                    editor.background: "#101010"
            YAML

            config = TestWorkspacePackage.vscode_config(dir)
            assert_equal false, config['editor.minimap.enabled']
            assert_equal 'Tomorrow Night Blue', config['workbench.colorTheme']
            assert_equal false, config['window.autoDetectColorScheme']
            assert_equal({'editor.background' => '#101010'}, config['workbench.colorCustomizations'])
        end
    end

    def test_red_exam_color_is_package_controlled
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), <<~YAML)
                workspace_package: 1
                exam:
                  color: red
            YAML

            config = TestWorkspacePackage.vscode_config(dir)
            assert_equal 'Red', config['workbench.colorTheme']
            assert_equal({}, config['workbench.colorCustomizations'])
        end
    end

    def test_printing_is_recursive_language_independent_and_honours_excludes
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            FileUtils.mkdir_p(File.join(dir, 'src', 'nested'))
            FileUtils.mkdir_p(File.join(dir, 'tests', 'nested', 'deeper'))
            FileUtils.mkdir_p(File.join(dir, '.cache', 'Microsoft', 'DeveloperTools'))
            FileUtils.mkdir_p(File.join(dir, 'src', '.build', 'generated'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), <<~YAML)
                workspace_package: 1
                print:
                  exclude:
                    - "tests/**"
                    - "**/test_runner.*"
            YAML
            File.write(File.join(dir, 'README'), "Aufgabe\n")
            File.write(File.join(dir, 'src', 'solution.weird'), "answer\n")
            File.write(File.join(dir, 'src', 'nested', 'test_runner.weird'), "runner\n")
            File.write(File.join(dir, 'tests', 'hidden.txt'), "test\n")
            File.write(File.join(dir, 'tests', 'nested', 'deeper', 'also_hidden.txt'), "test\n")
            File.write(File.join(dir, '.cache', 'Microsoft', 'DeveloperTools', 'deviceid'), "secret\n")
            File.write(File.join(dir, 'src', '.build', 'generated', 'artifact.txt'), "hidden\n")
            File.write(File.join(dir, '.gitignore'), "*.bak\n")
            File.binwrite(File.join(dir, 'src', 'binary.dat'), "\x00\x01\x02")

            assert_equal(
                ['.gitignore', 'README', 'src/solution.weird'],
                TestWorkspacePackage.printable_files(dir).map(&:first),
            )
        end
    end

    def test_print_include_keeps_nested_source_but_not_hidden_directories
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            FileUtils.mkdir_p(File.join(dir, 'src', 'nested', '.cache'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), <<~YAML)
                workspace_package: 1
                print:
                  include:
                    - "src/**"
                  exclude:
                    - "src/nested/ignore/**"
            YAML
            FileUtils.mkdir_p(File.join(dir, 'src', 'nested', 'ignore', 'deep'))
            File.write(File.join(dir, 'src', 'nested', 'solution.txt'), "yes\n")
            File.write(File.join(dir, 'src', 'nested', '.cache', 'deviceid'), "no\n")
            File.write(File.join(dir, 'src', 'nested', 'ignore', 'deep', 'secret.txt'), "no\n")
            assert_equal ['src/nested/solution.txt'], TestWorkspacePackage.printable_files(dir).map(&:first)
        end
    end

    def test_invalid_exam_color_is_rejected
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), <<~YAML)
                workspace_package: 1
                exam:
                  color: purple
            YAML

            error = assert_raises(TestWorkspacePackage::ConfigError) do
                TestWorkspacePackage.vscode_config(dir)
            end
            assert_includes error.message, 'blue und red'
        end
    end


    def write_config(dir, yaml)
        FileUtils.mkdir_p(File.join(dir, '.workspace'))
        File.write(File.join(dir, '.workspace', 'config.yaml'), yaml)
    end

    # the signal frame: the frame in the colour, a dark editor, the
    # package's own colours win
    def test_frame_look_colours_the_frame_and_keeps_a_dark_editor
        Dir.mktmpdir do |dir|
            write_config(dir, <<~YAML)
                workspace_package: 1
                exam:
                  color: yellow
                  look: frame
                vscode_config:
                  workbench.colorCustomizations:
                    statusBar.background: "#000000"
            YAML
            config = TestWorkspacePackage.vscode_config(dir)
            assert_equal 'Default Dark Modern', config['workbench.colorTheme']
            colors = config['workbench.colorCustomizations']
            assert_equal '#ffd60a', colors['activityBar.background']
            assert_equal '#ffd60a', colors['editorGroupHeader.tabsBackground']
            assert_equal '#1b1b1b', colors['activityBar.foreground']
            assert_equal '#000000', colors['statusBar.background']
            refute colors.key?('editor.background')
        end
    end

    def test_whole_theme_only_in_blue_and_red_and_unknown_looks_rejected
        Dir.mktmpdir do |dir|
            write_config(dir, "workspace_package: 1\nexam:\n  color: yellow\n")
            error = assert_raises(TestWorkspacePackage::ConfigError) { TestWorkspacePackage.vscode_config(dir) }
            assert_includes error.message, 'exam.look: frame'
            write_config(dir, "workspace_package: 1\nexam:\n  color: teal\n  look: frame\n")
            error = assert_raises(TestWorkspacePackage::ConfigError) { TestWorkspacePackage.vscode_config(dir) }
            assert_includes error.message, 'yellow, orange'
            write_config(dir, "workspace_package: 1\nexam:\n  look: neon\n")
            error = assert_raises(TestWorkspacePackage::ConfigError) { TestWorkspacePackage.vscode_config(dir) }
            assert_includes error.message, 'theme und frame'
        end
    end

    # a repository in a subfolder (a Git task): found, through no link,
    # not in hidden folders; the recovery repository leaves it out
    def test_nested_repositories_are_found_and_excluded_from_recovery
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, 'projekt', '.git'))
            FileUtils.mkdir_p(File.join(dir, 'a', 'b', 'repo', '.git'))
            FileUtils.mkdir_p(File.join(dir, '.cache', 'x', '.git'))
            Dir.mktmpdir do |outside|
                FileUtils.mkdir_p(File.join(outside, 'r', '.git'))
                File.symlink(File.join(outside, 'r'), File.join(dir, 'link'))
                assert_equal ['a/b/repo', 'projekt'], TestWorkspacePackage.nested_repositories(dir)
            end
            FileUtils.mkdir_p(File.join(dir, '.git', 'info'))
            TestWorkspacePackage.write_git_exclude(dir, :extra => ['/projekt/'])
            assert_includes File.read(File.join(dir, '.git', 'info', 'exclude')).lines.map(&:chomp), '/projekt/'
        end
    end

    # network.allow: names and IPv4 addresses with an optional port
    def test_network_allow_is_validated
        Dir.mktmpdir do |dir|
            assert_equal [], TestWorkspacePackage.network_allow(dir)
            write_config(dir, "network:\n  allow:\n    - api.schule.de\n    - 192.0.2.7:8080\n")
            assert_equal [{ 'host' => 'api.schule.de', 'ports' => [80, 443] }, { 'host' => '192.0.2.7', 'ports' => [8080] }],
                         TestWorkspacePackage.network_allow(dir)
            ['- "http://x.de"', '- "a.de:0"', '- "a.de:99999"', '- "-a.de"', '- "a b"'].each do |bad|
                write_config(dir, "network:\n  allow:\n    #{bad}\n")
                assert_raises(TestWorkspacePackage::ConfigError, bad) { TestWorkspacePackage.network_allow(dir) }
            end
            write_config(dir, "network:\n  open: true\n")
            assert_raises(TestWorkspacePackage::ConfigError) { TestWorkspacePackage.network_allow(dir) }
        end
    end

    def test_git_mode_defaults_to_fresh
        Dir.mktmpdir do |dir|
            assert_equal 'fresh', TestWorkspacePackage.git_mode(dir)
            assert_equal 'fresh', TestWorkspacePackage.validate_git_layout!(dir)
        end
    end

    def test_fresh_rejects_packaged_git_repository
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.git'))

            error = assert_raises(TestWorkspacePackage::ConfigError) do
                TestWorkspacePackage.validate_git_layout!(dir)
            end
            assert_includes error.message, 'git.mode: preserve'
        end
    end

    def test_preserve_requires_and_accepts_packaged_git_repository
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), <<~YAML)
                workspace_package: 1
                git:
                  mode: preserve
            YAML

            assert_raises(TestWorkspacePackage::ConfigError) do
                TestWorkspacePackage.validate_git_layout!(dir)
            end

            FileUtils.mkdir_p(File.join(dir, '.git'))
            assert_equal 'preserve', TestWorkspacePackage.validate_git_layout!(dir)
        end
    end

    def test_none_leaves_git_unmanaged_and_rejects_packaged_repository
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), <<~YAML)
                workspace_package: 1
                git:
                  mode: none
            YAML

            assert_equal 'none', TestWorkspacePackage.validate_git_layout!(dir)

            FileUtils.mkdir_p(File.join(dir, '.git'))
            error = assert_raises(TestWorkspacePackage::ConfigError) do
                TestWorkspacePackage.validate_git_layout!(dir)
            end
            assert_includes error.message, 'git.mode: preserve'
        end
    end

    def test_invalid_git_mode_is_rejected
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), <<~YAML)
                workspace_package: 1
                git:
                  mode: magic
            YAML

            error = assert_raises(TestWorkspacePackage::ConfigError) do
                TestWorkspacePackage.git_mode(dir)
            end
            assert_includes error.message, 'fresh, preserve und none'
        end
    end

    def test_git_exclude_contains_workspace_owned_files
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.git', 'info'))
            path = TestWorkspacePackage.write_git_exclude(dir)
            contents = File.read(path)
            assert_includes contents, '.workspace/'
            assert_includes contents, '.test_init'
            assert_includes contents, '.test_git_pending'
        end
    end

    def test_git_exclude_does_not_follow_a_link
        Dir.mktmpdir do |dir|
            outside = File.join(dir, 'outside')
            workspace = File.join(dir, 'workspace')
            FileUtils.mkdir_p([outside, File.join(workspace, '.git')])
            File.symlink(outside, File.join(workspace, '.git', 'info'))
            assert_raises(UserFiles::Refused) { TestWorkspacePackage.write_git_exclude(workspace) }
            assert_empty Dir.children(outside)
        end
    end
end
