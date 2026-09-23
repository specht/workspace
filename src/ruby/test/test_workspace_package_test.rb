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
                  color: green
            YAML

            error = assert_raises(TestWorkspacePackage::ConfigError) do
                TestWorkspacePackage.vscode_config(dir)
            end
            assert_includes error.message, 'blue und red'
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
end
