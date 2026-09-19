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
            assert_equal '#005a9c', config.dig('workbench.colorCustomizations', 'statusBar.background')
            assert_equal '#101010', config.dig('workbench.colorCustomizations', 'editor.background')
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

            assert_equal(
                '#a31515',
                TestWorkspacePackage.vscode_config(dir).dig('workbench.colorCustomizations', 'activityBar.background'),
            )
        end
    end

    def test_printing_is_recursive_language_independent_and_honours_excludes
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            FileUtils.mkdir_p(File.join(dir, 'src', 'nested'))
            FileUtils.mkdir_p(File.join(dir, 'tests'))
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
            File.binwrite(File.join(dir, 'src', 'binary.dat'), "\x00\x01\x02")

            assert_equal(
                ['README', 'src/solution.weird'],
                TestWorkspacePackage.printable_files(dir).map(&:first),
            )
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
