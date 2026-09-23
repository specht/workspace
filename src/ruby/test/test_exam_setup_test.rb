require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/test_workspace_package'
require_relative '../include/test_workspace_extensions'

class TestExamSetupTest < Minitest::Test
    def with_config(yaml)
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, '.workspace'))
            File.write(File.join(dir, '.workspace', 'config.yaml'), yaml)
            yield dir
        end
    end

    def test_defaults_and_separate_command_lists
        with_config("workspace_package: 1\n") do |dir|
            assert_equal({'once' => [], 'every_start' => []}, TestWorkspacePackage.setup_commands(dir))
        end
        with_config("exam:\n  setup:\n    once:\n      - dart --disable-analytics\n    every_start:\n      - echo ready\n") do |dir|
            assert_equal({'once' => ['dart --disable-analytics'], 'every_start' => ['echo ready']},
                TestWorkspacePackage.setup_commands(dir))
        end
    end

    def test_bad_setup_is_rejected_before_upload
        ['once: dart --disable-analytics', 'every_start: [true]', 'other: []',
            'once: ["  "]'].each do |invalid|
            with_config("exam:\n  setup:\n    #{invalid}\n") do |dir|
                assert_raises(TestWorkspacePackage::ConfigError) do
                    TestWorkspacePackage.setup_commands(dir)
                end
            end
        end
    end

    def test_prepared_manifest_locks_setup_commands
        with_config("exam:\n  setup:\n    once:\n      - dart --disable-analytics\n") do |dir|
            archive = File.join(dir, 'exam.tar.gz')
            source = File.join(dir, 'source')
            FileUtils.mkdir_p(File.join(source, '.workspace'))
            FileUtils.cp(File.join(dir, '.workspace', 'config.yaml'), File.join(source, '.workspace', 'config.yaml'))
            assert system('tar', '-czf', archive, '-C', source, '.')
            manifest = TestWorkspaceExtensions.prepare!(archive, tag: 'b' * 24)
            assert_equal ['dart --disable-analytics'], manifest.fetch('setup').fetch('once')
            assert_equal [], manifest.fetch('setup').fetch('every_start')
        end
    end
end
