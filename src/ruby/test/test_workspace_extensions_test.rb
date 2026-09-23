require 'minitest/autorun'
require 'minitest/mock'
require 'tmpdir'
require 'fileutils'
require_relative '../include/test_workspace_package'
require_relative '../include/test_workspace_extensions'

class TestWorkspaceExtensionsTest < Minitest::Test
    def test_requested_extensions_support_ids_and_versions
        assert_equal [
            {'id' => 'Dart-Code.dart-code', 'version' => nil},
            {'id' => 'example.foo', 'version' => '1.2.3'},
        ], TestWorkspaceExtensions.requested('extensions' => [
            'Dart-Code.dart-code', {'id' => 'example.foo', 'version' => '1.2.3'},
        ])
        assert_raises(TestWorkspaceExtensions::Error) do
            TestWorkspaceExtensions.requested('extensions' => ['example.foo', 'Example.FOO'])
        end
        assert_raises(TestWorkspaceExtensions::Error) do
            TestWorkspaceExtensions.requested('extensions' => [{'id' => 'example.foo', 'url' => 'https://evil.invalid'}])
        end
    end

    def test_archive_validation_rejects_links_and_reserved_paths
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, 'source', '.workspace'))
            File.write(File.join(dir, 'source', '.workspace', 'config.yaml'), "workspace_package: 1\n")
            archive = File.join(dir, 'test.tar.gz')
            system('tar', '-czf', archive, '-C', File.join(dir, 'source'), '.', exception: true)
            TestWorkspaceExtensions.validate_archive!(archive)
            File.symlink('config.yaml', File.join(dir, 'source', '.workspace', 'link'))
            system('tar', '-czf', archive, '-C', File.join(dir, 'source'), '.', exception: true)
            assert_raises(TestWorkspaceExtensions::Error) { TestWorkspaceExtensions.validate_archive!(archive) }
            File.delete(File.join(dir, 'source', '.workspace', 'link'))
            FileUtils.mkdir_p(File.join(dir, 'source', '.exam-extensions'))
            system('tar', '-czf', archive, '-C', File.join(dir, 'source'), '.', exception: true)
            assert_raises(TestWorkspaceExtensions::Error) { TestWorkspaceExtensions.validate_archive!(archive) }
        end
    end

    def test_resolve_download_verify_stage_and_dependency
        Dir.mktmpdir do |dir|
            cache = File.join(dir, 'cache')
            manifests = File.join(dir, 'manifests')
            workspace = File.join(dir, 'workspace')
            FileUtils.mkdir_p(workspace)
            vsix = {}
            ['publisher.first', 'publisher.second'].each do |id|
                publisher, name = id.split('.')
                source = File.join(dir, id)
                FileUtils.mkdir_p(File.join(source, 'extension'))
                data = {'publisher' => publisher, 'name' => name, 'version' => '1.2.3',
                    'engines' => {'vscode' => '^1.90.0'}}
                data['extensionDependencies'] = ['publisher.second'] if name == 'first'
                File.write(File.join(source, 'extension', 'package.json'), JSON.generate(data))
                vsix[id] = File.join(dir, "#{id}.vsix")
                Dir.chdir(source) { system('zip', '-q', '-r', vsix[id], 'extension', exception: true) }
            end
            fetcher = ->(url, **_) do
                id = url.include?('/first/') ? 'publisher.first' : 'publisher.second'
                JSON.generate({'version' => '1.2.3', 'files' => {'download' => "https://open-vsx.org/#{id}.vsix"}})
            end
            downloader = ->(url, path) do
                id = url.include?('first') ? 'publisher.first' : 'publisher.second'
                FileUtils.cp(vsix.fetch(id), path)
            end
            TestWorkspaceExtensions.stub(:fetch, fetcher) do
                TestWorkspaceExtensions.stub(:download, downloader) do
                    resolved = {}
                    TestWorkspaceExtensions.resolve!('publisher.first', nil, resolved, {}, cache: cache)
                    assert_equal ['publisher.first', 'publisher.second'], resolved.values.map { |v| v['id'] }.sort
                    tag = 'b' * 24
                    TestWorkspaceExtensions.publish!(
                        {'version' => 1, 'extensions' => resolved.values}, tag: tag, manifests: manifests,
                    )
                    TestWorkspaceExtensions.stage!(tag: tag, workspace_path: workspace,
                        cache: cache, manifests: manifests)
                    assert_equal 2, Dir.glob(File.join(workspace, '.exam-extensions', '*.vsix')).length
                    assert File.file?(File.join(workspace, '.exam-extensions', 'manifest.json'))
                end
            end
        end
    end
end
