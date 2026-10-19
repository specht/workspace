require 'minitest/autorun'
require 'json'
require 'open3'
require 'tmpdir'
require 'fileutils'

# tools/rebuildable.py: which folders »Platz schaffen« offers and the
# download leaves out, and which it never touches.
class RebuildableTest < Minitest::Test
    TOOL = File.expand_path('../tools/rebuildable.py', __dir__)
    NPM_TIME = Time.at(499_162_500)

    def run_tool(*args)
        out, status = Open3.capture2('python3', '-I', TOOL, *args)
        assert status.success?, out
        JSON.parse(out)
    end

    def write(root, path, text = 'x', time: nil)
        file = File.join(root, path)
        FileUtils.mkdir_p(File.dirname(file))
        File.write(file, text)
        File.utime(time, time, file) if time
    end

    def test_offers_what_a_tool_makes_again_and_keeps_the_rest
        Dir.mktmpdir do |root|
            write(root, 'svelte/package.json', '{}')
            write(root, 'svelte/node_modules/pkg/index.js', time: NPM_TIME)
            write(root, 'svelte/node_modules/.vite/dep.js')
            write(root, 'web/package.json', '{}')
            write(root, 'web/node_modules/lib/a.js', time: NPM_TIME)
            write(root, 'web/node_modules/lib/b.js')
            write(root, 'lose/node_modules/x.js', time: NPM_TIME)
            write(root, 'app/pubspec.yaml', 'name: app')
            write(root, 'app/build/out/app.bin')
            write(root, 'notizen/build/plan.txt', 'meine Notiz')
            write(root, 'py/.venv/pyvenv.cfg', 'home = /usr')
            write(root, 'py/ordner/venv/notiz.txt')

            found = run_tool('scan', root).to_h { |f| [f['path'], f] }
            assert_equal %w[app/build lose/node_modules py/.venv svelte/node_modules web/node_modules].sort, found.keys.sort
            assert found['svelte/node_modules']['tick'], 'npm dates, a tool cache inside'
            assert found['app/build']['tick']
            refute found['web/node_modules']['tick']
            assert_includes found['web/node_modules']['note'], 'lib/b.js'
            refute found['lose/node_modules']['tick'], 'no package.json beside it'
            refute found['py/.venv']['tick'], 'no requirements.txt beside it'

            deleted = run_tool('delete', root, 'svelte/node_modules', 'notizen/build', '../etwas')
            assert_equal ['svelte/node_modules'], deleted
            refute File.exist?(File.join(root, 'svelte/node_modules'))
            assert File.exist?(File.join(root, 'notizen/build/plan.txt'))
        end
    end
end
