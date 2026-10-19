require 'minitest/autorun'
require 'json'
require 'open3'
require 'tmpdir'
require 'fileutils'

# tools/rebuildable.py: which folders »Platz schaffen« offers and the
# download leaves out, and which it never touches.
class RebuildableTest < Minitest::Test
    TOOL = File.expand_path('../tools/rebuildable.py', __dir__)
    INSTALLED = Time.now - 3600   # npm's record of the install
    BEFORE = INSTALLED - 30       # files unpacked during the install

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

    # An npm project: package.json, a lock file, and node_modules with npm's
    # record of the install written last.
    def npm_project(root, dir, lock: true, record: true, bif: false)
        write(root, "#{dir}/package.json", '{}')
        write(root, "#{dir}/package-lock.json", '{}') if lock
        write(root, "#{dir}/.bif-project", '') if bif
        write(root, "#{dir}/node_modules/pkg/index.js", time: BEFORE)
        write(root, "#{dir}/node_modules/.package-lock.json", '{}', time: INSTALLED) if record
    end

    def test_offers_what_a_tool_makes_again_and_keeps_the_rest
        Dir.mktmpdir do |root|
            npm_project(root, 'svelte')
            write(root, 'svelte/node_modules/.vite/dep.js')          # a tool's cache, written later
            npm_project(root, 'web')
            write(root, 'web/node_modules/lib/b.js')                 # changed by hand afterwards
            npm_project(root, 'ohne-lock', lock: false)
            npm_project(root, 'ohne-vermerk', record: false)
            npm_project(root, 'geschichte', bif: true)
            write(root, 'lose/node_modules/x.js', time: BEFORE)
            write(root, 'app/pubspec.yaml', 'name: app')
            write(root, 'app/build/out/app.bin')
            write(root, 'c/CMakeLists.txt', 'project(c)')
            write(root, 'c/build/c')
            write(root, 'notizen/build/plan.txt', 'meine Notiz')
            write(root, 'py/.venv/pyvenv.cfg', 'home = /usr')
            write(root, 'py/requirements.txt', 'requests')
            write(root, 'py/ordner/venv/notiz.txt')
            # An extension's own node_modules: VS Code's, not npm install's.
            npm_project(root, '.extensions/some.extension-1.0.0')
            write(root, '.venv/pyvenv.cfg', 'home = /usr')

            found = run_tool('scan', root).to_h { |f| [f['path'], f] }
            assert_equal %w[.venv app/build c/build geschichte/node_modules lose/node_modules ohne-lock/node_modules
                ohne-vermerk/node_modules py/.venv svelte/node_modules web/node_modules].sort, found.keys.sort
            assert found['svelte/node_modules']['tick'], 'lock file, record, a tool cache inside'
            assert_equal 'npm ci', found['svelte/node_modules']['command']
            assert found['geschichte/node_modules']['tick']
            assert_includes found['geschichte/node_modules']['command'], 'BIF-Erweiterung'
            refute found['web/node_modules']['tick']
            assert_includes found['web/node_modules']['note'], 'lib/b.js'
            refute found['ohne-lock/node_modules']['tick'], 'without a lock file other versions may come'
            assert_equal 'npm install', found['ohne-lock/node_modules']['command']
            refute found['ohne-vermerk/node_modules']['tick'], 'no record of the install'
            refute found['lose/node_modules']['tick'], 'no package.json beside it'
            assert found['app/build']['tick']
            assert_includes found['c/build']['command'], 'cmake -B build'
            refute found['py/.venv']['tick'], 'a Python environment is never certain'

            deleted = run_tool('delete', root, 'svelte/node_modules', 'notizen/build', '../etwas',
                '.extensions/some.extension-1.0.0/node_modules')
            assert_equal ['svelte/node_modules'], deleted
            refute File.exist?(File.join(root, 'svelte/node_modules'))
            assert File.exist?(File.join(root, 'notizen/build/plan.txt'))
            assert File.exist?(File.join(root, '.extensions/some.extension-1.0.0/node_modules/pkg/index.js'))
        end
    end
end
