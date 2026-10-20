require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/backrooms_demo'

# Only files inside the exported folder, with a known type, no hidden ones.
class BackroomsDemoTest < Minitest::Test
    def with_site
        Dir.mktmpdir do |dir|
            root = File.join(dir, 'nachtschicht')
            FileUtils.mkdir_p(File.join(root, 'engine'))
            File.write(File.join(root, 'index.html'), '<!doctype html>')
            File.write(File.join(root, 'engine', 'main.js'), '')
            File.write(File.join(root, 'engine', '.secret.js'), '')
            File.write(File.join(root, 'notes.sh'), '')
            File.write(File.join(dir, 'outside.js'), '')
            File.symlink(File.join(dir, 'outside.js'), File.join(root, 'engine', 'link.js'))
            yield root
        end
    end

    def test_page_and_files
        with_site do |root|
            assert_equal File.join(root, 'index.html'), BackroomsDemo.file('', root: root)
            assert_equal File.join(root, 'engine', 'main.js'), BackroomsDemo.file('engine/main.js', root: root)
            assert_equal 'application/javascript; charset=utf-8', BackroomsDemo.mime('engine/main.js')
        end
    end

    def test_refuses_what_is_not_part_of_the_site
        with_site do |root|
            assert_nil BackroomsDemo.file('../outside.js', root: root)
            assert_nil BackroomsDemo.file('engine/.secret.js', root: root)
            assert_nil BackroomsDemo.file('engine/link.js', root: root)
            assert_nil BackroomsDemo.file('notes.sh', root: root)
            assert_nil BackroomsDemo.file('/etc/passwd', root: root)
            assert_nil BackroomsDemo.file('engine//main.js', root: root)
            assert_nil BackroomsDemo.file('missing.js', root: root)
        end
    end

    def test_without_the_folder
        assert_nil BackroomsDemo.file('index.html', root: '/nonexistent/nachtschicht')
    end
end
