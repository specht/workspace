require 'minitest/autorun'
require 'rubygems/package'
require 'stringio'
require 'tmpdir'
require 'zlib'
require_relative '../include/anaglyph_viewer'

class AnaglyphViewerTest < Minitest::Test
    def submission(files)
        io = StringIO.new
        Zlib::GzipWriter.wrap(io) do |gz|
            Gem::Package::TarWriter.new(gz) do |tar|
                files.each do |name, text|
                    tar.add_file_simple(name, 0o644, text.bytesize) { |f| f.write(text) }
                end
            end
        end
        path = File.join(Dir.mktmpdir, 'submission.tar.gz')
        File.binwrite(path, io.string)
        path
    end

    def test_reads_the_scene_from_a_submission
        path = submission('anaglyph/README.md' => '# x', 'anaglyph/szene.ini' => "camera = 20, 20, -300\nshape = box\n")
        assert_equal "camera = 20, 20, -300\nshape = box\n", AnaglyphViewer.scene_from_submission(path)
    end

    def test_a_submission_without_a_scene_has_none
        assert_nil AnaglyphViewer.scene_from_submission(submission('anaglyph/notes.txt' => 'hi'))
    end

    def test_refuses_paths_outside_the_viewer_before_fetching_anything
        ['', '/etc/passwd', '../secret', 'include/../../x', '.git/config', 'car-kit/.hidden', 'a//b'].each do |rel|
            assert_nil AnaglyphViewer.file(rel), rel
        end
    end

    def test_knows_the_mime_types_the_viewer_needs
        assert_equal 'application/javascript; charset=utf-8', AnaglyphViewer.mime('include/script.js')
        assert_equal 'image/png', AnaglyphViewer.mime('car-kit/textures/ambulance.png')
        assert_equal 'text/plain; charset=utf-8', AnaglyphViewer.mime('car-kit/ambulance.obj')
        assert_equal 'application/octet-stream', AnaglyphViewer.mime('x.bin')
    end
end
