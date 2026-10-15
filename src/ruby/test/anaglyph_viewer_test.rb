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

    VIEWER_SCRIPT = <<~JS
        function sendPictureWhenReady() {
            window.parent.postMessage({
                type: 'anaglyph-picture',
                scene: pictureScene,
            }, window.location.origin);
        }
        window.parent.postMessage({ type: 'anaglyph-key', key: e.key }, window.location.origin);
    JS

    def test_sandbox_patches_the_messages
        path = File.join(AnaglyphViewer.dir, 'include', 'script.js')
        out = AnaglyphViewer.for_sandbox(path, VIEWER_SCRIPT.b)
        refute_includes out, 'window.location.origin'
        assert_equal 2, out.scan("'*');").size
        assert_includes out, 'objects: Array.isArray(sceneDescription)'
    end

    def test_sandbox_fails_closed_on_another_viewer
        path = File.join(AnaglyphViewer.dir, 'include', 'script.js')
        assert_raises(AnaglyphViewer::Unpatchable) { AnaglyphViewer.for_sandbox(path, 'console.log(1)') }
    end

    def test_sandbox_index_gets_the_storage_stand_in
        path = File.join(AnaglyphViewer.dir, 'index.html')
        out = AnaglyphViewer.for_sandbox(path, "<!DOCTYPE html><html><head><title>x</title></head></html>")
        assert out.start_with?('<!DOCTYPE html><html><head><script>')
        assert_equal 'other', AnaglyphViewer.for_sandbox(File.join(AnaglyphViewer.dir, 'x.js'), 'other')
    end

    def test_scene_tokens
        token = AnaglyphViewer.scene_token({ 's' => 'a' * 32 }, :secret => 'k', :now => 1000)
        assert_equal({ 's' => 'a' * 32 }, AnaglyphViewer.verify_scene_token(token, :secret => 'k', :now => 1000))
        assert_nil AnaglyphViewer.verify_scene_token(token, :secret => 'other', :now => 1000)
        assert_nil AnaglyphViewer.verify_scene_token(token, :secret => 'k', :now => 1000 + SubmissionView::LIFETIME + 1)
        payload, expires, sig = token.split('.')
        forged = Base64.urlsafe_encode64(JSON.generate({ 'e' => 'x@y', 'f' => 'a.ini' }), padding: false)
        assert_nil AnaglyphViewer.verify_scene_token("#{forged}.#{expires}.#{sig}", :secret => 'k', :now => 1000)
        view = SubmissionView.token('a' * 32, :secret => 'k', :now => 1000)
        assert_nil AnaglyphViewer.verify_scene_token(view, :secret => 'k', :now => 1000)
    end
end
