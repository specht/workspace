require 'minitest/autorun'
require 'fileutils'
require 'tmpdir'
require_relative '../include/submission_view'

class SubmissionViewTest < Minitest::Test
    KEY = 'a' * 32
    SECRET = 'geheim'

    def setup
        @dir = Dir.mktmpdir
        @archive = File.join(@dir, 'abgabe.tar.gz')
        File.open(@archive, 'wb') do |out|
            gz = Zlib::GzipWriter.new(out)
            Gem::Package::TarWriter.new(gz) do |tar|
                { 'feuerwerk/index.html' => '<!DOCTYPE html><p>Bumm</p>', 'feuerwerk/script.js' => 'go();',
                  'feuerwerk/bild.png' => "\x89PNG".b }.each do |path, data|
                    tar.add_file_simple(path, 0o644, data.bytesize) { |io| io.write(data) }
                end
            end
            gz.finish
        end
    end

    def teardown
        FileUtils.rm_rf(@dir)
    end

    def test_tokens
        token = SubmissionView.token(KEY, :secret => SECRET, :now => 1000)
        assert_equal KEY, SubmissionView.verify(token, :secret => SECRET, :now => 2000)
        assert_nil SubmissionView.verify(token, :secret => SECRET, :now => 1000 + SubmissionView::LIFETIME + 1)
        assert_nil SubmissionView.verify(token, :secret => 'anders', :now => 2000)
        assert_nil SubmissionView.verify(token.sub(KEY, 'b' * 32), :secret => SECRET, :now => 2000)
        expires = 1000 + SubmissionView::LIFETIME
        assert_nil SubmissionView.verify(token.sub(".#{expires}.", ".#{expires + 9999}."), :secret => SECRET, :now => 2000)
        assert_nil SubmissionView.verify('../etc', :secret => SECRET)
    end

    def test_paths
        assert_equal 'a/b.html', SubmissionView.clean_path('/a/./b.html')
        assert_nil SubmissionView.clean_path('a/../../b')
        assert_nil SubmissionView.clean_path('')
        files = SubmissionView.files(@archive)
        assert_equal %w[feuerwerk/index.html feuerwerk/script.js feuerwerk/bild.png], files.map { |f| f[:path] }
        assert_equal 'feuerwerk/index.html', SubmissionView.resolve(files, 'feuerwerk')
        assert_nil SubmissionView.resolve(files, 'anderes')
    end

    def test_reading_and_running
        assert_equal 'go();', SubmissionView.read(@archive, 'feuerwerk/script.js')
        assert_nil SubmissionView.read(@archive, 'fehlt.js')
        assert_nil SubmissionView.read(@archive, 'feuerwerk/script.js', :max => 2)
        page = SubmissionView.for_viewer('feuerwerk/index.html', SubmissionView.read(@archive, 'feuerwerk/index.html'))
        assert page.start_with?('<script>')
        assert page.end_with?('<p>Bumm</p>')
        assert_equal 'go();', SubmissionView.for_viewer('feuerwerk/script.js', 'go();')
    end

    def test_headers_sandbox_every_answer
        h = SubmissionView.headers('x/index.html', 'http://workspace.test:8025')
        assert_equal 'text/html; charset=utf-8', h['Content-Type']
        assert_match(/\Asandbox allow-scripts /, h['Content-Security-Policy'])
        refute_includes h['Content-Security-Policy'], 'allow-same-origin'
        assert_includes h['Content-Security-Policy'], 'frame-ancestors http://workspace.test:8025'
        assert_equal 'application/octet-stream', SubmissionView.headers('x/datei.exe', 'x')['Content-Type']
        assert SubmissionView.text?('a/b.ini')
        refute SubmissionView.text?('a/b.png')
        # the sources of the activities, also by name
        %w[TIC-80/spiel.lua solar-system/hugo.toml src/routes/+page.svelte raylib-starter/Makefile].each do |path|
            assert SubmissionView.text?(path), path
            assert_equal 'text/plain; charset=utf-8', SubmissionView.content_type(path), path
        end
        assert_equal 'application/wasm', SubmissionView.content_type('raylib-starter/main.wasm')
        # the one exception: a PDF, which Chrome shows only outside a sandbox
        pdf = SubmissionView.headers('facharbeit/facharbeit.pdf', 'http://workspace.test:8025')
        assert_equal 'application/pdf', pdf['Content-Type']
        assert_equal 'frame-ancestors http://workspace.test:8025', pdf['Content-Security-Policy']
        assert_equal 'nosniff', pdf['X-Content-Type-Options']
    end
end
