require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/file_checks'

class FileChecksTest < Minitest::Test
    TEMPLATE_TITLES = ['Titel deiner Präsentation', 'Listen sind super', 'Zitate']

    def slide(title, body = '<p>Text</p>')
        "<section class=\"slide\">\n<h2>#{title}</h2>\n#{body}\n</section>\n"
    end

    def page(slides)
        <<~HTML
            <!DOCTYPE html>
            <html lang="de">
            <head>
            <meta charset="utf-8">
            <link rel="stylesheet" href="styles.css">
            <link rel="stylesheet" href="shower/themes/ribbon/styles.css">
            </head>
            <body class="shower list">
            #{slides.join}
            <script src="shower/shower.min.js"></script>
            </body>
            </html>
        HTML
    end

    def setup
        @tmp = Dir.mktmpdir
        @root = File.join(@tmp, 'workspace')
        @dir = File.join(@root, 'shower.js')
        FileUtils.mkdir_p(File.join(@dir, 'pictures'))
        FileUtils.mkdir_p(File.join(@dir, 'shower', 'themes', 'ribbon'))
        File.write(File.join(@dir, 'shower', 'shower.min.js'), '//')
        File.write(File.join(@dir, 'shower', 'themes', 'ribbon', 'styles.css'), 'body { margin: 0; }')
        %w[fuji.jpg monkey.jpg].each { |n| File.binwrite(File.join(@dir, 'pictures', n), "\xFF\xD8\xFF\xE0".b + 'x' * 100) }
        File.write(File.join(@dir, 'styles.css'), ".shower { --x: 1; }\n/* Eigene CSS-Regeln */\n")
        write_page([slide('Titel deiner Präsentation', '<p>Dein Name</p>'), slide('Listen sind super'), slide('Zitate')])
        @ws = WorkspaceFiles.new(@root)
        @tasks = TaskSpec.load_file(File.expand_path('../../content/showerjs/tasks.yaml', __dir__))['tasks'].to_h { |t| [t['id'], t['check']] }
    end

    def teardown
        FileUtils.rm_rf(@tmp)
    end

    def write_page(slides)
        File.write(File.join(@dir, 'index.html'), page(slides))
    end

    def run_check(id)
        check = @tasks.fetch(id)
        FileChecks.send(check['type'], check, @ws)
    end

    def test_format_detection
        assert_equal 'png', FileChecks.detect_format("\x89PNG\r\n\x1A\n...".b, 100)
        assert_equal 'jpeg', FileChecks.detect_format("\xFF\xD8\xFF\xE0".b, 100)
        assert_equal 'webp', FileChecks.detect_format('RIFF1234WEBPVP8 '.b, 100)
        assert_equal 'elf', FileChecks.detect_format("\x7FELF\x02\x01".b, 100)
        assert_equal 'svg', FileChecks.detect_format('<?xml version="1.0"?><svg xmlns="x">'.b, 100)
        assert_equal 'text', FileChecks.detect_format("Hallo Welt\n".b, 11)
        assert_equal 'empty', FileChecks.detect_format('', 0)
        assert_equal 'unknown', FileChecks.detect_format("\x00\x01\x02".b, 3)
    end

    def test_template_folder_exists
        outcome = run_check('vorlage')
        assert outcome.ok
        assert_includes outcome.read, 'index.html'
        assert_includes outcome.read, 'Ordner'
        FileUtils.rm_rf(@dir)
        outcome = run_check('vorlage')
        refute outcome.ok
        assert_equal :missing, outcome.problem
    end

    def test_missing_folder_with_other_case
        FileUtils.mv(@dir, File.join(@root, 'Shower.js'))
        outcome = run_check('vorlage')
        assert_equal :missing, outcome.problem
        assert_includes outcome.message, '`Shower.js`'
    end

    def test_first_slide
        outcome = run_check('titelfolie')
        assert_equal :first_slide_unchanged, outcome.problem
        assert_includes outcome.message, '»Titel deiner Präsentation« und »Dein Name«'
        write_page([slide('Vulkane', '<p>Lena Muster</p>'), slide('Zitate')])
        assert run_check('titelfolie').ok
    end

    def test_five_own_slides
        outcome = run_check('fuenf-folien')
        assert_equal :too_few_slides, outcome.problem
        assert_includes outcome.message, 'Ich finde 0 eigene Folien, verlangt sind 5'
        assert_includes outcome.read, '3 Folien'
        assert_includes outcome.read, '»Listen sind super«'

        write_page([slide('Vulkane'), slide('Magma'), slide('Ausbrüche'), slide('Fuji'), slide('Quellen'), slide('Zitate')])
        outcome = run_check('fuenf-folien')
        assert outcome.ok, outcome.message
        assert_match(/noch 1 Folien drin/, outcome.warnings.first)

        write_page([slide('Vulkane'), slide('Magma'), slide('Ausbrüche'), slide('Fuji'), "<section class=\"slide\"><p>ohne</p></section>"])
        outcome = run_check('fuenf-folien')
        assert_equal :slide_without_heading, outcome.problem

        write_page([slide('Vulkane', '<p>' + (%w[Wort] * 80).join(' ') + '</p>'), slide('Magma'), slide('Ausbrüche'), slide('Fuji'), slide('Quellen')])
        outcome = run_check('fuenf-folien')
        assert outcome.ok
        assert_match(/»Vulkane« stehen 81 Wörter/, outcome.warnings.join)
    end

    def test_parse_errors_name_lines
        File.write(File.join(@dir, 'index.html'), "<section class=\"slide\">\n<h2>A</h2>\n<div>\n</section>\n")
        outcome = run_check('fuenf-folien')
        assert_equal :parse_error, outcome.problem
        assert_includes outcome.message, 'Zeile 4'
        assert_includes outcome.message, '`<div>` aus Zeile 3'
    end

    def test_own_picture
        outcome = run_check('eigenes-bild')
        assert_equal :no_new_files, outcome.problem
        assert_includes outcome.message, 'nur Dateien aus der Vorlage'

        File.binwrite(File.join(@dir, 'pictures', 'Mein Bild.JPG'), "\xFF\xD8\xFF".b)
        outcome = run_check('eigenes-bild')
        assert_equal :bad_name, outcome.problem
        assert_includes outcome.message, 'Leerzeichen, Großbuchstaben'
        File.delete(File.join(@dir, 'pictures', 'Mein Bild.JPG'))

        File.binwrite(File.join(@dir, 'pictures', 'classroom.webp'), "\x89PNG\r\n\x1A\nrest".b)
        outcome = run_check('eigenes-bild')
        assert_equal :format_mismatch, outcome.problem
        assert_includes outcome.message, 'ein PNG-Bild'
        assert_includes outcome.message, '89 50 4E 47'
        File.delete(File.join(@dir, 'pictures', 'classroom.webp'))

        File.binwrite(File.join(@dir, 'pictures', 'vulkan.jpg'), "\xFF\xD8\xFF".b + 'x' * 600_000)
        outcome = run_check('eigenes-bild')
        assert_equal :too_big, outcome.problem
        assert_match(/600 kB groß; erlaubt sind höchstens 500 kB/, outcome.message)
        File.binwrite(File.join(@dir, 'pictures', 'vulkan.jpg'), "\xFF\xD8\xFF".b + 'x' * 1000)
        outcome = run_check('eigenes-bild')
        assert outcome.ok
        assert_includes outcome.read, 'vulkan.jpg'
    end

    def test_image_on_slide
        outcome = run_check('bild-auf-folie')
        assert_equal :too_few_images, outcome.problem

        write_page([slide('Vulkane', '<img src="pictures/Fuji.jpg" alt="Der Fuji">')])
        outcome = run_check('bild-auf-folie')
        assert_equal :image_case, outcome.problem
        assert_includes outcome.message, '`pictures/fuji.jpg`'

        write_page([slide('Vulkane', '<img src="pictures/fuji.jpg">')])
        assert_equal :alt_missing, run_check('bild-auf-folie').problem

        write_page([slide('Vulkane', '<img src="pictures/etna.jpg" alt="Etna">')])
        assert_equal :image_missing, run_check('bild-auf-folie').problem

        write_page([slide('Vulkane', '<img src="./pictures/fuji.jpg?v=2" alt="Der Fuji">')])
        assert run_check('bild-auf-folie').ok
    end

    def test_own_css_rule
        outcome = run_check('eigene-regel')
        assert_equal :too_few_rules, outcome.problem
        assert_includes outcome.message, 'unter »Eigene CSS-Regeln« 0 Regeln'

        File.write(File.join(@dir, 'styles.css'), ".shower { --x: 1; }\n/* Eigene CSS-Regeln */\nh2 {\n  color: red\n  font-size: 2em;\n}\n")
        outcome = run_check('eigene-regel')
        assert_equal :parse_error, outcome.problem
        assert_includes outcome.message, 'Zeile 4'

        File.write(File.join(@dir, 'styles.css'), ".shower { --x: 1; }\n/* Eigene CSS-Regeln */\nh2 { color: red; }\n")
        assert run_check('eigene-regel').ok

        File.write(File.join(@dir, 'styles.css'), "h2 { color: red; }\n")
        assert_equal :marker_missing, run_check('eigene-regel').problem
    end

    def test_everything_in_the_folder
        assert run_check('alles-im-ordner').ok, run_check('alles-im-ordner').message

        File.write(File.join(@dir, 'styles.css'), "@import url('https://fonts.googleapis.com/css2?family=Roboto');\n/* Eigene CSS-Regeln */\n")
        outcome = run_check('alles-im-ordner')
        assert_equal :external, outcome.problem
        assert_includes outcome.message, 'fonts.googleapis.com'

        File.write(File.join(@dir, 'styles.css'), "h1 { background: url(../bild.png); }\n")
        assert_equal :outside_root, run_check('alles-im-ordner').problem

        File.write(File.join(@dir, 'styles.css'), "h1 { background: url(pictures/Monkey.jpg); }\n")
        outcome = run_check('alles-im-ordner')
        assert_equal :local_missing, outcome.problem
        assert_includes outcome.message, '`pictures/monkey.jpg`'
    end

    def test_executable_file
        check = { 'type' => 'files', 'path' => '2048', 'expect' => { 'exists' => true, 'executable' => true, 'format' => 'elf' } }
        assert_equal :missing, FileChecks.files(check, @ws).problem
        File.write(File.join(@root, '2048'), "#include <stdio.h>\n")
        outcome = FileChecks.files(check, @ws)
        assert_equal :wrong_format, outcome.problem
        assert_includes outcome.message, 'eine Textdatei'
        File.binwrite(File.join(@root, '2048'), "\x7FELF\x02\x01\x01".b)
        assert_equal :not_executable, FileChecks.files(check, @ws).problem
        File.chmod(0o755, File.join(@root, '2048'))
        assert FileChecks.files(check, @ws).ok
    end

    def test_links_are_not_followed
        File.symlink('/etc', File.join(@dir, 'pictures', 'etc'))
        File.symlink('/etc/hostname', File.join(@dir, 'index2.html'))
        check = { 'type' => 'html', 'file' => 'shower.js/index2.html', 'expect' => { 'parses' => true } }
        assert_equal :symlink, FileChecks.html(check, @ws).problem
        outcome = run_check('eigenes-bild')
        assert_includes outcome.read, 'Link (wird nicht gelesen)'
    end
end
