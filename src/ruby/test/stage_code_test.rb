require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/stage_code'

class StageCodeTest < Minitest::Test
    def setup
        @root = Dir.mktmpdir
        @dir = File.join(@root, 'seite')      # the page's folder; secret.c lies beside it
        FileUtils.mkdir_p(File.join(@dir, 'stufen'))
        File.write(File.join(@dir, 'stufen', '01.c'), "int main(void) { return 1; }\n")
        File.write(File.join(@dir, 'stufen', '02.c'), "int main(void) { return 2; }\n")
        File.write(File.join(@dir, 'stufen', 'light.fs'), "void main() {}\n")
        File.write(File.join(@dir, '..', 'secret.c'), "geheim\n")
    end

    def teardown
        FileUtils.rm_rf(@root)
    end

    def recipe(*writes)
        "<!-- tutorial-screenshot\n" + writes.map { |w| "write-file: #{w}\n" }.join + "tab: preview\n-->\n"
    end

    def test_a_slot_after_the_picture_of_each_recipe
        md = "Text.\n\n" + recipe('main.c <- file:stufen/01.c') + "<img src='a.webp' alt=''>\n\nMehr.\n\n" +
             recipe('main.c <- file:stufen/02.c') + "<img src='b.webp' alt=''>\n"
        out, points = StageCode.mark(md, @dir)
        assert_equal 2, points.size
        assert_equal 'stufen/01.c', points[0].first.source
        assert_equal 'main.c', points[0].first.target
        assert_match(/<img src='a.webp' alt=''>\n\n<div class='stage-code-slot' data-stage='0'><\/div>/, out)
        assert out.index("data-stage='1'") > out.index("b.webp")
    end

    def test_the_same_file_again_gets_no_second_button
        md = recipe('main.c <- file:stufen/01.c') + "<img src='a.webp' alt=''>\n\n" +
             recipe('main.c <- file:stufen/01.c') + "<img src='b.webp' alt=''>\n"
        _, points = StageCode.mark(md, @dir)
        assert_equal 1, points.size
    end

    def test_only_the_changed_file_of_several
        md = recipe('main.c <- file:stufen/01.c', 'light.fs <- file:stufen/light.fs') + "<img src='a.webp' alt=''>\n\n" +
             recipe('main.c <- file:stufen/02.c', 'light.fs <- file:stufen/light.fs') + "<img src='b.webp' alt=''>\n"
        _, points = StageCode.mark(md, @dir)
        assert_equal %w[main.c light.fs], points[0].map(&:target)
        assert_equal %w[main.c], points[1].map(&:target)
    end

    def test_in_a_gallery_after_its_end
        md = "<div class='gallery columns-2'>\n<figure>\n" + recipe('main.c <- file:stufen/01.c') +
             "<img src='a.webp' alt=''>\n<figcaption>A</figcaption>\n</figure>\n<figure>\n" +
             recipe('main.c <- file:stufen/02.c') + "<img src='b.webp' alt=''>\n</figure>\n</div>\n\nDanach.\n"
        out, points = StageCode.mark(md, @dir)
        assert_equal 1, points.size, 'one slot for the gallery, with the last state of each file'
        assert_equal 'stufen/02.c', points[0].first.source
        assert out.index('stage-code-slot') > out.index("</div>\n"), 'after the gallery, not inside a figure'
    end

    def test_before_a_task_card_or_heading_when_no_picture_follows
        md = recipe('main.c <- file:stufen/01.c') + "\n_task(erste)\n\n" + recipe('main.c <- file:stufen/02.c') + "\n## Weiter\n"
        out, points = StageCode.mark(md, @dir)
        assert_equal 2, points.size
        assert out.index("data-stage='0'") < out.index('_task(erste)')
        assert out.index("data-stage='1'") < out.index('## Weiter')
    end

    def test_an_explicit_marker
        md = "Text.\n<!-- stage: stufen/light.fs as shader/light.fs -->\nMehr.\n"
        out, points = StageCode.mark(md, @dir)
        assert_equal 'shader/light.fs', points[0].first.target
        assert out.index('stage-code-slot') > out.index('stage:')
    end

    def test_files_outside_the_page_folder_and_missing_files_are_left_out
        md = recipe('x.c <- file:../secret.c', 'y.c <- file:stufen/fehlt.c', 'z.c <- file:/etc/passwd',
                    'w.c <- file:stufen/../../secret.c') + "<img src='a.webp' alt=''>\n" +
             "<!-- stage: ../secret.c -->\n"
        out, points = StageCode.mark(md, @dir)
        assert_empty points
        refute_includes out, 'stage-code-slot'
    end

    def test_only_text_files_from_stufen_are_steps
        File.write(File.join(@dir, 'hallo.py'), "print('hallo')\n")
        File.binwrite(File.join(@dir, 'stufen', 'bild.png'), "\x89PNG\r\n\x1a\n\0\0")
        File.binwrite(File.join(@dir, 'stufen', 'daten.bin2'), "a\0b")
        md = recipe('hallo.py <- file:hallo.py', 'bild.png <- file:stufen/bild.png', 'daten <- file:stufen/daten.bin2') +
             "<img src='a.webp' alt=''>\n"
        _, points = StageCode.mark(md, @dir)
        assert_empty points, 'an example outside stufen/, a picture and a file with NUL bytes are not steps'
        _, points = StageCode.mark("<!-- stage: hallo.py -->\n", @dir)
        assert_equal 'hallo.py', points[0].first.source, 'a marker may point anywhere in the page folder'
    end

    def test_buttons_escape_the_code_and_strip_removes_everything
        file = StageCode::File_.new(:target => 'raytracer/main.c', :source => 'stufen/01.c', :text => "if (a < b && c) { }\n</template><script>x</script>")
        html = "<p>vor</p>#{StageCode.buttons([file])}<p>nach</p>"
        assert_includes html, 'Code bis hier'
        assert_includes html, '&lt;/template&gt;&lt;script&gt;'
        refute_includes html, '<script>'
        assert_equal '<p>vor</p><p>nach</p>', StageCode.strip(html)
        # as Nokogiri writes it, with double quotes
        rendered = Nokogiri::HTML::DocumentFragment.parse(html).to_html
        assert_equal '<p>vor</p><p>nach</p>', StageCode.strip(rendered)
    end

    def test_several_files_name_each_button
        files = [StageCode::File_.new(:target => 'main.c', :source => 'a', :text => 'a'),
                 StageCode::File_.new(:target => 'light.fs', :source => 'b', :text => 'b')]
        html = StageCode.buttons(files)
        assert_includes html, "</i>main.c</button>"
        assert_includes html, "</i>light.fs</button>"
    end

    def test_a_real_page_gets_buttons
        page = File.expand_path('../../content/raytracing/raytracing.md', __dir__)
        _, points = StageCode.mark(File.read(page), File.dirname(page))
        assert_operator points.size, :>=, 15
        assert(points.all? { |files| files.all? { |f| f.source.start_with?('stufen/') } })
    end
end
