require 'minitest/autorun'
require 'yaml'
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

    # The checks at the end of the anaglyphs write each scene twice, first
    # one a check must reject, then the solution: one button, the solution.
    def test_a_file_written_twice_in_one_recipe_gets_one_button
        md = "<!-- tutorial-step\nwrite-file: main.c <- file:stufen/01.c\ncheck-task-fails: a\n" \
             "write-file: main.c <- file:stufen/02.c\ncheck-task: a\n-->\n"
        _, points = StageCode.mark(md, @dir)
        assert_equal 1, points.size
        assert_equal ['stufen/02.c'], points[0].map(&:source)
    end

    # Path Tracing in the raytracer: the step's picture and, beside it in a
    # gallery, the same scene without it (stufen/vergleich-…). The button is
    # the step's program, not the one changed for the comparison.
    def test_a_comparison_beside_the_step_is_no_step
        File.write(File.join(@dir, 'stufen', 'vergleich-ohne.c'), "int main(void) { return 3; }\n")
        md = "<div class='gallery'>\n<figure>\n" + recipe('main.c <- file:stufen/02.c') + "<img src='mit.webp' alt=''>\n</figure>\n" \
             "<figure>\n" + recipe('main.c <- file:stufen/vergleich-ohne.c') + "<img src='ohne.webp' alt=''>\n</figure>\n</div>\n"
        _, points = StageCode.mark(md, @dir)
        assert_equal [['stufen/02.c']], points.map { |p| p.map(&:source) }
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
        assert_includes html, "</i>main.c bis hier</button>"
        assert_includes html, "</i>light.fs bis hier</button>"
    end

    LONG = (1..12).map { |i| "line #{i}\n" }.join

    def test_markers_in_a_row_share_a_row_of_buttons
        Dir.mktmpdir do |dir|
            Dir.mkdir(File.join(dir, 'stufen'))
            %w[model-1.rb story-1.rb model-2.rb].each { |f| File.write(File.join(dir, 'stufen', f), "# #{f}\n" + LONG) }
            page = "Text\n\n<!-- stage: stufen/model-1.rb as model.rb -->\n<!-- stage: stufen/story-1.rb as story.rb -->\n\nMehr\n\n" \
                   "<!-- stage: stufen/model-2.rb as model.rb -->\n\nEnde\n"
            _, points = StageCode.mark(page, dir)
            assert_equal [%w[model.rb story.rb], %w[model.rb]], points.map { |files| files.map(&:target) }
        end
    end

    def test_a_page_with_several_files_names_the_file_on_every_button
        one = [StageCode::File_.new(:target => 'model.rb', :source => 'a', :text => 'a')]
        two = [StageCode::File_.new(:target => 'story.rb', :source => 'b', :text => 'b')]
        root = Nokogiri::HTML::DocumentFragment.parse("<div class='stage-code-slot' data-stage='0'></div><div class='stage-code-slot' data-stage='1'></div>")
        StageCode.insert(root, [one, two])
        assert_includes root.to_html, '</i>model.rb bis hier</button>'
        assert_includes root.to_html, '</i>story.rb bis hier</button>'
        # the same file in two languages is one file
        root = Nokogiri::HTML::DocumentFragment.parse("<div class='stage-code-slot' data-stage='0'></div><div class='stage-code-slot' data-stage='1'></div>")
        StageCode.insert(root, [one, [StageCode::File_.new(:target => 'model.py', :source => 'c', :text => 'c')]])
        refute_includes root.to_html, '</i>model.rb bis hier</button>'
        assert_includes root.to_html, '</i>Code bis hier</button>'
    end

    def test_a_real_page_gets_buttons
        page = File.expand_path('../../content/raytracing/raytracing.md', __dir__)
        _, points = StageCode.mark(File.read(page), File.dirname(page))
        assert_operator points.size, :>=, 15
        assert(points.all? { |files| files.all? { |f| f.source.nil? || f.source.start_with?('stufen/') } })
    end

    def test_a_recipe_that_writes_a_code_block_has_its_program
        long = "x;\n" * 5
        md = "```c\nint a;\n#{long}```\n\n" + recipe('main.c <- previous-code') + "<img src='a.webp' alt=''>\n\n" +
             "<!-- screenshot-code: zwei -->\n```c\n~ int a;\nint b;\n#{long}```\n\n```c\nint c;\n#{long}```\n\n" +
             recipe('main.c <- previous-code (zwei)') + "<img src='b.webp' alt=''>\n\n" +
             recipe('main.c <- code:int c') + "<img src='c.webp' alt=''>\n\n" +
             recipe('main.c <- code:int c') + "<img src='d.webp' alt=''>\n"
        _, points = StageCode.mark(md, @dir)
        assert_equal ["int a;\n", "int b;\n", "int c;\n"], points.map { |files| files.first.text.lines.first },
                     'the block before, the labelled one without its context line, the one with the text; the same again gets no button'
        assert_nil points.first.first.source
    end

    def test_a_step_without_a_picture_and_the_solution
        FileUtils.mkdir_p(File.join(@dir, 'loesung'))
        File.write(File.join(@dir, 'loesung', 'calc.py'), "print(42)\n")
        md = "```python\nprint(0)\n#{"x = 1\n" * 5}```\n\n<!-- tutorial-step\nwrite-file: calc.py <- previous-code\n-->\nText.\n\n" +
             "<!-- tutorial-step\nwrite-file: calc.py <- file:loesung/calc.py\n-->\n"
        out, points = StageCode.mark(md, @dir)
        assert_equal ["print(0)\n", "print(42)\n"], points.map { |f| f.first.text.lines.first }
        assert out.index("data-stage='0'") < out.index('Text.'), 'right after the step'
    end

    def test_a_file_outside_stufen_counts_when_the_page_writes_it_again
        five = "a = 1\nb = 2\nc = 3\nd = 4\ne = 5\n"
        File.write(File.join(@dir, 'step1.lua'), five)
        File.write(File.join(@dir, 'step2.lua'), five + "f = 6\n")
        File.write(File.join(@dir, 'beispiel.lua'), five + "g = 7\n")
        md = recipe('stars.lua <- file:step1.lua') + "<img src='a.webp' alt=''>\n\n" +
             recipe('stars.lua <- file:step2.lua') + "<img src='b.webp' alt=''>\n\n" +
             recipe('other.lua <- file:beispiel.lua') + "<img src='c.webp' alt=''>\n"
        _, points = StageCode.mark(md, @dir)
        assert_equal %w[step1.lua step2.lua], points.map { |f| f.first.source }, 'a one-off example is no step'
    end

    def test_examples_in_included_handbuch_parts_are_no_steps
        block = "```html\n<p>1</p>\n<p>2</p>\n<p>3</p>\n<p>4</p>\n<p>5</p>\n```\n\n"
        md = "\n<div class='reference-include-begin'></div>\n\n" + block + recipe('index.html <- previous-code') +
             "<img src='a.webp' alt=''>\n\n<div class='reference-include-end' data-part='html#tags'></div>\n"
        _, points = StageCode.mark(md, @dir)
        assert_empty points, 'the example in the part gives no button'
    end

    def test_previous_code_skips_the_blocks_of_included_parts
        own = "```html\n<h1>eigene</h1>\n<p>1</p>\n<p>2</p>\n<p>3</p>\n<p>4</p>\n```\n\n"
        part = "```html\n<h1>aus dem Handbuch</h1>\n<p>1</p>\n<p>2</p>\n<p>3</p>\n<p>4</p>\n```\n\n"
        md = own + "\n<div class='reference-include-begin'></div>\n\n" + part +
             "<div class='reference-include-end' data-part='html#tags'></div>\n\n" +
             recipe('seite/index.html <- previous-code') + "<img src='b.webp' alt=''>\n"
        _, points = StageCode.mark(md, @dir)
        assert_includes points.first.first.text, 'eigene', 'as the generator, which reads the page without its included parts'
    end

    def test_short_programs_are_no_steps
        md = "```python\nprin('hallo')\n```\n\n" + recipe('hello.py <- previous-code') + "<img src='a.webp' alt=''>\n"
        _, points = StageCode.mark(md, @dir)
        assert_empty points
    end

    def test_a_code_block_written_as_notes_is_no_program
        md = "```markdown\n# Seite\n```\n\n" + recipe('pages/1.md <- previous-code') + "<img src='a.webp' alt=''>\n"
        _, points = StageCode.mark(md, @dir)
        assert_empty points
    end

    # Where students program in files in the Workspace, a teacher gets the
    # code up to each step (docs/checkliste.md). Not where queries are typed
    # into a terminal (SQL) or a team works in Git; OPEN still lacks them.
    CODE_CHECKS = %w[ruby python c java dart javascript prolog].freeze
    NO_FILES = %w[mysql filmdatenbank class-quiz].freeze
    OPEN = %w[check-digits].freeze

    def test_activities_where_students_program_have_steps
        content = File.expand_path('../../content', __dir__)
        catalog = YAML.load_file(File.join(content, 'sections.yaml')).reject { |s| s['hidden'] }
            .flat_map { |s| s['entries'] || [] }.map(&:to_s)
        without = catalog.select do |slug|
            page = File.join(content, slug, "#{slug}.md")
            next false unless File.file?(page)
            markdown = File.read(page)
            next false if markdown[/^kind:\s*(\S+)/, 1] == 'steckbrief' || slug =~ /\A(?:kata-|pip-|turtle-)/
            tasks = (YAML.load_file(File.join(content, slug, 'tasks.yaml'))['tasks'] rescue nil) || []
            programs = tasks.any? { |t| CODE_CHECKS.include?(t.dig('check', 'type')) } || markdown.include?("class='hint own'")
            next false unless programs
            StageCode.mark(markdown, File.dirname(page))[1].empty?
        end
        assert_empty without - NO_FILES - OPEN, "Activities where students program, without code up to a step for teachers:\n#{(without - NO_FILES - OPEN).join("\n")}"
        assert_empty OPEN - without, "Remove these from OPEN, they have steps now: #{(OPEN - without).join(', ')}"
    end
end
