require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/page_code'

# The code a page shows is the code its screenshot recipes write and the
# task examples test (PageCode).
class PageCodeTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def test_pages_show_the_code_their_recipes_write
        # Handbuch pages show examples, not steps toward a recipe's files.
        pages = Dir[File.join(CONTENT, '**', '*.md')].reject { |f| f.start_with?(File.join(CONTENT, 'reference/')) }
        mismatches = pages.sort.flat_map { |f| PageCode.check(f) }
        assert_empty mismatches, "\n" + mismatches.map { |m|
            "#{m.page.delete_prefix("#{CONTENT}/")}:#{m.line}: the code block is not in #{m.source}: #{m.reason}"
        }.join("\n")
    end

    def test_excerpts_may_leave_out_code_and_indentation
        file = "def a\n  x = 1\n  y = 2\n  z = 3\nend\n"
        assert_nil PageCode.excerpt_problem("x = 1\ny = 2\n", file)
        assert_nil PageCode.excerpt_problem("def a\n  …\n  z = 3\nend\n", file)
        refute_nil PageCode.excerpt_problem("y = 2\nx = 1\n", file), 'the order counts'
        assert_match(/w = 4/, PageCode.excerpt_problem("x = 1\nw = 4\n", file))
    end

    # Context lines stand for code around the new code; they are not checked
    def test_context_lines_are_left_out_code
        file = "void f(void) {\n    int a = 1;\n    int b = 2;\n}\n"
        assert_nil PageCode.excerpt_problem("~ void f(void) {\n    int b = 2;\n~ }\n", file)
        assert_nil PageCode.excerpt_problem("~ void g(void) {\n    int b = 2;\n", file)
        refute_nil PageCode.excerpt_problem("~ void f(void) {\n    int c = 3;\n", file)
    end

    def test_a_recipe_without_its_code_is_reported
        Dir.mktmpdir do |dir|
            File.write(File.join(dir, 'server.rb'), "puts 'neu'\n")
            page = File.join(dir, 'demo.md')
            File.write(page, <<~MD)
                ```ruby
                puts 'alt'
                ```

                <!-- tutorial-screenshot
                write-file: demo/server.rb <- file:server.rb
                -->
            MD
            assert_equal 1, PageCode.check(page).size
            File.write(page, File.read(page).sub("```ruby", "<!-- page-code: own -->\n```ruby"))
            assert_empty PageCode.check(page)
        end
    end

    def test_a_stage_marker_counts_like_a_recipe
        Dir.mktmpdir do |dir|
            Dir.mkdir(File.join(dir, 'stufen'))
            File.write(File.join(dir, 'stufen', '01.c'), "int main(void)\n{\n    return 0;\n}\n")
            page = File.join(dir, 'demo.md')
            File.write(page, "```c\n    return 1;\n```\n\n<!-- stage: stufen/01.c as main.c -->\n")
            assert_equal 1, PageCode.check(page).size
            File.write(page, "```c\n    return 0;\n```\n\n<!-- stage: stufen/01.c as main.c -->\n")
            assert_empty PageCode.check(page)
        end
    end
end
