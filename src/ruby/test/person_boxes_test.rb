require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
Dir.chdir(File.expand_path('..', __dir__))
require './include/person_boxes.rb'

class PersonBoxesTest < Minitest::Test
    PAGE = <<~MD
        # Personen

        ## Werkzeuge

        <!-- part: thompson -->
        ### Ken Thompson

        <div class='person'>
        <img src='thompson.webp' alt='Ken Thompson'>
        <p><strong>Ken Thompson</strong> (geboren 1943).</p>
        <p class='person-credit'>Foto: gemeinfrei</p>
        </div>
        <!-- /part -->
    MD

    def with_content
        Dir.mktmpdir do |root|
            FileUtils.mkdir_p(File.join(root, 'reference', 'personen'))
            File.write(File.join(root, 'reference', 'personen', 'personen.md'), PAGE)
            FileUtils.mkdir_p(File.join(root, 'grep'))
            File.write(File.join(root, 'grep', 'grep.md'), "# Suchen mit grep\n\nText.\n\n_person(thompson)\n")
            yield root
        end
    end

    def test_expand_places_the_box_without_its_heading
        with_content do |root|
            html = PersonBoxes.expand(File.read(File.join(root, 'grep', 'grep.md')),
                page_dir: File.join(root, 'grep'), reference_root: File.join(root, 'reference'))
            assert_includes html, "<div class='person'>"
            refute_includes html, '### Ken Thompson'
            refute_includes html, '_person('
            assert_includes html, "src='../reference/personen/thompson.webp'"
        end
    end

    def test_an_unknown_person_warns
        with_content do |root|
            warnings = []
            html = PersonBoxes.expand("_person(nobody)\n", page_dir: root,
                reference_root: File.join(root, 'reference'), warn: ->(m) { warnings << m })
            assert_includes html, 'Person fehlt: nobody'
            assert_equal 1, warnings.size
        end
    end

    def test_the_personen_page_names_the_pages_that_place_a_person
        with_content do |root|
            uses = PersonBoxes.uses([['/grep', File.join(root, 'grep', 'grep.md')]])
            html = PersonBoxes.annotate(PAGE, uses)
            assert_includes html, "<p class='person-pages'>Im Workspace: <a href='/grep'>Suchen mit grep</a></p>\n<p class='person-credit'>"
        end
    end

    def test_a_person_nobody_places_gets_no_line
        assert_equal PAGE, PersonBoxes.annotate(PAGE, Hash.new { |h, k| h[k] = [] })
    end
end
