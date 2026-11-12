require 'minitest/autorun'
require 'yaml'
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

    # A title written with an entity (C&#35;) is escaped once, not twice.
    def test_a_title_with_an_entity_is_shown_as_text
        with_content do |root|
            File.write(File.join(root, 'grep', 'grep.md'), "# C&#35; <span style='font-size: 80%;'>(2000)</span>\n\n_person(thompson)\n")
            uses = PersonBoxes.uses([['/cs', File.join(root, 'grep', 'grep.md')]])
            html = PersonBoxes.annotate(PAGE, uses)
            assert_includes html, "<a href='/cs'>C# (2000)</a>"
        end
    end

    def test_a_box_without_a_credit_gets_the_line_at_its_end
        page = PAGE.sub("<p class='person-credit'>Foto: gemeinfrei</p>\n", '')
        uses = { 'thompson' => [{ :url => '/grep', :title => 'Suchen mit grep' }] }
        assert_includes PersonBoxes.annotate(page, uses), "<p class='person-pages'>Im Workspace: <a href='/grep'>Suchen mit grep</a></p>\n</div>\n<!-- /part -->"
    end

    def test_a_person_nobody_places_gets_no_line
        assert_equal PAGE, PersonBoxes.annotate(PAGE, Hash.new { |h, k| h[k] = [] })
    end

    # Every person on the Personen page stands on at least one page a reader
    # may be led to (not hidden, not parked, not only in development), so
    # that every box there has its line »Im Workspace«.
    CONTENT = File.expand_path('../../content', __dir__)

    def test_every_person_is_placed_on_a_visible_page
        visible = []
        YAML.load_file(File.join(CONTENT, 'sections.yaml')).each do |section|
            next if %w[hidden parked].include?(section['key'])
            Array(section['entries']).map(&:to_s).reject { |e| e.start_with?('.') }.each { |e| visible << File.join(CONTENT, e, "#{e}.md") }
        end
        YAML.load_file(File.join(CONTENT, 'reference', 'sections.yaml')).each do |section|
            Array(section['entries']).map(&:to_s).reject { |e| e.start_with?('.') }.each { |e| visible << File.join(CONTENT, 'reference', e, "#{e}.md") }
        end
        placed = visible.select { |f| File.file?(f) }.flat_map { |f| File.read(f).scan(PersonBoxes::PERSON).flatten }.uniq
        people = File.read(File.join(CONTENT, 'reference', 'personen', 'personen.md')).scan(ContentParts::PART_MARKER).flatten
        assert_empty people - placed, 'people on the Personen page without a visible page that places them'
    end

    # A box right after a part marker would stand inside that part, before its
    # heading: the heading would lose the part's anchor, and an _include of
    # the part would bring the box along.
    def test_no_box_starts_a_part
        Dir[File.join(CONTENT, '**', '*.md')].each do |path|
            text = File.read(path)
            refute_match(/<!--\s*part:[^\n]*-->\n(?:<!--\s*short:[^\n]*-->\n)?\s*_person\(/, text, path)
        end
    end
end
