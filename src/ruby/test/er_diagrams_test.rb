require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/er_diagrams'

# _include_er draws with the real ER-Werkstatt core, so the test needs node
# (in the Ruby container and in CI); without it, it skips.
class ErDiagramsTest < Minitest::Test
    MODEL = <<~ER
        entity movie
          id: int, primary key
          title: text

        entity copy
          id: int, primary key
          format: text

        relationship has_copy: movie (0,n) : (1,1) copy
    ER

    def setup
        skip 'node is not installed' unless system('node --version > /dev/null 2>&1')
    end

    def expand(line, files = { 'film.er' => MODEL })
        warnings = []
        html = Dir.mktmpdir do |dir|
            files.each { |name, text| File.write(File.join(dir, name), text) }
            ErDiagrams.expand("Vorher\n\n#{line}\n\nNachher\n", :page_dir => dir, :warn => ->(w) { warnings << w })
        end
        [html, warnings]
    end

    def test_every_kind_is_drawn_with_an_alt_text_from_the_model
        html, warnings = expand('_include_er(film.er, chen)')
        assert_empty warnings
        assert_includes html, "<div class='er-figure'><svg"
        assert_includes html, 'aria-label="Chen-Diagramm. Entitäten: movie mit id (Schlüssel), title; copy mit id (Schlüssel), format. Beziehungen: has_copy zwischen movie (0,n) und copy (1,1)."'
        assert_includes html, '>(0,n)<'
        assert html.start_with?("Vorher\n\n") && html.end_with?("\n\nNachher\n")

        html, = expand('_include_er(film.er, tables)')
        assert_includes html, 'aria-label="Tabellendiagramm. Tabellen: movie mit id (Schlüssel), title; copy mit id (Schlüssel), format, movie_id (Fremdschlüssel auf movie)."'

        html, = expand('_include_er(film.er, crowfoot)')
        assert_includes html, 'Krähenfuß-Diagramm.'
        refute_includes html, '<rect x=', 'no white ground: the page colours show through'
    end

    def test_positions_kept_by_the_extension_are_used
        moved = { 'chen' => { 'E:movie' => { 'x' => 1234, 'y' => 567 } } }.to_json
        html, = expand('_include_er(film.er, chen)', 'film.er' => MODEL, '.film.er.layout.json' => moved)
        assert_includes html, 'translate(1234.0,567.0)'
    end

    def test_problems_leave_the_line_and_say_why
        html, warnings = expand('_include_er(film.er, uml)')
        assert_includes html, '_include_er(film.er, uml)'
        assert_match(/no such kind/, warnings.first)

        _, warnings = expand('_include_er(missing.er, chen)')
        assert_match(/no such file/, warnings.first)

        _, warnings = expand('_include_er(film.er, chen)', 'film.er' => "entity movie\n  title text\n")
        assert_match(/the model has \d+ error\(s\), line 2/, warnings.first)
    end
end
