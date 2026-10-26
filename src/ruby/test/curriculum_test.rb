require 'minitest/autorun'
require 'yaml'
require_relative '../include/curriculum'

# The line of the curriculum (docs/lehrplan.md): its form, and that every
# activity of the catalog has one, apart from those parked.
class CurriculumTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def test_a_right_line
        assert_empty Curriculum.problems('Sek I: Algorithmisches Problemlösen (algorithmische Grundstrukturen: Wiederholung, Verzweigung); Oberstufe: Algorithmen und Datenstrukturen')
        assert_empty Curriculum.problems('Oberstufe: Vertiefung Computergrafik')
        assert_empty Curriculum.problems('Sek I: Information und Daten (Codierung), Datenbanken')
        assert_empty Curriculum.problems('Sek I: Datenbanken (suchen; einfügen); Oberstufe: Datenbanken'), '»; « inside brackets'
    end

    def test_entries
        assert_equal [['Sek I', 'Information und Daten', 'Codierung, Zahlensysteme'], ['Sek I', 'Datenbanken', nil],
                      ['Oberstufe', 'Vertiefung Computergrafik', 'Shader (Fragment)']],
                     Curriculum.entries('Sek I: Information und Daten (Codierung, Zahlensysteme), Datenbanken; Oberstufe: Vertiefung Computergrafik (Shader (Fragment))')
        assert_equal [], Curriculum.entries('Sek I: Kontrollstrukturen')
        assert_equal [], Curriculum.entries(nil)
    end

    def test_wrong_lines
        assert_equal ['no line'], Curriculum.problems('')
        refute_empty Curriculum.problems('Sek I: Kontrollstrukturen (Verzweigung)'), 'a content, not a name of the plan'
        refute_empty Curriculum.problems('Oberstufe: Computergrafik'), 'a Vertiefung is named as one'
        refute_empty Curriculum.problems('Sek I: 3.5 Algorithmisches Problemlösen'), 'no numbers'
        refute_empty Curriculum.problems('Oberstufe: Datenbanken; Sek I: Datenbanken'), 'Sek I first'
        refute_empty Curriculum.problems('Klasse 9: Datenbanken'), 'only the two levels'
        refute_empty Curriculum.problems('Sek I: Datenbanken (Tabellen'), 'a bracket left open'
    end

    def test_every_activity_of_the_catalog_has_a_right_line
        problems = []
        YAML.load_file(File.join(CONTENT, 'sections.yaml')).each do |section|
            next if section['key'] == 'parked'

            (section['entries'] || []).each do |slug|
                text = File.read(File.join(CONTENT, slug, "#{slug}.md"))
                meta = YAML.safe_load(text[/<div class=.meta.>\s*\n(.*?)\n\s*<\/div>/m, 1].to_s) || {}
                Curriculum.problems(meta['curriculum']).each { |problem| problems << "#{slug}: #{problem}" }
            end
        end
        assert_empty problems, problems.join("\n")
    end
end
