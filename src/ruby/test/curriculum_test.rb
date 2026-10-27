require 'minitest/autorun'
require 'yaml'
require_relative '../include/curriculum'

# The line of the curriculum (docs/lehrplan.md) and the plans of the states
# (docs/plan-bundeslaender.md): the form of a line, that every activity of
# the catalog has one and topics, apart from those parked, and that every
# plan names only what it has.
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

    def test_linked_html
        assert_equal "Sek I: <a href='/lehrplan#sek-i-information-und-daten'>Information und Daten</a> (Codierung), " \
                     "<a href='/lehrplan#sek-i-datenbanken'>Datenbanken</a>; " \
                     "Oberstufe: <a href='/lehrplan#oberstufe-vertiefung-computergrafik'>Vertiefung Computergrafik</a> (&lt;Shader&gt;)",
                     Curriculum.linked_html('Sek I: Information und Daten (Codierung), Datenbanken; Oberstufe: Vertiefung Computergrafik (<Shader>)')
        assert_equal 'Sek I: Kontrollstrukturen &amp; mehr', Curriculum.linked_html('Sek I: Kontrollstrukturen & mehr'), 'not right: text'
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

    # The catalog's activities (not those parked): [slug, meta].
    def activities
        YAML.load_file(File.join(CONTENT, 'sections.yaml')).flat_map do |section|
            next [] if section['key'] == 'parked'

            (section['entries'] || []).map do |entry|
                slug = entry.delete_prefix('.')   # .slug: in development only
                text = File.read(File.join(CONTENT, slug, "#{slug}.md"))
                [slug, YAML.safe_load(text[/<div class=.meta.>\s*\n(.*?)\n\s*<\/div>/m, 1].to_s) || {}]
            end
        end
    end

    def test_every_activity_of_the_catalog_has_a_right_line
        problems = []
        activities.each do |slug, meta|
            Curriculum.problems(meta['curriculum']).each { |problem| problems << "#{slug}: #{problem}" }
        end
        assert_empty problems, problems.join("\n")
    end

    def test_every_activity_names_topics_of_the_list
        problems = []
        activities.each do |slug, meta|
            topics = Array(meta['topics']).map(&:to_s)
            problems << "#{slug}: no topics:" if topics.empty?
            (topics - Curriculum.topics.keys).each { |t| problems << "#{slug}: »#{t}« is no topic of src/content/curricula/themen.yaml" }
        end
        assert_empty problems, problems.join("\n")
    end

    # Berlin and Brandenburg see the meta line itself, as before there were
    # other states; a teacher without a choice too.
    def test_berlin_and_brandenburg_see_the_meta_line
        entry = { :curriculum => 'Oberstufe: Vertiefung Computergrafik', :topics => %w[computergrafik] }
        %w[berlin brandenburg nirgendwo].each do |state|
            assert_equal entry[:curriculum], Curriculum.line(Curriculum.plan_for(state), 'x', entry), state
        end
        assert_equal Curriculum.plan_for('berlin'), Curriculum.plan_for(nil)
        assert_equal Curriculum.plan_for('berlin'), Curriculum.plan_for('brandenburg')
        assert_equal 'Rahmenlehrplan', Curriculum.default_plan[:label]
        refute Curriculum.draft?(Curriculum.default_plan)
    end

    # Every plan: states, levels with sources, and topics that lead only to
    # its own names; its own lines right in its names.
    def test_every_plan_names_only_what_it_has
        problems = []
        Curriculum.plans.each do |plan|
            problems << "#{plan[:key]}: no states" if plan[:states].empty?
            plan[:levels].each { |l| problems << "#{plan[:key]}: #{l[:level]} without a source" if l[:source].empty? }
            plan[:topics].each do |topic, places|
                problems << "#{plan[:key]}: »#{topic}« is no topic" unless Curriculum.topics.key?(topic)
                places.each { |place| Curriculum.problems(place, plan).each { |p| problems << "#{plan[:key]}, #{topic}: #{p}" } }
            end
            plan[:lines].each { |slug, line| Curriculum.problems(line, plan).each { |p| problems << "#{plan[:key]}, #{slug}: #{p}" } }
        end
        states = Curriculum.states.map(&:first)
        assert_equal states.uniq, states, 'a state in one plan only'
        assert_empty problems, problems.join("\n")
    end

    # A plan without meta lines derives one from the topics, for the levels
    # the activity's age allows, a name once, in the plan's order.
    def test_a_derived_line
        plan = Curriculum.plan_for('baden-wuerttemberg')
        assert Curriculum.draft?(plan)
        young = { :curriculum => 'Sek I: Information und Daten (Codierung)', :topics => %w[codierung kompression zahlensysteme] }
        assert_equal 'Klasse 7: Daten und Codierung (Codierung, Binärsystem); IMP 8–10: Daten und Codierung (Klasse 9: Kompression)',
                     Curriculum.line(plan, 'x', young)
        old = young.merge(:curriculum => 'Oberstufe: Algorithmen und Datenstrukturen')
        assert_equal 'Kursstufe: Bitebene (verlustfreie Kompression, Zahlen als Bitfolgen)', Curriculum.line(plan, 'x', old)
        assert_nil Curriculum.line(plan, 'x', { :curriculum => 'Oberstufe: Vertiefung Computergrafik', :topics => %w[computergrafik] })
        assert_empty Curriculum.problems(Curriculum.line(plan, 'x', young), plan)
        assert_equal ['Klasse 7', 'Daten und Codierung', 'Codierung, Binärsystem'], Curriculum.entries(Curriculum.line(plan, 'x', young), plan).first
    end
end
