require 'minitest/autorun'
require_relative '../include/page_kinds'
require_relative '../include/content_parts'

class PageKindsTest < Minitest::Test
    def content
        {
            'showerjs' => { :title => 'Eine Präsentation in HTML erstellen', :kind => 'projekt',
                            :duration => '2 bis 3 Doppelstunden', :references => %w[workspace html-folien] },
            'tcpip' => { :title => 'TCP/IP-Verbindungen', :references => ['workspace'] },
            'secret' => { :title => 'Nur Lehrkräfte', :teacher_only => true, :references => ['workspace'] },
            'draft' => { :title => 'Entwurf', :dev_only => true, :references => ['workspace'] },
            'level-x' => { :title => 'Verborgen', :hidden_section => true, :references => ['workspace'] },
            'reference/workspace' => { :title => 'Work&shy;space', :section => 'reference-workspace' },
            'reference/html-folien' => { :title => 'Folien mit <code>HTML</code>', :section => 'reference-presentations' },
        }
    end

    def sections
        { 'reference-workspace' => { :label => 'Workspace' }, 'reference-presentations' => { :label => 'Präsentationen' } }
    end

    def test_activity_shows_kind_duration_and_references
        html = PageKinds.header('showerjs', content, sections, :show_duration => true)
        assert_includes html, "class='page-kind page-kind-activity'"
        assert_includes html, 'Projekt'
        assert_includes html, '2 bis 3 Doppelstunden'
        assert_includes html, "<a href='/reference/workspace'><i class='bi bi-book'></i>Workspace</a>"
        assert_includes html, '>Folien mit HTML</a>'
    end

    # with a switch of languages the switch shows it, the label stays the same
    def test_a_page_with_a_language_switch_keeps_its_label
        c = content.merge('katas' => { :title => 'Katas', :kind => 'uebung', :languages => %w[ruby python] })
        html = PageKinds.header('katas', c, sections, :code_language => 'python')
        assert_includes html, '</i>Übung</span>'
        assert_includes html, "data-set-language='python'"
    end

    def test_activity_shows_its_language
        c = content.merge('drawing' => { :title => 'Pixelgrafik', :kind => 'projekt', :language => 'Ruby' })
        assert_includes PageKinds.header('drawing', c, sections), '</i>Projekt (Ruby)</span>'
        assert_equal 'Labor', PageKinds.kind_label('labor', '  ')
        assert_nil PageKinds.kind_label('nope', 'Ruby')
    end

    def test_activity_without_meta_is_still_an_activity
        html = PageKinds.header('tcpip', content.merge('tcpip' => { :title => 'X' }), sections)
        assert_includes html, 'Aktivität'
        refute_includes html, 'Im Handbuch'
        refute_includes html, 'bi-clock'
    end

    def test_reference_shows_breadcrumb_and_users
        html = PageKinds.header('reference/workspace', content, sections)
        assert_includes html, "class='page-kind page-kind-reference'"
        assert_includes html, "<a href='/reference'>"
        assert_includes html, 'Workspace'
        assert_includes html, "<a href='/showerjs'>Eine Präsentation in HTML erstellen</a>"
        assert_includes html, "<a href='/tcpip'>TCP/IP-Verbindungen</a>"
        refute_includes html, 'secret'
        refute_includes html, 'draft'
        assert_includes PageKinds.header('reference/workspace', content, sections, development: true), '/draft'
    end

    def test_teacher_pages_and_unknown_pages_get_nothing
        assert_equal '', PageKinds.header('secret', content, sections)
        assert_equal '', PageKinds.header('nope', content, sections)
    end

    def test_text_is_escaped
        c = content.merge('x' => { :title => 'X', :duration => '<b>1</b>' })
        assert_includes PageKinds.header('x', c, sections, :show_duration => true), '&lt;b&gt;1&lt;/b&gt;'
    end

    def test_the_curriculum_is_only_for_teachers
        c = content.merge('x' => { :title => 'X', :kind => 'projekt', :curriculum => 'Sek I: Datenbanken (<Tabellen>)' })
        refute_includes PageKinds.header('x', c, sections), 'Datenbanken'
        html = PageKinds.header('x', c, sections, :show_curriculum => true)
        assert_includes html, "class='page-kind-curriculum'"
        assert_includes html, "Sek I: <a href='/lehrplan#sek-i-datenbanken'>Datenbanken</a> (&lt;Tabellen&gt;)"
        refute_includes PageKinds.header('showerjs', content, sections, :show_curriculum => true), 'page-kind-curriculum'
    end

    def test_the_duration_is_only_for_teachers
        refute_includes PageKinds.header('showerjs', content, sections), 'Doppelstunden'
        assert_includes PageKinds.header('showerjs', content, sections, :show_duration => true), 'Doppelstunden'
    end

    def test_references_of_combines_meta_and_includes
        md = "_include(workspace#live-server)\n_include(git#push)\n"
        assert_equal %w[workspace html-folien git],
            PageKinds.references_of({ 'reference' => %w[workspace html-folien] }, md, ContentParts::INCLUDE)
        assert_equal [], PageKinds.references_of({}, 'nichts', ContentParts::INCLUDE)
    end

    # raylib names its own page, the Workspace basics come in by _include:
    # the basics still come first, as in the Handbuch catalog.
    def test_references_follow_the_catalog_basics_first
        md = "_include(workspace#repository-klonen)\n"
        assert_equal %w[workspace raylib glsl unlisted],
            PageKinds.references_of({ 'reference' => %w[glsl unlisted raylib] }, md, ContentParts::INCLUDE,
                :catalog => %w[workspace shortcuts terminal raylib glsl])
    end

    def test_real_pages_point_at_existing_reference_pages
        root = File.expand_path('../../content', __dir__)
        catalog = YAML.safe_load(File.read(File.join(root, 'reference/sections.yaml'), encoding: 'UTF-8'))
        # a dev-only page (».uml«) is a page too
        known = catalog.flat_map { |s| s['entries'] }.map { |entry| entry.delete_prefix('.') }
        Dir[File.join(root, '*/*.md')].each do |path|
            next if File.basename(path) == ContentParts::NOTE_FILE
            md = File.read(path, encoding: 'UTF-8')
            PageKinds.references_of(ContentParts.meta(md), md, ContentParts::INCLUDE).each do |name|
                assert_includes known, name, "#{path} uses unknown reference page #{name}"
            end
        end
    end

    # Every card in the catalog carries a label; one without would look
    # unfinished next to the others.
    def test_every_activity_in_the_catalog_has_a_known_kind
        root = File.expand_path('../../content', __dir__)
        catalog = YAML.safe_load(File.read(File.join(root, 'sections.yaml'), encoding: 'UTF-8'))
        catalog.flat_map { |s| s['entries'] || [] }.each do |entry|
            slug = entry.delete_prefix('.') # ».slug«: only in development (main.rb)
            md = File.read(File.join(root, slug, "#{slug}.md"), encoding: 'UTF-8')
            kind = ContentParts.meta(md)['kind'].to_s
            assert_includes PageKinds::KIND_LABELS.keys, kind, "#{slug}.md: kind »#{kind}« (#{PageKinds::KIND_LABELS.keys.join(', ')})"
        end
    end

    # requires: is information on the strip, linked by title, without the
    # pages a student cannot find in the catalog.
    def test_activity_names_what_it_builds_on
        c = content.merge('raytracing' => { :title => 'Raytracing', :kind => 'projekt',
                                            :requires => %w[tcpip showerjs secret draft level-x nope] })
        html = PageKinds.header('raytracing', c, sections)
        assert_includes html, "Baut auf <a href='/tcpip'>TCP/IP-Verbindungen</a> und <a href='/showerjs'>Eine Präsentation in HTML erstellen</a> auf"
        %w[secret draft level-x nope].each { |s| refute_includes html, "/#{s}'" }
        assert_equal %w[tcpip showerjs draft], PageKinds.requires_of('raytracing', c, development: true).map(&:first)
        refute_includes PageKinds.header('showerjs', content, sections), 'Baut auf'
    end

    def test_real_requires_name_activities_in_the_catalog
        root = File.expand_path('../../content', __dir__)
        catalog = YAML.safe_load(File.read(File.join(root, 'sections.yaml'), encoding: 'UTF-8'))
        listed = catalog.flat_map { |s| s['entries'] || [] }.map { |e| e.delete_prefix('.') }
        Dir[File.join(root, '*/*.md')].each do |path|
            next if File.basename(path) == ContentParts::NOTE_FILE
            Array(ContentParts.meta(File.read(path, encoding: 'UTF-8'))['requires']).each do |name|
                assert_includes listed, name.to_s, "#{path}: requires unknown activity #{name}"
            end
        end
    end

    def test_reference_does_not_give_away_hidden_pages
        html = PageKinds.header('reference/workspace', content, sections)
        refute_includes html, 'Verborgen'
        refute_includes html, 'level-x'
    end
end
