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
            'reference/workspace' => { :title => 'Work&shy;space', :section => 'reference-workspace' },
            'reference/html-folien' => { :title => 'Folien mit <code>HTML</code>', :section => 'reference-presentations' },
        }
    end

    def sections
        { 'reference-workspace' => { :label => 'Workspace' }, 'reference-presentations' => { :label => 'Präsentationen' } }
    end

    def test_activity_shows_kind_duration_and_references
        html = PageKinds.header('showerjs', content, sections)
        assert_includes html, "class='page-kind page-kind-activity'"
        assert_includes html, 'Projekt'
        assert_includes html, '2 bis 3 Doppelstunden'
        assert_includes html, "<a href='/reference/workspace'><i class='bi bi-book'></i>Workspace</a>"
        assert_includes html, '>Folien mit HTML</a>'
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
        assert_includes PageKinds.header('x', c, sections), '&lt;b&gt;1&lt;/b&gt;'
    end

    def test_references_of_combines_meta_and_includes
        md = "_include(workspace#live-server)\n_include(git#push)\n"
        assert_equal %w[workspace html-folien git],
            PageKinds.references_of({ 'reference' => %w[workspace html-folien] }, md, ContentParts::INCLUDE)
        assert_equal [], PageKinds.references_of({}, 'nichts', ContentParts::INCLUDE)
    end

    def test_real_pages_point_at_existing_reference_pages
        root = File.expand_path('../../content', __dir__)
        catalog = YAML.safe_load(File.read(File.join(root, 'reference/sections.yaml'), encoding: 'UTF-8'))
        known = catalog.flat_map { |s| s['entries'] }
        Dir[File.join(root, '*/*.md')].each do |path|
            next if File.basename(path) == ContentParts::NOTE_FILE
            md = File.read(path, encoding: 'UTF-8')
            PageKinds.references_of(ContentParts.meta(md), md, ContentParts::INCLUDE).each do |name|
                assert_includes known, name, "#{path} uses unknown reference page #{name}"
            end
        end
    end
end
