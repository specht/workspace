require 'minitest/autorun'
require 'json'
require 'yaml'
require_relative '../include/page_kinds'
require_relative '../include/curriculum_graph'

# An activity only in development (».slug« in sections.yaml) is nowhere in
# production: not at its address, not in what other pages list (builds on,
# used by, requires), not on /lehrplan and not in the stored picture of the
# ways through the activities, whose production layout is searched without it.
class DevOnlyTest < Minitest::Test
    ROOT = File.expand_path('../../..', __dir__)

    CONTENT = {
        'kata-bits' => { :title => 'Bits', :references => ['bits-und-bytes'] },
        'draft' => { :title => 'Entwurf', :dev_only => true, :requires => ['kata-bits'], :references => ['bits-und-bytes'] },
        'reference/bits-und-bytes' => { :title => 'Bits und Bytes' },
        'reference/uml' => { :title => 'Klassendiagramme', :dev_only => true },
    }.freeze

    def test_a_draft_activity_is_served_in_development_only
        refute PageKinds.served?('draft', CONTENT['draft'])
        assert PageKinds.served?('draft', CONTENT['draft'], :development => true)
        assert PageKinds.served?('kata-bits', CONTENT['kata-bits'])
        refute PageKinds.served?('nothing', nil, :development => true)
        # a Handbuch page in development stays at its address: live pages link to it
        assert PageKinds.served?('reference/uml', CONTENT['reference/uml'])
    end

    def test_no_list_names_a_draft_in_production
        slugs = %w[kata-bits draft]
        assert_empty PageKinds.builds_on('kata-bits', CONTENT, slugs)
        assert_equal [['draft', 'Entwurf']], PageKinds.builds_on('kata-bits', CONTENT, slugs, :development => true)
        assert_equal [['kata-bits', 'Bits']], PageKinds.used_by('reference/bits-und-bytes', CONTENT)
        assert_includes PageKinds.used_by('reference/bits-und-bytes', CONTENT, :development => true), ['draft', 'Entwurf']
    end

    def dev_only_slugs
        YAML.load_file(File.join(ROOT, 'src/content/sections.yaml'))
            .flat_map { |section| section['entries'] || [] }
            .select { |entry| entry.start_with?('.') }.map { |entry| entry.delete_prefix('.') }
    end

    def test_the_stored_picture_of_production_has_no_draft
        drafts = dev_only_slugs
        skip 'no activity is only in development' if drafts.empty?
        _content, slugs = CurriculumGraph.read_content(ROOT, :development => false)
        assert_empty slugs & drafts
        layouts = JSON.parse(File.read(CurriculumGraph::STORE))['layouts']
        production = layouts.find { |layout| layout['name'] == 'production' }
        refute_nil production
        ids = production['orders'].flat_map { |_, column| column }.map { |id| id.split(/[>#]/).first }
        assert_empty ids & drafts, 'the production layout of curriculum-graph.json names an activity only in development'
    end
end
