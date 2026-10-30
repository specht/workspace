require 'minitest/autorun'
require 'yaml'
require_relative '../include/tools'

# The sections »Wie geht es weiter?« that close an activity are written by
# hand: why a next step follows, a neighbouring topic, ideas for the own
# program. Their links must lead somewhere a student can go.
class NextStepsTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)
    HEADING = 'Wie geht es weiter?'

    # an activity of the catalog (not one that exists only in development),
    # a Handbuch page or a tool
    def test_the_links_of_the_sections_lead_somewhere
        catalog = YAML.load_file(File.join(CONTENT, 'sections.yaml')).flat_map { |s| s['entries'] || [] }
        public = catalog.map(&:to_s).reject { |e| e.start_with?('.') }
        broken = []
        checked = 0
        Dir.glob(File.join(CONTENT, '*', '*.md')).each do |path|
            next unless File.basename(path, '.md') == File.basename(File.dirname(path))

            section = File.read(path)[/^## #{Regexp.escape(HEADING)}\n(.*?)(?=^## |\z)/m, 1]
            next unless section

            section.scan(%r{\]\((/[^)\s#]*)|href=['"](/[^'"#]*)}).flatten.compact.each do |link|
                checked += 1
                target = link.sub(%r{/\z}, '')
                ok = if target.start_with?('/reference/')
                    File.directory?(File.join(CONTENT, target))
                else
                    public.include?(target.delete_prefix('/')) || Tools::PATHS.include?(target)
                end
                broken << "#{File.basename(path)}: #{link}" unless ok
            end
        end
        assert_operator checked, :>, 10, 'the sections were not found'
        assert_empty broken, "links in »#{HEADING}« that lead nowhere a student can go"
    end
end
