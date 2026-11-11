require 'minitest/autorun'
require 'yaml'
require_relative '../include/content_parts'
require_relative '../include/person_sources'

# The history boxes of the activities (<div class='history' id='history-…'>)
# and their sources in content/history-sources.yaml (docs/aktivitaet-schreiben.md,
# »Geschichte«): every box has an id, and every sentence a source or a
# reason why it needs none.
class HistorySourcesTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    PAGE = <<~MARKDOWN
        # Echo

        Text davor.

        <div class='history' id='history-tape'>
        <i class='bi bi-calendar3'></i>
        <p><strong>Das Bandecho</strong>, 1970. Ein Band lief über zwei Köpfe. Hör es dir an.</p>
        </div>

        <div class='history'>
        <i class='bi bi-calendar3'></i>
        <p><strong>Ohne id</strong>, 1999. Nichts.</p>
        </div>
    MARKDOWN

    SOURCES = YAML.safe_load(<<~YAML)
        tape:
          - text: Das Bandecho, 1970
            sources:
              - url: https://example.org/tape
          - text: Hör es dir an
            none: eine Einladung
    YAML

    def test_each_box_with_its_id_name_link_and_states
        boxes = PersonSources.history_overview({ 'echo' => PAGE }, SOURCES)
        assert_equal ['tape', nil], boxes.map { |b| b[:part] }
        assert_equal 'Das Bandecho', boxes.first[:name]
        assert_equal '/echo#history-tape', boxes.first[:link]
        assert_equal [%w[sourced open none]], boxes.first[:paragraphs].map { |p| p.map { |s| s[:state] } }
    end

    def real
        sources = YAML.safe_load(File.read(File.join(CONTENT, 'history-sources.yaml')), aliases: false) || {}
        [PersonSources.history_overview(PersonSources.activity_pages(CONTENT), sources), sources]
    end

    def test_every_box_has_a_unique_id
        boxes, = real
        assert_empty boxes.reject { |b| b[:part] }.map { |b| b[:slug] }, 'history boxes without id=history-…'
        ids = boxes.map { |b| b[:part] }
        assert_equal ids.uniq, ids
    end

    def test_every_sentence_has_a_source
        boxes, = real
        open = boxes.flat_map do |b|
            b[:paragraphs].flatten.reject { |s| %w[sourced none].include?(s[:state]) }.map { |s| "#{b[:link]}: #{s[:state]}: #{s[:text]}" }
        end
        assert_empty open
    end

    def test_no_claim_has_lost_its_sentence
        boxes, sources = real
        missing = boxes.flat_map { |b| b[:missing].map { |c| "#{b[:link]}: #{c['text']}" } }
        assert_empty missing
        assert_empty sources.keys - boxes.map { |b| b[:part] }, 'sources for boxes that no longer exist'
    end
end
