require 'minitest/autorun'
require 'yaml'
require_relative '../include/content_parts'
require_relative '../include/person_sources'

class PersonSourcesTest < Minitest::Test
    PAGE = <<~MARKDOWN
        # Personen

        <!-- part: ada -->
        ### Ada Lovelace

        <div class='person'>
        <img src='ada.webp' alt='Ada'>
        <p><strong>Ada Lovelace</strong> (1815–1852), englische Mathematikerin. Sie schrieb 1843 das erste
        Programm. Dein <code>loop</code> zählt bis 10.</p>
        <p>Im 19. Jahrhundert war das neu. Sie war die Tochter von Lord Byron.</p>
        <p class='person-credit'>Bild: <a href='https://example.org'>gemeinfrei</a></p>
        </div>
        <!-- /part -->

        <!-- part: ohne -->
        ### Kein Kasten

        Nur Text.
        <!-- /part -->
    MARKDOWN

    SOURCES = YAML.safe_load(<<~YAML)
        ada:
          - text: (1815–1852), englische Mathematikerin
            sources:
              - url: https://example.org/ada
                quote: born 1815
          - text: schrieb 1843 das erste Programm
            verdict: partly
            note: umstritten
            sources:
              - url: https://example.org/note
          - text: zählt bis 10
            none: Workspace
          - text: Tochter von Lord Byron
            verdict: wrong
          - text: ein Satz, den es nicht mehr gibt
    YAML

    def overview
        PersonSources.overview(PAGE, SOURCES)
    end

    def test_only_parts_with_a_box
        assert_equal ['ada'], overview.map { |p| p[:part] }
        assert_equal 'Ada Lovelace', overview.first[:name]
    end

    def test_sentences_ignore_ordinals_and_abbreviations
        assert_equal ['Im 19. Jahrhundert war das neu.', 'Das z. B. auch.'],
                     PersonSources.sentences('Im 19. Jahrhundert war das neu. Das z. B. auch.')
    end

    def test_each_sentence_gets_its_state
        states = overview.first[:paragraphs].map { |p| p.map { |s| s[:state] } }
        assert_equal [%w[sourced partly none], %w[open wrong]], states
    end

    def test_markup_and_line_breaks_are_plain_text
        sentence = overview.first[:paragraphs][0][1][:text]
        assert_equal 'Sie schrieb 1843 das erste Programm.', sentence
    end

    def test_claims_the_box_no_longer_has_are_missing
        assert_equal ['ein Satz, den es nicht mehr gibt'], overview.first[:missing].map { |c| c['text'] }
    end

    def test_credit_line_is_kept_apart
        assert_includes overview.first[:credit], 'https://example.org'
        refute overview.first[:paragraphs].flatten.any? { |s| s[:text].include?('gemeinfrei') }
    end

    def test_a_claim_without_sources_leaves_the_sentence_open
        assert_equal 'open', PersonSources.state([{ 'text' => 'x' }])
        assert_equal 'sourced', PersonSources.state([{ 'text' => 'x', 'sources' => [{ 'url' => 'u' }] }])
    end
end
