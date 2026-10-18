require 'minitest/autorun'
require_relative '../include/privacy_documents'

class PrivacyDocumentsTest < Minitest::Test
    FULL = {
        'OPERATOR_NAME' => 'Dr. Erika Muster',
        'OPERATOR_SCHOOL' => 'Beispiel-Gymnasium',
        'OPERATOR_ADDRESS' => "Schulweg 1\n12345 Musterstadt",
        'CONTACT_EMAIL' => 'muster@example.org',
        'HOSTING' => 'Rechenzentrum GmbH, Deutschland',
        'MAIL_PROVIDER' => 'Post AG',
        'SCHOOL_LAW' => '§ 64 Abs. 11 SchulG und § 2 DigLLV',
        'LEGAL_BASIS' => :school,
        'WEB_ROOT' => 'https://code.example.org',
    }.freeze

    def settings(values)
        Privacy.settings(Module.new.tap { |m| values.each { |name, value| m.const_set(name, value) } })
    end

    def each_page(values)
        s = settings(values)
        ([nil] + PrivacyDocuments::DOCUMENTS.keys).each { |slug| yield slug, PrivacyDocuments.page(slug, s) }
    end

    def test_every_document_renders_with_and_without_settings
        [{}, FULL, FULL.merge('LEGAL_BASIS' => :consent, 'PARENTAL_CONSENT_REQUIRED' => true)].each do |values|
            each_page(values) do |slug, html|
                assert_includes html, '<!DOCTYPE html>', slug.inspect
                refute_match(/\bnil\b|#\{|mailto:''/, html, slug.inspect)
                text = html.gsub(/»[^«]*«/, '').gsub(/<[^>]+>/, ' ')
                refute_match(/\b(du|dich|dir|dein\w*|euch|euer|eure\w*)\b/i, text, "#{slug}: addresses someone as du")
            end
        end
    end

    def test_the_index_links_every_document
        index = PrivacyDocuments.page(nil, settings(FULL))
        PrivacyDocuments::DOCUMENTS.each_key { |slug| assert_includes index, "href='/datenschutz/#{slug}'" }
    end

    def test_records_and_measures_come_from_the_facts
        s = settings(FULL)
        vvt = PrivacyDocuments.body('vvt', s)
        loesch = PrivacyDocuments.body('loeschkonzept', s)
        Privacy.records.each do |r|
            assert_includes vvt, Privacy.h(r[:data])
            assert_includes loesch, Privacy.h(r[:how_long])
        end
        tom = PrivacyDocuments.body('tom', s)
        Privacy.measures(s).each_value { |items| items.each { |m| assert_includes tom, Privacy.h(m) } }
    end

    def test_berlin_rules_only_where_configured
        assert_includes PrivacyDocuments.body('loeschkonzept', settings(FULL)), '§ 2 Abs. 6 DigLLV'
        refute_includes PrivacyDocuments.body('loeschkonzept', settings(FULL.merge('SCHOOL_LAW' => 'Schulgesetz NRW'))), 'DigLLV'
        refute_includes PrivacyDocuments.body('pruefuebersicht', settings({})), 'DigLLV'
    end

    def test_the_consent_form_only_when_consent_is_wanted
        refute_includes PrivacyDocuments.body('elternbrief', settings(FULL)), 'Einverständnis</h2>'
        assert_includes PrivacyDocuments.body('elternbrief', settings(FULL.merge('PARENTAL_CONSENT_REQUIRED' => true))), 'Einverständnis</h2>'
        assert_includes PrivacyDocuments.body('elternbrief', settings(FULL)), 'https://code.example.org/impressum'
    end

    def test_the_webhosting_agreement_says_it_is_public_and_what_is_stored
        html = PrivacyDocuments.body('webhosting', settings(FULL))
        assert_includes html, 'Die Website ist öffentlich'
        assert_includes html, 'öffentliche'
        assert_includes html, 'Dr. Erika Muster'
        assert_includes html, 'erziehungsberechtigten Person'
    end
end
