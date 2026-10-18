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
        'BACKUP' => 'auf einem Speicher im Rechenzentrum',
        'BACKUP_RETENTION_MONTHS' => 6,
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

    PARTNER = { 'key' => 'partner-1', 'name' => 'Partnerschule', 'address' => 'Weg 9, 54321 Anderswo', 'data_protection_officer' => 'Frau Partner-DSB' }.freeze

    def partner(signer: nil)
        Privacy.for_school(settings(FULL), PARTNER, :teachers => [{ :name => 'Tom Lehrer', :email => 'tom@partner.example' }], :signer => signer)
    end

    def test_the_package_of_a_partner_school
        s = partner
        ([nil] + PrivacyDocuments::DOCUMENTS.keys).each do |slug|
            html = PrivacyDocuments.page(slug, s)
            refute_match(/\bnil\b|#\{/, html, slug.inspect)
            assert_includes html, '· Partnerschule ·', slug.inspect
            assert_includes html, "href='/datenschutz?schule=partner-1'", slug.inspect if slug
        end
        PrivacyDocuments::DOCUMENTS.each_key { |slug| assert_includes PrivacyDocuments.page(nil, s), "href='/datenschutz/#{slug}?schule=partner-1'" }
        assert_includes PrivacyDocuments.page(nil, s), "href='/datenschutz/elternbrief?schule=partner-1&amp;einverstaendnis=1'"
        assert_includes PrivacyDocuments.page(nil, settings(FULL)), "href='/datenschutz/elternbrief?einverstaendnis=1'"

        schulleitung = PrivacyDocuments.body('schulleitung', s)
        assert_includes schulleitung, 'Für Partnerschule ist der Betreiber Auftragsverarbeiter'
        assert_includes schulleitung, 'Tom Lehrer'
        vvt = PrivacyDocuments.body('vvt', s)
        assert_includes vvt, 'Partnerschule, vertreten durch die Schulleitung<br>Weg 9, 54321 Anderswo'
        assert_includes vvt, 'Frau Partner-DSB'
        refute_includes vvt, 'Schulweg 1'
        avv = PrivacyDocuments.body('auftragsverarbeitung', s)
        assert_includes avv, '<strong>Partnerschule</strong>, vertreten durch die Schulleitung'
        assert_includes avv, '<strong>Dr. Erika Muster am Beispiel-Gymnasium</strong>, Schulweg 1, 12345 Musterstadt (Auftragsverarbeiter)'
        assert_includes PrivacyDocuments.body('schulleitung', s), 'Verantwortlich für den Betrieb ist Dr. Erika Muster am Beispiel-Gymnasium.'
        assert_includes PrivacyDocuments.body('nutzungsordnung', s), 'Vereinbarung mit Dr. Erika Muster am Beispiel-Gymnasium'
        letter = PrivacyDocuments.body('elternbrief', s)
        assert_includes letter, 'Mit freundlichen Grüßen<br>Tom Lehrer'
        assert_includes letter, 'erhält Ihr Kind mit der Einladung als PDF'
        refute_includes letter, '/datenschutz/datenschutzerklaerung?schule=partner-1'
        assert_includes letter, 'tom@partner.example'
        refute_includes letter, 'muster@example.org'
        assert_includes PrivacyDocuments.body('elternbrief', partner(:signer => 'Anna Lehrerin')), 'Mit freundlichen Grüßen<br>Anna Lehrerin'
        assert_includes PrivacyDocuments.body('elternbrief', settings(FULL)), 'Mit freundlichen Grüßen<br>Dr. Erika Muster'
        assert_includes PrivacyDocuments.body('elternbrief', settings(FULL)), 'https://code.example.org/impressum'
    end

    def test_contributions_are_never_a_condition
        plain = PrivacyDocuments.body('schulleitung', settings(FULL))
        assert_includes plain, 'Für die Schule ist die Nutzung kostenlos'
        refute_includes plain, 'keine Bedingung'
        with_note = settings(FULL.merge('COST_NOTE' => 'Der Server kostet <500 €> im Jahr.'))
        assert_includes PrivacyDocuments.body('schulleitung', with_note), 'Der Server kostet &lt;500 €&gt; im Jahr. Ein Beitrag ist keine Bedingung'
        assert_includes PrivacyDocuments.body('auftragsverarbeitung', with_note), 'unentgeltlich. Freiwillige Beiträge'
        refute_includes PrivacyDocuments.body('auftragsverarbeitung', settings(FULL)), 'Freiwillige Beiträge'
    end

    def test_pdf_names_are_plain_ascii
        s = partner
        PrivacyDocuments::DOCUMENTS.each_key do |slug|
            name = PrivacyDocuments.pdf_name(slug, s)
            assert_match(/\A[A-Za-z0-9 .,()_-]+\.pdf\z/, name, slug)
            assert_includes name, 'Partnerschule'
        end
        assert_equal 'Elternbrief mit Einverstaendniserklaerung - Partnerschule.pdf', PrivacyDocuments.pdf_name('elternbrief', s, :consent => true)
        assert_equal 'Pruefuebersicht Datenschutz.pdf', PrivacyDocuments.pdf_name('pruefuebersicht', settings({}))
        assert_includes PrivacyDocuments.page(nil, s), "href='/datenschutz/unterlagen.zip?schule=partner-1'"
        assert_includes PrivacyDocuments.page(nil, s), "href='/datenschutz/vvt.pdf?schule=partner-1'"
    end

    def test_the_index_links_every_document
        index = PrivacyDocuments.page(nil, settings(FULL))
        PrivacyDocuments::DOCUMENTS.each_key { |slug| assert_includes index, "href='/datenschutz/#{slug}'" }
    end

    def test_records_and_measures_come_from_the_facts
        s = settings(FULL)
        vvt = PrivacyDocuments.body('vvt', s)
        loesch = PrivacyDocuments.body('loeschkonzept', s)
        Privacy.records(s).each do |r|
            assert_includes vvt, Privacy.h(r[:data])
            assert_includes loesch, Privacy.h(r[:how_long])
        end
        tom = PrivacyDocuments.body('tom', s)
        Privacy.measures(s).each_value { |items| items.each { |m| assert_includes tom, Privacy.h(m) } }
    end

    def test_berlin_rules_only_where_configured
        assert_includes PrivacyDocuments.body('loeschkonzept', settings(FULL)), '§ 2 Abs. 6 DigLLV'
        assert_includes PrivacyDocuments.body('pruefuebersicht', settings(FULL)), 'Einordnung: digitales Lehr- und Lernmittel (§ 2 DigLLV)'
        assert_includes PrivacyDocuments.body('pruefuebersicht', settings(FULL)), 'Leistungsüberprüfungen'
        refute_includes PrivacyDocuments.body('pruefuebersicht', settings(FULL.merge('SCHOOL_LAW' => 'Schulgesetz NRW'))), 'Einordnung'
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
