require 'minitest/autorun'
require 'uri'
require_relative '../include/privacy'
require_relative '../include/weather_mirror'
require_relative '../include/test_workspace_extensions'

class PrivacyTest < Minitest::Test
    ROOT = File.expand_path('../..', __dir__)

    FULL = {
        'OPERATOR_NAME' => 'Dr. Erika Muster',
        'OPERATOR_ROLE' => 'Lehrerin für Informatik',
        'OPERATOR_SCHOOL' => 'Beispiel-Gymnasium',
        'OPERATOR_ADDRESS' => "Schulweg 1\n12345 Musterstadt",
        'CONTACT_EMAIL' => 'muster@example.org',
        'DATA_PROTECTION_OFFICER' => "Max Schutz\ndsb@example.org",
        'HOSTING' => 'Rechenzentrum <Beispiel>',
        'MAIL_PROVIDER' => 'Post AG',
        'SCHOOL_LAW' => '§ 1 Schulgesetz',
        'SUPERVISORY_AUTHORITY' => "Landesbeauftragte\nStraße 2",
    }.freeze

    def scope(values)
        Module.new.tap { |m| values.each { |name, value| m.const_set(name, value) } }
    end

    def page(values = {})
        Privacy.page_html(Privacy.settings(scope(values)))
    end

    def test_renders_with_every_setting_missing
        html = page
        ['Impressum', 'Datenschutzerklärung', 'Wer verantwortlich ist', 'Rechtsgrundlage', 'Welche Daten gespeichert werden',
         'Dienste außerhalb des Servers', 'Speicherdauer', 'Rechte der betroffenen Personen', 'Wer nicht einverstanden ist'].each do |heading|
            assert_includes html, heading
        end
        assert_includes html, 'noch nicht eingetragen'
        assert_includes html, 'die Lehrkraft, die zum Workspace eingeladen hat'
        assert_includes html, 'Aufsichtsbehörde des Bundeslandes'
        refute_match(/mailto:|\bnil\b|#\{/, html)
    end

    def test_empty_settings_count_as_missing
        assert_equal page, page('OPERATOR_NAME' => '  ', 'CONTACT_EMAIL' => '', 'LEGAL_BASIS' => :unknown)
    end

    def test_names_and_escapes_every_setting
        html = page(FULL.merge('LEGAL_BASIS' => :school))
        assert_includes html, 'Dr. Erika Muster, Lehrerin für Informatik am Beispiel-Gymnasium'
        assert_includes html, 'Schulweg 1<br>12345 Musterstadt'
        assert_includes html, "<a href='mailto:muster@example.org'>"
        assert_includes html, 'Max Schutz<br>dsb@example.org'
        assert_includes html, 'Rechenzentrum &lt;Beispiel&gt;'
        assert_includes html, '(Post AG)'
        assert_includes html, 'Landesbeauftragte<br>Straße 2'
    end

    def test_the_school_is_responsible_only_by_its_decision
        school = page(FULL.merge('LEGAL_BASIS' => :school))
        assert_includes school, 'Verantwortlich ist die Schule: <strong>Beispiel-Gymnasium</strong>'
        assert_includes school, 'Art. 6 Abs. 1 Buchst. e DSGVO in Verbindung mit § 1 Schulgesetz'
        assert_includes school, 'widersprechen (Art. 21)'
        refute_includes school, 'Art. 20)'

        consent = page(FULL.merge('LEGAL_BASIS' => :consent))
        assert_includes consent, 'Verantwortlich ist Dr. Erika Muster'
        assert_includes consent, 'Art. 6 Abs. 1 Buchst. a DSGVO'
        assert_includes consent, 'freiwillig'
        assert_includes consent, 'Datenübertragbarkeit (Art. 20)'

        assert_includes page(FULL), 'Verantwortlich ist Dr. Erika Muster'
    end

    def test_parental_consent_on_top_of_the_school_decision
        refute_includes page(FULL.merge('LEGAL_BASIS' => :school)), 'Einverständnis erklärt'
        assert_includes page(FULL.merge('LEGAL_BASIS' => :school, 'PARENTAL_CONSENT_REQUIRED' => true)), 'Einverständnis erklärt'
    end

    # Formal, for parents and the school: no »du« outside quoted notices.
    def test_addresses_nobody_as_du
        [{}, FULL, FULL.merge('LEGAL_BASIS' => :school), FULL.merge('LEGAL_BASIS' => :consent)].each do |values|
            text = page(values).gsub(/»[^«]*«/, '').gsub(/<[^>]+>/, ' ')
            refute_match(/\b(du|dich|dir|dein\w*|euch|euer|eure\w*)\b/i, text)
        end
    end

    # Every point of Art. 13 DSGVO has its heading.
    def test_names_every_point_of_article_13
        html = page(FULL)
        ['Abs. 1 Buchst. a', 'Buchst. c', 'Buchst. e', 'Buchst. f', 'Abs. 2 Buchst. a', 'Buchst. b, c, d', 'Abs. 2 Buchst. e', 'Abs. 2 Buchst. f'].each do |point|
            assert_includes html, point
        end
        Privacy.records.each { |r| assert r[:categories].all? { |c| Privacy::CATEGORIES.key?(c) } }
    end

    def test_times_come_from_the_code
        html = page
        assert_includes html, "#{Authentication::LOGIN_REQUEST_TTL_SECONDS / 60} Minuten"
        assert_includes html, "#{Authentication::SESSION_LIFETIME_DAYS} Tage"
    end

    def test_names_every_outside_host
        html = page
        Privacy::OUTSIDE_HOSTS.each_value { |name| assert_includes html, name }
        hosts = WeatherMirror::UPSTREAMS.values.map { |url| URI(url).host } + [URI(TestWorkspaceExtensions::REGISTRY).host]
        hosts.each { |host| assert Privacy::OUTSIDE_HOSTS.key?(host), "#{host} is used by the code but missing in Privacy::OUTSIDE_HOSTS" }
    end

    # The page says the site loads nothing from other servers.
    def test_site_pages_load_nothing_from_other_servers
        Dir[File.join(ROOT, 'src/static/*.html')].each do |path|
            html = File.read(path).gsub(/<!--.*?-->/m, '')
            html.scan(/<(?:script|link|img|iframe)\b[^>]*\b(?:src|href)=['"](https?:)?\/\/[^'"]+/i) do
                tag = Regexp.last_match(0)
                next if tag.start_with?('<link') && !tag.match?(/stylesheet|preload|icon/i)

                flunk "#{File.basename(path)} loads from another server: #{tag}"
            end
        end
    end

    def test_backups_only_when_named
        refute_includes page(FULL), 'Sicherungen'
        html = page(FULL.merge('BACKUP' => 'in einer Storage Box', 'BACKUP_RETENTION_MONTHS' => 6))
        assert_includes html, 'Sicherungen werden höchstens 6 Monate aufbewahrt.'
        assert_includes html, 'erneut gelöscht'
        assert_includes page(FULL.merge('BACKUP' => 'in einer Storage Box')), 'noch nicht eingetragen'
    end

    def test_no_old_claims
        html = page(FULL)
        refute_includes html, 'keine externen Dienste'
        refute_includes html, 'schulischen Server'
        refute_includes html, 'Keine Weitergabe an Dritte'
    end
end
