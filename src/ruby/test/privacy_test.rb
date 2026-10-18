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
        ['Impressum', 'Datenschutzerklärung', 'Wer verantwortlich ist', 'Rechtsgrundlage', 'Was gespeichert wird',
         'Dienste außerhalb des Servers', 'Wie lange', 'Deine Rechte', 'Wenn du nicht einverstanden bist'].each do |heading|
            assert_includes html, heading
        end
        assert_includes html, 'noch nicht eingetragen'
        assert_includes html, 'an die Lehrkraft, die dich eingeladen hat'
        assert_includes html, 'Aufsichtsbehörde deines Bundeslandes'
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
        assert_includes school, 'Art. 6 Abs. 1 Buchstabe e DSGVO in Verbindung mit § 1 Schulgesetz'
        assert_includes school, 'widersprechen (Art. 21)'
        refute_includes school, 'Art. 20'

        consent = page(FULL.merge('LEGAL_BASIS' => :consent))
        assert_includes consent, 'Verantwortlich ist Dr. Erika Muster'
        assert_includes consent, 'Art. 6 Abs. 1 Buchstabe a DSGVO'
        assert_includes consent, 'freiwillig'
        assert_includes consent, 'Datenübertragbarkeit (Art. 20)'

        assert_includes page(FULL), 'Verantwortlich ist Dr. Erika Muster'
    end

    def test_parental_consent_on_top_of_the_school_decision
        refute_includes page(FULL.merge('LEGAL_BASIS' => :school)), 'Einverständnis gegeben'
        assert_includes page(FULL.merge('LEGAL_BASIS' => :school, 'PARENTAL_CONSENT_REQUIRED' => true)), 'Einverständnis gegeben'
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
        Dir[File.join(ROOT, 'src/content/**/*.md')].each do |path|
            File.read(path).scan(%r{https?://(git\.[a-z0-9.-]+)}).flatten.uniq.each do |host|
                assert Privacy::OUTSIDE_HOSTS.key?(host), "#{path}: #{host} missing in Privacy::OUTSIDE_HOSTS"
            end
        end
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

    def test_no_old_claims
        html = page(FULL)
        refute_includes html, 'keine externen Dienste'
        refute_includes html, 'schulischen Server'
        refute_includes html, 'Keine Weitergabe an Dritte'
    end
end
