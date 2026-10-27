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

    # Who hosts the DNS: Cloudflare without the setting (the only one
    # documented before deSEC), otherwise the one named.
    def test_dns_provider
        assert_includes page, 'Die Namen der Seite (DNS) verwaltet Cloudflare.'
        html = page('DNS_PROVIDER' => 'deSEC e. V.')
        assert_includes html, 'Die Namen der Seite (DNS) verwaltet deSEC e. V.'
        refute_includes html, 'Cloudflare'
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

        # While the schools decide: said plainly, with the intended basis and
        # the date, the operator responsible, the right to object
        pending = page(FULL.merge('LEGAL_BASIS' => :pending, 'TRANSITION_DEADLINE' => '2027-04-12'))
        assert_includes pending, 'stehen noch aus'
        assert_includes pending, 'soll dann Art. 6 Abs. 1 Buchst. e DSGVO in Verbindung mit § 1 Schulgesetz'
        assert_includes pending, 'bis zum 12.04.2027'
        assert_includes pending, 'Verantwortlich ist Dr. Erika Muster'
        assert_includes pending, 'widersprechen (Art. 21)'
        refute_includes pending, 'noch nicht eingetragen'
    end

    PARTNER = {
        'key' => 'partner-1', 'name' => 'Partner & Co. Schule', 'address' => "Weg 9\n54321 Anderswo",
        'data_protection_officer' => 'Frau Partner-DSB', 'school_law' => '§ 2 Schulgesetz Anderswo', 'supervisory_authority' => '',
    }.freeze
    TEACHERS = [{ :name => 'Tom Lehrer', :email => 'tom@partner.example' }, { :name => nil, :email => 'zweite@partner.example' }].freeze

    def test_a_partner_school_is_responsible_for_its_own_package
        s = Privacy.for_school(Privacy.settings(scope(FULL.merge('LEGAL_BASIS' => :school))), PARTNER, :teachers => TEACHERS)
        html = Privacy.page_html(s).split("<h1 id='datenschutz'>").last
        assert_includes html, 'Verantwortlich ist die Schule: <strong>Partner &amp; Co. Schule</strong>, vertreten durch die Schulleitung<br>Weg 9<br>54321 Anderswo'
        assert_includes html, 'als Auftragsverarbeiter (Art. 28 DSGVO) Dr. Erika Muster, Lehrerin für Informatik am Beispiel-Gymnasium, Schulweg 1, 12345 Musterstadt'
        assert_includes html, "Ansprechperson an der Schule für alle Fragen zum Datenschutz: Tom Lehrer (<a href='mailto:tom@partner.example'>tom@partner.example</a>), <a href='mailto:zweite@partner.example'>zweite@partner.example</a>."
        assert_includes html, 'in Verbindung mit § 2 Schulgesetz Anderswo'
        assert_includes html, 'Frau Partner-DSB'
        refute_includes html, 'Max Schutz'
        refute_includes html, 'muster@example.org', 'the operator is not the contact of a partner school'
        assert_includes html, 'Aufsichtsbehörde des Bundeslandes', 'an empty field of the school is missing, not the operator\'s'
        assert_equal 'Tom Lehrer', s[:signer]
        assert_equal 'Anna', Privacy.for_school(s, PARTNER, :teachers => TEACHERS, :signer => 'Anna')[:signer]

        # A partner school is responsible even before the operator's school decided.
        assert_includes Privacy.page_html(Privacy.for_school(Privacy.settings(scope(FULL)), PARTNER)), 'Verantwortlich ist die Schule: <strong>Partner'
        assert_includes Privacy.page_html(Privacy.for_school(Privacy.settings(scope(FULL)), PARTNER)), 'die Lehrkraft, die zum Workspace eingeladen hat'
    end

    def test_the_operators_school_and_the_guests_get_the_plain_package
        s = Privacy.settings(scope(FULL))
        assert_same s, Privacy.for_school(s, { 'key' => 'home', 'name' => 'Anders', 'home' => true })
        assert_same s, Privacy.for_school(s, { :key => 'guests', :name => 'Gäste', :guests => true })
    end

    def test_approved_partner_schools_are_listed
        partners = [{ :key => 'p 1', :name => 'Erste <Schule>' }, { :key => 'p2', :name => 'Zweite' }]
        html = Privacy.page_html(Privacy.settings(scope(FULL.merge('LEGAL_BASIS' => :school))), :partners => partners)
        assert_includes html, 'Auch andere Schulen setzen den Workspace ein'
        refute_includes html, 'Erste', 'partner schools are not named on the public page'
        refute_includes html, 'schule=p2'
        refute_includes page(FULL), 'ebenso entschieden'
    end

    def test_parental_consent_on_top_of_the_school_decision
        refute_includes page(FULL.merge('LEGAL_BASIS' => :school)), 'Einverständnis der Erziehungsberechtigten ein'
        assert_includes page(FULL.merge('LEGAL_BASIS' => :school, 'PARENTAL_CONSENT_REQUIRED' => true)), 'Einverständnis der Erziehungsberechtigten ein'
        refute_includes page(FULL.merge('LEGAL_BASIS' => :school, 'PARENTAL_CONSENT_REQUIRED' => true)), 'erst freigeschaltet'
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

    # The Git server of the groups (GIT_SERVER): its repositories and keys
    # are named only when the installation has it.
    def test_git_repositories_only_when_switched_on
        refute_includes page(FULL), 'Git-Repositorys von Gruppen'
        html = page(FULL.merge('GIT_SERVER' => true))
        assert_includes html, 'Git-Repositorys von Gruppen'
        assert_includes html, 'Öffentlicher SSH-Schlüssel'
        refute_includes page(FULL.merge('GIT_SERVER' => 'ja')), 'Git-Repositorys von Gruppen'
    end

    def test_no_old_claims
        html = page(FULL)
        refute_includes html, 'keine externen Dienste'
        refute_includes html, 'schulischen Server'
        refute_includes html, 'Keine Weitergabe an Dritte'
    end
end
