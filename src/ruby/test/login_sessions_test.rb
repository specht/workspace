require 'minitest/autorun'
require 'date'
require_relative '../include/login_sessions'

class LoginSessionsTest < Minitest::Test
    TODAY = Date.new(2026, 10, 22)

    # Real User-Agent strings of current browsers, and what the profile says.
    AGENTS = {
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36' => 'Chrome auf Windows',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:144.0) Gecko/20100101 Firefox/144.0' => 'Firefox auf Windows',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36 Edg/141.0.0.0' => 'Edge auf Windows',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36 OPR/124.0.0.0' => 'Opera auf Windows',
        # Safari on a Mac, and an iPad, which asks for the desktop pages
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.0 Safari/605.1.15' => 'Safari auf Mac oder iPad',
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36' => 'Chrome auf Mac',
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:144.0) Gecko/20100101 Firefox/144.0' => 'Firefox auf Mac',
        'Mozilla/5.0 (iPhone; CPU iPhone OS 18_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.0 Mobile/15E148 Safari/604.1' => 'Safari auf iPhone',
        'Mozilla/5.0 (iPhone; CPU iPhone OS 18_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) CriOS/141.0.7390.41 Mobile/15E148 Safari/604.1' => 'Chrome auf iPhone',
        'Mozilla/5.0 (iPhone; CPU iPhone OS 18_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) FxiOS/144.0 Mobile/15E148 Safari/605.1.15' => 'Firefox auf iPhone',
        'Mozilla/5.0 (iPad; CPU OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1' => 'Safari auf iPad',
        'Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Mobile Safari/537.36' => 'Chrome auf Android',
        'Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36' => 'Chrome auf Android-Tablet',
        'Mozilla/5.0 (Linux; Android 14; SAMSUNG SM-S921B) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/28.0 Chrome/130.0.0.0 Mobile Safari/537.36' => 'Samsung Internet auf Android',
        'Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Mobile Safari/537.36 EdgA/141.0.0.0' => 'Edge auf Android',
        'Mozilla/5.0 (Android 14; Mobile; rv:144.0) Gecko/144.0 Firefox/144.0' => 'Firefox auf Android',
        'Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36' => 'Chrome auf ChromeOS',
        'Mozilla/5.0 (X11; Linux x86_64; rv:144.0) Gecko/20100101 Firefox/144.0' => 'Firefox auf Linux',
        'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:144.0) Gecko/20100101 Firefox/144.0' => 'Firefox auf Linux',
        'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/141.0.7390.37 Safari/537.36' => 'Chrome auf Linux',
        'curl/8.5.0' => 'unbekannter Browser auf unbekanntem System',
        '' => 'unbekannter Browser auf unbekanntem System',
    }.freeze

    def test_device_labels_of_real_browsers
        AGENTS.each do |ua, label|
            assert_equal label, LoginSessions.device_label(ua), ua
        end
        assert_equal 'unbekannter Browser auf unbekanntem System', LoginSessions.device_label(nil)
        assert_equal 'Firefox auf unbekanntem System', LoginSessions.device_label('Mozilla/5.0 (Haiku) Firefox/115.0')
    end

    def test_icons_by_kind_of_device
        assert_equal 'bi-phone', LoginSessions.icon('Safari auf iPhone')
        assert_equal 'bi-phone', LoginSessions.icon('Chrome auf Android')
        assert_equal 'bi-tablet', LoginSessions.icon('Safari auf iPad')
        assert_equal 'bi-tablet', LoginSessions.icon('Chrome auf Android-Tablet')
        assert_equal 'bi-laptop', LoginSessions.icon('Safari auf Mac oder iPad')
        assert_equal 'bi-laptop', LoginSessions.icon('Chrome auf ChromeOS')
        assert_equal 'bi-pc-display', LoginSessions.icon('Firefox auf Windows')
        assert_equal 'bi-display', LoginSessions.icon('Firefox auf unbekanntem System')
        assert_equal 'bi-question-circle', LoginSessions.icon(nil)
    end

    def test_icons_exist_in_the_sites_bootstrap_icons
        css = File.read(File.join(__dir__, '../../static/include/bootstrap-icons-1.11.3/font/bootstrap-icons.min.css'))
        (LoginSessions::ICONS.values + %w[bi-display bi-question-circle bi-box-arrow-right bi-chevron-down]).uniq.each do |icon|
            assert_includes css, ".#{icon}::before", icon
        end
    end

    def test_new_sessions_store_days_and_a_label_but_not_the_user_agent
        ua = AGENTS.keys.first
        fields = LoginSessions.new_session_fields(ua, :today => TODAY)
        assert_equal({ :created_on => '2026-10-22', :last_used_on => '2026-10-22', :device => 'Chrome auf Windows' }, fields)
        refute fields.values.any? { |v| v.include?('Mozilla') }
    end

    def test_day_text
        assert_equal 'heute', LoginSessions.day_text('2026-10-22', :today => TODAY)
        assert_equal 'gestern', LoginSessions.day_text('2026-10-21', :today => TODAY)
        assert_equal 'am 3. Oktober 2026', LoginSessions.day_text('2026-10-03', :today => TODAY)
        assert_equal 'am 1. März 2025', LoginSessions.day_text('2025-03-01', :today => TODAY)
        assert_nil LoginSessions.day_text(nil, :today => TODAY)
        assert_nil LoginSessions.day_text('kaputt', :today => TODAY)
    end

    def test_when_text
        assert_equal 'angemeldet am 3. Oktober 2026 · zuletzt gestern',
            LoginSessions.when_text({ :created_on => '2026-10-03', :last_used_on => '2026-10-21' }, :today => TODAY)
        assert_equal 'angemeldet heute', LoginSessions.when_text({ :created_on => '2026-10-22', :last_used_on => '2026-10-22' }, :today => TODAY)
        # From before the days were stored; used once since.
        assert_equal 'Tag der Anmeldung unbekannt · zuletzt heute',
            LoginSessions.when_text({ :last_used_on => '2026-10-22' }, :today => TODAY)
        assert_equal 'Tag der Anmeldung unbekannt', LoginSessions.when_text({}, :today => TODAY)
    end

    def test_public_id_is_stable_and_not_the_sid
        sid = 'abcdefghijklmnopqrstuvwx'
        id = LoginSessions.public_id(sid)
        assert_equal id, LoginSessions.public_id(sid)
        refute_includes id, sid
        assert LoginSessions.valid_public_id?(id)
        refute LoginSessions.valid_public_id?(sid)
        refute LoginSessions.valid_public_id?(nil)
        refute_equal id, LoginSessions.public_id('abcdefghijklmnopqrstuvwy')
    end

    SESSIONS = [
        { :sid => 'old', :email => 'lena@example.org' },
        { :sid => 'phone', :email => 'lena@example.org', :created_on => '2026-09-01', :last_used_on => '2026-10-21', :device => 'Safari auf iPhone' },
        { :sid => 'here', :email => 'lena@example.org', :created_on => '2026-01-10', :last_used_on => '2026-10-01', :device => 'Firefox auf Windows' },
        { :sid => 'school', :email => 'lena@example.org', :created_on => '2026-10-20', :last_used_on => '2026-10-22', :device => 'Chrome auf ChromeOS' },
        { :sid => 'teacher', :email => 'lena@example.org', :impersonated_by => 'lehrer@example.org', :created_on => '2026-10-22', :last_used_on => '2026-10-22' },
        { :sid => 'mia', :email => 'mia@example.org', :created_on => '2026-10-22', :last_used_on => '2026-10-22' },
    ].freeze

    def own
        SESSIONS.select { |s| s[:email] == 'lena@example.org' }
    end

    def test_list_puts_this_browser_first_then_the_newest_use_and_hides_look_ins
        sids = LoginSessions.listed(own, :current_sid => 'here').map { |s| s[:sid] }
        assert_equal %w[here school phone old], sids
    end

    def test_list_html_names_sessions_by_public_id_only
        html = LoginSessions.list_html(own, :current_sid => 'here', :today => TODAY)
        %w[here school phone old teacher].each { |sid| refute_includes html, "'#{sid}'" }
        assert_includes html, "data-id='#{LoginSessions.public_id('school')}'"
        refute_includes html, LoginSessions.public_id('teacher')
        assert_includes html, 'dieser Browser'
        assert_includes html, "href='/logout'"
        assert_equal 3, html.scan('bu-end-session').size
        assert_includes html, 'Unbekanntes Gerät'
        assert_includes html, 'bi-question-circle'
        assert_includes html, 'angemeldet am 20. Oktober 2026 · zuletzt heute'
        refute_includes html, 'bu_all_sessions'
    end

    def test_list_html_shows_the_first_ten_and_a_button_for_all
        many = (1..25).map { |i| { :sid => "s#{i}", :created_on => (TODAY - i).to_s, :last_used_on => (TODAY - i).to_s, :device => 'Chrome auf Windows' } }
        html = LoginSessions.list_html(many, :current_sid => 's25', :today => TODAY)
        assert_equal 10, html.scan("<li class='login-session").size
        assert_includes html, 'alle 25 anzeigen'
        assert_includes html, "data-id='#{LoginSessions.public_id('s25')}'" # this browser, though the oldest
        all = LoginSessions.list_html(many, :current_sid => 's25', :today => TODAY, :limit => nil)
        assert_equal 25, all.scan("<li class='login-session").size
        refute_includes all, 'bu_all_sessions'
    end

    def test_the_device_label_is_escaped
        html = LoginSessions.list_html([{ :sid => 'x', :device => '<b>Firefox</b> auf Linux' }], :current_sid => 'y', :today => TODAY)
        refute_includes html, '<b>'
    end

    def end_one(id, owner: 'lena@example.org', current_sid: 'here', impersonating: false)
        LoginSessions.sid_to_end(SESSIONS, id, :owner => owner, :current_sid => current_sid, :impersonating => impersonating)
    end

    def test_ending_one_session_of_ones_own
        assert_equal [:ok, 'phone'], end_one(LoginSessions.public_id('phone'))
        assert_equal [:ok, 'old'], end_one(LoginSessions.public_id('old'))
    end

    def test_only_the_owner_ends_a_session
        assert_equal [:not_found], end_one(LoginSessions.public_id('mia'))
        assert_equal [:ok, 'mia'], end_one(LoginSessions.public_id('mia'), :owner => 'Mia@example.org', :current_sid => 'other')
    end

    def test_not_this_browser_nor_a_teachers_look_in_nor_while_impersonating
        assert_equal [:current], end_one(LoginSessions.public_id('here'))
        assert_equal [:not_found], end_one(LoginSessions.public_id('teacher'))
        assert_equal [:impersonating], end_one(LoginSessions.public_id('phone'), :impersonating => true)
    end

    def test_a_sid_or_garbage_is_no_id
        assert_equal [:invalid], end_one('phone')
        assert_equal [:invalid], end_one('')
        assert_equal [:invalid], end_one(LoginSessions.public_id('phone').upcase)
    end
end
