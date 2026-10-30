require 'minitest/autorun'
require_relative '../include/authentication'

class AuthenticationTest < Minitest::Test
    def test_development_login_code_is_stable
        assert_equal '123456', Authentication.generate_login_code(:development => true)
    end

    # The database stores the hash; the cookie's sid is not in it.
    def test_session_key_is_the_sha256_of_the_sid
        sid = 'abcdefghijklmnopqrstuvwx'
        key = Authentication.session_key(sid)
        assert_equal Digest::SHA256.hexdigest(sid), key
        assert_equal 64, key.size
        refute_includes key, sid
        refute_equal key, Authentication.session_key('abcdefghijklmnopqrstuvwy')
    end

    def test_production_login_codes_are_six_digits
        25.times do
            assert_match(/\A[0-9]{6}\z/, Authentication.generate_login_code(:development => false))
        end
    end

    def test_login_request_expiry_and_attempt_limit
        now = 1_000
        expires_at = Authentication.login_request_expires_at(:now => now)

        assert_equal now + 600, expires_at
        assert Authentication.login_request_active?(:expires_at => expires_at, :attempts => 0, :now => now)
        refute Authentication.login_request_active?(:expires_at => now, :attempts => 0, :now => now)
        refute Authentication.login_request_active?(:expires_at => expires_at, :attempts => 5, :now => now)
    end

    def test_login_request_input_shapes
        assert Authentication.valid_login_tag?('1234bcdfghjk')
        refute Authentication.valid_login_tag?('1234bcdfghj')
        refute Authentication.valid_login_tag?('../123456789')

        assert Authentication.valid_login_code?('000123')
        refute Authentication.valid_login_code?('12345')
        refute Authentication.valid_login_code?('12345x')
    end

    def test_qr_login_request_expiry_and_input_shapes
        now = 2_000
        assert_equal now + 300, Authentication.qr_login_request_expires_at(:now => now)

        assert Authentication.valid_qr_login_tag?('b' * 16)
        refute Authentication.valid_qr_login_tag?('b' * 15)
        refute Authentication.valid_qr_login_tag?('../' + ('b' * 13))

        assert Authentication.valid_qr_login_secret?('c' * 32)
        refute Authentication.valid_qr_login_secret?('c' * 31)
        refute Authentication.valid_qr_login_secret?('C' * 32)
    end

    def test_session_cookie_policy
        assert_equal 'hs_sid', Authentication.session_cookie_name(:development => true)
        assert_equal '__Host-hs_sid', Authentication.session_cookie_name(:development => false)

        expires = Time.at(1234)
        production = Authentication.session_cookie_options(
            :value => 'abc',
            :expires => expires,
            :development => false,
        )
        assert_equal '/', production[:path]
        assert production[:httponly]
        assert production[:secure]
        assert_equal :lax, production[:same_site]
        refute production.key?(:domain)

        shared = Authentication.domain_cookie_options(
            :value => 'abc',
            :expires => expires,
            :domain => 'workspace.example',
            :development => false,
        )
        assert_equal '.workspace.example', shared[:domain]
    end

    def test_same_origin_rejects_sibling_subdomains
        expected = 'https://workspace.example'

        assert Authentication.same_origin?(
            :origin => 'https://workspace.example',
            :referer => nil,
            :expected_origin => expected,
        )
        assert Authentication.same_origin?(
            :origin => nil,
            :referer => 'https://workspace.example/profil',
            :expected_origin => expected,
        )
        refute Authentication.same_origin?(
            :origin => 'https://student.workspace.example',
            :referer => nil,
            :expected_origin => expected,
        )
        refute Authentication.same_origin?(
            :origin => 'https://workspace.example.evil.invalid',
            :referer => nil,
            :expected_origin => expected,
        )
        assert Authentication.same_origin?(
            :origin => nil,
            :referer => nil,
            :expected_origin => expected,
        )
        refute Authentication.same_origin?(
            :origin => 'http://workspace.example:8026',
            :referer => nil,
            :expected_origin => 'http://workspace.example:8025',
        )
    end

    # /login?next=…: a path on this site, else »/«.
    def test_next_path_keeps_paths_on_this_site
        ['/', '/rle', '/rle#aufgabe-von-hand', '/profil', '/klasse?tab=lernende',
         '/reference/git#zum-nachschlagen', '/%2F%2Fevil.example', '/login-hilfe',
        ].each do |path|
            assert_equal path, Authentication.safe_next_path(path), path
        end
    end

    def test_next_path_rejects_other_sites_and_loops
        ['//evil.example', '//evil.example/rle', '/\\evil.example', '/\\/evil.example',
         'https://evil.example', 'http:/evil.example', 'javascript:alert(1)', 'evil.example',
         "/\t/evil.example", "/\n/evil.example", "/ /evil.example", '/rle\\x',
         '/<script>', '/"quoted"', '', nil, 42, ['/rle'], '/' + 'a' * 1000,
         '/login', '/login.html', '/login?next=/rle', '/logout', '/logout?x=1', '/l/abc',
         '/./logout', '/x/../logout', '/%2e%2e/logout', '/.', '/rle/..',
        ].each do |path|
            assert_equal '/', Authentication.safe_next_path(path), path.inspect
        end
    end
end
