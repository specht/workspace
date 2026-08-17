require 'minitest/autorun'
require_relative '../include/authentication'

class AuthenticationTest < Minitest::Test
    def test_development_login_code_is_stable
        assert_equal '123456', Authentication.generate_login_code(:development => true)
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
end
