require 'securerandom'
require 'uri'

module Authentication
    LOGIN_REQUEST_TTL_SECONDS = 10 * 60
    LOGIN_REQUEST_MAX_ATTEMPTS = 5
    LOGIN_REQUEST_RESEND_SECONDS = 60
    SESSION_LIFETIME_DAYS = 365

    def self.generate_login_code(development: false)
        return '123456' if development

        SecureRandom.random_number(1_000_000).to_s.rjust(6, '0')
    end

    def self.login_request_expires_at(now: Time.now.to_i)
        now.to_i + LOGIN_REQUEST_TTL_SECONDS
    end

    def self.login_request_active?(expires_at:, attempts:, now: Time.now.to_i)
        expires_at.to_i > now.to_i && attempts.to_i < LOGIN_REQUEST_MAX_ATTEMPTS
    end

    def self.valid_login_tag?(tag)
        tag.is_a?(String) && tag.match?(/\A[0-9a-z]{12}\z/)
    end

    def self.valid_login_code?(code)
        code.is_a?(String) && code.match?(/\A[0-9]{6}\z/)
    end

    def self.session_cookie_name(development:)
        development ? 'hs_sid' : '__Host-hs_sid'
    end

    def self.session_cookie_options(value:, expires:, development:)
        {
            :value => value,
            :expires => expires,
            :path => '/',
            :httponly => true,
            :secure => !development,
            :same_site => :lax,
        }
    end

    def self.domain_cookie_options(value:, expires:, domain:, development:)
        session_cookie_options(
            :value => value,
            :expires => expires,
            :development => development,
        ).merge(:domain => ".#{domain}")
    end

    def self.same_origin?(origin:, referer:, expected_origin:)
        expected = normalize_origin(expected_origin)
        return false if expected.nil?

        unless origin.to_s.empty?
            return normalize_origin(origin) == expected
        end

        unless referer.to_s.empty?
            return normalize_origin(referer) == expected
        end

        # Non-browser/internal clients may omit both. Browser cross-origin POSTs
        # and WebSocket handshakes send Origin, so this still closes the CSRF /
        # cross-site-WebSocket boundary without breaking operational scripts.
        true
    end

    def self.normalize_origin(value)
        uri = URI.parse(value.to_s)
        return nil unless uri.is_a?(URI::HTTP) && uri.host && uri.userinfo.nil?

        scheme = uri.scheme.downcase
        host = uri.host.downcase
        default_port = scheme == 'https' ? 443 : 80
        port = uri.port == default_port ? '' : ":#{uri.port}"
        "#{scheme}://#{host}#{port}"
    rescue URI::InvalidURIError
        nil
    end
    private_class_method :normalize_origin
end
