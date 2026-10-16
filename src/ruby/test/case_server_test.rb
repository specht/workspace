require 'minitest/autorun'
require 'open3'
require_relative '../include/case_server'

# The accounts of the case server: the website and the container's
# entrypoint (docker/case-server/entrypoint.sh) must agree on every password.
class CaseServerTest < Minitest::Test
    SECRET = CaseServer.secret('ein-salt')

    def test_login_is_one_of_the_accounts_and_stays_the_same
        login = CaseServer.login(SECRET, 'Lena@Example.org')
        assert_match(/\Ae\d{3}\z/, login)
        assert_equal login, CaseServer.login(SECRET, 'lena@example.org')
        refute_equal CaseServer.login(SECRET, 'lena@example.org'), CaseServer.login(CaseServer.secret('anders'), 'lena@example.org')
    end

    def test_the_shell_computes_the_same_password
        %w[e000 e042 e999].each do |login|
            out, status = Open3.capture2('sh', '-c',
                %(printf '%s' "password:$1" | openssl dgst -sha256 -hmac "$2" | sed 's/^.*= //' | cut -c1-12), 'sh', login, SECRET)
            assert status.success?
            assert_equal CaseServer.password(SECRET, login), out.strip
        end
    end

    def test_workspace_environment
        env = CaseServer.workspace_environment(SECRET, 'lena@example.org')
        assert_equal %w[CASE_USER CASE_PASSWORD], env.keys
        assert_equal CaseServer.password(SECRET, env['CASE_USER']), env['CASE_PASSWORD']
        assert_match(/\A[0-9a-f]{12}\z/, env['CASE_PASSWORD'])
    end
end
