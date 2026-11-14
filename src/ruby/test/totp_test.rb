require 'minitest/autorun'
require_relative '../include/totp'

# Signing in with an Authenticator app (Totp): the codes of RFC 6238, a
# code once only, the clock off by a step, the secret encrypted.
class TotpTest < Minitest::Test
    # RFC 6238, appendix B: the secret »12345678901234567890« (SHA-1), its
    # eight-digit codes; apps show the last six
    RFC = Totp.base32('12345678901234567890')

    def test_the_codes_of_rfc_6238
        assert_equal '94287082', Totp.code(RFC, Totp.step_at(59), :digits => 8)
        assert_equal '07081804', Totp.code(RFC, Totp.step_at(1_111_111_109), :digits => 8)
        assert_equal '89005924', Totp.code(RFC, Totp.step_at(1_234_567_890), :digits => 8)
        assert_equal '287082', Totp.code(RFC, Totp.step_at(59))
    end

    def test_base32_both_ways
        secret = Totp.new_secret
        assert_match(/\A[A-Z2-7]{32}\z/, secret)
        assert_equal 20, Totp.unbase32(secret).bytesize
        assert_equal 'JBSWY3DPEHPK3PXP', Totp.base32('Hello!' + "\xDE\xAD\xBE\xEF".b)
        assert_equal 'Hello!', Totp.unbase32('jbsw y3dp ehpk 3pxp')[0, 6]
    end

    def test_a_code_once_and_a_step_either_side
        now = Time.at(1_700_000_000)
        step = Totp.step_at(now)
        code = Totp.code(RFC, step)
        assert_equal step, Totp.verify(RFC, code, :now => now)
        assert_nil Totp.verify(RFC, code, :now => now, :last_step => step), 'used already'
        assert_equal step - 1, Totp.verify(RFC, Totp.code(RFC, step - 1), :now => now)
        assert_equal step + 1, Totp.verify(RFC, Totp.code(RFC, step + 1), :now => now)
        assert_nil Totp.verify(RFC, Totp.code(RFC, step - 2), :now => now), 'a minute late'
        assert_nil Totp.verify(RFC, '12345', :now => now)
        assert_nil Totp.verify(RFC, nil, :now => now)
    end

    def test_the_uri_an_app_scans
        uri = Totp.uri('JBSWY3DPEHPK3PXP', 'a.b@schule.de', :issuer => 'Hackschule Workspace')
        assert_equal 'otpauth://totp/Hackschule%20Workspace%3Aa.b%40schule.de?secret=JBSWY3DPEHPK3PXP&issuer=Hackschule+Workspace&algorithm=SHA1&digits=6&period=30', uri
        assert_equal 'JBSW Y3DP EHPK 3PXP', Totp.grouped('JBSWY3DPEHPK3PXP')
    end

    def test_encrypted_and_only_with_its_salt
        secret = Totp.new_secret
        stored = Totp.encrypt(secret, 'salt')
        refute_includes stored, secret
        assert_equal secret, Totp.decrypt(stored, 'salt')
        assert_nil Totp.decrypt(stored, 'another salt')
        assert_nil Totp.decrypt('v1:kaputt', 'salt')
        assert_nil Totp.decrypt(nil, 'salt')
    end
end
