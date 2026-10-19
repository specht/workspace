require 'minitest/autorun'
require_relative '../include/login_names'
require_relative '../include/database_identity'

class LoginNamesTest < Minitest::Test
    def test_umlauts_as_one_writes_them_without
        assert_equal 'juergen.mueller', LoginNames.linux_login('Jürgen.Müller')
        assert_equal 'aeoeue-strasse', LoginNames.linux_login('ÄÖÜ-Straße')
        assert_equal 'juergen.mueller', DatabaseIdentity.stem_for_email('Jürgen.Müller@example.org')
    end

    def test_other_letters_lose_their_accents
        assert_equal 'zoe.olsen', LoginNames.linux_login('Zoë.Ølsen')
        assert_equal 'francois-nunez', LoginNames.linux_login('françois-núñez')
        assert_equal 'lukasz', LoginNames.linux_login('Łukasz')
        assert_equal 'zoe.olsen', DatabaseIdentity.stem_for_email('Zoë.Ølsen@example.org')
    end

    def test_what_cannot_be_written_becomes_a_dash_or_a_fallback
        assert_equal 'anna-informatik', LoginNames.linux_login('anna+informatik')
        assert_equal 'o-brien', LoginNames.linux_login("o'brien")
        assert_equal 'user-2a.lea', LoginNames.linux_login('2a.lea')
        assert_equal 'student', LoginNames.linux_login('李')
        assert_equal 'ae', LoginNames.linux_login('ä')
    end

    def test_at_most_32_characters_and_no_dot_or_dash_at_the_end
        login = LoginNames.linux_login('sehr.langer.vorname.und.nachname.mit.zusatz')
        assert_operator login.length, :<=, 32
        refute_match(/[.-]\z/, LoginNames.linux_login("#{'a' * 31}.b"))
    end
end
