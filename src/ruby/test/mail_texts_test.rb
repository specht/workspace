require 'minitest/autorun'
require 'date'
require_relative '../include/mail_texts'

class MailTextsTest < Minitest::Test
    WEB = 'https://code.example.org'

    def test_welcome_student_says_how_to_log_in_and_escapes
        subject, html = MailTexts.welcome_student(:web_root => WEB, :group => '10a <b>', :teacher => 'Frau Berger', :email => 'lena@example.org')
        assert_equal 'Willkommen im Workspace: 10a <b>', subject
        assert_includes html, '10a &lt;b&gt;'
        assert_includes html, 'lena@example.org'
        assert_includes html, "#{WEB}/impressum#datenschutz"
        assert_includes html, 'Frau Berger'
    end

    def test_welcome_teacher_follows_the_approval
        args = { :web_root => WEB, :school => 'BS 7', :email => 't@example.org', :signature => 'Admin', :documents_url => "#{WEB}/datenschutz?schule=bs7" }
        _, waiting = MailTexts.welcome_teacher(**args, :school_approved => false)
        _, ready = MailTexts.welcome_teacher(**args, :school_approved => true, :cost_note => 'Der Server kostet 40 € im Monat.')
        assert_includes waiting, 'noch nicht zugestimmt'
        assert_includes waiting, 'schriftliches Einverständnis'
        assert_includes waiting, 'Anmelden können sich deine Schülerinnen und Schüler schon jetzt'
        refute_includes waiting, 'Server kostet'
        assert_includes ready, 'hat dem Einsatz zugestimmt'
        assert_includes ready, 'Der Server kostet 40 € im Monat.'
        assert_includes ready, "#{WEB}/gruppen"
        assert_includes ready, "#{WEB}/datenschutz?schule=bs7"
    end

    def test_welcome_student_links_the_notice_that_applies
        _, plain = MailTexts.welcome_student(:web_root => WEB, :group => '10a', :teacher => 'T', :email => 's@example.org')
        assert_includes plain, "#{WEB}/impressum#datenschutz"
        _, partner = MailTexts.welcome_student(:web_root => WEB, :group => '10a', :teacher => 'T', :email => 's@example.org', :privacy_url => "#{WEB}/datenschutz/datenschutzerklaerung?schule=bs7")
        assert_includes partner, 'schule=bs7'
    end

    def test_deletion_notice_says_the_date_and_that_nothing_is_to_do
        subject, html = MailTexts.deletion_notice(:web_root => WEB, :date => Date.new(2026, 11, 1), :signature => 'Admin')
        assert_includes subject, '01.11.2026'
        assert_includes html, "#{WEB}/profil#my-data"
        assert_includes html, 'als Gast'
        assert_includes html, 'musst du nichts tun'
    end

    def test_deletion_texts
        _, html = MailTexts.account_deleted(:web_root => WEB, :signature => 'X', :still_invited => false)
        assert_includes html, 'nicht mehr eingeladen'
        subject, = MailTexts.deletion_code(:code => '123456', :minutes => 10, :signature => 'X')
        assert_includes subject, '123456'
    end
end
