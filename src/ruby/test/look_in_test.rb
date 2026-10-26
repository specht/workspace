require 'minitest/autorun'
require 'set'
require 'date'
require_relative '../include/look_in'

class LookInTest < Minitest::Test
    ADMINS = ['admin@example.org'].freeze
    TEACHERS = {
        'lehrer@example.org' => Set['LK Informatik'],
        'kollegin@example.org' => Set['Klasse 11'],
    }.freeze
    MEMBERS = {
        'Klasse 11' => %w[tom@example.org kollegin@example.org],
        'Klasse 12' => %w[lena@example.org],
        'LK Informatik' => %w[lena@example.org mia@example.org],
    }.freeze

    def allowed?(viewer, target)
        LookIn.allowed?(viewer, target, :admins => ADMINS, :teachers => TEACHERS, :group_members => MEMBERS)
    end

    def test_a_teacher_looks_into_students_of_its_groups_only
        assert allowed?('lehrer@example.org', 'lena@example.org') # LK, though Klasse 12 comes first
        assert allowed?('lehrer@example.org', 'mia@example.org')
        refute allowed?('lehrer@example.org', 'tom@example.org')
    end

    def test_never_into_teachers_administrators_or_oneself
        refute allowed?('lehrer@example.org', 'kollegin@example.org')
        refute allowed?('lehrer@example.org', 'admin@example.org')
        refute allowed?('lehrer@example.org', 'lehrer@example.org')
        refute allowed?('lena@example.org', 'mia@example.org') # a student
        assert allowed?('admin@example.org', 'kollegin@example.org')
        refute allowed?('admin@example.org', 'admin@example.org')
    end

    def test_the_test_account_is_the_teachers_address_with_plus_test
        assert_equal 'lehrer+test@schule.de', LookIn.test_account_email('Lehrer@Schule.de')
        assert_nil LookIn.test_account_email('lehrer+test@schule.de')
        assert_nil LookIn.test_account_email('kein-at')
    end

    def test_everyone_needs_a_reason_administrators_too
        assert_nil LookIn.reason('')
        assert_nil LookIn.reason(' ok ')
        assert_equal 'Ich sehe mir deine Abgabe an.', LookIn.reason("  Ich sehe mir\n deine Abgabe an.  ")
        assert_nil LookIn.reason('', :admin => true)
        assert_nil LookIn.reason('kurz', :admin => true)
        assert_equal LookIn::MAX_REASON, LookIn.reason('x' * 500).size
    end

    def test_the_notice_says_who_when_what_and_why_and_escapes
        now = Time.local(2026, 10, 14, 15, 0)
        look = { :by_name => 'Herr <Beispiel>', :ts => Time.local(2026, 10, 14, 10, 12).to_i, :kind => 'workspace',
                 :reason => 'Ich sehe mir deine <b>Abgabe</b> an.' }
        html = LookIn.notice_html(look, :now => now)
        assert_includes html, 'Herr &lt;Beispiel&gt;'
        assert_includes html, 'heute um 10:12 Uhr deinen Workspace angesehen'
        assert_includes html, '»Ich sehe mir deine &lt;b&gt;Abgabe&lt;/b&gt; an.«'
        earlier = LookIn.notice_html(look.merge(:ts => Time.local(2026, 10, 9, 8, 5).to_i, :kind => 'impersonate'), :now => now)
        assert_includes earlier, 'am 09.10. um 08:05 Uhr dein Konto angesehen'
    end

    def test_stop_and_cleanup_say_what_happened_and_that_the_files_stay
        now = Time.local(2026, 10, 19, 15, 0)
        base = { :by_name => 'Admin', :ts => Time.local(2026, 10, 19, 14, 0).to_i, :reason => 'Der Server war voll.' }
        stop = LookIn.notice_html(base.merge(:kind => 'stop'), :now => now)
        assert_includes stop, 'heute um 14:00 Uhr deinen Workspace gestoppt: »Der Server war voll.«'
        assert_includes stop, 'Deine Dateien sind alle noch da'
        cleanup = LookIn.notice_html(base.merge(:kind => 'cleanup', :details => 'svelte/node_modules (1,2 GB, npm install)'), :now => now)
        assert_includes cleanup, 'Platz geschaffen'
        assert_includes cleanup, 'svelte/node_modules (1,2 GB, npm install)'
        assert_equal %w[impersonate workspace stop cleanup], LookIn::KINDS
    end
end
