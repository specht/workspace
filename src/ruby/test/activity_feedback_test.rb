require 'minitest/autorun'
require_relative '../include/activity_feedback'

# Feedback of teachers on an activity (ActivityFeedback): what is stored,
# when the class page asks, what the administration's page counts.
class ActivityFeedbackTest < Minitest::Test
    DAY = ActivityFeedback::DAY
    NOW = 1_800_000_000

    def test_normalize_keeps_known_choices_and_cuts_texts
        raw = { 'overall' => 'good', 'duration' => 'forever', 'level' => 'hard', 'overall_text' => "  lief gut \r\n",
                'text' => 'x' * 5000, 'languages' => %w[python java cobol ruby], 'languages_text' => ' Kotlin ', 'contact' => true }
        a = ActivityFeedback.normalize(raw, :offered => ['ruby'])
        assert_equal 'good', a['overall']
        assert_nil a['duration']
        assert_equal 'hard', a['level']
        assert_equal 'lief gut', a['overall_text']
        assert_equal ActivityFeedback::TEXTS['text'], a['text'].size
        # only the five, and not those the activity has already
        assert_equal %w[python java], a['languages']
        assert_equal 'Kotlin', a['languages_text']
        assert_equal true, a['contact']
        # every key is there, so that a cleared answer is removed (SET f += $answers)
        assert_equal (ActivityFeedback::CHOICES.keys + ActivityFeedback::TEXTS.keys + %w[languages contact]).sort, a.keys.sort
    end

    def test_at_least_one_answer
        assert_nil ActivityFeedback.normalize({})
        assert_nil ActivityFeedback.normalize(nil)
        assert_nil ActivityFeedback.normalize({ 'contact' => true, 'text' => '   ', 'overall' => 'great' })
        assert_nil ActivityFeedback.normalize({ 'languages' => ['ruby'] }, :offered => ['ruby'])
        assert ActivityFeedback.normalize({ 'languages' => ['dart'] })
        assert ActivityFeedback.normalize({ 'level' => 'fit' })
        assert_equal false, ActivityFeedback.normalize({ 'text' => 'Gut.', 'contact' => 'yes' })['contact']
    end

    def prompt(feedback: nil, quiet_days: 8, students: 5, size: 20)
        last = quiet_days && NOW - quiet_days * DAY
        ActivityFeedback.prompt(:feedback => feedback, :last => last, :students => students, :size => size, :now => NOW)
    end

    # The rule: results of at least three students (all, in a smaller
    # group), none for seven days, the last one at most sixty days ago.
    def test_looks_finished
        assert_equal :due, prompt
        assert_equal :due, prompt(:quiet_days => 7)
        assert_equal :in_progress, prompt(:quiet_days => 6)
        assert_equal :in_progress, ActivityFeedback.prompt(:feedback => nil, :last => NOW - 7 * DAY + 1, :students => 5, :size => 20, :now => NOW)
        assert_equal :due, prompt(:quiet_days => 60)
        assert_equal :too_old, prompt(:quiet_days => 61)
        assert_equal :not_worked, prompt(:quiet_days => nil, :students => 0)
        assert_equal :not_worked, prompt(:students => 2)
        assert_equal :due, prompt(:students => 3)
        # a group of two: both
        assert_equal :due, prompt(:students => 2, :size => 2)
        assert_equal :not_worked, prompt(:students => 1, :size => 2)
        # a group without students counted: one result is enough to ask
        assert_equal :due, prompt(:students => 1, :size => 0)
    end

    def test_not_now_never_and_answered
        assert_equal :snoozed, prompt(:feedback => { :snoozed_until => NOW + 1 })
        assert_equal :due, prompt(:feedback => { :snoozed_until => NOW - 1 })
        assert_equal :never, prompt(:feedback => { :never => true, :snoozed_until => NOW - 1 })
        assert_equal :due, prompt(:feedback => { :never => false, :snoozed_until => NOW - 1 })
        # an answer wins over everything, also while the group still works
        assert_equal :answered, prompt(:feedback => { :created_at => NOW - DAY, :never => true }, :quiet_days => 0)
    end

    def test_results_count_students_only
        states = {
            'a@example.org' => { 't1' => { :updated => NOW - 9 * DAY }, 't2' => { :updated => NOW - 8 * DAY } },
            'b@example.org' => { 't1' => { :updated => NOW - 20 * DAY } },
            'teacher@example.org' => { 't1' => { :updated => NOW } },
            'c@example.org' => {},
        }
        r = ActivityFeedback.results(states, %w[a@example.org b@example.org c@example.org])
        assert_equal NOW - 8 * DAY, r[:last]
        assert_equal 2, r[:students]
        assert_equal({ :last => nil, :students => 0 }, ActivityFeedback.results({}, ['a@example.org']))
    end

    def test_current_languages
        assert_equal %w[ruby python], ActivityFeedback.current_languages(:languages => %w[ruby python], :language => 'Ruby / Python')
        assert_equal %w[ruby python], ActivityFeedback.current_languages(:languages => [], :language => 'Ruby / Python / SQL')
        assert_equal %w[python javascript dart java], ActivityFeedback.current_languages(:language => 'Dart / Python / JavaScript / Java')
        assert_equal [], ActivityFeedback.current_languages(:language => 'C')
        assert_equal [], ActivityFeedback.current_languages({})
    end

    def test_form_marks_the_languages_the_activity_has
        info = { :group => '9b', :activity => 'crypto', :title => 'Kryptographie', :duration => '2 Doppelstunden',
                 :language => 'Ruby / Python', :current => %w[ruby python], :email => 'lehrer@example.org' }
        html = ActivityFeedback.form_html(info, { :created_at => NOW, :updated_at => NOW, :level => 'hard', :languages => ['java'], :text => 'a < b' })
        assert_match(/id='feedback-language-ruby' disabled/, html)
        assert_includes html, 'gibt es schon'
        refute_match(/name='languages' id='feedback-language-ruby'/, html)
        assert_match(/name='languages' id='feedback-language-java' value='java' autocomplete='off' checked/, html)
        assert_match(/id='feedback-level-hard' value='hard' autocomplete='off' checked/, html)
        assert_includes html, 'Angesetzt: 2 Doppelstunden'
        assert_includes html, 'Jetzt: Ruby / Python'
        assert_includes html, 'a &lt; b'
        assert_includes html, 'Änderung speichern'
        assert_includes html, 'lehrer@example.org'
        assert_includes html, ' hidden>'
        refute_includes ActivityFeedback.form_html(info.merge(:duration => nil), nil), 'Angesetzt'
        assert_includes ActivityFeedback.form_html(info, nil), 'Rückmeldung senden'
    end

    def test_card_and_hint
        card = ActivityFeedback.card_html('9b', 'Kryptographie', 9)
        assert_includes card, 'Wie lief die Aktivität?'
        assert_includes card, 'Seit 9 Tagen kam aus 9b keine neue Antwort zu »Kryptographie«'
        %w[Nicht\ jetzt Nicht\ mehr\ fragen Rückmeldung\ geben].each { |label| assert_includes card, label }
        assert_includes ActivityFeedback.card_html('9b', 'X', 1), 'Seit einem Tag'
        hint = ActivityFeedback.home_hint_html('9b', 'crypto', 'Kryptographie', 8, '/klasse?group=9b&tab=fortschritt&activity=crypto#rueckmeldung')
        assert_includes hint, "data-activity='crypto'"
        assert_includes hint, 'activity=crypto#rueckmeldung'
        # shown by the script, so that a browser that hid it never flashes it
        assert_includes hint, ' hidden>'
    end

    def rows
        [
            { :activity => 'crypto', :title => 'Kryptographie', :slug => 'crypto', :group => '9b', :teacher => 'Lehrkraft A', :email => 'a@example.org',
              :feedback => { :group => '9b', :overall => 'good', :duration => 'longer', :duration_planned => '2 Doppelstunden', :languages => %w[python java],
                             :text => 'Mehr Zeit für Aufgabe 3.', :contact => true, :created_at => NOW - 2 * DAY, :updated_at => NOW - DAY } },
            { :activity => 'crypto', :title => 'Kryptographie', :slug => 'crypto', :group => nil, :teacher => 'Lehrkraft B', :email => 'b@example.org',
              :feedback => { :former_group => '10a', :overall => 'mixed', :level => 'hard', :languages => ['python'], :languages_text => 'Kotlin',
                             :contact => false, :created_at => NOW - 3 * DAY } },
            { :activity => 'sql', :title => 'SQL', :slug => 'sql', :group => '9b', :teacher => 'Lehrkraft A', :email => 'a@example.org',
              :feedback => { :group => '9b', :level => 'easy', :created_at => NOW - 30 * DAY } },
        ]
    end

    def test_summary_counts_per_activity
        summary = ActivityFeedback.summary(rows)
        assert_equal %w[crypto sql], summary.map { |a| a[:activity] }
        crypto = summary.first
        assert_equal 2, crypto[:count]
        assert_equal({ 'good' => 1, 'mixed' => 1, 'bad' => 0 }, crypto[:counts]['overall'])
        assert_equal({ 'shorter' => 0, 'fit' => 0, 'longer' => 1 }, crypto[:counts]['duration'])
        assert_equal({ 'python' => 2, 'java' => 1 }, crypto[:languages])
        assert_equal ['Kotlin'], crypto[:languages_texts]
        # the address only where the teacher ticked the box
        assert_equal ['a@example.org', nil], crypto[:entries].map { |e| e[:email] }
        assert_equal '10a', crypto[:entries].last[:former_group]
    end

    def test_admin_html
        html = ActivityFeedback.admin_html(ActivityFeedback.summary(rows))
        assert_includes html, '3 Rückmeldungen zu 2 Aktivitäten'
        # the overview: a card per activity, newest first, with the bar and the summaries
        assert_operator html.index("data-title='kryptographie'"), :<, html.index("data-title='sql'")
        assert_includes html, "<span class='fb-good' style='flex-grow: 1'></span><span class='fb-mixed' style='flex-grow: 1'></span>"
        assert_includes html, "aria-label='Insgesamt: gut 1, teils, teils 1, nicht gut 0'"
        assert_includes html, '1× länger'
        assert_includes html, "<span class='badge fb-lang'>Python 2</span> <span class='badge fb-lang'>Java 1</span>"
        assert_includes html, 'Kotlin'
        assert_includes html, '1 Text lesen'
        assert_includes html, "href='#fb-crypto'"
        assert_includes html, "id='fb-crypto'"
        assert_includes html, "<a href='/crypto'>Kryptographie</a>"
        # the answers: the address only where the teacher ticked the box
        assert_includes html, 'mailto:a@example.org'
        refute_includes html, 'b@example.org'
        assert_includes html, '10a (Gruppe gelöscht)'
        assert_includes html, 'Mehr Zeit für Aufgabe 3.'
        assert_includes html, "data-text='1' data-contact='1'"
        assert_includes html, "data-text='0' data-contact='0'"
        refute_includes html, ' · '
        assert_includes ActivityFeedback.admin_html([]), 'Noch keine Rückmeldung'
        assert_includes ActivityFeedback.admin_html([]), 'Rückmeldung geben'
    end

    def test_tally_text
        assert_equal '2× länger, 1× passte', ActivityFeedback.tally_text('duration', { 'shorter' => 0, 'fit' => 1, 'longer' => 2 })
        assert_nil ActivityFeedback.tally_text('level', { 'easy' => 0, 'fit' => 0, 'hard' => 0 })
        assert_includes ActivityFeedback.bar_html({ 'good' => 0, 'mixed' => 0, 'bad' => 0 }), 'fb-bar-empty'
    end

    # Who may: a teacher of the group, through feedback_group!, before
    # anything is read or written; the page /rueckmeldungen only for the
    # administration; the node hangs on the User by BY, so the account's
    # deletion takes it (account_deletion_test.rb).
    def test_routes_check_access_first
        root = File.expand_path('..', __dir__)
        page = File.read(File.join(root, 'include', 'activity_feedback_page.rb'))
        %w[save dismiss].each do |route|
            body = page[/post '\/api\/activity_feedback\/#{route}' do.*?\n    end\n/m]
            assert body, route
            assert_operator body.index('feedback_group!'), :<, body.index('neo4j_query'), route
        end
        assert_includes page, 'admins_only!'
        access = File.read(File.join(root, 'include', 'access.rb'))
        assert_includes access, 'def feedback_group!'
        assert_match(/@impersonated_by\.nil\?/, access[/def feedback_groups.*?\n    end/m])
        assert_match(/@@test_groups/, access[/def feedback_groups.*?\n    end/m])
        main = File.read(File.join(root, 'main.rb'))
        assert_match(%r{if path == '/rueckmeldungen'\n\s*this_is_a_page_for_logged_in_users!\n\s*Main\.load_invitations\(\)\n\s*halt_forbidden_page! unless admin_logged_in\?}, main)
    end
end
