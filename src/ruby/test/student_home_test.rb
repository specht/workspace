require 'minitest/autorun'
require_relative '../include/student_home'

class StudentHomeTest < Minitest::Test
    ACTIVITIES = [
        { :slug => 'tcpip', :activity => 'tcpip', :title => 'TCP/IP', :tasks => [
            { :id => 'adresse', :title => 'Deine Adresse', :version => 'a' },
            { :id => 'loopback', :title => 'Loopback', :version => 'b' },
            { :id => 'ttl', :title => 'TTL', :version => 'c' }] },
        { :slug => 'uebung-strings', :activity => 'uebung-strings', :title => 'Übung: Strings', :tasks => [
            { :id => 'reverse', :title => 'Umkehren', :version => 'x' }] },
        { :slug => 'showerjs', :activity => 'showerjs', :title => 'shower.js', :tasks => [
            { :id => 'vorlage', :title => 'Vorlage', :version => 'v' }] },
    ]

    def st(status, version, updated)
        { :status => status, :version => version, :updated => updated }
    end

    def progress
        StudentHome.progress(ACTIVITIES, {
            'tcpip/adresse' => st('correct', 'a', 10), 'tcpip/ttl' => st('wrong', 'c', 30),
            'uebung-strings/reverse' => st('correct', 'x', 50),
            'showerjs/vorlage' => st('correct', 'old', 20),
        })
    end

    def test_progress
        p = progress
        assert_equal %w[tcpip showerjs uebung-strings], p.map { |x| x[:slug] }
        assert_equal [1, 3], [p[0][:done], p[0][:total]]
        assert_equal %i[done open wrong], p[0][:marks]
        assert_equal 'loopback', p[0][:next][:id]
        assert_equal [:stale], p[1][:marks]
        assert_nil p[2][:next]
    end

    # An extra counts for nobody's »x von y«; done, it is said besides.
    def test_extras_count_apart
        katas = [{ :slug => 'katas', :activity => 'katas', :title => 'Katas', :tasks => [
            { :id => 'add', :title => 'Addieren', :version => 'a' },
            { :id => 'primes', :title => 'Primzahlen', :version => 'b', :extra => true },
            { :id => 'double', :title => 'Verdoppeln', :version => 'c' }] }]
        p = StudentHome.progress(katas, { 'katas/add' => st('correct', 'a', 10), 'katas/primes' => st('correct', 'b', 20),
                                          'katas/double' => st('correct', 'c', 30) }).first
        assert_equal [2, 2, 1], [p[:done], p[:total], p[:extras_done]]
        assert_nil p[:next]
        html = StudentHome.card('katas', 'Katas', p)
        assert_includes html, '2 von 2 gelöst, dazu 1 Extra'
        p = StudentHome.progress(katas, { 'katas/primes' => st('correct', 'b', 20) }).first
        assert_equal [0, 2, 1], [p[:done], p[:total], p[:extras_done]]
        assert_equal 'add', p[:next][:id]
    end

    def test_pinned_activities_come_first_with_their_progress
        pinned = [{ :slug => 'tcpip', :title => 'TCP/IP', :group => '10b' },
                  { :slug => 'latex', :title => 'LaTeX', :abstract => 'Setze <b>Formeln</b>.', :group => 'WP 10' }]
        html = StudentHome.html(:progress => progress, :pinned => pinned)
        assert_includes html, 'Von deiner Lehrkraft'
        assert_includes html, "href='/tcpip#aufgabe-loopback'"
        assert_includes html, '1 von 3 gelöst'
        assert_includes html, 'Angeheftet für 10b'
        assert_includes html, 'Setze Formeln.'
        refute_includes html, 'shc-template'
        assert_operator html.index('Von deiner Lehrkraft'), :<, html.index('student-continue')
        with_template = StudentHome.html(:progress => progress,
            :pinned => [{ :slug => 'sandbox-cpu', :title => 'CPU', :group => '12', :template_folder => 'cpu-sandbox' }])
        assert_includes with_template, "class='shc-template' data-slug='sandbox-cpu' data-folder='cpu-sandbox'"
    end

    # »Weiter mit«: started and unfinished, most recent first, without the
    # pinned ones (they show their progress above) and without finished ones.
    def test_continue_with_recent_unfinished_activities
        html = StudentHome.html(:progress => progress)
        assert_includes html, 'Weiter mit'
        assert_includes html, "href='/tcpip#aufgabe-loopback'"
        assert_includes html, '1 von 3 gelöst'
        assert_includes html, '0 von 1 gelöst<'
        assert_operator html.index('/tcpip'), :<, html.index('/showerjs')
        refute_includes html, 'uebung-strings'
        refute_includes html, 'So geht'

        pinned = StudentHome.html(:progress => progress, :pinned => [{ :slug => 'tcpip', :title => 'TCP/IP', :group => '10b' }])
        refute_includes pinned[pinned.index('student-continue')..], '/tcpip'
        assert_equal [], StudentHome.continue_entries(progress, :listed => ['uebung-strings'])
        many = (1..5).map { |i| { :slug => "a#{i}", :title => "A#{i}", :done => 0, :total => 2, :marks => %i[wrong open], :updated => i, :next => { :id => 'x' } } }
        assert_equal %w[a5 a4 a3], StudentHome.continue_entries(many).map { |e| e[:slug] }
    end

    # »So geht's« only before the first check; it names no activity to
    # start with, but points at what the teacher pinned, if anything.
    def test_how_it_works_until_the_first_check
        html = StudentHome.html(:progress => [])
        assert_includes html, 'So geht'
        assert_includes html, 'frag deine Lehrkraft'
        assert_includes html, 'Prüfen'
        assert_includes html, "href='/reference'"
        assert_includes html, '<strong>Labor</strong>'
        refute_includes html, 'student-continue'
        with_pins = StudentHome.html(:progress => [], :pinned => [{ :slug => 'tcpip', :title => 'TCP/IP', :group => '10b' }])
        assert_includes with_pins, 'die deine Lehrkraft oben für deine Klasse ausgewählt hat'
        assert_operator with_pins.index('Von deiner Lehrkraft'), :<, with_pins.index('So geht')
        refute_includes StudentHome.html(:progress => progress), 'So geht'
    end

    def test_teacher_accounts_see_only_what_is_pinned
        assert_equal '', StudentHome.html(:progress => [], :guide => false)
        assert_equal '', StudentHome.html(:progress => progress, :guide => false)
        html = StudentHome.html(:progress => progress, :pinned => [{ :slug => 'tcpip', :title => 'TCP/IP', :group => '10b' }], :guide => false)
        assert_includes html, 'Von deiner Lehrkraft'
        refute_includes html, 'student-continue'
    end

    def test_card_badges
        assert_equal '', StudentHome.card_badges(nil)
        assert_includes StudentHome.card_badges('uebung'), 'Übung'
        assert_includes StudentHome.card_badges('uebung'), "title='Eine Übung trainiert eine Fertigkeit'"
        assert_includes StudentHome.card_badges('projekt', 'C / raylib'), '>Projekt (C / raylib)<'
        assert_equal '', StudentHome.card_badges(nil, 'Ruby')
    end

    def test_card_requires
        assert_equal '', StudentHome.card_requires([])
        assert_includes StudentHome.card_requires(['Echtzeitgrafik mit C']), 'Baut auf Echtzeitgrafik mit C auf'
        assert_includes StudentHome.card_requires(['Dateien', 'Git', 'HTML &amp; CSS']), 'Baut auf Dateien, Git und HTML &amp; CSS auf'
    end
end
