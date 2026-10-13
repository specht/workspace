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

    def test_html
        html = StudentHome.html(:name => 'Lena Muster', :progress => progress)
        assert_includes html, 'Hallo Lena'
        assert_includes html, "href='/tcpip#aufgabe-loopback'"
        assert_includes html, 'Weiter mit: Loopback'
        assert_includes html, '1 von 3 erledigt'
        assert_includes html, "Fertig: <a href='/uebung-strings'>Übung: Strings</a>"
        assert_includes StudentHome.html(:name => 'x@y.de', :progress => []), "<h2 id='student-home-title'>Hallo</h2>"
        assert_includes StudentHome.html(:name => 'Ada', :progress => []), 'Du hast noch keine Aufgabe geprüft'
        assert_includes StudentHome.html(:name => '<b>', :progress => []), '&lt;b&gt;'
    end

    def test_card_badges
        assert_equal '', StudentHome.card_badges(nil, nil)
        assert_includes StudentHome.card_badges('uebung', nil), 'Übung'
        assert_includes StudentHome.card_badges('projekt', { :done => 2, :total => 2 }), 'card-progress-done'
    end

    def test_first_name_skips_titles
        assert_equal 'Michael', StudentHome.first_name('Dr. Michael Specht')
        assert_equal 'Anna', StudentHome.first_name('Prof. Dr. Anna Schmidt')
        assert_equal 'Lena', StudentHome.first_name('Lena Muster')
        assert_equal 'Kim', StudentHome.first_name('Kim')
        assert_equal '', StudentHome.first_name('kim@example.org')
        assert_includes StudentHome.html(:name => 'Dr. Michael Specht', :progress => []), 'Hallo Michael<'
    end

    def test_hidden_activities_come_back_when_worked_on
        p = progress
        html = StudentHome.html(:name => 'Lena', :progress => p, :hidden => { 'tcpip' => 30 })
        refute_includes html, "data-slug='tcpip'"
        assert_includes html, '1 angefangene Aktivität ist ausgeblendet'
        assert_includes html, 'student-home-unhide'
        html = StudentHome.html(:name => 'Lena', :progress => p, :hidden => { 'tcpip' => 29 })
        assert_includes html, "data-slug='tcpip'"
        assert_includes html, "class='shc-hide'"
    end

    def test_pinned_activities_come_first_and_cannot_be_hidden
        pinned = [{ :slug => 'tcpip', :title => 'TCP/IP', :group => '10b' },
                  { :slug => 'latex', :title => 'LaTeX', :abstract => 'Setze <b>Formeln</b>.', :group => 'WP 10' }]
        html = StudentHome.html(:name => 'Lena', :progress => progress, :pinned => pinned, :hidden => { 'tcpip' => 99 })
        assert html.index('Von deiner Lehrkraft') < html.index('Weitermachen')
        pinned_part = html[html.index('Von deiner Lehrkraft')...html.index('Weitermachen')]
        assert_includes pinned_part, "data-slug='tcpip'"
        assert_includes pinned_part, 'Angeheftet für 10b'
        assert_includes pinned_part, 'Setze Formeln.'
        refute_includes pinned_part, 'shc-hide'
        assert_equal 1, html.scan("data-slug='tcpip'").size
    end
end
