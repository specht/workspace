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
end
