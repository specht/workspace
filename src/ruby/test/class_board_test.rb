require 'minitest/autorun'
require_relative '../include/class_board'

class ClassBoardTest < Minitest::Test
    TASKS = [
        { :id => 'adresse', :title => 'Deine Adresse', :type => 'answer', :version => 'v1' },
        { :id => 'ttl-drei', :title => 'TTL 3', :type => 'choice', :version => 'v1', :prediction => true },
        { :id => 'erklaeren', :title => 'In eigenen Worten', :type => 'reflection', :version => 'v1' },
        { :id => 'abgabe', :title => 'Abgabe', :type => 'submit', :version => 'v1' },
    ]
    MEMBERS = [
        { :email => 'tom@x', :name => 'Tom' }, { :email => 'ada@x', :name => 'Ada' },
        { :email => 'ben@x', :name => 'Ben' }, { :email => 'eve@x', :name => 'Eve <script>' },
    ]

    def state(status, attempts: 1, answer: 'x', version: 'v1', first: nil)
        { :status => status, :attempts => attempts, :last_answer => answer, :version => version, :first_status => first || status, :updated => 1 }
    end

    def board
        ClassBoard.build(MEMBERS, TASKS, {
            'ada@x' => { 'adresse' => state('correct', attempts: 2), 'ttl-drei' => state('correct', first: 'wrong'),
                         'erklaeren' => state('sent', answer: 'Es schickt Pakete mit wachsender TTL.'),
                         'abgabe' => state('sent', answer: '14 Dateien, 2,3 MB') },
            'ben@x' => { 'adresse' => state('wrong', attempts: 4, answer: '127.0.0.1'), 'ttl-drei' => state('correct') },
            'tom@x' => { 'adresse' => state('wrong', answer: '10.0.0.1'), 'erklaeren' => state('sent', version: 'v0') },
            'eve@x' => { 'adresse' => state('correct') },
        })
    end

    def test_rows_are_sorted_and_counted
        b = board
        assert_equal ['Ada', 'Ben', 'Eve <script>', 'Tom'], b[:rows].map { |r| r[:name] }
        assert_equal 4, b[:rows][0][:solved]
        assert_equal %w[wrong correct none none], b[:rows][1][:cells].map(&:status)
        assert b[:rows][1][:cells][0].stuck
        refute b[:rows][3][:cells][0].stuck
        assert_equal 'stale', b[:rows][3][:cells][2].status
    end

    def test_summary
        s = board[:summary]
        assert_equal 2, s[0][:done]
        assert_equal 4, s[0][:tried]
        assert_equal 1, s[0][:stuck]
        assert s[0][:often_wrong] # 2 of 4 wrong
        refute s[1][:often_wrong]
        assert_equal 1, s[1][:predicted_right]
        assert_nil s[0][:predicted_right]
    end

    def test_html
        html = ClassBoard.html(board, activity: 'tcpip', submission_url: ->(email, id) { "/dl/#{id}?email=#{email}" })
        assert_includes html, 'Eve &lt;script&gt;'
        refute_includes html, '<script>'
        assert_includes html, "class='cb-cell cb-wrong cb-stuck'"
        assert_includes html, "<use href='/include/tabler/tabler.svg#x'/></svg><small>4</small>"
        assert_includes html, "aria-label='gelöst'"
        refute_match(/[✓✗●↻]/, html)
        assert_includes html, '1 hängen'
        assert_includes html, '1 richtig vermutet'
        assert_includes html, "title='gelöst, 1 Versuch, Vermutung zuerst falsch'"
        assert_includes html, 'Es schickt Pakete mit wachsender TTL.'
        assert_includes html, "<a class='cb-submission' href='/dl/abgabe?email=ada@x' data-view-title='Ada: Abgabe'>14 Dateien, 2,3 MB</a>"
        assert_includes html, '3. In eigenen Worten: 2 Antworten'
        assert_includes html, "cb-task cb-often-wrong"
    end

    def test_own_row_below_the_class
        states = { 'ada@x' => { 'adresse' => state('correct') }, 'me@x' => { 'abgabe' => state('sent', answer: '3 Dateien, 1 kB') } }
        b = ClassBoard.build(MEMBERS, TASKS, states, viewer: { :email => 'me@x', :name => 'Du (Lehrkraft)' })
        assert_equal 4, b[:rows].size
        assert_equal 'sent', b[:own][:cells][3].status
        assert_equal 0, b[:summary][3][:done]
        html = ClassBoard.html(b, activity: 'tcpip', submission_url: ->(email, id) { "/dl/#{id}?email=#{email}" })
        assert_match(%r{</tbody><tbody class='cb-own'><tr data-email='me@x'><th class='cb-name'>Du \(Lehrkraft\)</th>}, html)
        assert_includes html, "href='/dl/abgabe?email=me@x' data-view-title='Du (Lehrkraft): Abgabe'>3 Dateien, 1 kB</a>"
        assert_includes html, '4. Abgabe: 1 Abgabe<'

        # Nothing tried yet, or a member of the group anyway: no extra row.
        assert_nil ClassBoard.build(MEMBERS, TASKS, {}, viewer: { :email => 'me@x', :name => 'Du' })[:own]
        assert_nil ClassBoard.build(MEMBERS, TASKS, states, viewer: { :email => 'ada@x', :name => 'Du' })[:own]
    end

    def test_every_icon_is_in_the_bundled_sprite
        sprite = File.read(File.expand_path('../../static/include/tabler/tabler.svg', __dir__), encoding: 'UTF-8')
        used = ClassBoard::SYMBOLS.values.map(&:first) + %w[help]
        script = File.read(File.expand_path('../../static/include/class_board.js', __dir__), encoding: 'UTF-8')
        used += script[/const ICONS = \{[^}]*\}/].scan(/: '([a-z-]+)'/).flatten
        used.uniq.each { |id| assert_includes sprite, "<symbol id=\"#{id}\"", id }
    end
end
