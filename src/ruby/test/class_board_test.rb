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

    # An extra counts for nobody's »gelöst«; its column is marked.
    def test_extras_do_not_count
        tasks = TASKS.first(1) + [{ :id => 'bonus', :title => 'Bonus', :type => 'answer', :version => 'v1', :extra => true }]
        b = ClassBoard.build(MEMBERS, tasks, { 'ada@x' => { 'adresse' => state('correct'), 'bonus' => state('correct') } })
        assert_equal 1, b[:rows][0][:solved]
        html = ClassBoard.html(b, :activity => 'tcpip')
        assert_includes html, "<td class='cb-total'>1/1</td>"
        assert_includes html, "<th class='cb-task cb-extra'"
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
        assert_includes html, "<use href='#{Icons::SPRITE}#x'/></svg><small>4</small>"
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
        used = ClassBoard::SYMBOLS.values.map(&:first).reject { |id| id.start_with?('bi-') } + %w[help]
        script = File.read(File.expand_path('../../static/include/class_board.js', __dir__), encoding: 'UTF-8')
        used += script[/const ICONS = \{[^}]*\}/].scan(/: '([a-z-]+)'/).flatten
        used.uniq.each { |id| assert_includes sprite, "<symbol id=\"#{id}\"", id }
    end

    # When each one last did something: Ada and Ben are at work, Tom tried
    # once 25 minutes ago, at the start, Eve never. Ada is done, so only
    # Tom and Eve are quiet.
    def timed_board(now, tom: 25 * 60)
        at = ->(status, ago) { state(status).merge(:updated => now - ago) }
        ClassBoard.build(MEMBERS, TASKS[0, 2], {
            'ada@x' => { 'adresse' => at.('correct', 120), 'ttl-drei' => at.('correct', 60) },
            'ben@x' => { 'adresse' => at.('wrong', 300) },
            'tom@x' => { 'adresse' => at.('wrong', tom) },
        })
    end

    def test_nobody_is_quiet_in_the_first_minutes_of_a_lesson
        now = 1_800_000_000
        # the class began 5 minutes ago: Eve, who did nothing yet, is not flagged
        b = timed_board(now, :tom => 5 * 60)
        assert_empty ClassBoard.quiet(b[:rows], b[:tasks], now)
        # 11 minutes in, she is, and Tom, whose only check was at the start
        b = timed_board(now, :tom => 11 * 60)
        assert_equal %w[eve@x tom@x], ClassBoard.quiet(b[:rows], b[:tasks], now).sort
    end

    def test_quiet_while_the_class_is_at_work
        now = 1_800_000_000
        b = timed_board(now)
        assert_equal now - 60, ClassBoard.last_active(b[:rows][0])
        assert_nil ClassBoard.last_active(b[:rows][2])
        assert_equal %w[eve@x tom@x], ClassBoard.quiet(b[:rows], b[:tasks], now).sort
        # an hour later nobody is at work any more: no one is flagged
        assert_empty ClassBoard.quiet(b[:rows], b[:tasks], now + 3600)
    end

    def test_ago_reads_like_a_person
        now = 1_800_000_000
        assert_equal '–', ClassBoard.ago(nil, now)
        assert_equal 'gerade eben', ClassBoard.ago(now - 20, now)
        assert_equal 'vor 12 min', ClassBoard.ago(now - 12 * 60 - 5, now)
        assert_equal 'vor 3 h', ClassBoard.ago(now - 3 * 3600, now)
        assert_equal 'gestern', ClassBoard.ago(now - 30 * 3600, now)
        assert_equal 'vor 4 Tagen', ClassBoard.ago(now - 4 * 86_400, now)
    end

    def test_the_board_shows_when_and_who_is_quiet
        now = 1_800_000_000
        html = ClassBoard.html(timed_board(now), :activity => 'dns', :now => now)
        assert_includes html, "<th class=\"cb-last\""
        assert_includes html, "data-updated='#{now - 60}'>vor 1 min</td>"
        assert_includes html, "data-updated='0'>–</td>"
        assert_match(/<tr data-email='tom@x' class='cb-quiet'>/, html)
        refute_match(/<tr data-email='ada@x' class='cb-quiet'>/, html)
        assert_includes html, "nichts geprüft oder abgegeben: <span class='cb-quiet-names'>Eve &lt;script&gt;, Tom</span>"
        # quiet class, nobody at work: the line is there but hidden
        assert_includes ClassBoard.html(timed_board(now), :activity => 'dns', :now => now + 3600), "<p class='cb-quiet-line' hidden>"
    end

    def test_overview_counts_without_extras_and_old_versions
        activities = [
            { :key => 'dns', :title => 'DNS', :tasks => TASKS + [{ :id => 'mehr', :title => 'Mehr', :type => 'answer', :version => 'v1', :extra => true }] },
            { :key => 'rle', :title => 'RLE', :tasks => [{ :id => 'a', :title => 'A', :type => 'answer', :version => 'v2' }] },
        ]
        states = {
            'ada@x' => { 'dns/adresse' => state('correct').merge(:updated => 50), 'dns/mehr' => state('correct'), 'rle/a' => state('correct', version: 'v2').merge(:updated => 90) },
            'ben@x' => { 'dns/adresse' => state('wrong'), 'rle/a' => state('correct', version: 'v1') },
        }
        o = ClassBoard.overview(MEMBERS, activities, states)
        ada, ben, eve = o[:rows][0], o[:rows][1], o[:rows][2]
        assert_equal [{ :solved => 1, :total => 4, :tried => 1 }, { :solved => 1, :total => 1, :tried => 1 }], ada[:cells]
        assert_equal 90, ada[:last]
        # an answer to an older version counts as tried, not as solved
        assert_equal({ :solved => 0, :total => 1, :tried => 1 }, ben[:cells][1])
        assert_equal 0, eve[:cells][0][:tried]
        html = ClassBoard.overview_html(o, :link => ->(k) { "/klasse?activity=#{k}" }, :pinned => ['rle'], :now => 100)
        assert_includes html, "<a href='/klasse?activity=rle'><span class='cb-task-title'><i class='bi bi-pin-angle-fill' aria-label='angeheftet'></i> RLE</span></a>"
        assert_includes html, "<td class='co-cell co-done' title='1 von 1 gelöst'>"
        assert_includes html, "<span class='co-bar' aria-hidden='true'><span style='width: 25%'></span></span>"
        assert_includes html, "<td class='co-cell co-none' title='noch nichts bearbeitet'>–</td>"
        assert_includes html, "<td class='cb-summary'>1/4</td>"
        assert_includes ClassBoard.overview_html({ :activities => [], :rows => [] }, :link => ->(k) { k }), 'class-overview-empty'
    end
end
