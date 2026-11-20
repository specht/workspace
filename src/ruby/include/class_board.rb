require 'cgi'
require 'json'
require_relative 'icons'

# The class board (docs/aufgaben.md, »Klassenübersicht«): students × tasks of
# one activity for one group, built from the stored TaskStates. Plain Ruby,
# so it can be tested without Neo4j; main.rb only fetches the data.
module ClassBoard
    STUCK_ATTEMPTS = 3
    # A task is »häufig falsch« when at least this share of those who tried
    # it are currently wrong, and at least MIN_FOR_PATTERN students tried.
    OFTEN_WRONG_SHARE = 1.0 / 3
    MIN_FOR_PATTERN = 3
    TEXT_TYPES = %w[answer number reflection sql].freeze
    # »zuletzt«: when a student last checked, answered or handed in a task
    # of this activity (the cells' updated). The class is at work when at
    # least LESSON_MIN students did so within LESSON_WINDOW; the lesson
    # began, roughly, with the first of those within LESSON_SPAN. Then
    # whoever is not done and has done nothing for QUIET_AFTER (counted
    # from the lesson's start for who did nothing in it) is »still«, also
    # who never pressed »Prüfen«. Work in the Workspace without a check is
    # not seen, so the board says »nichts geprüft«, not »nichts getan«.
    LESSON_WINDOW = 20 * 60
    LESSON_SPAN = 90 * 60
    LESSON_MIN = 2
    QUIET_AFTER = 10 * 60

    Cell = Struct.new(:status, :attempts, :answer, :updated, :first_status, :first_answer, :stuck, keyword_init: true)

    # members: [{ email:, name: }]
    # tasks:   [{ id:, title:, type:, version:, prediction: }] in page order
    # states:  { email => { task_id => stored TaskState (symbol keys) } }
    # viewer:  { email:, name: } of the teacher looking, if not a member: their
    #          own row once they worked on a task (board[:own]), shown below
    #          the class and not counted with it
    # extras:  more such rows, not members either (the teacher's test
    #          account), each once it worked on a task (board[:extras])
    def self.build(members, tasks, states, viewer: nil, extras: [])
        row = lambda do |m|
            cells = tasks.map do |t|
                s = (states[m[:email]] || {})[t[:id]]
                next Cell.new(:status => 'none') if s.nil?

                status = s[:version] == t[:version] ? s[:status].to_s : 'stale'
                attempts = s[:attempts].to_i
                Cell.new(:status => status, :attempts => attempts, :answer => s[:last_answer],
                    :updated => s[:updated], :first_status => s[:first_status], :first_answer => s[:first_answer],
                    :stuck => status == 'wrong' && attempts >= STUCK_ATTEMPTS)
            end
            # extras (tasks with extra: true) count for nobody's »gelöst«
            { :email => m[:email], :name => m[:name], :cells => cells,
              :solved => cells.each_with_index.count { |c, i| !tasks[i][:extra] && %w[correct sent].include?(c.status) } }
        end
        rows = members.sort_by { |m| m[:name].to_s.downcase }.map(&row)
        own = nil
        if viewer && members.none? { |m| m[:email] == viewer[:email] }
            own = row.call(viewer)
            own = nil if own[:cells].all? { |c| c.status == 'none' }
        end
        more = extras.reject { |m| members.any? { |x| x[:email] == m[:email] } }.map(&row)
                     .reject { |r| r[:cells].all? { |c| c.status == 'none' } }
        summary = tasks.each_with_index.map do |t, i|
            cells = rows.map { |r| r[:cells][i] }
            tried = cells.count { |c| %w[correct wrong sent].include?(c.status) }
            wrong = cells.count { |c| c.status == 'wrong' }
            {
                :done => cells.count { |c| %w[correct sent].include?(c.status) },
                :tried => tried,
                :wrong => wrong,
                :stuck => cells.count(&:stuck),
                :often_wrong => tried >= MIN_FOR_PATTERN && wrong.to_f / tried >= OFTEN_WRONG_SHARE,
                :predicted_right => t[:prediction] ? cells.count { |c| c.first_status == 'correct' } : nil,
            }
        end
        { :rows => rows, :own => own, :extras => more, :summary => summary, :tasks => tasks }
    end

    # What the class predicted first on prediction task i (the members only,
    # the current version, only states that kept their first answer,
    # TaskStore.next_state): [{ label:, message:, correct:, count: }], the
    # right answer first, then by count. judge: lambda(task, answer) ->
    # { key:, label:, message: } or nil: the same answer written two ways
    # (7 and 7,0) has one key, message is the feedback a wrong one gets.
    # Counts only, no names.
    def self.predictions(board, i, judge)
        task = board[:tasks][i]
        return [] unless task[:prediction]

        groups = {}
        board[:rows].each do |r|
            c = r[:cells][i]
            next if c.first_answer.nil? || %w[none stale].include?(c.status)

            seen = judge.call(task, c.first_answer)
            next if seen.nil?

            correct = c.first_status == 'correct'
            g = (groups[[correct, seen[:key]]] ||= { :label => seen[:label], :message => correct ? nil : seen[:message], :correct => correct, :count => 0 })
            g[:count] += 1
        end
        groups.values.sort_by { |g| [g[:correct] ? 0 : 1, -g[:count], g[:label].to_s] }
    end

    # Code in backticks as <code>, the rest escaped (the feedback sentences).
    def self.inline_code(text)
        h(text).gsub(/`([^`]+)`/, '<code>\\1</code>')
    end

    # The list below the board for each prediction: what the class predicted
    # first, how often, and what a wrong prediction is told.
    def self.predictions_html(board, judge)
        io = +''
        board[:tasks].each_index do |i|
            list = predictions(board, i, judge)
            next if list.empty?

            total = list.sum { |g| g[:count] }
            wrong = list.reject { |g| g[:correct] }.sum { |g| g[:count] }
            io << "<details class='class-answers class-predictions'><summary>#{i + 1}. #{h(board[:tasks][i][:title])}: " \
                "#{total} #{total == 1 ? 'Vermutung' : 'Vermutungen'}, #{wrong} zuerst falsch</summary>"
            io << "<p class='cb-predictions-note'>Die erste Antwort jeder Person, ohne Namen: Was viele zuerst vermuten, ist ein Thema für die Besprechung.</p><ul>"
            list.each do |g|
                mark = g[:correct] ? "<i class='bi bi-check-lg' aria-label='richtig'></i>" : "<i class='bi bi-x-lg' aria-label='falsch'></i>"
                io << "<li class='cb-prediction cb-#{g[:correct] ? 'correct' : 'wrong'}'>#{mark}<span class='cb-prediction-count'>#{g[:count]}×</span> " \
                    "<span class='cb-prediction-answer'>#{inline_code(g[:label])}</span>"
                io << "<span class='cb-prediction-message'>#{inline_code(g[:message])}</span>" if g[:message]
                io << '</li>'
            end
            io << '</ul></details>'
        end
        io
    end

    # The last time a row did anything on the activity (epoch seconds), or nil.
    def self.last_active(row)
        row[:cells].map { |c| c.updated.to_i }.reject(&:zero?).max
    end

    def self.done?(row, tasks)
        row[:solved] >= tasks.count { |t| !t[:extra] }
    end

    # The emails of the students who are quiet now (see QUIET_AFTER): none
    # while the class is not at work on this activity.
    def self.quiet(rows, tasks, now)
        lasts = rows.to_h { |r| [r[:email], last_active(r)] }
        at_work = lasts.values.count { |t| t && now - t <= LESSON_WINDOW }
        return [] if at_work < LESSON_MIN

        since = rows.flat_map { |r| r[:cells].map { |c| c.updated.to_i } }.select { |t| t > 0 && now - t <= LESSON_SPAN }.min
        rows.reject { |r| done?(r, tasks) }
            .select do |r|
                t = lasts[r[:email]]
                now - (t && t >= since ? t : since) > QUIET_AFTER
            end
            .map { |r| r[:email] }
    end

    # Who may need help now: a card still wrong after STUCK_ATTEMPTS checks
    # or more, the last one within the lesson (LESSON_SPAN). Only from the
    # checks: which tips someone opened stays in their browser
    # (docs/didaktik.md). [{ email:, tasks: [index, …] }] by name.
    def self.needs_help(rows, now)
        rows.filter_map do |r|
            stuck = r[:cells].each_index.select { |i| r[:cells][i].stuck && now - r[:cells][i].updated.to_i <= LESSON_SPAN }
            stuck.empty? ? nil : { :email => r[:email], :tasks => stuck }
        end
    end

    # Where many are stuck at once: cards on which at least a third of the
    # students at work in this lesson (and at least MIN_FOR_PATTERN) are
    # stuck. [[index, count], …]
    def self.crowded(rows, tasks, now)
        at_work = rows.count { |r| (t = last_active(r)) && now - t <= LESSON_SPAN }
        return [] if at_work < MIN_FOR_PATTERN

        tasks.each_index.filter_map do |i|
            n = rows.count { |r| r[:cells][i].stuck && now - r[:cells][i].updated.to_i <= LESSON_SPAN }
            n >= MIN_FOR_PATTERN && n >= at_work * OFTEN_WRONG_SHARE ? [i, n] : nil
        end
    end

    # Who has passed every card but the extras: [{ email:, open_extras: n }].
    def self.ready(rows, tasks)
        extras = tasks.each_index.select { |i| tasks[i][:extra] }
        rows.select { |r| done?(r, tasks) && tasks.any? { |t| !t[:extra] } }.map do |r|
            { :email => r[:email], :open_extras => extras.count { |i| !%w[correct sent].include?(r[:cells][i].status) } }
        end
    end

    # The lines about help above the board (empty: none): who may need it,
    # where many are stuck, who is done and could go on or help. Asked
    # again every minute with the quiet line (class_last_active.js).
    def self.help_html(board, now)
        rows = board[:rows]
        tasks = board[:tasks]
        name = ->(email) { h(rows.find { |r| r[:email] == email }[:name]) }
        io = +''
        help = needs_help(rows, now)
        unless help.empty?
            list = help.map { |x| "#{name.(x[:email])} (#{x[:tasks].map { |i| "Aufgabe #{i + 1}" }.join(', ')})" }
            io << "<p class='cb-help-line'><i class='bi bi-life-preserver' aria-hidden='true'></i>" \
                "Braucht vielleicht Hilfe, mehrmals geprüft und noch nicht gelöst: #{list.join(', ')}</p>"
        end
        crowded(rows, tasks, now).each do |i, n|
            io << "<p class='cb-help-line cb-crowded-line'><i class='bi bi-people' aria-hidden='true'></i>" \
                "Bei Aufgabe #{i + 1} »#{h(tasks[i][:title])}« hängen #{n} gleichzeitig: vielleicht ein Thema für alle.</p>"
        end
        done = ready(rows, tasks)
        unless done.empty?
            extras = tasks.count { |t| t[:extra] }
            list = done.map { |x| extras.positive? && x[:open_extras].positive? ? "#{name.(x[:email])} (#{x[:open_extras]} von #{extras} Extras offen)" : name.(x[:email]) }
            io << "<p class='cb-help-line cb-ready-line'><i class='bi bi-flag' aria-hidden='true'></i>" \
                "Alle Aufgaben gelöst: #{list.join(', ')}. #{extras.positive? ? 'Sie können mit den Extras weitermachen oder anderen helfen.' : 'Sie können anderen helfen.'}</p>"
        end
        io
    end

    # »vor 12 min« (class_last_active.js says it the same way while the page
    # stays open).
    def self.ago(t, now)
        return '–' if t.nil? || t.to_i.zero?

        s = [now - t.to_i, 0].max
        return 'gerade eben' if s < 60
        return "vor #{s / 60} min" if s < 3600
        return "vor #{s / 3600} h" if s < 86_400

        days = s / 86_400
        days == 1 ? 'gestern' : "vor #{days} Tagen"
    end

    # The line above the board: who is quiet, by name (empty: hidden).
    def self.quiet_html(rows, quiet)
        names = rows.select { |r| quiet.include?(r[:email]) }.map { |r| h(r[:name]) }
        "<p class='cb-quiet-line'#{names.empty? ? ' hidden' : ''}><i class='bi bi-hourglass-split' aria-hidden='true'></i>" \
            "Seit über #{QUIET_AFTER / 60} Minuten nichts geprüft oder abgegeben: <span class='cb-quiet-names'>#{names.join(', ')}</span></p>"
    end

    # »Überblick« (the class page's tab): how far each student has come in
    # each of the group's activities, not graded: solved of the tasks
    # (extras left out, as on the board), and when the student last did
    # something on any of them.
    # activities: [{ key:, title:, tasks: }] in the order to show
    # states:     { email => { 'activity/task' => stored TaskState } }
    def self.overview(members, activities, states)
        rows = members.sort_by { |m| m[:name].to_s.downcase }.map do |m|
            mine = states[m[:email]] || {}
            cells = activities.map do |a|
                counted = a[:tasks].reject { |t| t[:extra] }
                current = counted.map { |t| [t, mine["#{a[:key]}/#{t[:id]}"]] }.select { |_, s| s }
                solved = current.count { |t, s| s[:version] == t[:version] && %w[correct sent].include?(s[:status].to_s) }
                { :solved => solved, :total => counted.size, :tried => current.size }
            end
            last = mine.values.map { |s| s[:updated].to_i }.reject(&:zero?).max
            { :email => m[:email], :name => m[:name], :cells => cells, :last => last }
        end
        { :activities => activities, :rows => rows }
    end

    # The table of the overview. link: lambda(activity key) -> the board.
    def self.overview_html(overview, link:, pinned: [], now: Time.now.to_i)
        activities = overview[:activities]
        return "<p class='class-overview-empty'>In dieser Gruppe hat noch niemand eine Aufgabe bearbeitet, und es ist nichts angeheftet.</p>" if activities.empty?

        io = +"<p class='class-overview-note'>Wie weit jede Person in den Aktivitäten der Gruppe ist: gelöste Aufgaben von allen, ohne Extras; " \
            "angeheftete Aktivitäten zuerst. Eine Spalte führt zum Fortschritt der Aktivität. Noten sind das nicht.</p>"
        # »zuletzt« right after the name: with many activities the table
        # scrolls, and when someone last worked matters at every width
        io << "<div class='class-board-scroll'><table class='class-overview'><thead><tr><th class='cb-name'>Name</th><th class='cb-last'>zuletzt</th>"
        activities.each do |a|
            pin = pinned.include?(a[:key]) ? "<i class='bi bi-pin-angle-fill' aria-label='angeheftet'></i> " : ''
            io << "<th class='co-activity' title='#{h(a[:title])}'><a href='#{h(link.call(a[:key]))}'><span class='cb-task-title'>#{pin}#{h(a[:short] || a[:title])}</span></a></th>"
        end
        io << "</tr></thead><tbody>"
        overview[:rows].each do |r|
            io << "<tr data-email='#{h(r[:email])}'><th class='cb-name'>#{h(r[:name])}</th>"
            io << "<td class='cb-last' data-updated='#{r[:last].to_i}'>#{h(ago(r[:last], now))}</td>"
            r[:cells].each do |c|
                if c[:tried].zero?
                    io << "<td class='co-cell co-none' title='noch nichts bearbeitet'>–</td>"
                    next
                end
                share = c[:total].zero? ? 0 : (100.0 * c[:solved] / c[:total]).round
                done = c[:total] > 0 && c[:solved] >= c[:total]
                io << "<td class='co-cell#{done ? ' co-done' : ''}' title='#{c[:solved]} von #{c[:total]} gelöst'>" \
                    "<span class='co-count'>#{c[:solved]}/#{c[:total]}</span><span class='co-bar' aria-hidden='true'><span style='width: #{share}%'></span></span></td>"
            end
            io << '</tr>'
        end
        io << '</tbody><tfoot><tr><th class="cb-name">fertig</th><td></td>'
        activities.each_index do |i|
            done = overview[:rows].count { |r| c = r[:cells][i]; c[:total] > 0 && c[:solved] >= c[:total] }
            io << "<td class='cb-summary'>#{done}/#{overview[:rows].size}</td>"
        end
        io << '</tr></tfoot></table></div>'
    end

    # Where each student of the class stands, for the projector (/beamer,
    # docs/teacher-access.md »Am Beamer«): at the first task in page order
    # that is not an Extra and not solved, as the meeting point counts
    # (MeetingPoint.arrived?), or done (at nil). Who solved a later task
    # first still stands at the gap. started: checked, answered or handed
    # in anything of the activity. since: when they got there, the last
    # solved task before it (0 for who solved none), for the order in a
    # column. extras: how many Extras they solved, wherever they stand.
    # [{ email:, name:, at:, started:, since:, extras: }], by name like the
    # rows.
    def self.positions(board)
        tasks = board[:tasks]
        board[:rows].map do |r|
            solved = ->(i) { %w[correct sent].include?(r[:cells][i].status) }
            at = tasks.each_index.find { |i| !tasks[i][:extra] && !solved.(i) }
            before = (0...(at || tasks.size)).select { |i| !tasks[i][:extra] && solved.(i) }
            { :email => r[:email], :name => r[:name], :at => at, :started => r[:cells].any? { |c| c.status != 'none' },
              :since => before.map { |i| r[:cells][i].updated.to_i }.max || 0,
              :extras => tasks.each_index.count { |i| tasks[i][:extra] && solved.(i) } }
        end
    end

    # The same for a series (»Reihe«): boards of its sheets in order, built
    # for the same members; each student at the first sheet not finished
    # (every task but the Extras solved, as the series table counts).
    # [{ email:, name:, at: sheet index or nil, started:, since:, extras: }]
    # (extras: the Extras solved on all sheets together)
    def self.series_positions(boards)
        return [] if boards.empty?

        boards.first[:rows].map do |r|
            rows = boards.map { |b| b[:rows].find { |x| x[:email] == r[:email] } }
            at = boards.each_index.find { |i| !done?(rows[i], boards[i][:tasks]) }
            # when the last sheet before it was finished
            own = boards.map { |b| positions(b).find { |p| p[:email] == r[:email] } }
            finished = (0...(at || boards.size)).map { |i| own[i][:since] }
            { :email => r[:email], :name => r[:name], :at => at,
              :started => rows.any? { |x| x[:cells].any? { |c| c.status != 'none' } }, :since => finished.max || 0,
              :extras => own.sum { |p| p[:extras] } }
        end
    end

    # Names short enough for a projector: the first name, with the initial
    # of the last name where two share it, and the whole name where even
    # that is shared. Names without a space stay whole (an address shows
    # what comes before the @). { email => short }
    def self.short_names(members)
        first = ->(m) { m[:name].to_s.split('@').first.to_s.split(/\s+/).first.to_s }
        initial = ->(m) { (w = m[:name].to_s.split(/\s+/)).size > 1 ? "#{first.(m)} #{w.last[0]}." : first.(m) }
        firsts = members.map(&first).tally
        short = members.to_h { |m| [m[:email], firsts[first.(m)] == 1 ? first.(m) : initial.(m)] }
        shorts = short.values.tally
        members.to_h { |m| [m[:email], shorts[short[m[:email]]] == 1 ? short[m[:email]] : m[:name].to_s] }
    end

    # Icon and meaning per status: Tabler, or an <i class='bi …'> for the
    # folded paper plane that hand-ins have everywhere on the site.
    SYMBOLS = {
        'correct' => ['check', 'gelöst'],
        'sent' => ['bi-paper-plane', 'abgegeben'],
        'wrong' => ['x', 'noch nicht gelöst'],
        'stale' => ['refresh', 'Aufgabe geändert, noch nicht neu geprüft'],
        'none' => ['minus', 'noch nicht bearbeitet'],
    }.freeze

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.cell_title(cell)
        label = SYMBOLS.fetch(cell.status, SYMBOLS['none'])[1]
        parts = [label]
        parts << "#{cell.attempts} #{cell.attempts == 1 ? 'Versuch' : 'Versuche'}" if cell.attempts.to_i > 0
        parts << "Vermutung zuerst #{cell.first_status == 'correct' ? 'richtig' : 'falsch'}" if cell.first_status && cell.status != 'none'
        parts.join(', ')
    end

    # The cells of one row, its »gelöst« count and when it did something
    # last. meeting: the index of the task the meeting point follows (a
    # line after its column).
    def self.row_cells(r, tasks, activity, meeting = nil, now = Time.now.to_i)
        io = +''
        r[:cells].each_with_index do |c, i|
            icon, label = SYMBOLS.fetch(c.status, SYMBOLS['none'])
            symbol = icon.start_with?('bi-') ? "<i class='bi #{icon}' role='img' aria-label='#{h(label)}'></i>" : Icons.tabler(icon, :label => label)
            klass = "cb-cell cb-#{c.status}#{c.stuck ? ' cb-stuck' : ''}#{tasks[i][:extra] ? ' cb-extra' : ''}#{i == meeting ? ' cb-meeting' : ''}"
            count = c.attempts.to_i > 1 && c.status == 'wrong' ? "<small>#{c.attempts}</small>" : ''
            io << "<td class='#{klass}' data-task='#{h(activity)}/#{h(tasks[i][:id])}' title='#{h(cell_title(c))}'>#{symbol}#{count}</td>"
        end
        io << "<td class='cb-total'>#{r[:solved]}/#{tasks.count { |t| !t[:extra] }}</td>"
        last = last_active(r)
        io << "<td class='cb-last' data-updated='#{last.to_i}'>#{h(ago(last, now))}</td></tr>"
    end

    # HTML of the board. submission_url: lambda(email, task_id) -> URL.
    # meeting: the id of the task the group's meeting point follows.
    # judge: for the predictions' list (predictions), nil leaves it out.
    def self.html(board, activity:, submission_url: nil, meeting: nil, now: Time.now.to_i, judge: nil)
        tasks = board[:tasks]
        meeting = tasks.index { |t| t[:id] == meeting } if meeting
        still = quiet(board[:rows], tasks, now)
        io = quiet_html(board[:rows], still)
        io << "<div class='cb-help'>#{help_html(board, now)}</div>"
        io << "<div class='class-board-scroll'><table class='class-board'>"
        io << '<thead><tr><th class="cb-name">Name</th>'
        tasks.each_with_index do |t, i|
            s = board[:summary][i]
            flags = []
            flags << 'cb-often-wrong' if s[:often_wrong]
            flags << 'cb-extra' if t[:extra]
            flags << 'cb-meeting' if i == meeting
            io << "<th class='cb-task #{flags.join(' ')}' data-task='#{h(activity)}/#{h(t[:id])}' title='#{h(t[:title])}'>"
            # Title first, upright; the numbers then line up right above the cells.
            io << "<span class='cb-task-title'>#{t[:extra] ? "<i class='bi bi-stars' aria-label='Extra'></i> " : ''}#{h(t[:title])}</span><span class='cb-task-number'>#{i + 1}</span></th>"
        end
        io << '<th class="cb-total">gelöst</th><th class="cb-last" title="Wann zuletzt eine Aufgabe dieser Aktivität geprüft, beantwortet oder abgegeben wurde">zuletzt</th></tr></thead><tbody>'
        board[:rows].each do |r|
            io << "<tr data-email='#{h(r[:email])}'#{still.include?(r[:email]) ? " class='cb-quiet'" : ''}><th class='cb-name'>#{h(r[:name])}</th>"
            io << row_cells(r, tasks, activity, meeting, now)
        end
        io << '</tbody>'
        mine = [board[:own], *board[:extras]].compact
        unless mine.empty?
            io << "<tbody class='cb-own'>"
            mine.each do |own|
                io << "<tr data-email='#{h(own[:email])}'><th class='cb-name'>#{h(own[:name])}</th>"
                io << row_cells(own, tasks, activity, meeting, now)
            end
            io << '</tbody>'
        end
        io << '<tfoot><tr><th class="cb-name">Klasse</th>'
        board[:summary].each_with_index do |s, i|
            notes = ["#{s[:done]}/#{board[:rows].size}"]
            notes << "#{s[:stuck]} hängen" if s[:stuck] > 0
            notes << "#{s[:predicted_right]} richtig vermutet" if s[:predicted_right]
            io << "<td class='cb-summary#{s[:often_wrong] ? ' cb-often-wrong' : ''}#{i == meeting ? ' cb-meeting' : ''}'>#{notes.map { |n| h(n) }.join('<br>')}</td>"
        end
        io << '<td></td><td></td></tr></tfoot></table></div>'
        io << predictions_html(board, judge) if judge
        rows = board[:rows] + [board[:own], *board[:extras]].compact

        # What students wrote and handed in, per task.
        tasks.each_with_index do |t, i|
            text = TEXT_TYPES.include?(t[:type])
            submit = t[:type] == 'submit'
            document = t[:type] == 'document'
            next unless text || submit || document

            entries = rows.map { |r| [r, r[:cells][i]] }.reject { |_, c| c.status == 'none' }
            next if entries.empty?

            io << "<details class='class-answers'><summary>#{i + 1}. #{h(t[:title])}: #{entries.size} #{submit || document ? (entries.size == 1 ? 'Abgabe' : 'Abgaben') : (entries.size == 1 ? 'Antwort' : 'Antworten')}</summary><ul>"
            entries.each do |r, c|
                io << "<li class='cb-answer cb-#{c.status}'><span class='cb-answer-name'>#{h(r[:name])}</span> "
                if document
                    # the last checked version: its marked pages, and the file
                    query = "?email=#{CGI.escape(r[:email])}"
                    io << "<span class='cb-answer-text'>#{h(c.answer)}</span> " \
                        "<a class='cb-document' href='/abgabe/dokument/#{h(activity)}/#{h(t[:id])}#{h(query)}' target='_blank' rel='noopener'><i class='bi bi-eye' aria-hidden='true'></i>Ansehen</a> " \
                        "<a class='cb-document' href='/api/task/document_file/#{h(activity)}/#{h(t[:id])}#{h(query)}' download><i class='bi bi-download' aria-hidden='true'></i>Herunterladen</a>"
                elsif submit && submission_url && %w[sent stale].include?(c.status)
                    # A click opens the viewer (submission_viewer.js); without
                    # scripts the link downloads the hand-in.
                    io << "<a class='cb-submission' href='#{h(submission_url.call(r[:email], t[:id]))}' data-view-title='#{h("#{r[:name]}: #{t[:title]}")}'>#{h(c.answer)}</a>"
                else
                    io << "<span class='cb-answer-text'>#{h(c.answer)}</span>"
                end
                io << '</li>'
            end
            io << '</ul>'
            # A new round: the group's hand-ins of this task go (class_board.js
            # asks first). The teacher's own row is not counted.
            if submit
                count = entries.count { |r, c| r != board[:own] && !Array(board[:extras]).include?(r) && %w[sent stale].include?(c.status) }
                if count > 0
                    io << "<button type='button' class='btn btn-sm btn-outline-danger class-delete-submissions' " \
                        "data-task='#{h(activity)}/#{h(t[:id])}' data-title='#{h(t[:title])}' data-count='#{count}'>" \
                        "<i class='bi bi-trash' aria-hidden='true'></i>Alle Abgaben löschen</button>"
                end
            end
            io << '</details>'
        end
        io
    end
end
