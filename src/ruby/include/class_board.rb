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

    Cell = Struct.new(:status, :attempts, :answer, :updated, :first_status, :stuck, keyword_init: true)

    # members: [{ email:, name: }]
    # tasks:   [{ id:, title:, type:, version:, prediction: }] in page order
    # states:  { email => { task_id => stored TaskState (symbol keys) } }
    # viewer:  { email:, name: } of the teacher looking, if not a member: their
    #          own row once they worked on a task (board[:own]), shown below
    #          the class and not counted with it
    def self.build(members, tasks, states, viewer: nil)
        row = lambda do |m|
            cells = tasks.map do |t|
                s = (states[m[:email]] || {})[t[:id]]
                next Cell.new(:status => 'none') if s.nil?

                status = s[:version] == t[:version] ? s[:status].to_s : 'stale'
                attempts = s[:attempts].to_i
                Cell.new(:status => status, :attempts => attempts, :answer => s[:last_answer],
                    :updated => s[:updated], :first_status => s[:first_status],
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
        { :rows => rows, :own => own, :summary => summary, :tasks => tasks }
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
    def self.html(board, activity:, submission_url: nil, meeting: nil, now: Time.now.to_i)
        tasks = board[:tasks]
        meeting = tasks.index { |t| t[:id] == meeting } if meeting
        still = quiet(board[:rows], tasks, now)
        io = quiet_html(board[:rows], still)
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
        if (own = board[:own])
            io << "<tbody class='cb-own'><tr data-email='#{h(own[:email])}'><th class='cb-name'>#{h(own[:name])}</th>"
            io << row_cells(own, tasks, activity, meeting, now) << '</tbody>'
        end
        io << '<tfoot><tr><th class="cb-name">Klasse</th>'
        board[:summary].each_with_index do |s, i|
            notes = ["#{s[:done]}/#{board[:rows].size}"]
            notes << "#{s[:stuck]} hängen" if s[:stuck] > 0
            notes << "#{s[:predicted_right]} richtig vermutet" if s[:predicted_right]
            io << "<td class='cb-summary#{s[:often_wrong] ? ' cb-often-wrong' : ''}#{i == meeting ? ' cb-meeting' : ''}'>#{notes.map { |n| h(n) }.join('<br>')}</td>"
        end
        io << '<td></td><td></td></tr></tfoot></table></div>'
        rows = board[:own] ? board[:rows] + [board[:own]] : board[:rows]

        # What students wrote and handed in, per task.
        tasks.each_with_index do |t, i|
            text = TEXT_TYPES.include?(t[:type])
            submit = t[:type] == 'submit'
            next unless text || submit

            entries = rows.map { |r| [r, r[:cells][i]] }.reject { |_, c| c.status == 'none' }
            next if entries.empty?

            io << "<details class='class-answers'><summary>#{i + 1}. #{h(t[:title])}: #{entries.size} #{submit ? (entries.size == 1 ? 'Abgabe' : 'Abgaben') : (entries.size == 1 ? 'Antwort' : 'Antworten')}</summary><ul>"
            entries.each do |r, c|
                io << "<li class='cb-answer cb-#{c.status}'><span class='cb-answer-name'>#{h(r[:name])}</span> "
                if submit && submission_url && %w[sent stale].include?(c.status)
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
                count = entries.count { |r, c| r != board[:own] && %w[sent stale].include?(c.status) }
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
