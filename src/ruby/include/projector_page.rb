require 'cgi'
require 'json'

# The projector (/beamer, docs/teacher-access.md »Am Beamer«): how far the
# class is in one activity, for the whole class to see. One column per
# task that is not an Extra, and one for who is done; each student is a
# dot in the column of the first task still open for them
# (ClassBoard.positions). For a series, a column per sheet. Without names unless the teacher shows them,
# first names only (ClassBoard.short_names). projector.js draws it and asks
# again when a student of the group solved a task (/ws_meeting).
class Main < Sinatra::Base
    # { group:, activity:, title:, tasks:, students:, meeting: } for a group
    # of the teacher's and an activity of its class board, or nil.
    def projector_data(group, activity)
        return nil unless class_groups.include?(group)

        activities = class_activities(group)
        return projector_series_data(group, activities, activity) if activity.start_with?('series:')

        key, title, tasks = activities.find { |a, _, _| a == activity }
        return nil if key.nil?

        members = class_members(group)
        board = ClassBoard.build(members, tasks, class_board_states(members.map { |m| m[:email] }, key))
        short = ClassBoard.short_names(members)
        present = projector_present(members, [board])
        meeting = meeting_points([group], key)[group]
        {
            :group => group, :activity => key, :title => title, :unit => 'Aufgabe', :watch => [key],
            # the number each task has on the page and the board
            :tasks => tasks.each_with_index.reject { |t, _| t[:extra] }.map { |t, i| { :index => i, :title => t[:title] } },
            :extras => tasks.count { |t| t[:extra] },
            # the email only to leave out who is absent today (meeting_status.js)
            :students => projector_students(ClassBoard.positions(board), short, present),
            :absent => members.size - present.size,
            :meeting => meeting && tasks.index { |t| t[:id] == meeting },
        }
    end

    # Who of the members is in the room now: with a page of the site open
    # (its /ws_meeting, which pings every half minute), or active in the
    # last PRESENT_MINUTES (a page of the site, Main.note_activity, or a task
    # checked, the boards' cells). Not »on the site today«: a class taught
    # in two halves one after the other, or two lessons a day, would show
    # the first half in the second. The others are left out; a student
    # appears as soon as they open a page and goes some minutes after
    # leaving. Kept in memory: after a restart of the server, open pages
    # connect again and the others count from their next page.
    PRESENT_MINUTES = 20
    @@last_active = {}
    @@last_active_mutex = Mutex.new

    def self.note_activity(email)
        @@last_active_mutex.synchronize { @@last_active[email] = Time.now.to_i }
    end

    def projector_present(members, boards = [])
        emails = members.map { |m| m[:email] }
        since = Time.now.to_i - PRESENT_MINUTES * 60
        open = @@meeting_ws_mutex.synchronize { @@meeting_ws.values.map { |e| e[:email] } }
        seen = @@last_active_mutex.synchronize { emails.select { |e| @@last_active[e].to_i >= since } }
        worked = boards.flat_map { |b| b[:rows] }.select { |r| ClassBoard.last_active(r).to_i >= since }.map { |r| r[:email] }
        (emails & (open + seen + worked)).to_set
    end

    # The students for the page: only who is in the room, in each column in
    # the order they got there, the first at the bottom (projector.js keeps
    # the order). The times stay here: the page shows no ranking. extras:
    # how many Extras each solved, shown on the dot.
    def projector_students(positions, short, present)
        positions.select { |p| present.include?(p[:email]) }
            .sort_by { |p| [p[:since], p[:name].to_s.downcase] }
            .map { |p| { :email => p[:email], :name => short[p[:email]], :at => p[:at], :started => p[:started], :extras => p[:extras] } }
    end

    # A whole series (»Reihe: …« on the class board): a column per sheet,
    # each student at the first sheet not finished (ClassBoard.series_positions).
    def projector_series_data(group, activities, key)
        series = class_series(group, activities).find { |k, _, _| k == key }
        return nil if series.nil?

        _, title, sheets = series
        members = class_members(group)
        emails = members.map { |m| m[:email] }
        boards = sheets.map { |sheet_key, _, tasks, _| ClassBoard.build(members, tasks, class_board_states(emails, sheet_key)) }
        short = ClassBoard.short_names(members)
        present = projector_present(members, boards)
        {
            :group => group, :activity => key, :title => title, :unit => 'Blatt',
            :watch => sheets.map(&:first),
            :tasks => sheets.each_with_index.map { |(_, sheet_title, _, _), i| { :index => i, :title => sheet_title } },
            :extras => sheets.sum { |_, _, tasks, _| tasks.count { |t| t[:extra] } },
            :students => projector_students(ClassBoard.series_positions(boards), short, present),
            :absent => members.size - present.size,
            :meeting => nil,
        }
    end

    def projector_page
        data = projector_data(params['group'].to_s, params['activity'].to_s)
        return "<p class='mt-4'>Diese Aktivität gibt es für die Gruppe nicht. <a href='/klasse?tab=fortschritt'>Zur Klassenseite</a></p>" if data.nil?

        h = ->(t) { CGI.escapeHTML(t.to_s) }
        back = "/klasse?group=#{CGI.escape(data[:group])}&tab=fortschritt&activity=#{CGI.escape(data[:activity])}"
        # inside <script>: no </script> from a name or a title
        json = data.to_json.gsub('<') { '\u003c' }
        <<~END_OF_HTML
            <section class='projector' data-group='#{h.(data[:group])}' data-activity='#{h.(data[:activity])}' aria-labelledby='projector-title'>
                <script type='application/json' class='projector-data'>#{json}</script>
                <header class='projector-head'>
                    <div class='projector-heading'>
                        <h1 id='projector-title'>#{h.(data[:title])}</h1>
                        <p class='projector-sub'>#{h.(data[:group])}</p>
                    </div>
                    <p class='projector-total' aria-live='polite'></p>
                    <div class='projector-controls'>
                        <button type='button' class='btn btn-outline-secondary projector-names' aria-pressed='false'><i class='bi bi-person' aria-hidden='true'></i>Namen zeigen</button>
                        <button type='button' class='btn btn-outline-secondary projector-fullscreen'><i class='bi bi-arrows-fullscreen' aria-hidden='true'></i>Vollbild</button>
                        <a class='btn btn-outline-secondary projector-back' href='#{h.(back)}'><i class='bi bi-arrow-left' aria-hidden='true'></i>Klassenseite</a>
                    </div>
                </header>
                <div class='projector-board'></div>
                <p class='projector-note'></p>
            </section>
        END_OF_HTML
    end

    # projector.js asks again when a student of the group solved a task,
    # and every minute anyway.
    post '/api/class/projector' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :activity], :max_value_lengths => { :group => 200, :activity => 64 })
        halt 403 unless class_groups.include?(data[:group])
        found = projector_data(data[:group], data[:activity])
        halt 404 if found.nil?
        respond(:data => found)
    end
end
