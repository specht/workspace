require 'cgi'
require 'json'

# The projector (/beamer, docs/teacher-access.md »Am Beamer«): how far the
# class is in one activity, for the whole class to see. One column per
# task that is not an Extra, and one for who is done; each student is a
# dot in the column of the first task still open for them
# (ClassBoard.positions). Without names unless the teacher shows them,
# first names only (ClassBoard.short_names). projector.js draws it and asks
# again when a student of the group solved a task (/ws_meeting).
class Main < Sinatra::Base
    # { group:, activity:, title:, tasks:, students:, meeting: } for a group
    # of the teacher's and an activity of its class board, or nil.
    def projector_data(group, activity)
        return nil unless class_groups.include?(group)

        key, title, tasks = class_activities(group).find { |a, _, _| a == activity }
        return nil if key.nil?

        members = class_members(group)
        board = ClassBoard.build(members, tasks, class_board_states(members.map { |m| m[:email] }, key))
        short = ClassBoard.short_names(members)
        meeting = meeting_points([group], key)[group]
        {
            :group => group, :activity => key, :title => title,
            # the number each task has on the page and the board
            :tasks => tasks.each_with_index.reject { |t, _| t[:extra] }.map { |t, i| { :index => i, :title => t[:title] } },
            :extras => tasks.count { |t| t[:extra] },
            # the email only to leave out who is absent today (meeting_status.js)
            :students => ClassBoard.positions(board).map { |p| { :email => p[:email], :name => short[p[:email]], :at => p[:at], :started => p[:started] } },
            :meeting => meeting && tasks.index { |t| t[:id] == meeting },
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
