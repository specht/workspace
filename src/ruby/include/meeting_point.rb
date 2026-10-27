require 'cgi'

# The meeting point of an activity (docs/plan-treffpunkt.md): a teacher sets
# it for a group after one of the activity's tasks; on the page a box says
# »here we meet«, and the class board counts who has arrived. Nothing about
# a student is stored for it: arrival is computed from the task states the
# board reads. Plain Ruby, so it can be tested without Neo4j; main.rb stores
# the point (MeetingPoint nodes) and sends it to open pages.
module MeetingPoint
    SOLVED = %w[correct sent].freeze

    # The tasks a point after task_id waits for: those up to it in the
    # page's order that are not Extras. nil when the task is not in tasks.
    # tasks: [{ id:, extra: }]
    def self.needed(tasks, task_id)
        index = tasks.index { |t| t[:id] == task_id }
        return nil if index.nil?

        tasks[0..index].reject { |t| t[:extra] }.map { |t| t[:id] }
    end

    # Whether a student has arrived: every needed task solved (correct, or
    # handed in). states: { task_id => { status: } } of that student.
    def self.arrived?(tasks, task_id, states)
        ids = needed(tasks, task_id)
        return false if ids.nil?

        ids.all? { |id| SOLVED.include?(((states || {})[id] || {})[:status].to_s) }
    end

    # Of several points (one per group of a student), the one that comes
    # first on the page: { group => task_id } -> [group, task_id], or nil.
    def self.earliest(points, tasks)
        order = tasks.map { |t| t[:id] }
        points.select { |_, id| order.include?(id) }.min_by { |group, id| [order.index(id), group] }
    end

    # Who of a group has arrived, for the class board: members [{ email:,
    # name: }], states { email => { task_id => state } } (ClassBoard's).
    # { :index, :title, :arrived => [names], :waiting => [names] }, or nil
    # when the task is not one of tasks.
    def self.status(tasks, task_id, members, states)
        index = tasks.index { |t| t[:id] == task_id }
        return nil if index.nil?

        arrived, waiting = members.sort_by { |m| m[:name].to_s.downcase }
            .partition { |m| arrived?(tasks, task_id, states[m[:email]] || {}) }
        { :index => index, :title => tasks[index][:title], :task => task_id,
          :arrived => arrived.map { |m| m[:name] }, :waiting => waiting.map { |m| m[:name] } }
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # The line above the class board: where the point is and who is there.
    def self.status_html(status)
        return '' if status.nil?

        total = status[:arrived].size + status[:waiting].size
        waiting = status[:waiting].empty? ? '' :
            "<details class='class-meeting-waiting'><summary>Noch nicht da: #{status[:waiting].size}</summary>" \
            "<p>#{status[:waiting].map { |n| h(n) }.join(', ')}</p></details>"
        "<div class='class-meeting-status' data-task='#{h(status[:task])}'>" \
            "<i class='bi bi-flag' aria-hidden='true'></i>" \
            "<span>Treffpunkt nach Aufgabe #{status[:index] + 1} (#{h(status[:title])}): " \
            "<strong class='class-meeting-count'>#{status[:arrived].size} von #{total} da</strong></span>#{waiting}</div>"
    end
end
