# Feedback of teachers on an activity (ActivityFeedback): the card and the
# form in the tab »Fortschritt« of the class page, the hint on the
# teacher's start page, the two routes that store it and the
# administration's page /rueckmeldungen. Who may: feedback_group! in
# include/access.rb.
class Main < Sinatra::Base
    # The group's students, for »looks finished«: no teachers, admins or
    # test accounts, who try things out without the class.
    def feedback_students(group)
        others = Set.new(ADMIN_USERS.to_a + @@teachers.keys + @@teacher_emails.to_a + @@test_accounts.keys)
        (@@group_members[group] || []).reject { |email| others.include?(email) }
    end

    # The logged-in teacher's feedback nodes for these groups:
    # { [group, activity] => node }.
    def my_feedback(groups)
        return {} if groups.empty?

        neo4j_query(<<~END_OF_QUERY, :email => my_email, :groups => groups).to_h { |row| [[row['f'][:group], row['f'][:activity]], row['f']] }
            MATCH (f:ActivityFeedback)-[:BY]->(:User {email: $email})
            WHERE f.group IN $groups
            RETURN f;
        END_OF_QUERY
    end

    # What the form needs of an activity (by its task key): slug, title,
    # planned duration, its languages.
    def feedback_activity_info(group, activity_key)
        @feedback_info ||= {}
        return @feedback_info[[group, activity_key]] if @feedback_info.key?([group, activity_key])

        @feedback_info[[group, activity_key]] = find_feedback_activity_info(group, activity_key)
    end

    def find_feedback_activity_info(group, activity_key)
        activity = class_activities(group).find { |a, _, _| a == activity_key }
        return nil unless activity

        slug = @@content.find { |_, c| (c[:tasks] || {})[:activity] == activity_key }&.first
        content = @@content[slug] || {}
        { :group => group, :activity => activity_key, :slug => slug, :title => activity[1], :duration => content[:duration],
          :language => content[:language], :current => ActivityFeedback.current_languages(content), :email => my_email }
    end

    # The stored node of the logged-in teacher for a group and activity,
    # or nil; once per request.
    def feedback_stored(group, activity_key)
        @feedback_stored ||= {}
        @feedback_stored[group] ||= my_feedback([group])
        @feedback_stored[group][[group, activity_key]]
    end

    # In the toolbar of »Fortschritt«: always there for a teacher of the
    # group ('' for anyone else, feedback_groups).
    def activity_feedback_button(group, activity_key)
        return '' unless feedback_groups.include?(group) && feedback_activity_info(group, activity_key)

        ActivityFeedback.open_button_html(!(feedback_stored(group, activity_key) || {})[:created_at].nil?)
    end

    # Above the board: the card when the group looks finished, and the
    # form, hidden until opened. states: the board's TaskStates.
    def activity_feedback_html(group, activity_key, states)
        return '' unless feedback_groups.include?(group)

        info = feedback_activity_info(group, activity_key)
        return '' unless info

        stored = feedback_stored(group, activity_key)
        students = feedback_students(group)
        results = ActivityFeedback.results(states, students)
        now = Time.now.to_i
        state = ActivityFeedback.prompt(:feedback => stored, :last => results[:last], :students => results[:students],
                                        :size => students.size, :now => now)
        card = state == :due ? ActivityFeedback.card_html(group, info[:title], (now - results[:last]) / ActivityFeedback::DAY) : ''
        "<div class='feedback-place' id='rueckmeldung' data-state='#{state}'>#{card}#{ActivityFeedback.form_html(info, stored)}</div>" \
            "#{feedback_css}#{feedback_script}"
    end

    def feedback_script
        "<script src='/include/activity_feedback.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
    end

    def feedback_css
        "<link href='/include/activity_feedback.css?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}' rel='stylesheet'>"
    end

    # On the start page of a teacher: the newest card that waits in one of
    # the teacher's groups, as a small hint (one, not a list); '' if none.
    # The browser hides it for a week after »Nicht jetzt« (activity_feedback.js).
    def teacher_feedback_hint
        return '' unless user_logged_in? && my_running_tests.empty?

        groups = feedback_groups
        return '' if groups.empty?

        students = groups.to_h { |g| [g, feedback_students(g)] }
        emails = students.values.flatten.uniq
        return '' if emails.empty?

        # newest result per student and activity
        last = Hash.new { |hash, key| hash[key] = {} }
        neo4j_query(<<~END_OF_QUERY, :emails => emails).each { |row| last[row['email']][row['activity']] = row['last'].to_i }
            MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE u.email IN $emails
            RETURN u.email AS email, split(s.task, '/')[0] AS activity, max(s.updated) AS last;
        END_OF_QUERY
        stored = my_feedback(groups)
        now = Time.now.to_i
        due = []
        groups.each do |group|
            activities = class_activities(group).to_h { |key, title, _| [key, title] }
            per_activity = Hash.new { |hash, key| hash[key] = [] }
            students[group].each { |email| last[email].each { |activity, t| per_activity[activity] << t if t > 0 } }
            per_activity.each do |activity, times|
                next unless activities[activity]

                state = ActivityFeedback.prompt(:feedback => stored[[group, activity]], :last => times.max, :students => times.size,
                                                :size => students[group].size, :now => now)
                due << [times.max, group, activity, activities[activity]] if state == :due
            end
        end
        return '' if due.empty?

        at, group, activity, title = due.max_by(&:first)
        link = "/klasse?group=#{CGI.escape(group)}&tab=fortschritt&activity=#{CGI.escape(activity)}#rueckmeldung"
        ActivityFeedback.home_hint_html(group, activity, title, (now - at) / ActivityFeedback::DAY, link) + feedback_css + feedback_script
    end

    # Stores the answers, the first time or again (one node per teacher,
    # group and activity). At least one answer.
    post '/api/activity_feedback/save' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :activity, :answers], :types => { :answers => Hash },
            :max_body_length => 8192, :max_value_lengths => { :group => 200, :activity => 64 })
        group = feedback_group!(data[:group])
        info = feedback_activity_info(group, data[:activity].to_s)
        halt 404, { :error => 'no_such_activity' }.to_json unless info
        answers = ActivityFeedback.normalize(data[:answers], :offered => info[:current])
        halt 400, { :error => 'no_answer' }.to_json unless answers

        now = Time.now.to_i
        values = { :email => my_email, :group => group, :activity => info[:activity], :slug => info[:slug].to_s,
                   :planned => info[:duration].to_s, :answers => answers, :now => now }
        neo4j_query(<<~END_OF_QUERY, values)
            MATCH (u:User {email: $email})
            MERGE (f:ActivityFeedback {group: $group, activity: $activity})-[:BY]->(u)
            SET f += $answers, f.slug = $slug, f.duration_planned = $planned, f.updated_at = $now,
                f.created_at = coalesce(f.created_at, $now)
            REMOVE f.snoozed_until, f.never;
        END_OF_QUERY
        respond(:saved => now)
    end

    # »Nicht jetzt« (asks again in SNOOZE_DAYS) or »Nicht mehr fragen«
    # for this group and activity.
    post '/api/activity_feedback/dismiss' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :activity, :mode], :max_value_lengths => { :group => 200, :activity => 64 })
        group = feedback_group!(data[:group])
        halt 404, { :error => 'no_such_activity' }.to_json unless feedback_activity_info(group, data[:activity].to_s)
        halt 400, { :error => 'mode' }.to_json unless %w[later never].include?(data[:mode])

        until_at = Time.now.to_i + ActivityFeedback::SNOOZE_DAYS * ActivityFeedback::DAY
        neo4j_query(<<~END_OF_QUERY, :email => my_email, :group => group, :activity => data[:activity], :never => data[:mode] == 'never', :until => until_at)
            MATCH (u:User {email: $email})
            MERGE (f:ActivityFeedback {group: $group, activity: $activity})-[:BY]->(u)
            SET f.snoozed_until = $until, f.never = $never;
        END_OF_QUERY
        respond(:ok => true)
    end

    # /rueckmeldungen: every answered feedback, per activity
    # (administrators only; the page checks it in main.rb as well).
    def activity_feedback_admin_page
        admins_only!
        titles = @@content.each_with_object({}) do |(slug, c), all|
            all[c[:tasks][:activity]] ||= [slug, plain_title(slug)] if c[:tasks]
        end
        rows = neo4j_query(<<~END_OF_QUERY).map do |row|
            MATCH (f:ActivityFeedback)-[:BY]->(u:User)
            WHERE f.created_at IS NOT NULL
            RETURN f, u.email AS email;
        END_OF_QUERY
            f = row['f']
            slug, title = titles[f[:activity]] || [nil, f[:activity]]
            { :activity => f[:activity], :slug => slug, :title => title, :group => f[:group], :email => row['email'],
              :teacher => ((@@invitations[row['email']] || {})[:name] || row['email']), :feedback => f }
        end
        "#{feedback_css}<div class='feedback-admin'>#{ActivityFeedback.admin_html(ActivityFeedback.summary(rows))}</div>"
    end
end
