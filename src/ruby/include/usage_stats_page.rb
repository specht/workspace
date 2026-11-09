require_relative 'usage_stats'
require_relative 'workspace_usage'

# /statistik (UsageStats): what is used at which school and where tasks
# are hard, for the administration, without names. The attempts of the
# window are read once per request; each student becomes a number at once,
# and the address goes no further than this method. The classes (groups)
# are named on »Stolpersteine«, where at least five of one tried a task.
# Teachers, administrators and the test groups are left out, as on the
# class board's »looks finished« (feedback_students).
class Main < Sinatra::Base
    def usage_stats_page
        admins_only!
        days = params['tage'].to_i
        days = UsageStats::DEFAULT_WINDOW unless UsageStats::WINDOWS.include?(days)
        now = Time.now.to_i

        school_of_group = neo4j_query(<<~END_OF_QUERY).to_h { |r| [r['name'], r['school']] }
            MATCH (g:Group) OPTIONAL MATCH (g)-[:AT]->(s:School)
            RETURN g.name AS name, s.key AS school;
        END_OF_QUERY
        school_names = neo4j_query('MATCH (s:School) RETURN s.key AS key, s.name AS name;').to_h { |r| [r['key'], r['name'].to_s] }
        school_names[HOME_SCHOOL] = 'Eigene Schule' if school_names[HOME_SCHOOL].to_s.strip.empty?

        # each student's school: that of the first group (in the order of
        # the roster) that is no test group
        others = Set.new(ADMIN_USERS.to_a + @@teachers.keys + @@teacher_emails.to_a + @@test_accounts.keys)
        school_of = {}
        groups_of = Hash.new { |hash, key| hash[key] = [] }
        group_school = {}
        @@user_group_order.each do |group|
            next if @@test_groups.include?(group)

            group_school[group] = school_of_group.fetch(group, HOME_SCHOOL) || HOME_SCHOOL
            (@@group_members[group] || []).each do |email|
                next if others.include?(email)

                school_of[email] ||= group_school[group]
                groups_of[email] << group
            end
        end

        number = {}
        rows = neo4j_query(<<~END_OF_QUERY, :since => now - days * 86_400).filter_map do |r|
            MATCH (a:TaskAttempt)-[:BY]->(u:User) WHERE a.ts >= $since
            RETURN u.email AS email, a.task AS task, a.status AS status, a.ts AS ts;
        END_OF_QUERY
            school = school_of[r['email']] or next
            task = r['task'].to_s
            { :person => (number[r['email']] ||= number.size), :school => school, :groups => groups_of[r['email']], :activity => task.split('/').first,
              :task => task, :status => r['status'], :ts => r['ts'].to_i }
        end

        titles = {}
        tasks = {}
        @@content.each do |slug, c|
            next unless c[:tasks]

            activity = c[:tasks][:activity]
            titles[activity] ||= [plain_title(slug), slug]
            (c[:tasks][:by_id] || {}).each { |id, t| tasks["#{activity}/#{id}"] ||= t['title'].to_s }
        end
        hours = workspace_hours(now - days * 86_400 - 7 * 86_400)
        UsageStats.html(UsageStats.summary(rows, :days => days, :now => now, :group_school => group_school, :hours => hours),
                        :titles => titles, :tasks => tasks, :schools => school_names)
    end

    # Running Workspaces, every few minutes from main.rb (WorkspaceUsage)
    @@workspace_tally = WorkspaceUsage::Tally.new

    def sample_running_workspaces
        tags = workspace_runtime.running_workspaces(:timeout => shell_timeout(:docker_network_inspect)).keys
        numbers = @@workspace_tally.add(tags, Time.now.to_i)
        neo4j_query(<<~END_OF_QUERY, numbers)
            MERGE (h:WorkspaceHour {hour: $hour})
            SET h.running = CASE WHEN coalesce(h.running, 0) < $running THEN $running ELSE h.running END,
                h.peak = CASE WHEN coalesce(h.peak, 0) < $peak THEN $peak ELSE h.peak END;
        END_OF_QUERY
        neo4j_query('MATCH (h:WorkspaceHour) WHERE h.hour < $before DELETE h;', :before => Time.now.to_i - WorkspaceUsage::KEEP_DAYS * 86_400)
    end

    # The hours of the window, oldest first: [{ hour:, running:, peak: }]
    def workspace_hours(since)
        neo4j_query(<<~END_OF_QUERY, :since => since).map { |r| { :hour => r['hour'].to_i, :running => r['running'].to_i, :peak => r['peak'].to_i } }
            MATCH (h:WorkspaceHour) WHERE h.hour >= $since
            RETURN h.hour AS hour, h.running AS running, h.peak AS peak ORDER BY hour;
        END_OF_QUERY
    end
end
