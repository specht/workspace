require_relative 'usage_stats'

# /statistik (UsageStats): what is used at which school and where tasks
# are hard, for the administration, without names. The attempts of the
# window are read once per request; each student becomes a number at once,
# and the address goes no further than this method. Teachers,
# administrators and the test groups are left out, as on the class board's
# »looks finished« (feedback_students).
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
        @@user_group_order.each do |group|
            next if @@test_groups.include?(group)

            (@@group_members[group] || []).each do |email|
                next if others.include?(email) || school_of.key?(email)

                school_of[email] = school_of_group.fetch(group, HOME_SCHOOL) || HOME_SCHOOL
            end
        end

        number = {}
        rows = neo4j_query(<<~END_OF_QUERY, :since => now - days * 86_400).filter_map do |r|
            MATCH (a:TaskAttempt)-[:BY]->(u:User) WHERE a.ts >= $since
            RETURN u.email AS email, a.task AS task, a.status AS status, a.ts AS ts;
        END_OF_QUERY
            school = school_of[r['email']] or next
            task = r['task'].to_s
            { :person => (number[r['email']] ||= number.size), :school => school, :activity => task.split('/').first,
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
        UsageStats.html(UsageStats.summary(rows, :days => days, :now => now), :titles => titles, :tasks => tasks, :schools => school_names)
    end
end
