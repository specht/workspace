require_relative 'usage_stats'
require_relative 'workspace_usage'
require_relative 'server_load'
require_relative 'phone_calls'

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
        phone = phone_hours(now - days * 86_400 - 7 * 86_400)
        server = server_hours(now - days * 86_400 - 7 * 86_400)
        first_server = neo4j_query('MATCH (h:ServerHour) RETURN min(h.hour) AS first;').first&.dig('first')
        server_periods = ServerLoad.periods(server, days, now)
        today = UsageStats.day_of(now)
        # what each class has pinned (no test groups), the newest first
        pins = neo4j_query('MATCH (p:Pin) RETURN p.group AS group, p.slug AS slug, p.ts AS ts;')
            .reject { |r| @@test_groups.include?(r['group']) || @@content[r['slug']].nil? }
            .group_by { |r| r['group'] }
            .map do |group, list|
                pages = list.sort_by { |r| -r['ts'].to_i }.map { |r| { :title => plain_title(r['slug']), :href => "/#{r['slug']}", :ts => r['ts'].to_i } }
                { :group => group, :school => school_of_group.fetch(group, HOME_SCHOOL) || HOME_SCHOOL, :pages => pages }
            end
            .sort_by { |g| -g[:pages].first[:ts] }
        "<h1 class='mt-4'>Nutzung</h1>" \
            "<p class='abstract'>Welche Aktivitäten und Aufgaben an welcher Schule und in welcher Klasse bearbeitet werden und wo sie haken, " \
            'aus den Ergebnissen der automatischen Prüfung, ohne Namen, und was der Server dabei leistet. Zahlen, die auf weniger als fünf Lernenden beruhen, ' \
            'stehen hier nicht; Lehrkräfte, die Administration und die Testgruppen zählen nicht mit.</p>' +
            UsageStats.html(UsageStats.summary(rows, :days => days, :now => now, :group_school => group_school, :hours => hours,
                                          :phone => phone[:hours], :phone_games => phone[:games]),
                        :titles => titles, :tasks => tasks, :schools => school_names, :phone_titles => PhoneCalls.titles,
                        :server => { :periods => server_periods, :rhythm => ServerLoad.rhythm(server), :since => first_server, :today => today },
                        :pins => pins) +
            ServerLoad.html(server_periods, :since => first_server, :today => today)
    end

    # /statistik?server=2026-11-09: one day of the server in five-minute steps
    def server_day_page
        admins_only!
        today = Date.today
        day = (Date.iso8601(params['server'].to_s) rescue today)
        day = today if day > today
        start = Time.new(day.year, day.month, day.day).to_i
        samples = neo4j_query(<<~END_OF_QUERY, :from => start, :to => start + 86_400).map { |r| r['s'].transform_keys(&:to_sym) }
            MATCH (s:ServerSample) WHERE s.ts >= $from AND s.ts < $to
            RETURN properties(s) AS s ORDER BY s.ts;
        END_OF_QUERY
        ServerLoad.day_html(day, samples, :today => today)
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

    # The server's load (ServerLoad), at the same five minutes: the hour's
    # numbers and the look itself, which goes after SAMPLE_DAYS
    @@server_reader = ServerLoad::Reader.new

    def sample_server_load
        now = Time.now.to_i
        sample = @@server_reader.read
        values = sample.merge(:hour => now - now % 3600, :ts => now)
        neo4j_query(ServerLoad.store_query(sample), values)
        neo4j_query(ServerLoad.sample_query(sample), values)
        neo4j_query('MATCH (s:ServerSample) WHERE s.ts < $before DELETE s;', :before => now - ServerLoad::SAMPLE_DAYS * 86_400)
    end

    # The server's hours of the window, oldest first
    def server_hours(since)
        neo4j_query(<<~END_OF_QUERY, :since => since).map { |r| r['h'].transform_keys(&:to_sym) }
            MATCH (h:ServerHour) WHERE h.hour >= $since
            RETURN properties(h) AS h ORDER BY h.hour;
        END_OF_QUERY
    end

    # The calls of the Telefonspiel (PhoneCalls) of the window: per hour and
    # line, and per hour, line and game
    def phone_hours(since)
        hours = neo4j_query(<<~END_OF_QUERY, :since => since).map { |r| { :hour => r['hour'].to_i, :line => r['line'], :calls => r['calls'].to_i, :seconds => r['seconds'].to_i } }
            MATCH (h:PhoneHour) WHERE h.hour >= $since
            RETURN h.hour AS hour, h.line AS line, h.calls AS calls, h.seconds AS seconds ORDER BY hour;
        END_OF_QUERY
        games = neo4j_query(<<~END_OF_QUERY, :since => since).map { |r| { :hour => r['hour'].to_i, :line => r['line'], :game => r['game'].to_s, :calls => r['calls'].to_i } }
            MATCH (h:PhoneGameHour) WHERE h.hour >= $since
            RETURN h.hour AS hour, h.line AS line, h.game AS game, h.calls AS calls;
        END_OF_QUERY
        { :hours => hours, :games => games }
    end

    # The hours of the window, oldest first: [{ hour:, running:, peak: }]
    def workspace_hours(since)
        neo4j_query(<<~END_OF_QUERY, :since => since).map { |r| { :hour => r['hour'].to_i, :running => r['running'].to_i, :peak => r['peak'].to_i } }
            MATCH (h:WorkspaceHour) WHERE h.hour >= $since
            RETURN h.hour AS hour, h.running AS running, h.peak AS peak ORDER BY hour;
        END_OF_QUERY
    end
end
