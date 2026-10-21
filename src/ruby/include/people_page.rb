# /personen, for administrators: schools and whether they have agreed,
# teachers and their school (only administrators assign one), welcome mails
# to teachers. A school has agreed (approved_at) once both the
# Schulleitung's decision and the agreement on processing
# (Auftragsverarbeitung) are recorded, the school of the installation too;
# nothing else, not the import, sets it. Until then a student's own written
# consent, which the teacher records on /gruppen, covers that student.
# Neither locks anyone out (the maintainer's decision); the pages show what
# is still open. »Gäste« is a school of its own for adults from outside
# (Nutzungsordnung), which needs no school's decision.
#
# During the transition (school_transition.rb) the administration also keeps
# track of how the talks with each school stand: a status, dates, the steps,
# a note. That is kept apart from approved_at and never changes it.
require './include/school_transition.rb'

class Main < Sinatra::Base
    GUESTS_SCHOOL = 'guests'

    def school_key_for(name)
        base = name.downcase.gsub(/[^a-z0-9]+/, '-').gsub(/\A-|-\z/, '')[0, 30]
        base = 'schule' if base.empty?
        "#{base}-#{SecureRandom.hex(3)}"
    end

    # students: invited non-teachers in the school's groups; consent: of
    # those, the ones whose written consent a teacher has recorded.
    def schools_overview
        neo4j_query(<<~END_OF_QUERY).map { |r| r['s'].merge(:groups => r['groups'], :teachers => r['teachers'], :students => r['students'], :consent => r['consent']) }
            MATCH (s:School)
            OPTIONAL MATCH (g:Group)-[:AT]->(s)
            WITH s, count(DISTINCT g) AS groups
            OPTIONAL MATCH (t:Invitation)-[:AT]->(s)
            WITH s, groups, count(DISTINCT t) AS teachers
            OPTIONAL MATCH (m:Invitation)-[:MEMBER_OF]->(:Group)-[:AT]->(s)
            WHERE coalesce(m.teacher, false) = false AND NOT (m)-[:TEACHES]->(:Group) AND NOT (m)-[:AT]->(:School)
            RETURN s, groups, teachers, count(DISTINCT m) AS students,
                count(DISTINCT CASE WHEN m.consent_at IS NOT NULL THEN m END) AS consent
            ORDER BY coalesce(s.home, false) DESC, coalesce(s.guests, false), toLower(s.name);
        END_OF_QUERY
    end

    def school_approval!(key)
        neo4j_query(<<~END_OF_QUERY, :key => key, :now => Time.now.to_i, :me => my_email)
            MATCH (s:School {key: $key})
            WHERE coalesce(s.guests, false) = false
            SET s.approved_at = CASE WHEN s.decision_at IS NOT NULL AND s.agreement_at IS NOT NULL
                THEN coalesce(s.approved_at, $now) ELSE null END,
                s.approved_by = CASE WHEN s.decision_at IS NOT NULL AND s.agreement_at IS NOT NULL
                THEN coalesce(s.approved_by, $me) ELSE null END;
        END_OF_QUERY
    end

    post '/api/people/schools' do
        halt 403, { :error => 'admins_only' }.to_json unless admin_logged_in?
        imported = !Main.roster_imported.nil?
        schools = imported ? schools_overview : []
        deadline = SchoolTransition.deadline
        transition = SchoolTransition.summary(schools, Date.today, deadline).merge(
            :states => SchoolTransition::STATES, :steps => SchoolTransition::STEPS,
            :templates => SchoolTransition::TEMPLATES.transform_values { |t| t[:title] })
        schools.each do |school|
            school[:transition] = {
                :category => SchoolTransition.category(school, Date.today),
                :next_step => SchoolTransition.next_step(school),
                :template => SchoolTransition.template_for(transition[:phase][:key]),
            }
        end
        respond(:schools => schools, :imported => imported, :transition => transition)
    end

    # The progress of the talks with one school (school_transition.rb): only
    # the keys sent are changed. Never decision_at, agreement_at, approved_at
    # or anyone's consent, never access, never a mail.
    post '/api/people/school_transition' do
        admin_only!
        data = parse_request_data(:required_keys => [:key], :optional_keys => SchoolTransition::FIELDS.map(&:to_sym),
            :max_body_length => 8192, :max_string_length => 2000)
        values = begin
            SchoolTransition.clean(data.reject { |k, _| k == :key })
        rescue SchoolTransition::Invalid => e
            halt 400, { :error => e.code }.to_json
        end
        found = neo4j_query(<<~END_OF_QUERY, :key => data[:key], :values => values, :now => Time.now.to_i, :me => my_email).first
            MATCH (s:School {key: $key})
            WHERE coalesce(s.guests, false) = false
            SET s += $values, s.transition_updated_at = $now, s.transition_updated_by = $me
            RETURN s.key AS key;
        END_OF_QUERY
        halt 404, { :error => 'no_school' }.to_json if found.nil?
        respond(:ok => true)
    end

    # A message to copy into one's own mail, filled in for the school; sent from nowhere
    post '/api/people/school_template' do
        admin_only!
        data = parse_request_data(:required_keys => [:key, :template])
        halt 400, { :error => 'invalid_template' }.to_json unless SchoolTransition::TEMPLATES.key?(data[:template])
        school = neo4j_query('MATCH (s:School {key: $key}) RETURN s;', :key => data[:key]).first
        halt 404, { :error => 'no_school' }.to_json if school.nil?
        operator = Privacy.settings[:operator_name]
        respond(:title => SchoolTransition::TEMPLATES[data[:template]][:title],
            :text => SchoolTransition.fill(data[:template], school['s'], SchoolTransition.deadline, operator))
    end

    SCHOOL_FIELDS = %w[name address school_law supervisory_authority data_protection_officer].freeze

    post '/api/people/save_school' do
        admin_only!
        data = parse_request_data(:required_keys => [:name], :optional_keys => [:key] + SCHOOL_FIELDS.map(&:to_sym) - [:name],
            :max_body_length => 8192, :max_string_length => 2000)
        name = data[:name].to_s.strip
        halt 400, { :error => 'invalid_name' }.to_json if name.empty? || name.size > 120
        values = SCHOOL_FIELDS.to_h { |f| [f, data[f.to_sym].to_s.strip.empty? ? nil : data[f.to_sym].to_s.strip] }
        key = data[:key].to_s
        if key.empty?
            key = school_key_for(name)
            neo4j_query('CREATE (s:School {key: $key, created_at: $now, created_by: $me}) SET s += $values;',
                :key => key, :now => Time.now.to_i, :me => my_email, :values => values)
        else
            neo4j_query('MATCH (s:School {key: $key}) SET s += $values;', :key => key, :values => values)
        end
        Main.note_roster_change!
        respond(:key => key)
    end

    # The guests' school, made on first use.
    post '/api/people/ensure_guests' do
        admin_only!
        neo4j_query(<<~END_OF_QUERY, :key => GUESTS_SCHOOL, :now => Time.now.to_i, :me => my_email)
            MERGE (s:School {key: $key})
            ON CREATE SET s.name = 'Gäste', s.guests = true, s.approved_at = $now, s.approved_by = $me, s.created_at = $now;
        END_OF_QUERY
        Main.note_roster_change!
        respond(:key => GUESTS_SCHOOL)
    end

    # decision_at / agreement_at: a date (YYYY-MM-DD) or empty to remove it.
    post '/api/people/school_status' do
        admin_only!
        data = parse_request_data(:required_keys => [:key], :optional_keys => [:decision_at, :agreement_at])
        dates = {}
        %i[decision_at agreement_at].each do |field|
            next unless data.key?(field)

            text = data[field].to_s.strip
            dates[field.to_s] = text.empty? ? nil : (Date.iso8601(text).to_s rescue halt(400, { :error => 'invalid_date' }.to_json))
        end
        neo4j_query('MATCH (s:School {key: $key}) SET s += $dates;', :key => data[:key], :dates => dates)
        school_approval!(data[:key])
        Main.note_roster_change!
        STDERR.puts "School #{data[:key]} status set by #{my_email}: #{dates.inspect}"
        respond(:ok => true)
    end

    post '/api/people/delete_school' do
        admin_only!
        data = parse_request_data(:required_keys => [:key])
        used = neo4j_query('MATCH (s:School {key: $key}) OPTIONAL MATCH (x)-[:AT]->(s) RETURN count(x) AS n, coalesce(s.home, false) AS home;', :key => data[:key]).first
        halt 400, { :error => 'school_in_use' }.to_json if used.nil? || used['n'].to_i > 0 || used['home']
        neo4j_query('MATCH (s:School {key: $key}) DELETE s;', :key => data[:key])
        Main.note_roster_change!
        respond(:ok => true)
    end

    # A teacher always comes with a school.
    post '/api/people/save_teacher' do
        admin_only!
        data = parse_request_data(:required_keys => [:email, :school], :optional_keys => [:person_name])
        email = TeacherRoles.normalize_email(data[:email])
        halt 400, { :error => 'invalid_email' }.to_json unless email
        halt 400, { :error => 'no_school' }.to_json if neo4j_query('MATCH (s:School {key: $key}) RETURN s.key;', :key => data[:school]).empty?
        name = data[:person_name].to_s.strip
        neo4j_query(<<~END_OF_QUERY, :email => email, :name => name.empty? ? nil : name, :school => data[:school], :now => Time.now.to_i, :me => my_email)
            MERGE (i:Invitation {email: $email})
            ON CREATE SET i.created_at = $now, i.created_by = $me
            SET i.teacher = true, i.name = coalesce($name, i.name)
            WITH i
            OPTIONAL MATCH (i)-[old:AT]->(:School)
            DELETE old
            WITH DISTINCT i
            MATCH (s:School {key: $school})
            CREATE (i)-[:AT]->(s);
        END_OF_QUERY
        Main.note_roster_change!
        STDERR.puts "Teacher #{email} saved by #{my_email} at #{data[:school]}"
        respond(:email => email)
    end

    # No longer a teacher: the school and the groups it taught go; if it is
    # in no group as a member either, it is no longer invited.
    post '/api/people/remove_teacher' do
        admin_only!
        data = parse_request_data(:required_keys => [:email])
        neo4j_query(<<~END_OF_QUERY, :email => data[:email].to_s.downcase)
            MATCH (i:Invitation {email: $email})
            OPTIONAL MATCH (i)-[r:AT|TEACHES]->()
            DELETE r
            SET i.teacher = false;
        END_OF_QUERY
        drop_orphan_invitations
        Main.note_roster_change!
        respond(:ok => true)
    end

    post '/api/people/welcome_teacher' do
        admin_only!
        data = parse_request_data(:required_keys => [:email])
        email = data[:email].to_s.downcase
        school = school_of_teacher(email)
        halt 400, { :error => 'no_school' }.to_json unless school
        mail_subject, html = MailTexts.welcome_teacher(:web_root => WEB_ROOT, :school => school[:name], :school_approved => school[:approved],
            :email => email, :signature => mail_signature, :documents_url => "#{WEB_ROOT}/datenschutz?schule=#{CGI.escape(school[:key])}",
            :cost_note => Privacy.settings[:cost_note])
        reply = defined?(CONTACT_EMAIL) ? CONTACT_EMAIL.to_s : nil
        deliver_mail do
            to email
            from SMTP_FROM
            reply_to reply if reply && !reply.empty?
            subject mail_subject
            html
        end
        neo4j_query('MATCH (i:Invitation {email: $email}) SET i.welcomed_at = $now;', :email => email, :now => Time.now.to_i)
        respond(:sent => email)
    end

    # --- Invitations nobody used ---------------------------------------------
    #
    # Whole classes are often invited at once (the school list, for
    # Vertretungsstunden); many never log in. An invitation counts as unused
    # while no account exists for its address (no User node: nobody has even
    # asked for a code). Teachers never count: who teaches a group, is marked
    # as a teacher or has a school stays.
    UNUSED_INVITATION = <<~END_OF_CYPHER
        NOT EXISTS { MATCH (:User {email: i.email}) }
        AND coalesce(i.teacher, false) = false AND NOT (i)-[:TEACHES]->(:Group) AND NOT (i)-[:AT]->(:School)
    END_OF_CYPHER

    # Per group: members, unused invitations, and of the others how many have
    # an account and the range of their last visits, so that a group whose
    # people never came, or stopped coming long ago, stands out.
    def unused_invitations_by_group
        rows = neo4j_query(<<~END_OF_QUERY).map { |r| r.transform_keys(&:to_sym) }
            MATCH (g:Group)
            OPTIONAL MATCH (g)-[:AT]->(s:School)
            OPTIONAL MATCH (m:Invitation)-[:MEMBER_OF]->(g)
            WITH g, s, collect(m.email) AS emails
            OPTIONAL MATCH (i:Invitation)-[:MEMBER_OF]->(g) WHERE #{UNUSED_INVITATION}
            RETURN g.name AS name, s.name AS school, emails, count(i) AS unused
            ORDER BY unused DESC, toLower(g.name);
        END_OF_QUERY
        accounts = Set.new(neo4j_query('MATCH (u:User) RETURN u.email AS email;').map { |r| r['email'] })
        activity = activity_by_email
        rows.map do |r|
            days = r[:emails].filter_map { |e| (activity[e] || {})[:day] }.sort
            { :name => r[:name], :school => r[:school], :members => r[:emails].size, :unused => r[:unused],
              :accounts => r[:emails].count { |e| accounts.include?(e) },
              :active_from => days.first&.to_s, :active_to => days.last&.to_s }
        end
    end

    def unused_count(groups)
        neo4j_query(<<~END_OF_QUERY, :groups => groups).first['n'].to_i
            MATCH (i:Invitation)-[:MEMBER_OF]->(g:Group) WHERE g.name IN $groups AND #{UNUSED_INVITATION}
            RETURN count(DISTINCT i) AS n;
        END_OF_QUERY
    end

    # Who »Nie Angemeldete entfernen« takes out of these groups: name,
    # address, the groups among them, and how many other groups the person
    # stays in (none: the invitation goes, with name and address).
    def unused_people(groups)
        neo4j_query(<<~END_OF_QUERY, :groups => groups).map { |r| { :email => r['email'], :name => r['name'], :groups => r['here'], :elsewhere => r['elsewhere'].to_i } }
            MATCH (i:Invitation)-[:MEMBER_OF]->(g:Group) WHERE g.name IN $groups AND #{UNUSED_INVITATION}
            WITH i, collect(g.name) AS here
            OPTIONAL MATCH (i)-[:MEMBER_OF]->(o:Group) WHERE NOT o.name IN $groups
            RETURN i.email AS email, i.name AS name, here, count(o) AS elsewhere
            ORDER BY toLower(coalesce(i.name, i.email));
        END_OF_QUERY
    end

    post '/api/people/unused_invitations' do
        admin_only!
        groups = unused_invitations_by_group
        total = neo4j_query("MATCH (i:Invitation) WHERE #{UNUSED_INVITATION} AND (i)-[:MEMBER_OF]->(:Group) RETURN count(i) AS n;").first['n'].to_i
        respond(:groups => groups, :total => total)
    end

    # One invitation without an account, from every group. Teachers are
    # refused (»Keine Lehrkraft mehr« first), an account too (that is
    # the row action in »Alle Personen«).
    post '/api/people/remove_invitation' do
        admin_only!
        data = parse_request_data(:required_keys => [:email])
        email = data[:email].to_s.downcase
        row = neo4j_query(<<~END_OF_QUERY, :email => email).first
            MATCH (i:Invitation {email: $email})
            RETURN EXISTS { MATCH (:User {email: $email}) } AS account,
                coalesce(i.teacher, false) OR EXISTS { (i)-[:TEACHES]->(:Group) } OR EXISTS { (i)-[:AT]->(:School) } AS teacher;
        END_OF_QUERY
        halt 404, { :error => 'not_invited' }.to_json unless row
        halt 400, { :error => 'admin_account' }.to_json if ADMIN_USERS.include?(email)
        halt 409, { :error => 'has_account' }.to_json if row['account']
        halt 409, { :error => 'is_teacher' }.to_json if row['teacher']
        neo4j_query('MATCH (i:Invitation {email: $email}) DETACH DELETE i;', :email => email)
        Main.note_roster_change!
        STDERR.puts "Invitation removed by #{my_email}"
        respond(:removed => email)
    end

    # Groups past their end date, for the yearly cleanup: who teaches them,
    # how many members, when anyone was last here.
    post '/api/people/ended_groups' do
        admin_only!
        rows = neo4j_query(<<~END_OF_QUERY, :today => Date.today.to_s)
            MATCH (g:Group) WHERE g.ends_on IS NOT NULL AND g.ends_on < $today
            OPTIONAL MATCH (g)-[:AT]->(s:School)
            OPTIONAL MATCH (m:Invitation)-[:MEMBER_OF]->(g)
            WITH g, s, count(DISTINCT m) AS members
            OPTIONAL MATCH (t:Invitation)-[:TEACHES]->(g)
            RETURN g.name AS name, g.ends_on AS ends_on, s.name AS school, members, collect(coalesce(t.name, t.email)) AS teachers
            ORDER BY g.ends_on;
        END_OF_QUERY
        respond(:groups => rows.map { |r| r.transform_keys(&:to_sym) })
    end

    # Read only: students in groups of more than one school (guest groups
    # do not count). One account per address, so a teacher of either school
    # sees the student's Workspace; the administration resolves each case by
    # hand (take the person out of one group). Nothing is changed here.
    # The addresses of /api/people/cross_school, for the filter on /personen.
    def people_in_several_schools
        Set.new(cross_school_rows.map { |r| r['email'] })
    end

    def cross_school_rows
        neo4j_query(<<~END_OF_QUERY).to_a
            MATCH (i:Invitation)-[:MEMBER_OF]->(g:Group)-[:AT]->(s:School)
            WHERE coalesce(s.guests, false) = false AND coalesce(i.teacher, false) = false
                AND NOT (i)-[:TEACHES]->(:Group) AND NOT (i)-[:AT]->(:School)
            WITH i, collect(DISTINCT s.name) AS schools, collect(DISTINCT g.name) AS groups
            WHERE size(schools) > 1
            RETURN i.email AS email, i.name AS name, schools, groups
            ORDER BY toLower(coalesce(i.name, i.email));
        END_OF_QUERY
    end

    post '/api/people/cross_school' do
        admin_only!
        rows = cross_school_rows
        respond(:people => rows.map { |r| r.transform_keys(&:to_sym) })
    end

    # Teachers who were invited but never logged in (no account): school and
    # the groups they teach with their number of teachers, so that the page
    # can say which groups would be left without one. Administrators never.
    UNUSED_TEACHER = <<~END_OF_CYPHER
        NOT EXISTS { MATCH (:User {email: i.email}) } AND NOT i.email IN $admins
        AND (coalesce(i.teacher, false) OR (i)-[:TEACHES]->(:Group) OR (i)-[:AT]->(:School))
    END_OF_CYPHER

    post '/api/people/unused_teachers' do
        admin_only!
        rows = neo4j_query(<<~END_OF_QUERY, :admins => ADMIN_USERS)
            MATCH (i:Invitation) WHERE #{UNUSED_TEACHER}
            OPTIONAL MATCH (i)-[:AT]->(s:School)
            OPTIONAL MATCH (i)-[:TEACHES]->(g:Group)
            OPTIONAL MATCH (t:Invitation)-[:TEACHES]->(g)
            WITH i, s, g, collect(DISTINCT t.email) AS teachers
            RETURN i.email AS email, i.name AS name, s.name AS school,
                collect(CASE WHEN g IS NULL THEN NULL ELSE { name: g.name, teachers: teachers } END) AS groups
            ORDER BY toLower(coalesce(i.name, i.email));
        END_OF_QUERY
        respond(:teachers => rows.map { |r| { :email => r['email'], :name => r['name'], :school => r['school'], :groups => r['groups'] } })
    end

    # Removes the invitations of teachers without an account entirely
    # (school, teaching, memberships); confirmation: their number.
    post '/api/people/remove_unused_teachers' do
        admin_only!
        data = parse_request_data(:required_keys => [:emails, :confirmation], :types => { :emails => Array }, :max_body_length => 64_000)
        emails = data[:emails].map { |e| e.to_s.downcase }.uniq
        halt 400, { :error => 'no_groups' }.to_json if emails.empty?
        halt 409, { :error => 'count_changed' }.to_json unless data[:confirmation].to_s.strip == emails.size.to_s
        removed = neo4j_query(<<~END_OF_QUERY, :emails => emails, :admins => ADMIN_USERS).first['n'].to_i
            MATCH (i:Invitation) WHERE i.email IN $emails AND #{UNUSED_TEACHER}
            DETACH DELETE i
            RETURN count(*) AS n;
        END_OF_QUERY
        Main.note_roster_change!
        STDERR.puts "Unused teacher invitations removed by #{my_email}: #{removed}"
        respond(:removed => removed)
    end

    # How many distinct people removing the unused invitations of these
    # groups would touch: the number the administrator confirms.
    # Teachers for their own groups too (/gruppen): they may take members
    # out one by one anyway.
    post '/api/people/unused_count' do
        teacher_or_admin!
        data = parse_request_data(:required_keys => [:groups], :types => { :groups => Array }, :max_body_length => 64_000)
        groups = data[:groups].map(&:to_s).uniq
        groups.each { |name| managed_group!(name) }
        respond(:count => unused_count(groups), :people => unused_people(groups))
    end

    # groups: the group names to clean up; confirmation: the number of people
    # the page showed, so that nothing else goes than what was confirmed.
    post '/api/people/remove_unused_invitations' do
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:groups, :confirmation], :types => { :groups => Array }, :max_body_length => 64_000)
        groups = data[:groups].map(&:to_s).uniq
        halt 400, { :error => 'no_groups' }.to_json if groups.empty?
        groups.each { |name| managed_group!(name) }
        count = unused_count(groups)
        halt 409, { :error => 'count_changed' }.to_json unless data[:confirmation].to_s.strip == count.to_s
        removed = neo4j_query(<<~END_OF_QUERY, :groups => groups).first['n'].to_i
            MATCH (i:Invitation)-[r:MEMBER_OF]->(g:Group) WHERE g.name IN $groups AND #{UNUSED_INVITATION}
            DELETE r
            RETURN count(r) AS n;
        END_OF_QUERY
        before = neo4j_query('MATCH (i:Invitation) RETURN count(i) AS n;').first['n'].to_i
        drop_orphan_invitations
        gone = before - neo4j_query('MATCH (i:Invitation) RETURN count(i) AS n;').first['n'].to_i
        Main.note_roster_change!
        STDERR.puts "Unused invitations removed by #{my_email}: #{removed} memberships in #{groups.size} groups, #{gone} people no longer invited"
        respond(:memberships => removed, :uninvited => gone)
    end

    # Teachers with their school, for the list on /personen.
    post '/api/people/teachers' do
        halt 403, { :error => 'admins_only' }.to_json unless admin_logged_in?
        rows = Main.roster_imported.nil? ? [] : neo4j_query(<<~END_OF_QUERY)
            MATCH (i:Invitation)
            WHERE i.teacher = true OR (i)-[:TEACHES]->(:Group)
            OPTIONAL MATCH (i)-[:AT]->(s:School)
            OPTIONAL MATCH (i)-[:TEACHES]->(g:Group)
            RETURN i.email AS email, i.name AS name, i.welcomed_at AS welcomed_at, s.key AS school, s.name AS school_name,
                collect(g.name) AS groups
            ORDER BY toLower(coalesce(i.name, i.email));
        END_OF_QUERY
        accounts = Set.new(neo4j_query('MATCH (u:User) RETURN u.email AS email;').map { |r| r['email'] })
        respond(:teachers => rows.map { |r| r.transform_keys(&:to_sym).merge(:account => accounts.include?(r['email'])) })
    end
end
