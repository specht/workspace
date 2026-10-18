# /gruppen: teachers manage their own groups once the roster is in Neo4j
# (roster_store.rb): create a group, add addresses (names optional, also
# later), co-teachers of the same school, welcome mails. A teacher's groups
# belong to the teacher's school, which only an administrator assigns
# (/personen). Students of a school that has not agreed yet cannot log in
# (Roster.from_records), their teachers can.
class Main < Sinatra::Base
    GROUP_NAME_MAX = 60
    WELCOME_MAILS_PER_TEACHER = RateLimit.new(:limit => 600, :period => 3600)

    def roster_writable!
        halt 409, { :error => 'roster_in_files' }.to_json unless Main.roster_imported
    end

    def my_email
        @session_user[:email]
    end

    # The school of a teacher, from its invitation: { key:, name:, approved: }.
    def school_of_teacher(email)
        row = neo4j_query(<<~END_OF_QUERY, :email => email).first
            MATCH (:Invitation {email: $email})-[:AT]->(s:School)
            RETURN s.key AS key, s.name AS name, s.approved_at IS NOT NULL AS approved;
        END_OF_QUERY
        row && { :key => row['key'], :name => row['name'], :approved => row['approved'] == true }
    end

    # The school of every group, once the roster is in Neo4j:
    # { group name => school name }; groups without a school are left out.
    def school_names_of_groups
        return {} if Main.roster_imported.nil?

        neo4j_query('MATCH (g:Group)-[:AT]->(s:School) RETURN g.name AS name, s.name AS school;').to_h { |r| [r['name'], r['school']] }
    end

    def school_of_group(name)
        row = neo4j_query(<<~END_OF_QUERY, :name => name).first
            MATCH (:Group {name: $name})-[:AT]->(s:School)
            RETURN s.key AS key, s.name AS name, s.approved_at IS NOT NULL AS approved;
        END_OF_QUERY
        row && { :key => row['key'], :name => row['name'], :approved => row['approved'] == true }
    end

    # Administrators manage every group, teachers the ones they teach.
    def may_manage_group?(name)
        return false unless neo4j_query('MATCH (g:Group {name: $name}) RETURN g.name;', :name => name).first
        return true if admin_logged_in?

        !neo4j_query(<<~END_OF_QUERY, :email => my_email, :name => name).first.nil?
            MATCH (:Invitation {email: $email})-[:TEACHES]->(:Group {name: $name})
            RETURN 1 AS ok;
        END_OF_QUERY
    end

    def managed_group!(name)
        halt 403, { :error => 'not_your_group' }.to_json unless may_manage_group?(name.to_s)
        name.to_s
    end

    # An invitation that is in no group, teaches none and is no teacher
    # invites nobody any more: it goes (the account, if any, stays until the
    # deletion rules remove it).
    def drop_orphan_invitations
        neo4j_query(<<~END_OF_QUERY)
            MATCH (i:Invitation)
            WHERE NOT (i)-[:MEMBER_OF|TEACHES]->(:Group) AND coalesce(i.teacher, false) = false
            DETACH DELETE i;
        END_OF_QUERY
    end

    def groups_overview
        Main.load_invitations()
        names = if admin_logged_in?
            neo4j_query('MATCH (g:Group) RETURN g.name AS name ORDER BY g.name;').map { |r| r['name'] }
        else
            neo4j_query(<<~END_OF_QUERY, :email => my_email).map { |r| r['name'] }
                MATCH (:Invitation {email: $email})-[:TEACHES]->(g:Group)
                RETURN g.name AS name ORDER BY g.name;
            END_OF_QUERY
        end
        accounts = Set.new(neo4j_query('MATCH (u:User) RETURN u.email AS email;').map { |r| r['email'] })
        names.map do |name|
            members = neo4j_query(<<~END_OF_QUERY, :name => name).map do |r|
                MATCH (i:Invitation)-[:MEMBER_OF]->(:Group {name: $name})
                RETURN i.email AS email, i.name AS name, i.welcomed_at AS welcomed_at
                ORDER BY toLower(coalesce(i.name, i.email));
            END_OF_QUERY
                { :email => r['email'], :name => r['name'], :welcomed_at => r['welcomed_at'],
                  :account => accounts.include?(r['email']), :teacher => @@teacher_emails.include?(r['email']) }
            end
            teachers = neo4j_query(<<~END_OF_QUERY, :name => name).map { |r| { :email => r['email'], :name => r['name'] } }
                MATCH (i:Invitation)-[:TEACHES]->(:Group {name: $name})
                RETURN i.email AS email, i.name AS name ORDER BY i.email;
            END_OF_QUERY
            { :name => name, :school => school_of_group(name), :members => members, :teachers => teachers }
        end
    end

    post '/api/groups/list' do
        assert(teacher_account_logged_in?)
        imported = Main.roster_imported
        respond(:imported => !imported.nil?, :admin => admin_logged_in?, :school => imported ? school_of_teacher(my_email) : nil,
            :groups => imported ? groups_overview : [],
            :schools => admin_logged_in? && imported ? neo4j_query('MATCH (s:School) RETURN s.key AS key, s.name AS name ORDER BY s.name;').map { |r| { :key => r['key'], :name => r['name'] } } : [])
    end

    post '/api/groups/create' do
        assert(teacher_account_logged_in?)
        roster_writable!
        data = parse_request_data(:required_keys => [:name], :optional_keys => [:school])
        name = data[:name].to_s.strip.squeeze(' ')
        halt 400, { :error => 'invalid_name' }.to_json if name.empty? || name.size > GROUP_NAME_MAX || name.match?(/[<>]/)
        Main.load_invitations()
        reserved = ['Administrator', Roster::TEACHERS_GROUP, Roster::NO_GROUP]
        taken = reserved.include?(name) || @@group_members.key?(name) || @@test_groups.include?(name) ||
            name.start_with?('Testgruppe') || !neo4j_query('MATCH (g:Group {name: $name}) RETURN g;', :name => name).empty?
        halt 409, { :error => 'name_taken' }.to_json if taken
        school = if admin_logged_in? && data[:school]
            data[:school].to_s
        else
            (school_of_teacher(my_email) || {})[:key]
        end
        halt 400, { :error => 'no_school' }.to_json unless school
        now = Time.now.to_i
        created = neo4j_query(<<~END_OF_QUERY, :name => name, :school => school, :now => now, :me => my_email).first
            MATCH (s:School {key: $school})
            CREATE (g:Group {name: $name, created_at: $now, created_by: $me})-[:AT]->(s)
            RETURN g.name AS name;
        END_OF_QUERY
        halt 400, { :error => 'no_school' }.to_json unless created
        # The teacher who creates a group teaches it.
        unless admin_logged_in? && !@@teacher_emails.include?(my_email)
            neo4j_query(<<~END_OF_QUERY, :email => my_email, :name => name, :now => now)
                MERGE (i:Invitation {email: $email})
                ON CREATE SET i.teacher = true, i.created_at = $now
                WITH i
                MATCH (g:Group {name: $name})
                MERGE (i)-[:TEACHES]->(g);
            END_OF_QUERY
        end
        Main.note_roster_change!
        STDERR.puts "Group #{name} created by #{my_email}"
        respond(:created => name)
    end

    post '/api/groups/delete' do
        assert(teacher_account_logged_in?)
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :confirmation])
        name = managed_group!(data[:name])
        halt 400, { :error => 'confirmation_mismatch' }.to_json unless data[:confirmation].to_s.strip == name
        neo4j_query('MATCH (g:Group {name: $name}) DETACH DELETE g;', :name => name)
        drop_orphan_invitations
        Main.note_roster_change!
        STDERR.puts "Group #{name} deleted by #{my_email}"
        respond(:deleted => name)
    end

    post '/api/groups/add_members' do
        assert(teacher_account_logged_in?)
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :lines], :max_body_length => 200_000, :max_value_lengths => { :lines => 190_000 })
        name = managed_group!(data[:name])
        parsed = Roster.parse_member_lines(data[:lines])
        halt 400, { :error => 'too_many' }.to_json if parsed[:people].size > 1000
        rows = neo4j_query(<<~END_OF_QUERY, :people => parsed[:people].map { |e, n| { 'email' => e, 'name' => n } }, :name => name, :now => Time.now.to_i, :me => my_email)
            UNWIND $people AS p
            MATCH (g:Group {name: $name})
            MERGE (i:Invitation {email: p.email})
            ON CREATE SET i.teacher = false, i.created_at = $now, i.created_by = $me
            WITH i, p, g, EXISTS { (i)-[:MEMBER_OF]->(g) } AS already
            SET i.name = coalesce(i.name, p.name)
            MERGE (i)-[:MEMBER_OF]->(g)
            RETURN p.email AS email, already, p.name IS NOT NULL AND i.name <> p.name AS kept_name;
        END_OF_QUERY
        Main.note_roster_change!
        STDERR.puts "#{my_email} added #{rows.count { |r| !r['already'] }} to #{name}"
        respond(:added => rows.count { |r| !r['already'] }, :already => rows.count { |r| r['already'] },
            :kept_names => rows.select { |r| r['kept_name'] }.map { |r| r['email'] }, :rejected => parsed[:rejected])
    end

    post '/api/groups/remove_member' do
        assert(teacher_account_logged_in?)
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :email])
        name = managed_group!(data[:name])
        neo4j_query('MATCH (:Invitation {email: $email})-[r:MEMBER_OF]->(:Group {name: $name}) DELETE r;', :email => data[:email].to_s.downcase, :name => name)
        drop_orphan_invitations
        Main.note_roster_change!
        respond(:removed => data[:email])
    end

    # The name shown for a person; the teacher must teach one of its groups.
    post '/api/groups/set_name' do
        assert(teacher_account_logged_in?)
        roster_writable!
        data = parse_request_data(:required_keys => [:email, :person_name])
        email = data[:email].to_s.downcase
        allowed = admin_logged_in? || !neo4j_query(<<~END_OF_QUERY, :me => my_email, :email => email).empty?
            MATCH (:Invitation {email: $me})-[:TEACHES]->(g:Group)<-[:MEMBER_OF]-(:Invitation {email: $email})
            RETURN 1 AS ok LIMIT 1;
        END_OF_QUERY
        halt 403, { :error => 'not_your_student' }.to_json unless allowed
        name = data[:person_name].to_s.strip.squeeze(' ')[0, 100]
        neo4j_query('MATCH (i:Invitation {email: $email}) SET i.name = $name;', :email => email, :name => name.empty? ? nil : name)
        Main.note_roster_change!
        respond(:email => email, :name => name.empty? ? nil : name)
    end

    # A co-teacher: an existing teacher of the same school.
    post '/api/groups/add_teacher' do
        assert(teacher_account_logged_in?)
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :email])
        name = managed_group!(data[:name])
        email = TeacherRoles.normalize_email(data[:email])
        Main.load_invitations()
        halt 400, { :error => 'not_a_teacher' }.to_json unless email && @@teacher_emails.include?(email) && !ADMIN_USERS.include?(email)
        unless admin_logged_in?
            halt 400, { :error => 'other_school' }.to_json unless (school_of_teacher(email) || {})[:key] == (school_of_group(name) || {})[:key]
        end
        neo4j_query(<<~END_OF_QUERY, :email => email, :name => name)
            MATCH (i:Invitation {email: $email}), (g:Group {name: $name})
            MERGE (i)-[:TEACHES]->(g);
        END_OF_QUERY
        Main.note_roster_change!
        respond(:added => email)
    end

    post '/api/groups/remove_teacher' do
        assert(teacher_account_logged_in?)
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :email])
        name = managed_group!(data[:name])
        email = data[:email].to_s.downcase
        others = neo4j_query('MATCH (i:Invitation)-[:TEACHES]->(:Group {name: $name}) WHERE i.email <> $email RETURN count(i) AS n;', :name => name, :email => email).first['n'].to_i
        halt 400, { :error => 'last_teacher' }.to_json if others.zero? && !admin_logged_in?
        neo4j_query('MATCH (:Invitation {email: $email})-[r:TEACHES]->(:Group {name: $name}) DELETE r;', :email => email, :name => name)
        Main.note_roster_change!
        respond(:removed => email)
    end

    # Welcome mails to the members of a group: to the addresses given, or to
    # everyone who has none yet.
    post '/api/groups/send_welcome' do
        assert(teacher_account_logged_in?)
        roster_writable!
        data = parse_request_data(:required_keys => [:name], :optional_keys => [:emails], :types => { :emails => Array }, :max_body_length => 100_000)
        name = managed_group!(data[:name])
        rows = neo4j_query(<<~END_OF_QUERY, :name => name)
            MATCH (i:Invitation)-[:MEMBER_OF]->(:Group {name: $name})
            RETURN i.email AS email, i.welcomed_at AS welcomed_at;
        END_OF_QUERY
        wanted = data[:emails] ? Set.new(data[:emails].map { |e| e.to_s.downcase }) : nil
        targets = rows.select { |r| wanted ? wanted.include?(r['email']) : r['welcomed_at'].nil? }.map { |r| r['email'] }
        Main.load_invitations()
        teacher = (@@invitations[my_email] || {})[:name] || my_email
        sent = []
        targets.each do |email|
            break unless DEVELOPMENT || WELCOME_MAILS_PER_TEACHER.allow?(my_email)

            mail_subject, html = MailTexts.welcome_student(:web_root => WEB_ROOT, :group => name, :teacher => teacher, :email => email)
            reply = my_email
            deliver_mail do
                to email
                from SMTP_FROM
                reply_to reply
                subject mail_subject
                html
            end
            sent << email
        end
        neo4j_query('MATCH (i:Invitation) WHERE i.email IN $emails SET i.welcomed_at = $now;', :emails => sent, :now => Time.now.to_i) unless sent.empty?
        STDERR.puts "#{my_email} sent #{sent.size} welcome mails for #{name}"
        respond(:sent => sent.size, :limited => sent.size < targets.size)
    end
end
