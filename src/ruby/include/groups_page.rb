# /gruppen: teachers manage their own groups once the roster is in Neo4j
# (roster_store.rb): create a group, add addresses (names optional, also
# later), co-teachers of the same school, welcome mails. A teacher's groups
# belong to the teacher's school, which only an administrator assigns
# (/personen). Students of a school that has not agreed yet cannot log in
# (Roster.from_records), their teachers can.
class Main < Sinatra::Base
    GROUP_NAME_MAX = 60
    WELCOME_MAILS_PER_TEACHER = RateLimit.new(:limit => 600, :period => 3600)

    # Teachers manage their own groups, administrators every group; anyone
    # else gets a refusal, not an error.
    def teacher_or_admin!
        halt 403, { :error => 'teachers_only' }.to_json unless teacher_account_logged_in? || admin_logged_in?
    end

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
            RETURN s.key AS key, s.name AS name, s.approved_at IS NOT NULL AS approved, coalesce(s.guests, false) AS guests, coalesce(s.home, false) AS home;
        END_OF_QUERY
        row && { :key => row['key'], :name => row['name'], :approved => row['approved'] == true, :guests => row['guests'] == true, :home => row['home'] == true }
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

    # The groups the logged-in person manages, with members, teachers and
    # school, in three queries for all groups. For administrators also when
    # each member was last here and how much disk space the Workspace takes
    # (Main#activity_by_email, Main#workspace_disk_usage); the
    # Datenschutzerklärung says only the administration sees these.
    def groups_overview
        Main.load_invitations()
        admin = admin_logged_in?
        names = if admin
            neo4j_query('MATCH (g:Group) RETURN g.name AS name ORDER BY g.name;').map { |r| r['name'] }
        else
            neo4j_query(<<~END_OF_QUERY, :email => my_email).map { |r| r['name'] }
                MATCH (:Invitation {email: $email})-[:TEACHES]->(g:Group)
                RETURN g.name AS name ORDER BY g.name;
            END_OF_QUERY
        end
        accounts = Set.new(neo4j_query('MATCH (u:User) RETURN u.email AS email;').map { |r| r['email'] })
        members = Hash.new { |h, k| h[k] = [] }
        neo4j_query(<<~END_OF_QUERY, :names => names).each { |r| members[r['group']] << r }
            MATCH (i:Invitation)-[:MEMBER_OF]->(g:Group) WHERE g.name IN $names
            RETURN g.name AS group, i.email AS email, i.name AS name, i.welcomed_at AS welcomed_at, i.consent_at AS consent_at
            ORDER BY toLower(coalesce(i.name, i.email));
        END_OF_QUERY
        teachers = Hash.new { |h, k| h[k] = [] }
        neo4j_query(<<~END_OF_QUERY, :names => names).each { |r| teachers[r['group']] << { :email => r['email'], :name => r['name'] } }
            MATCH (i:Invitation)-[:TEACHES]->(g:Group) WHERE g.name IN $names
            RETURN g.name AS group, i.email AS email, i.name AS name ORDER BY i.email;
        END_OF_QUERY
        ends = neo4j_query('MATCH (g:Group) WHERE g.name IN $names AND g.ends_on IS NOT NULL RETURN g.name AS name, g.ends_on AS ends_on;', :names => names)
            .to_h { |r| [r['name'], r['ends_on']] }
        schools = neo4j_query(<<~END_OF_QUERY, :names => names).to_h { |r| [r['group'], r.transform_keys(&:to_sym).except(:group)] }
            MATCH (g:Group)-[:AT]->(s:School) WHERE g.name IN $names
            RETURN g.name AS group, s.key AS key, s.name AS name, s.approved_at IS NOT NULL AS approved,
                coalesce(s.guests, false) AS guests, coalesce(s.home, false) AS home;
        END_OF_QUERY
        activity = admin ? activity_by_email : {}
        disk = admin ? workspace_disk_usage[:by_tag] : {}
        names.map do |name|
            list = members[name].map do |r|
                m = { :email => r['email'], :name => r['name'], :welcomed_at => r['welcomed_at'], :consent_at => r['consent_at'],
                      :account => accounts.include?(r['email']), :teacher => @@teacher_emails.include?(r['email']) }
                if admin
                    active = activity[r['email']]
                    m[:active_on] = active && active[:day] ? active[:day].to_s : nil
                    m[:active_source] = active && active[:day] ? active[:source] : nil
                    m[:disk_kb] = m[:account] ? disk[fs_tag_for_email(r['email'])] : nil
                end
                m
            end
            group = { :name => name, :school => schools[name], :members => list, :teachers => teachers[name], :ends_on => ends[name] }
            if admin
                students = list.reject { |m| m[:teacher] }
                group[:last_active] = students.map { |m| m[:active_on] }.compact.max
                group[:disk_kb] = students.sum { |m| m[:disk_kb].to_i }
            end
            group
        end
    end

    post '/api/groups/list' do
        teacher_or_admin!
        imported = Main.roster_imported
        respond(:imported => !imported.nil?, :admin => admin_logged_in?, :school => imported ? school_of_teacher(my_email) : nil,
            :groups => imported ? groups_overview : [], :fat_kb => admin_logged_in? ? workspace_disk_usage[:fat_kb] : nil,
            :schools => admin_logged_in? && imported ? neo4j_query('MATCH (s:School) RETURN s.key AS key, s.name AS name, s.approved_at IS NOT NULL AS approved ORDER BY coalesce(s.home, false) DESC, coalesce(s.guests, false), toLower(s.name);').map { |r| { :key => r['key'], :name => r['name'], :approved => r['approved'] == true } } : [])
    end

    # Suggestions while typing, by part of a name or address, at most ten.
    # kind 'teacher': teachers of the group's school (who may co-teach it);
    # kind 'member': people already invited, for a teacher only those in
    # groups of the group's school, never the whole installation; for an
    # administrator everyone. Who is in the group already is left out.
    post '/api/groups/suggest' do
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :q, :kind], :max_value_lengths => { :q => 100 })
        name = managed_group!(data[:name])
        q = data[:q].to_s.strip.downcase
        halt 400, { :error => 'invalid_kind' }.to_json unless %w[teacher member].include?(data[:kind])
        school = (school_of_group(name) || {})[:key]
        rows = if q.size < 2
            []
        elsif data[:kind] == 'teacher'
            neo4j_query(<<~END_OF_QUERY, :q => q, :name => name, :school => school)
                MATCH (i:Invitation)-[:AT]->(:School {key: $school})
                WHERE NOT (i)-[:TEACHES]->(:Group {name: $name})
                    AND (toLower(i.email) CONTAINS $q OR toLower(coalesce(i.name, '')) CONTAINS $q)
                RETURN i.email AS email, i.name AS name ORDER BY toLower(coalesce(i.name, i.email)) LIMIT 10;
            END_OF_QUERY
        elsif admin_logged_in?
            neo4j_query(<<~END_OF_QUERY, :q => q, :name => name)
                MATCH (i:Invitation)
                WHERE NOT (i)-[:MEMBER_OF]->(:Group {name: $name})
                    AND (toLower(i.email) CONTAINS $q OR toLower(coalesce(i.name, '')) CONTAINS $q)
                RETURN i.email AS email, i.name AS name ORDER BY toLower(coalesce(i.name, i.email)) LIMIT 10;
            END_OF_QUERY
        else
            neo4j_query(<<~END_OF_QUERY, :q => q, :name => name, :school => school)
                MATCH (i:Invitation)-[:MEMBER_OF]->(:Group)-[:AT]->(:School {key: $school})
                WHERE NOT (i)-[:MEMBER_OF]->(:Group {name: $name})
                    AND (toLower(i.email) CONTAINS $q OR toLower(coalesce(i.name, '')) CONTAINS $q)
                RETURN DISTINCT i.email AS email, i.name AS name ORDER BY toLower(coalesce(i.name, i.email)) LIMIT 10;
            END_OF_QUERY
        end
        respond(:people => rows.map { |r| { :email => r['email'], :name => r['name'] } })
    end

    # When a group ends (a course, a school year, a workshop): afterwards its
    # teachers get one reminder to delete it or set a new end
    # (Main#remind_ended_groups), and »Aufräumen« lists it. Empty removes it.
    post '/api/groups/set_end' do
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :date])
        name = managed_group!(data[:name])
        text = data[:date].to_s.strip
        date = text.empty? ? nil : (Date.iso8601(text).to_s rescue halt(400, { :error => 'invalid_date' }.to_json))
        neo4j_query('MATCH (g:Group {name: $name}) SET g.ends_on = $date REMOVE g.end_reminded_at;', :name => name, :date => date)
        Main.note_roster_change!
        respond(:ends_on => date)
    end

    # A student's written consent (of the parents, or their own when of
    # age), recorded by a teacher of the group, who keeps the form and is
    # responsible for it: date (YYYY-MM-DD) or empty to remove it. It matters
    # while the school has not agreed; it never decides who can log in.
    post '/api/groups/set_consent' do
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :email, :date])
        name = managed_group!(data[:name])
        email = data[:email].to_s.downcase
        text = data[:date].to_s.strip
        date = text.empty? ? nil : (Date.iso8601(text).to_s rescue halt(400, { :error => 'invalid_date' }.to_json))
        halt 400, { :error => 'invalid_date' }.to_json if date && Date.iso8601(date) > Date.today
        done = neo4j_query(<<~END_OF_QUERY, :email => email, :name => name, :date => date, :me => date ? my_email : nil).first
            MATCH (i:Invitation {email: $email})-[:MEMBER_OF]->(:Group {name: $name})
            SET i.consent_at = $date, i.consent_by = $me
            RETURN i.email AS email;
        END_OF_QUERY
        halt 403, { :error => 'not_your_student' }.to_json unless done
        Main.note_roster_change!
        STDERR.puts "Consent for a member of #{name} #{date ? 'recorded' : 'removed'} by #{my_email}"
        respond(:email => email, :consent_at => date)
    end

    # --- Moving a group to another school (administrators only) ----------
    #
    # Only the group's AT link changes: members, their names, welcome dates,
    # accounts, Workspaces and everything else stay as they are. Whether the
    # students can log in then follows the target school's approval.
    #
    # What has to be decided first:
    # - teachers of the group whose own school is a different one: moved
    #   along (only if they teach no group outside the target school) or
    #   taken off the group; never silently left behind;
    # - students who are also in a group of another school: the move is
    #   refused, as a teacher of the target school would then see a student
    #   of another school (one account per address).
    def school_change_plan(name, target)
        from = school_of_group(name)
        to = neo4j_query('MATCH (s:School {key: $key}) RETURN s.key AS key, s.name AS name, s.approved_at IS NOT NULL AS approved;', :key => target).first
        return { :error => 'no_school' } unless to

        to = { :key => to['key'], :name => to['name'], :approved => to['approved'] == true }
        return { :error => 'same_school' } if from && from[:key] == to[:key]

        teachers = neo4j_query(<<~END_OF_QUERY, :name => name, :target => to[:key]).map { |r| r.transform_keys(&:to_sym) }
            MATCH (t:Invitation)-[:TEACHES]->(:Group {name: $name})
            OPTIONAL MATCH (t)-[:AT]->(ts:School)
            OPTIONAL MATCH (t)-[:TEACHES]->(o:Group) WHERE o.name <> $name AND NOT (o)-[:AT]->(:School {key: $target})
            OPTIONAL MATCH (t)-[:MEMBER_OF]->(mg:Group)
            RETURN t.email AS email, t.name AS name, ts.key AS school, ts.name AS school_name,
                collect(DISTINCT o.name) AS teaches_elsewhere, collect(DISTINCT mg.name) AS member_of
            ORDER BY t.email;
        END_OF_QUERY
        teachers.each do |t|
            t[:matches] = t[:school] == to[:key]
            t[:admin] = ADMIN_USERS.include?(t[:email])
        end
        # Guest groups (workshops, events, companies) are no other school: the
        # same people may be in them.
        to_guests = neo4j_query('MATCH (s:School {key: $key}) RETURN coalesce(s.guests, false) AS g;', :key => to[:key]).first['g'] == true
        elsewhere = neo4j_query(<<~END_OF_QUERY, :name => name, :target => to[:key], :to_guests => to_guests).map { |r| { :email => r['email'], :name => r['name'], :groups => r['groups'] } }
            MATCH (m:Invitation)-[:MEMBER_OF]->(:Group {name: $name})
            WHERE coalesce(m.teacher, false) = false AND NOT (m)-[:TEACHES]->(:Group {name: $name})
            MATCH (m)-[:MEMBER_OF]->(o:Group) WHERE o.name <> $name
            OPTIONAL MATCH (o)-[:AT]->(os:School)
            WITH m, o, os WHERE (os IS NULL OR os.key <> $target) AND coalesce(os.guests, false) = false AND NOT $to_guests
            RETURN m.email AS email, m.name AS name, collect(DISTINCT o.name) AS groups
            ORDER BY m.email;
        END_OF_QUERY
        members = neo4j_query('MATCH (m:Invitation)-[:MEMBER_OF]->(:Group {name: $name}) RETURN count(m) AS n;', :name => name).first['n'].to_i
        { :group => name, :from => from, :to => to, :members => members, :teachers => teachers, :students_elsewhere => elsewhere }
    end

    post '/api/groups/school_change_preview' do
        admin_only!
        data = parse_request_data(:required_keys => [:name, :school])
        name = managed_group!(data[:name])
        plan = school_change_plan(name, data[:school].to_s)
        halt 400, { :error => plan[:error] }.to_json if plan[:error]
        respond(plan)
    end

    # teachers: 'move' (teachers of another school move to the target
    # school with the group) or 'remove' (they no longer teach it).
    # Administrators keep their rights either way and are not moved.
    post '/api/groups/change_school' do
        admin_only!
        data = parse_request_data(:required_keys => [:name, :school], :optional_keys => [:teachers])
        name = managed_group!(data[:name])
        plan = school_change_plan(name, data[:school].to_s)
        halt 400, { :error => plan[:error] }.to_json if plan[:error]
        halt 409, { :error => 'students_elsewhere', :students => plan[:students_elsewhere] }.to_json unless plan[:students_elsewhere].empty?

        mismatched = plan[:teachers].reject { |t| t[:matches] || t[:admin] }
        mode = data[:teachers].to_s
        halt 400, { :error => 'teachers_undecided' }.to_json if !mismatched.empty? && !%w[move remove].include?(mode)
        if mode == 'move'
            blocked = mismatched.reject { |t| t[:teaches_elsewhere].empty? }
            halt 409, { :error => 'teacher_elsewhere', :teachers => blocked }.to_json unless blocked.empty?
        end
        emails = mismatched.map { |t| t[:email] }
        now = Time.now.to_i
        $neo4j.transaction do
            $neo4j.neo4j_query(<<~END_OF_QUERY, :name => name, :target => plan[:to][:key], :now => now, :me => my_email)
                MATCH (g:Group {name: $name})
                OPTIONAL MATCH (g)-[old:AT]->(from:School)
                WITH g, old, from
                MATCH (to:School {key: $target})
                SET g.moved_from = from.key, g.moved_at = $now, g.moved_by = $me
                DELETE old
                CREATE (g)-[:AT]->(to);
            END_OF_QUERY
            unless emails.empty?
                if mode == 'move'
                    $neo4j.neo4j_query(<<~END_OF_QUERY, :emails => emails, :target => plan[:to][:key])
                        MATCH (t:Invitation) WHERE t.email IN $emails
                        OPTIONAL MATCH (t)-[old:AT]->(:School)
                        DELETE old
                        WITH DISTINCT t
                        MATCH (to:School {key: $target})
                        CREATE (t)-[:AT]->(to);
                    END_OF_QUERY
                else
                    $neo4j.neo4j_query('MATCH (t:Invitation)-[r:TEACHES]->(:Group {name: $name}) WHERE t.email IN $emails DELETE r;', :emails => emails, :name => name)
                end
            end
        end
        drop_orphan_invitations
        Main.note_roster_change!
        STDERR.puts "Group #{name} moved by #{my_email} from #{(plan[:from] || {})[:key] || '-'} to #{plan[:to][:key]}; " \
            "teachers of another school: #{emails.empty? ? 'none' : "#{emails.size} #{mode == 'move' ? 'moved' : 'removed'}"}"
        respond(:moved => name, :to => plan[:to])
    end

    # A new group name, cleaned up, or a refusal: not empty, not too long,
    # no < or >, not reserved and not taken (group names are unique).
    def free_group_name!(text)
        name = text.to_s.strip.squeeze(' ')
        halt 400, { :error => 'invalid_name' }.to_json if name.empty? || name.size > GROUP_NAME_MAX || name.match?(/[<>]/)
        Main.load_invitations()
        reserved = ['Administrator', Roster::TEACHERS_GROUP, Roster::NO_GROUP]
        taken = reserved.include?(name) || @@group_members.key?(name) || @@test_groups.include?(name) ||
            name.start_with?('Testgruppe') || !neo4j_query('MATCH (g:Group {name: $name}) RETURN g;', :name => name).empty?
        halt 409, { :error => 'name_taken' }.to_json if taken
        name
    end

    # Renaming a group, by its teachers or an administrator. The name is also
    # the key of the pins on the start page (Pin, key »group|slug«), of the
    # group's programming language (GroupSetting) and of the teachers' last
    # choice on the class page (User.class_prefs): all of them follow, in one
    # transaction.
    post '/api/groups/rename' do
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :new_name])
        old_name = managed_group!(data[:name])
        new_name = free_group_name!(data[:new_name])
        prefs = neo4j_query('MATCH (u:User) WHERE u.class_prefs CONTAINS $quoted RETURN u.email AS email, u.class_prefs AS prefs;',
            :quoted => old_name.to_json).filter_map do |r|
            parsed = (JSON.parse(r['prefs'].to_s) rescue nil)
            next unless parsed.is_a?(Hash)

            parsed['group'] = new_name if parsed['group'] == old_name
            parsed['activities'][new_name] = parsed['activities'].delete(old_name) if parsed['activities'].is_a?(Hash) && parsed['activities'].key?(old_name)
            { 'email' => r['email'], 'prefs' => parsed.to_json }
        end
        $neo4j.transaction do
            $neo4j.neo4j_query('MATCH (g:Group {name: $old}) SET g.name = $new, g.renamed_from = $old, g.renamed_at = $now, g.renamed_by = $me;',
                :old => old_name, :new => new_name, :now => Time.now.to_i, :me => my_email)
            $neo4j.neo4j_query('MATCH (p:Pin {group: $old}) SET p.group = $new, p.key = $new + "|" + p.slug;', :old => old_name, :new => new_name)
            $neo4j.neo4j_query('MATCH (s:GroupSetting {group: $old}) SET s.group = $new;', :old => old_name, :new => new_name)
            $neo4j.neo4j_query('UNWIND $rows AS r MATCH (u:User {email: r.email}) SET u.class_prefs = r.prefs;', :rows => prefs)
        end
        Main.note_roster_change!
        STDERR.puts "Group #{old_name} renamed to #{new_name} by #{my_email}"
        respond(:renamed => new_name)
    end

    post '/api/groups/create' do
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:name], :optional_keys => [:school])
        name = free_group_name!(data[:name])
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
        teacher_or_admin!
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
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :lines], :max_body_length => 200_000, :max_value_lengths => { :lines => 190_000 })
        name = managed_group!(data[:name])
        parsed = Roster.parse_member_lines(data[:lines])
        halt 400, { :error => 'too_many' }.to_json if parsed[:people].size > 1000
        # A student already in a group of another school is not added: one
        # account per address, so this group's teachers would see that
        # student's Workspace (as with »Schule wechseln«). Teachers are not
        # students here; workshop groups have teachers of other schools.
        elsewhere = neo4j_query(<<~END_OF_QUERY, :emails => parsed[:people].map(&:first), :name => name).map { |r| r['email'] }
            MATCH (g:Group {name: $name})
            OPTIONAL MATCH (g)-[:AT]->(gs:School)
            MATCH (i:Invitation)-[:MEMBER_OF]->(o:Group) WHERE i.email IN $emails AND o.name <> $name
                AND coalesce(i.teacher, false) = false AND NOT (i)-[:TEACHES]->(:Group) AND NOT (i)-[:AT]->(:School)
            OPTIONAL MATCH (o)-[:AT]->(os:School)
            WITH i, gs, os WHERE coalesce(os.key, '') <> coalesce(gs.key, '')
                AND coalesce(os.guests, false) = false AND coalesce(gs.guests, false) = false
            RETURN DISTINCT i.email AS email;
        END_OF_QUERY
        people = parsed[:people].reject { |email, _| elsewhere.include?(email) }
        rows = neo4j_query(<<~END_OF_QUERY, :people => people.map { |e, n| { 'email' => e, 'name' => n } }, :name => name, :now => Time.now.to_i, :me => my_email)
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
            :kept_names => rows.select { |r| r['kept_name'] }.map { |r| r['email'] }, :rejected => parsed[:rejected], :other_school => elsewhere.sort)
    end

    post '/api/groups/remove_member' do
        teacher_or_admin!
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
        teacher_or_admin!
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
        teacher_or_admin!
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
        STDERR.puts "Teacher #{email} added to #{name} by #{my_email}"
        respond(:added => email)
    end

    post '/api/groups/remove_teacher' do
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:name, :email])
        name = managed_group!(data[:name])
        email = data[:email].to_s.downcase
        others = neo4j_query('MATCH (i:Invitation)-[:TEACHES]->(:Group {name: $name}) WHERE i.email <> $email RETURN count(i) AS n;', :name => name, :email => email).first['n'].to_i
        halt 400, { :error => 'last_teacher' }.to_json if others.zero? && !admin_logged_in?
        neo4j_query('MATCH (:Invitation {email: $email})-[r:TEACHES]->(:Group {name: $name}) DELETE r;', :email => email, :name => name)
        Main.note_roster_change!
        STDERR.puts "Teacher #{email} taken off #{name} by #{my_email}"
        respond(:removed => email)
    end

    # Welcome mails to the members of a group: to the addresses given, or to
    # everyone who has none yet.
    # The Datenschutzerklärung that applies to a group, as the PDF that goes
    # with its welcome mails: a partner school's own version, otherwise the
    # operator's (also online on /impressum).
    def welcome_attachment(name)
        school = school_of_group(name)
        partner = school && !school[:home] && !school[:guests]
        s = partner ? privacy_settings_for(school[:key]) : Privacy.settings
        { :filename => PrivacyDocuments.pdf_name('datenschutzerklaerung', s), :content => document_pdf('datenschutzerklaerung', s),
          :url => partner ? "/datenschutz/datenschutzerklaerung.pdf?schule=#{CGI.escape(school[:key])}" : '/datenschutz/datenschutzerklaerung.pdf',
          :public_url => partner ? nil : "#{WEB_ROOT}/impressum#datenschutz" }
    end

    def welcome_mail_for(name, teacher, email, attachment)
        MailTexts.welcome_student(:web_root => WEB_ROOT, :group => name, :teacher => teacher, :email => email,
            :privacy_url => attachment[:public_url], :attached => true)
    end

    # What the welcome mail will look like, before it goes: subject, text for
    # the first recipient, how many get it, and the attached PDF to look at.
    post '/api/groups/welcome_preview' do
        teacher_or_admin!
        roster_writable!
        data = parse_request_data(:required_keys => [:name], :optional_keys => [:emails], :types => { :emails => Array }, :max_body_length => 100_000)
        name = managed_group!(data[:name])
        rows = neo4j_query('MATCH (i:Invitation)-[:MEMBER_OF]->(:Group {name: $name}) RETURN i.email AS email, i.welcomed_at AS welcomed_at;', :name => name)
        wanted = data[:emails] ? Set.new(data[:emails].map { |e| e.to_s.downcase }) : nil
        targets = rows.select { |r| wanted ? wanted.include?(r['email']) : r['welcomed_at'].nil? }.map { |r| r['email'] }.sort
        Main.load_invitations()
        teacher = (@@invitations[my_email] || {})[:name] || my_email
        school = school_of_group(name)
        partner = school && !school[:home] && !school[:guests]
        s = partner ? privacy_settings_for(school[:key]) : Privacy.settings
        attachment = { :filename => PrivacyDocuments.pdf_name('datenschutzerklaerung', s), :public_url => partner ? nil : "#{WEB_ROOT}/impressum#datenschutz",
                       :url => partner ? "/datenschutz/datenschutzerklaerung.pdf?schule=#{CGI.escape(school[:key])}" : '/datenschutz/datenschutzerklaerung.pdf' }
        mail_subject, html = welcome_mail_for(name, teacher, targets.first || 'name@example.org', attachment)
        respond(:subject => mail_subject, :html => html, :recipients => targets.size, :first => targets.first,
            :reply_to => my_email, :attachment => { :filename => attachment[:filename], :url => attachment[:url] })
    end

    post '/api/groups/send_welcome' do
        teacher_or_admin!
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
        attachment = welcome_attachment(name)
        sent = []
        targets.each do |email|
            break unless DEVELOPMENT || WELCOME_MAILS_PER_TEACHER.allow?(my_email)

            mail_subject, html = welcome_mail_for(name, teacher, email, attachment)
            reply = my_email
            deliver_mail(:attachments => [attachment]) do
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
