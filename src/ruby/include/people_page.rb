# /personen, for administrators: schools and whether they have agreed,
# teachers and their school (only administrators assign one), welcome mails
# to teachers. A school counts as approved (its students can log in) once
# both the Schulleitung's decision and the agreement on processing
# (Auftragsverarbeitung) are recorded; the school of the installation is
# approved by the import, as it was before. »Gäste« is a school of its own
# for adults from outside (Nutzungsordnung), approved from the start.
class Main < Sinatra::Base
    GUESTS_SCHOOL = 'guests'

    def admin_only!
        assert(admin_logged_in?)
        roster_writable!
    end

    def school_key_for(name)
        base = name.downcase.gsub(/[^a-z0-9]+/, '-').gsub(/\A-|-\z/, '')[0, 30]
        base = 'schule' if base.empty?
        "#{base}-#{SecureRandom.hex(3)}"
    end

    def schools_overview
        neo4j_query(<<~END_OF_QUERY).map { |r| r['s'].merge(:groups => r['groups'], :teachers => r['teachers']) }
            MATCH (s:School)
            OPTIONAL MATCH (g:Group)-[:AT]->(s)
            WITH s, count(DISTINCT g) AS groups
            OPTIONAL MATCH (t:Invitation)-[:AT]->(s)
            RETURN s, groups, count(DISTINCT t) AS teachers
            ORDER BY coalesce(s.home, false) DESC, coalesce(s.guests, false), toLower(s.name);
        END_OF_QUERY
    end

    def school_approval!(key)
        neo4j_query(<<~END_OF_QUERY, :key => key, :now => Time.now.to_i, :me => my_email)
            MATCH (s:School {key: $key})
            WHERE coalesce(s.home, false) = false AND coalesce(s.guests, false) = false
            SET s.approved_at = CASE WHEN s.decision_at IS NOT NULL AND s.agreement_at IS NOT NULL
                THEN coalesce(s.approved_at, $now) ELSE null END,
                s.approved_by = CASE WHEN s.decision_at IS NOT NULL AND s.agreement_at IS NOT NULL
                THEN coalesce(s.approved_by, $me) ELSE null END;
        END_OF_QUERY
    end

    post '/api/people/schools' do
        assert(admin_logged_in?)
        imported = !Main.roster_imported.nil?
        respond(:schools => imported ? schools_overview : [], :imported => imported)
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

    # Teachers with their school, for the list on /personen.
    post '/api/people/teachers' do
        assert(admin_logged_in?)
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
