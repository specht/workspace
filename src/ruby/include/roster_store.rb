# The roster in Neo4j (Roster, docs/teacher-access.md):
#
#   (:School {key, name, address, approved_at, approved_by, …})
#   (:Group {name, created_at, created_by})-[:AT]->(:School)
#   (:Invitation {email, name, teacher, welcomed_at, created_at})
#   (:Invitation)-[:MEMBER_OF]->(:Group), (:Invitation)-[:TEACHES]->(:Group),
#   (:Invitation)-[:AT]->(:School) for teachers
#   (:Setting {key: 'roster', imported_at, imported_by, files, changed_at})
#
# Without the Setting node the invitation files are the roster, as before.
# The import writes the records, reads them back and compares what they mean
# with what the files mean (Roster.content), all in one transaction: on any
# difference nothing is written. The import is final (the maintainer's
# decision): there is no way back to the files but a backup of Neo4j.
class Main < Sinatra::Base
    ROSTER_SETTING = 'roster'
    HOME_SCHOOL = 'home'
    @@roster_cache = nil
    @@roster_cache_mutex = Mutex.new
    @@roster_checked_at = 0.0
    # How often the cache asks Neo4j whether the roster changed behind its
    # back (a database dump loaded, a change from another process).
    ROSTER_RECHECK_SECONDS = 10

    class RosterImportMismatch < StandardError
        attr_reader :differences

        def initialize(differences)
            @differences = differences
            super("#{differences.size} differences")
        end
    end

    # After every change to the roster in Neo4j.
    def self.roster_changed!
        @@roster_cache_mutex.synchronize { @@roster_cache = nil }
    end

    def self.cached_roster
        reloaded = false
        roster = @@roster_cache_mutex.synchronize do
            if @@roster_cache && Time.now.to_f - @@roster_checked_at > ROSTER_RECHECK_SECONDS
                @@roster_checked_at = Time.now.to_f
                if roster_fingerprint != @@roster_cache[:fingerprint]
                    @@roster_cache = nil
                    reloaded = true
                end
            end
            @@roster_cache ||= begin
                @@roster_checked_at = Time.now.to_f
                fingerprint = roster_fingerprint
                setting = $neo4j.neo4j_query('MATCH (s:Setting {key: $key}) RETURN s;', :key => ROSTER_SETTING).first
                { :imported => setting && setting['s'], :records => setting ? read_roster_records : nil, :fingerprint => fingerprint }
            end
        end
        if reloaded
            STDERR.puts 'Roster changed in Neo4j outside this process (a dump loaded?): reloaded'
            # Whoever lost access loses a running Workspace too.
            Thread.new { load_invitations; refresh_nginx_config }
        end
        roster
    end

    # Changes with every write through the pages (changed_at) and with a
    # different database (another import, other counts).
    def self.roster_fingerprint
        $neo4j.neo4j_query(<<~END_OF_QUERY, :key => ROSTER_SETTING).first.to_h
            OPTIONAL MATCH (s:Setting {key: $key})
            RETURN s.imported_at AS imported_at, s.changed_at AS changed_at,
                COUNT { (:Invitation) } AS invitations,
                COUNT { (:Invitation)-[:MEMBER_OF|TEACHES|AT]->() } AS links,
                COUNT { (:Group) } AS groups;
        END_OF_QUERY
    end

    # The Setting node's properties once imported, else nil.
    def self.roster_imported
        cached_roster[:imported]
    end

    def self.read_roster_records
        {
            :groups => $neo4j.neo4j_query(<<~END_OF_QUERY).map { |r| { :name => r['name'], :school => r['school'], :approved => r['approved'] == true } },
                MATCH (g:Group) OPTIONAL MATCH (g)-[:AT]->(s:School)
                RETURN g.name AS name, s.key AS school, s.approved_at IS NOT NULL AS approved;
            END_OF_QUERY
            :invitations => $neo4j.neo4j_query(<<~END_OF_QUERY).map { |r| { :email => r['email'], :name => r['name'], :teacher => r['teacher'] == true, :school => r['school'] } },
                MATCH (i:Invitation) OPTIONAL MATCH (i)-[:AT]->(s:School)
                RETURN i.email AS email, i.name AS name, i.teacher AS teacher, s.key AS school;
            END_OF_QUERY
            :memberships => $neo4j.neo4j_query('MATCH (i:Invitation)-[:MEMBER_OF]->(g:Group) RETURN i.email AS email, g.name AS name;').map { |r| [r['email'], r['name']] },
            :teaching => $neo4j.neo4j_query('MATCH (i:Invitation)-[:TEACHES]->(g:Group) RETURN i.email AS email, g.name AS name;').map { |r| [r['email'], r['name']] },
        }
    end

    # The invitation files: :all, :tools (written by tools, name starts
    # with _) or :people (the others: what the import takes).
    def self.invitation_files(which = :all)
        Dir['/invitations/*.txt'].sort.reject do |path|
            name = File.basename(path)
            name == '_template.txt' || (which == :tools && !name.start_with?('_')) || (which == :people && name.start_with?('_'))
        end
    end

    def self.invitation_file_texts(which = :all)
        invitation_files(which).map { |path| File.read(path) }
    end

    # Where the roster comes from: the files before the import; afterwards
    # Neo4j and the tools' files.
    def self.roster_source
        cached = cached_roster
        return Roster.parse_files(invitation_file_texts(:all)) unless cached[:imported]

        db = Roster.from_records(cached[:records])
        tools = Roster.parse_files(invitation_file_texts(:tools))
        {
            :groups => db[:groups] + tools[:groups],
            :members => db[:members] + tools[:members],
            :designated => db[:designated] + tools[:designated],
        }
    end

    def self.roster_content(source)
        Roster.content(Roster.build(:groups => source[:groups], :members => source[:members], :designated => source[:designated], :admins => []))
    end

    # What the import would take, for the administration to look at first.
    def self.invitation_import_preview
        parsed = Roster.parse_files(invitation_file_texts(:people))
        records = Roster.records_from_files(parsed, :school => HOME_SCHOOL)
        counts = Hash.new(0)
        records[:memberships].each { |_, group| counts[group] += 1 }
        {
            :files => invitation_files(:people).map { |p| File.basename(p) },
            :groups => records[:groups].map { |g| { :name => g[:name], :members => counts[g[:name]] } },
            :invitations => records[:invitations].size,
            :without_name => records[:invitations].count { |i| i[:name].nil? },
            :teachers => records[:invitations].count { |i| i[:school] },
            :imported => roster_imported,
        }
    end

    def self.write_roster_records(records, by:, now:)
        school_name = defined?(OPERATOR_SCHOOL) && !OPERATOR_SCHOOL.to_s.strip.empty? ? OPERATOR_SCHOOL.to_s.strip : 'Schule'
        school_address = defined?(OPERATOR_ADDRESS) ? OPERATOR_ADDRESS.to_s.strip : ''
        $neo4j.neo4j_query(<<~END_OF_QUERY, :key => HOME_SCHOOL, :name => school_name, :address => school_address, :now => now, :by => by)
            MERGE (s:School {key: $key})
            ON CREATE SET s.name = $name, s.address = $address, s.home = true, s.created_at = $now;
        END_OF_QUERY
        $neo4j.neo4j_query(<<~END_OF_QUERY, :groups => records[:groups].map { |g| { 'name' => g[:name], 'school' => g[:school] } }, :now => now, :by => by)
            UNWIND $groups AS g
            MATCH (s:School {key: g.school})
            CREATE (:Group {name: g.name, created_at: $now, created_by: $by})-[:AT]->(s);
        END_OF_QUERY
        $neo4j.neo4j_query(<<~END_OF_QUERY, :invitations => records[:invitations].map { |i| { 'email' => i[:email], 'name' => i[:name], 'teacher' => i[:teacher] } }, :now => now)
            UNWIND $invitations AS i
            CREATE (:Invitation {email: i.email, name: i.name, teacher: i.teacher, created_at: $now});
        END_OF_QUERY
        $neo4j.neo4j_query(<<~END_OF_QUERY, :pairs => records[:invitations].select { |i| i[:school] }.map { |i| [i[:email], i[:school]] })
            UNWIND $pairs AS p
            MATCH (i:Invitation {email: p[0]}), (s:School {key: p[1]})
            CREATE (i)-[:AT]->(s);
        END_OF_QUERY
        { 'MEMBER_OF' => records[:memberships], 'TEACHES' => records[:teaching] }.each do |relation, pairs|
            $neo4j.neo4j_query(<<~END_OF_QUERY, :pairs => pairs)
                UNWIND $pairs AS p
                MATCH (i:Invitation {email: p[0]}), (g:Group {name: p[1]})
                CREATE (i)-[:#{relation}]->(g);
            END_OF_QUERY
        end
    end

    # The import (see above). simulate_difference: in development only, to
    # see that a difference stops it.
    def self.import_invitations!(by:, simulate_difference: false)
        raise 'already imported' if roster_imported

        existing = $neo4j.neo4j_query('MATCH (n) WHERE n:Group OR n:Invitation RETURN count(n) AS n;').first['n'].to_i
        raise 'groups or invitations exist already' if existing > 0

        files = invitation_files(:people).map { |p| File.basename(p) }
        parsed = Roster.parse_files(invitation_file_texts(:people))
        before = roster_content(parsed)
        records = Roster.records_from_files(parsed, :school => HOME_SCHOOL)
        now = Time.now.to_i
        $neo4j.transaction do
            write_roster_records(records, :by => by, :now => now)
            if simulate_difference && DEVELOPMENT
                $neo4j.neo4j_query('MATCH (:Invitation)-[r:MEMBER_OF]->() WITH r LIMIT 1 DELETE r;')
            end
            after = roster_content(Roster.from_records(read_roster_records))
            differences = Roster.differences(before, after)
            raise RosterImportMismatch.new(differences) unless differences.empty?

            $neo4j.neo4j_query(<<~END_OF_QUERY, :key => ROSTER_SETTING, :now => now, :by => by, :files => files)
                CREATE (:Setting {key: $key, imported_at: $now, imported_by: $by, files: $files});
            END_OF_QUERY
        end
        roster_changed!
        load_invitations
        { :ok => true, :groups => records[:groups].size, :invitations => records[:invitations].size }
    rescue RosterImportMismatch => e
        roster_changed!
        { :ok => false, :differences => e.differences }
    end

    # Every change in the browser notes the time, shown on /admin.
    def self.note_roster_change!
        $neo4j.neo4j_query('MATCH (s:Setting {key: $key}) SET s.changed_at = $now;', :key => ROSTER_SETTING, :now => Time.now.to_i)
        before = Set.new(@@invitations.keys)
        roster_changed!
        load_invitations
        # Someone no longer invited loses a running Workspace at once
        # (may_use_workspace?), not when it is stopped for being idle.
        Thread.new { refresh_nginx_config } unless Set.new(@@invitations.keys) == before
    end

    # The state of the roster for the administration page.
    def roster_status_html
        imported = Main.roster_imported
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        if imported
            at = Time.at(imported[:imported_at].to_i).strftime('%d.%m.%Y, %H:%M')
            changed = imported[:changed_at] ? " Seitdem im Browser geändert: #{Time.at(imported[:changed_at].to_i).strftime('%d.%m.%Y, %H:%M')}." : ' Seitdem nichts im Browser geändert.'
            "<p>Die Einladungen stehen seit dem #{at} in der Datenbank (übernommen aus #{h.(Array(imported[:files]).join(', '))}).#{changed} " \
                'Dateien, deren Name mit _ beginnt, werden weiter gelesen; die anderen nicht mehr.</p>'
        else
            files = Main.invitation_files(:people).map { |p| File.basename(p) }
            "<p>Die Einladungen kommen aus #{files.size} #{files.size == 1 ? 'Datei' : 'Dateien'} unter <code>/invitations</code>: #{h.(files.join(', '))}. " \
                'Mit dem Import kommen sie in die Datenbank; Lehrkräfte verwalten ihre Gruppen dann im Browser.</p>'
        end
    end

    post '/api/admin/invitation_import_preview' do
        assert(admin_logged_in?)
        respond(Main.invitation_import_preview)
    end

    post '/api/admin/import_invitations' do
        assert(admin_logged_in?)
        data = parse_request_data(:optional_keys => [:simulate_difference], :types => { :simulate_difference => Object })
        result = Main.import_invitations!(:by => @session_user[:email], :simulate_difference => data[:simulate_difference] == true)
        STDERR.puts "Invitation import by #{@session_user[:email]}: #{result[:ok] ? 'ok' : "#{result[:differences].size} differences, nothing written"}"
        respond(result)
    end
end
