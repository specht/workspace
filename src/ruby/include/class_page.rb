# Parts of the class page (/klasse) that are not one activity's board:
# the tab »Überblick« (how far each student has come, ClassBoard.overview),
# the school of the group with its documents (tab »Einstellungen«), what a
# teacher without a school is told instead of a missing »Neue Gruppe«, and
# the board's »zuletzt« kept fresh while the page stays open
# (class_last_active.js).
class Main < Sinatra::Base
    # The group's activities for the overview: pinned ones first (newest
    # pin first, as on the students' start page), then every other activity
    # someone of the group worked on, in the order of the catalog.
    def class_overview_activities(group, states)
        activities = class_activities(group)
        # the short names of /lehrplan's boxes (»Pip: Hinab in die Stollen«,
        # »Kata: Listen«), so that the upright headings stay short
        by_key = activities.to_h { |key, title, tasks| [key, { :key => key, :title => title, :short => CurriculumGraph.short(title.to_s), :tasks => tasks }] }
        key_of = @@content.each_with_object({}) { |(slug, c), all| all[slug] = c[:tasks][:activity] if c[:tasks] }
        pinned = pins_for_groups([group]).map { |p| key_of[p[:slug]] }.compact.uniq.select { |key| by_key[key] }
        worked = Set.new
        states.each_value { |mine| mine.each_key { |task| worked << task.split('/', 2).first } }
        catalog = @@section_order.flat_map { |s| (@@sections[s] || {})[:entries] || [] }.map { |slug| key_of[slug] }.compact.uniq
        rest = (catalog + by_key.keys).uniq.select { |key| worked.include?(key) && !pinned.include?(key) && by_key[key] }
        [(pinned + rest).map { |key| by_key[key] }, pinned]
    end

    def class_overview_html(group)
        members = class_members(group)
        states = Hash.new { |h, k| h[k] = {} }
        neo4j_query(<<~END_OF_QUERY, :emails => members.map { |m| m[:email] }).each { |row| states[row['email']][row['s'][:task]] = row['s'] }
            MATCH (u:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE u.email IN $emails
            RETURN u.email AS email, s;
        END_OF_QUERY
        activities, pinned = class_overview_activities(group, states)
        overview = ClassBoard.overview(members, activities, states)
        link = ->(key) { "/klasse?group=#{CGI.escape(group)}&tab=fortschritt&activity=#{CGI.escape(key)}" }
        "<section class='class-overview-panel'>#{ClassBoard.overview_html(overview, :link => link, :pinned => pinned)}</section>"
    end

    # Who to write to, as a sentence's end: » an a@b.de« or nothing.
    def class_contact_html
        link = contact_email_link
        link.empty? ? 'an die Administration' : "an #{link}"
    end

    # Instead of a missing »Neue Gruppe«: a teacher without a school is told
    # why, and whom to ask (the button would only fail with »no_school«).
    def new_group_note_html
        return '' if admin_logged_in? || !teacher_account_logged_in? || Main.roster_imported.nil? || may_create_group?

        "<p class='class-group-new-note'><i class='bi bi-info-circle' aria-hidden='true'></i>Neue Gruppen legst du an, sobald die Administration " \
            "dir eine Schule zugeordnet hat. Schreib dafür #{class_contact_html}, an welcher Schule du unterrichtest.</p>"
    end

    # In »Einstellungen«: the school the group belongs to, whether it has
    # agreed, and its documents (they were on /gruppen, which forwards
    # teachers here now). Nothing while the roster is still in files.
    def class_school_html(group)
        return '' if Main.roster_imported.nil?
        # a group from the invitation files (the tools' groups in
        # development) has no school to show; »Lernende« says who keeps it
        return '' if neo4j_query('MATCH (g:Group {name: $name}) RETURN 1 AS yes LIMIT 1;', :name => group).empty?

        h = ->(t) { CGI.escapeHTML(t.to_s) }
        school = school_of_group(group)
        io = +"<section class='class-setting class-school'><h3 class='class-setting-title'>Schule</h3>"
        if school.nil?
            io << "<p class='class-setting-hint'>Diese Gruppe gehört noch zu keiner Schule. Das trägt die Administration ein; schreib #{class_contact_html}.</p>"
        else
            key = CGI.escape(school[:key].to_s)
            documents = school[:home] ? '/datenschutz' : "/datenschutz?schule=#{key}"
            io << "<p><strong>#{h.(school[:name])}</strong>"
            if school[:approved]
                io << " hat dem Einsatz zugestimmt. <a href='#{h.(documents)}'><i class='bi bi-file-earmark-text' aria-hidden='true'></i>Unterlagen für deine Schule</a></p>"
            elsif school[:guests]
                io << "</p><p class='class-setting-hint'>Gäste: Erwachsene, die den Workspace kennenlernen. Für sie gilt die Nutzungsordnung, keine Entscheidung einer Schule.</p>"
            else
                io << " hat noch nicht zugestimmt.</p><p class='class-setting-hint'>Am besten unterschreibt die Schulleitung die Entscheidung und die Vereinbarung aus den " \
                    "<a href='#{h.(documents)}'>Unterlagen für deine Schule</a>; schick beides dann #{class_contact_html}. Bis dahin genügt für eine Schülerin oder einen Schüler " \
                    "ein schriftliches Einverständnis (<a href='#{h.("/datenschutz/elternbrief?schule=#{key}&einverstaendnis=1")}'>Elternbrief mit Einverständniserklärung</a>): " \
                    "Du bewahrst es auf und trägst das Datum im Reiter »Lernende« ein. Anmelden können sich alle schon jetzt.</p>"
            end
        end
        io << '</section>'
    end

    # The board's »zuletzt« and who is quiet, asked again every minute while
    # the class page is open (class_last_active.js): { email => epoch }.
    post '/api/class/last_active' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :activity], :max_value_lengths => { :group => 200, :activity => 64 })
        halt 403 unless class_groups.include?(data[:group])
        _, _, tasks = class_activities(data[:group]).find { |a, _, _| a == data[:activity] }
        halt 404 if tasks.nil?

        members = class_members(data[:group])
        board = ClassBoard.build(members, tasks, class_board_states(members.map { |m| m[:email] }, data[:activity]))
        now = Time.now.to_i
        respond(:now => now, :last => board[:rows].to_h { |r| [r[:email], ClassBoard.last_active(r)] },
            :quiet => ClassBoard.quiet(board[:rows], tasks, now))
    end
end
