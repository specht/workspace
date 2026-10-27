# Who may do what: the rules every route, the nginx map and the
# background jobs ask, in one place (the review of 2026-10-19 asked for a
# central authorization point). Moved here unchanged from main.rb,
# groups_page.rb and people_page.rb.
#
# - may_use_workspace?: may this account use the Workspace at all (session,
#   login, running Workspace, shared links, live apps)
# - who is logged in as what: user / teacher account / + teacher / admin
# - may_look_in?: into whose account (LookIn)
# - terms_required?: a guest who has not agreed to the Nutzungsordnung
# - route guards: teacher_or_admin!, admin_only!, managed_group!
class Main < Sinatra::Base
    # Who may use the Workspace: whoever is invited, the administration,
    # test accounts, and an account whose deletion is announced (until the
    # day, to take out its data). Everyone else loses existing sessions and
    # access to a running Workspace and its shared links too, not only the
    # next login.
    #
    # A suspended account (User.suspended_at, set on /personen) may not, by
    # no path, whatever else is true; only lifting the suspension undoes it.
    # Without loaded invitations nobody may (fail closed; the start loads
    # them before the first nginx map).
    def self.may_use_workspace?(email, user = nil)
        return false unless (user || {})[:suspended_at].to_s.empty?
        return true if ADMIN_USERS.include?(email)
        return false unless class_variable_defined?(:@@invitations) && class_variable_defined?(:@@test_accounts)
        return true if @@invitations.key?(email) || @@test_accounts.key?(email)

        !(user || {})[:deletion_at].to_s.empty?
    end

    # Not logged in: to the login, which comes back to this page afterwards
    # (/login?next=/profil; login.html, Authentication.safe_next_path).
    def this_is_a_page_for_logged_in_users!
        if @session_user.nil?
            redirect "#{WEB_ROOT}/login#{Main.login_next_query(request.fullpath)}", 302
        end
    end

    # »?next=…« for a link to the login that comes back to path, or nothing
    # for the start page and for paths the login would not go back to.
    def self.login_next_query(path)
        path = Authentication.safe_next_path(path.to_s)
        path == '/' ? '' : "?next=#{CGI.escape(path)}"
    end

    # A page someone may not open: the page in the frame of the site
    # (_forbidden.html) instead of an empty 403, as a missing page gets
    # _not_found.html. The template reads the locals path and running_tests.
    def halt_forbidden_page!
        path = request.path
        running_tests = my_running_tests()
        template = File.read(File.join(@@static_dir, '_template.html'))
        content = File.read(File.join(@@static_dir, '_forbidden.html'))
        html = Main.inject_page_metadata(TrustedTemplate.render_page(template, content, binding), request.path)
        halt 403, { 'Content-Type' => 'text/html; charset=utf-8' }, html
    end

    def user_logged_in?
        return (!@session_user.nil?)
    end

    # Teaching resources are available to designated teachers without any
    # access to group administration, students' accounts, or test archives.
    def teacher_account_logged_in?
        user_logged_in? && @@teacher_emails.include?(@session_user[:email])
    end

    # Existing administrative permissions stay tied to + group assignments.
    def teacher_logged_in?
        admin_logged_in? || (user_logged_in? && @@teachers.include?(@session_user[:email]))
    end

    def admin_logged_in?
        user_logged_in? && ADMIN_USERS.include?(@session_user[:email])
    end

    # Who may look into whose account (LookIn): administrators into anyone's,
    # teachers into their own students'.
    def may_look_in?(email)
        Main.load_invitations()
        LookIn.allowed?(@session_user && @session_user[:email], email,
            :admins => ADMIN_USERS, :teachers => @@teachers, :group_members => @@group_members)
    end

    # The same for what a look-in leaves behind (an imitated session, a
    # watched Workspace): may viewer still look into target, with its own
    # access (invitation, suspension) and its groups as they are now, not as
    # they were when it began. viewer_user: viewer's User node, if at hand.
    def self.may_still_look_in?(viewer, target, viewer_user = nil)
        load_invitations()
        viewer_user ||= $neo4j.neo4j_query('MATCH (u:User {email: $email}) RETURN u;', :email => viewer.to_s).first&.[]('u')
        may_use_workspace?(viewer.to_s, viewer_user) &&
            LookIn.allowed?(viewer, target, :admins => ADMIN_USERS, :teachers => @@teachers, :group_members => @@group_members)
    end

    # Guests (adults in a group of the guests' school: workshops, companies,
    # former students who keep their account) agree to the Nutzungsordnung
    # once, before their own Workspace starts; it is their legal basis
    # (Art. 6 Abs. 1 Buchst. b DSGVO). Administrators and test accounts do
    # not. The date and the version (Privacy::CHECKED) stay on the account.
    def terms_required?
        return false unless user_logged_in? && @impersonated_by.nil?
        return false if @session_user[:terms_accepted_at]
        return @terms_required unless @terms_required.nil?

        email = @session_user[:email]
        @terms_required = !ADMIN_USERS.include?(email) && !@@test_accounts.key?(email) && !Main.roster_imported.nil? &&
            !neo4j_query(<<~END_OF_QUERY, :email => email).empty?
                MATCH (:Invitation {email: $email})-[:MEMBER_OF]->(:Group)-[:AT]->(:School {guests: true})
                RETURN 1 AS yes LIMIT 1;
            END_OF_QUERY
    end

    # Teachers manage their own groups, administrators every group; anyone
    # else gets a refusal, not an error.
    def teacher_or_admin!
        halt 403, { :error => 'teachers_only' }.to_json unless teacher_account_logged_in? || admin_logged_in?
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

    # A refusal, not an error: 403 for anyone who is not an administrator.
    def admins_only!
        halt 403, { :error => 'admins_only' }.to_json unless admin_logged_in?
    end

    # The same for what changes the roster, which must be writable.
    def admin_only!
        admins_only!
        roster_writable!
    end
end
