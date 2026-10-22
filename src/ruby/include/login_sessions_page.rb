require_relative 'login_sessions'

# The browsers in which the account is logged in, in the profile
# (»Konto & Darstellung«), and ending one of them. The logic is in
# login_sessions.rb; main.rb stores the fields at the login (new_session_fields)
# and the day of the last use in its before filter.
class Main < Sinatra::Base
    # The fields of a new Session node, from this request's User-Agent.
    def new_session_fields
        LoginSessions.new_session_fields(request.user_agent)
    end

    def own_login_sessions
        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email]).map { |r| r['s'].to_h.merge(:email => @session_user[:email]) }
            MATCH (s:Session)-[:FOR]->(:User {email: $email})
            WHERE s.impersonated_by IS NULL
            RETURN s;
        END_OF_QUERY
    end

    # Not while a teacher looks in: where a person is logged in is the
    # person's own business (Privacy, »Sitzungen«).
    def login_sessions_html(limit: 10)
        return '' unless user_logged_in? && @impersonated_by.nil?

        LoginSessions.list_html(own_login_sessions, :current_sid => current_session_sid.to_s, :limit => limit)
    end

    post '/api/login_sessions' do
        assert(user_logged_in?)
        halt 403, { :error => 'impersonating' }.to_json if @impersonated_by
        respond(:html => login_sessions_html(:limit => nil))
    end

    # Ends one other session of the person, named by its public id.
    post '/api/end_login_session' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:id])
        status, sid = LoginSessions.sid_to_end(own_login_sessions, data[:id].to_s,
            :owner => @session_user[:email], :current_sid => current_session_sid.to_s, :impersonating => !@impersonated_by.nil?)
        case status
        when :impersonating then halt 403, { :error => 'impersonating' }.to_json
        when :invalid then halt 400, { :error => 'invalid_id' }.to_json
        when :not_found then halt 404, { :error => 'not_found' }.to_json
        when :current then halt 400, { :error => 'current_session' }.to_json
        end
        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :sid => sid)
            MATCH (s:Session {sid: $sid})-[:FOR]->(:User {email: $email})
            WHERE s.impersonated_by IS NULL
            DETACH DELETE s;
        END_OF_QUERY
        STDERR.puts "#{@session_user[:email]} ended one other session"
        respond(:ended => 1, :others => other_sessions_count)
    end
end
