# Logging in and staying logged in: the session cookies, the login with a
# code sent by email (request_login, complete_login), the login with a QR
# code from a phone that is logged in, the accounts of invited people, the
# rate limits per client and per address, the terms of use, and signing out
# the other sessions. Moved out of main.rb, which loads it; the list of
# sessions in the profile is login_sessions_page.rb.
class Main < Sinatra::Base
    def current_session_sid
        raw = request.cookies[session_cookie_name]
        raw ||= request.cookies['hs_sid'] unless DEVELOPMENT
        return nil unless raw.is_a?(String)

        raw.split(',').first
    end
    # The session's sid as the database stores it (Authentication.session_key).
    def current_session_key
        sid = current_session_sid
        sid && Authentication.session_key(sid)
    end
    def session_cookie_expiry
        Time.now + 3600 * 24 * Authentication::SESSION_LIFETIME_DAYS
    end
    def set_session_cookie(sid, expires = session_cookie_expiry)
        response.set_cookie(
            session_cookie_name,
            Authentication.session_cookie_options(
                :value => sid,
                :expires => expires,
                :development => DEVELOPMENT,
            ),
        )
        clear_legacy_session_cookie
    end
    def set_server_sid_cookie(server_sid, expires = session_cookie_expiry)
        response.set_cookie(
            'hs_server_sid',
            Authentication.domain_cookie_options(
                :value => server_sid,
                :expires => expires,
                :domain => WEBSITE_HOST.split(':').first,
                :development => DEVELOPMENT,
            ),
        )
    end
    def clear_legacy_session_cookie
        response.set_cookie(
            'hs_sid',
            Authentication.domain_cookie_options(
                :value => '',
                :expires => Time.at(0),
                :domain => WEBSITE_HOST.split(':').first,
                :development => DEVELOPMENT,
            ),
        )
    end
    def clear_domain_cookie(name)
        response.set_cookie(
            name,
            Authentication.domain_cookie_options(
                :value => '',
                :expires => Time.at(0),
                :domain => WEBSITE_HOST.split(':').first,
                :development => DEVELOPMENT,
            ),
        )
    end
    def clear_session_cookies
        response.set_cookie(
            session_cookie_name,
            Authentication.session_cookie_options(
                :value => '',
                :expires => Time.at(0),
                :development => DEVELOPMENT,
            ),
        )
        clear_legacy_session_cookie
        clear_domain_cookie('hs_server_sid')
        clear_domain_cookie('hs_watch_tag')
    end
    def cleanup_login_requests
        cleanup_expired_sessions
        cleanup_params = {
            :now => Time.now.to_i,
            :max_attempts => Authentication::LOGIN_REQUEST_MAX_ATTEMPTS,
        }
        neo4j_query(<<~END_OF_QUERY, cleanup_params)
            MATCH (l:LoginRequest)
            WHERE COALESCE(l.expires_at, 0) <= $now
               OR COALESCE(l.attempts, 0) >= $max_attempts
            DETACH DELETE l;
        END_OF_QUERY
    end
    # Sessions past their expiry go, at most once an hour, so that a session
    # is kept no longer than the Datenschutzerklärung says (Privacy).
    @@sessions_cleaned_at = 0
    def cleanup_expired_sessions
        return if Time.now.to_i - @@sessions_cleaned_at < 3600

        @@sessions_cleaned_at = Time.now.to_i
        now = DateTime.now
        expired = neo4j_query('MATCH (s:Session) RETURN s.sid AS sid, s.expires AS expires;').map do |row|
            expires = DateTime.parse(row['expires'].to_s) rescue nil
            row['sid'] if expires.nil? || expires <= now
        end.compact
        return if expired.empty?

        neo4j_query('MATCH (s:Session) WHERE s.sid IN $sids DETACH DELETE s;', :sids => expired)
    end
    def cleanup_qr_login_requests
        neo4j_query(<<~END_OF_QUERY, :now => Time.now.to_i)
            MATCH (q:QrLoginRequest)
            WHERE COALESCE(q.expires_at, 0) <= $now
            DETACH DELETE q;
        END_OF_QUERY
    end
    post '/api/qr_login/start' do
        headers 'Cache-Control' => 'no-store'
        unless DEVELOPMENT || QR_LOGIN_STARTS_PER_CLIENT.allow?(client_ip)
            content_type :json
            halt 429, { :error => 'rate_limited' }.to_json
        end
        tag = RandomTag::generate(16)
        browser_secret = RandomTag::generate(32)
        approval_secret = RandomTag::generate(32)
        now = Time.now.to_i
        expires_at = Authentication.qr_login_request_expires_at(:now => now)
        approval_url = "#{WEB_ROOT}/qr-login##{tag}.#{approval_secret}"

        begin
            svg, qr_error, qr_status = Open3.capture3(
                'qrencode', '-t', 'SVG', '-m', '2', '-o', '-',
                :stdin_data => approval_url,
            )
        rescue Errno::ENOENT => e
            STDERR.puts ">>> QR login unavailable: #{e.message}"
            content_type :json
            halt 503, { :error => 'qr_login_unavailable' }.to_json
        end
        unless qr_status.success?
            STDERR.puts ">>> qrencode failed: #{qr_error}"
            content_type :json
            halt 503, { :error => 'qr_login_unavailable' }.to_json
        end

        create_params = {
            :tag => tag,
            :browser_secret_sha256 => Digest::SHA256.hexdigest(browser_secret),
            :approval_secret_sha256 => Digest::SHA256.hexdigest(approval_secret),
            :created_at => now,
            :expires_at => expires_at,
        }
        @@qr_login_mutex.synchronize do
            cleanup_qr_login_requests
            neo4j_query_expect_one(<<~END_OF_QUERY, create_params)
                CREATE (q:QrLoginRequest {
                    tag: $tag,
                    browser_secret_sha256: $browser_secret_sha256,
                    approval_secret_sha256: $approval_secret_sha256,
                    created_at: $created_at,
                    expires_at: $expires_at
                })
                RETURN q.tag;
            END_OF_QUERY
        end

        payload = {
            :status => 'pending',
            :tag => tag,
            :browser_secret => browser_secret,
            :qr_image => "data:image/svg+xml;base64,#{Base64.strict_encode64(svg)}",
            :expires_at => expires_at,
        }
        # The raw approval URL is useful to the browser tests, but production
        # clients only receive the rendered QR code.
        payload[:approval_url] = approval_url if DEVELOPMENT
        respond(payload)
    end
    post '/api/qr_login/approve' do
        headers 'Cache-Control' => 'no-store'
        unless user_logged_in?
            content_type :json
            halt 401, { :error => 'qr_login_requires_login' }.to_json
        end

        data = parse_request_data(:required_keys => [:tag, :approval_secret])
        tag = data[:tag].to_s.downcase.strip
        approval_secret = data[:approval_secret].to_s.downcase.strip
        unless Authentication.valid_qr_login_tag?(tag) &&
               Authentication.valid_qr_login_secret?(approval_secret)
            content_type :json
            halt 400, { :error => 'invalid_qr_login_request' }.to_json
        end

        approval_params = {
            :tag => tag,
            :approval_secret_sha256 => Digest::SHA256.hexdigest(approval_secret),
            :email => @session_user[:email],
            :now => Time.now.to_i,
        }
        approval_status = @@qr_login_mutex.synchronize do
            cleanup_qr_login_requests
            row = neo4j_query(<<~END_OF_QUERY, approval_params).to_a.first
                MATCH (q:QrLoginRequest {
                    tag: $tag,
                    approval_secret_sha256: $approval_secret_sha256
                })
                WHERE q.expires_at > $now
                OPTIONAL MATCH (q)-[:APPROVED_BY]->(existing:User)
                RETURN existing.email AS approved_email;
            END_OF_QUERY
            next :expired unless row

            approved_email = row['approved_email']
            next :claimed if approved_email && approved_email != @session_user[:email]

            neo4j_query_expect_one(<<~END_OF_QUERY, approval_params)
                MATCH (q:QrLoginRequest {
                    tag: $tag,
                    approval_secret_sha256: $approval_secret_sha256
                })
                WHERE q.expires_at > $now
                MATCH (u:User {email: $email})
                MERGE (q)-[:APPROVED_BY]->(u)
                SET q.approved_at = $now
                RETURN q.tag;
            END_OF_QUERY
            :approved
        end

        case approval_status
        when :approved
            respond(:status => 'approved')
        when :claimed
            content_type :json
            halt 409, { :error => 'qr_login_already_approved' }.to_json
        else
            content_type :json
            halt 410, { :error => 'qr_login_expired' }.to_json
        end
    end
    post '/api/qr_login/status' do
        headers 'Cache-Control' => 'no-store'
        data = parse_request_data(:required_keys => [:tag, :browser_secret])
        tag = data[:tag].to_s.downcase.strip
        browser_secret = data[:browser_secret].to_s.downcase.strip
        unless Authentication.valid_qr_login_tag?(tag) &&
               Authentication.valid_qr_login_secret?(browser_secret)
            content_type :json
            halt 400, { :error => 'invalid_qr_login_request' }.to_json
        end

        lookup_params = {
            :tag => tag,
            :browser_secret_sha256 => Digest::SHA256.hexdigest(browser_secret),
            :now => Time.now.to_i,
        }
        # the cookie gets the sid, the database its hash (Authentication.session_key)
        sid = RandomTag::generate(24)
        qr_result = @@qr_login_mutex.synchronize do
            cleanup_qr_login_requests
            row = neo4j_query(<<~END_OF_QUERY, lookup_params).to_a.first
                MATCH (q:QrLoginRequest {
                    tag: $tag,
                    browser_secret_sha256: $browser_secret_sha256
                })
                WHERE q.expires_at > $now
                OPTIONAL MATCH (q)-[:APPROVED_BY]->(u:User)
                RETURN q.expires_at AS expires_at, u.email AS approved_email;
            END_OF_QUERY
            next { :status => :expired } unless row
            unless row['approved_email']
                next { :status => :pending, :expires_at => row['expires_at'].to_i }
            end

            session_expires = (DateTime.now + Authentication::SESSION_LIFETIME_DAYS).to_s
            complete_params = lookup_params.merge(
                :email => row['approved_email'],
                :sid => Authentication.session_key(sid),
                :session_expires => session_expires,
                :session_info => new_session_fields,
            )
            login_result = neo4j_query(<<~END_OF_QUERY, complete_params).to_a.first
                MATCH (q:QrLoginRequest {
                    tag: $tag,
                    browser_secret_sha256: $browser_secret_sha256
                })-[:APPROVED_BY]->(u:User {email: $email})
                WHERE q.expires_at > $now
                DETACH DELETE q
                WITH u
                CREATE (s:Session {sid: $sid, expires: $session_expires})-[:FOR]->(u)
                SET s += $session_info
                RETURN u.email AS email, u.server_sid AS server_sid;
            END_OF_QUERY
            next { :status => :expired } unless login_result

            { :status => :approved, :login_result => login_result }
        end

        case qr_result[:status]
        when :pending
            respond(:status => 'pending', :expires_at => qr_result[:expires_at])
        when :approved
            expires = session_cookie_expiry
            set_session_cookie(sid, expires)
            server_sid = qr_result[:login_result]['server_sid']
            set_server_sid_cookie(server_sid, expires) if server_sid
            respond(:status => 'approved')
        else
            respond(:status => 'expired')
        end
    end
    post '/api/qr_login/cancel' do
        headers 'Cache-Control' => 'no-store'
        data = parse_request_data(:required_keys => [:tag, :browser_secret])
        tag = data[:tag].to_s.downcase.strip
        browser_secret = data[:browser_secret].to_s.downcase.strip

        if Authentication.valid_qr_login_tag?(tag) &&
           Authentication.valid_qr_login_secret?(browser_secret)
            cancel_params = {
                :tag => tag,
                :browser_secret_sha256 => Digest::SHA256.hexdigest(browser_secret),
            }
            @@qr_login_mutex.synchronize do
                neo4j_query(<<~END_OF_QUERY, cancel_params)
                    MATCH (q:QrLoginRequest {
                        tag: $tag,
                        browser_secret_sha256: $browser_secret_sha256
                    })
                    DETACH DELETE q;
                END_OF_QUERY
            end
        end

        respond(:status => 'cancelled')
    end
    # The account of an invited person, created as at its first login: a
    # teacher may look into a student (or its test account) that has never
    # logged in.
    def ensure_user_account(email)
        # Create the user node before the login-request lock. Neo4j's uniqueness
        # constraint keeps this idempotent even if two browsers arrive together.
        user = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email)['n']
            MERGE (n:User {email: $email})
            RETURN n;
        END_OF_QUERY
        user[:db_login] = Main.db_login_for_email(email)
        # The name is optional and may come later than the address: the one
        # in the invitation list wins.
        name = (@@invitations[email] || {})[:name]
        if name && user[:name] != name
            user = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email, :name => name)['n']
                MATCH (n:User {email: $email})
                SET n.name = $name
                RETURN n;
            END_OF_QUERY
        end

        if user[:server_tag].nil? || user[:server_sid].nil?
            neo4j_query(<<~END_OF_QUERY, {:email => email, :server_tag => gen_server_tag(), :server_sid => gen_server_sid()})
                MATCH (u:User {email: $email})
                SET u.server_tag = COALESCE(u.server_tag, $server_tag)
                SET u.server_sid = COALESCE(u.server_sid, $server_sid)
            END_OF_QUERY
        end
        user
    end
    # Limits for the login: per client generously, because all Workspaces
    # and classrooms of a school share one address; per address so tight
    # that guessing a code stays hopeless (20 codes a day, 5 tries each,
    # out of a million). Not in development, where the screenshot tool and
    # the browser tests log in all day from one machine.
    LOGIN_REQUESTS_PER_CLIENT = RateLimit.new(:limit => 120, :period => 600)
    LOGIN_TRIES_PER_CLIENT = RateLimit.new(:limit => 300, :period => 600)
    LOGIN_CODES_PER_ADDRESS = RateLimit.new(:limit => 20, :period => 24 * 3600)
    # Two more things anyone may start without a login: a QR login (qrencode
    # and an entry in the database) and a Workspace behind a share link.
    QR_LOGIN_STARTS_PER_CLIENT = RateLimit.new(:limit => 30, :period => 600)
    SHARE_STARTS_PER_CLIENT = RateLimit.new(:limit => 30, :period => 600)
    # The browser's address: Traefik appends it to X-Forwarded-For (the last
    # entry; earlier ones come from the client and prove nothing), nginx
    # passes the header on. Without Traefik (development) the peer itself.
    # The header counts only when the request comes from nginx: Workspaces
    # reach the backend directly on the user network and could otherwise set
    # any address and slip past the limits per address.
    @@nginx_addresses = nil
    @@nginx_addresses_at = 0
    # Every address of the nginx container, on each network it is in (it may
    # reach the backend over either): from docker inspect, else by name.
    def self.nginx_addresses
        if @@nginx_addresses.nil? || Time.now.to_i - @@nginx_addresses_at > 300
            names, = Open3.capture2('docker', 'ps', '--filter', 'label=com.docker.compose.service=nginx', '--format', '{{.Names}}')
            found = names.split.flat_map do |name|
                out, = Open3.capture2('docker', 'inspect', '-f', '{{range .NetworkSettings.Networks}}{{.IPAddress}} {{end}}', name)
                out.split
            end
            @@nginx_addresses = (found + (Resolv.getaddresses('nginx') rescue [])).uniq
            @@nginx_addresses_at = Time.now.to_i
        end
        @@nginx_addresses
    rescue => e
        STDERR.puts "nginx addresses: #{e.class}: #{e.message}"
        @@nginx_addresses || []
    end
    # In the user network, but not its gateway (that is the host, through
    # which browsers can arrive when ports are published).
    def self.in_user_network?(address)
        ip = (IPAddr.new(address.to_s) rescue nil)
        !ip.nil? && user_subnets.any? { |s| s[:net].include?(ip) && address.to_s != s[:gateway] }
    end
    # The peer is REMOTE_ADDR, not request.ip: Rack's request.ip already
    # follows X-Forwarded-For when the peer has a private address, which
    # every container has. Through nginx: if nginx got the request from a
    # Workspace (X-Hs-Peer in the user network), that Workspace; otherwise it
    # came through Traefik, whose entry is the last in X-Forwarded-For.
    def client_ip
        peer = request.env['REMOTE_ADDR'].to_s
        return peer unless Main.nginx_addresses.include?(peer)

        nginx_peer = request.env['HTTP_X_HS_PEER'].to_s
        return nginx_peer if Main.in_user_network?(nginx_peer)

        forwarded = request.env['HTTP_X_FORWARDED_FOR'].to_s.split(',').map(&:strip).reject(&:empty?)
        forwarded.last || (nginx_peer.empty? ? peer : nginx_peer)
    end
    post '/api/request_login' do
        unless DEVELOPMENT || LOGIN_REQUESTS_PER_CLIENT.allow?(client_ip)
            content_type :json
            halt 429, { :error => 'login_request_rate_limited' }.to_json
        end
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:email])
        email = data[:email].downcase.strip

        unless @@invitations.include?(email)
            # A test account (specht+test@…) does not make »specht« ambiguous.
            candidates = @@invitations.keys.select do |x|
                x[0, email.size] == email && !@@test_accounts.key?(x)
            end
            email = candidates.first if candidates.size == 1
        end

        # Said plainly, so a typo is noticed at once; the limits per client
        # keep this from being a fast way to find out who is invited.
        unless neo4j_query('MATCH (u:User {email: $email}) WHERE u.suspended_at IS NOT NULL RETURN 1 AS yes;', :email => email).empty?
            content_type :json
            halt 403, { :error => 'suspended' }.to_json
        end
        announced = !@@invitations.include?(email) &&
            !neo4j_query('MATCH (u:User {email: $email}) WHERE u.deletion_at IS NOT NULL RETURN 1 AS yes;', :email => email).empty?
        unless @@invitations.include?(email) || announced
            content_type :json
            halt 404, { :error => 'no_invitation_found' }.to_json
        end

        user = ensure_user_account(email)

        login_request = @@login_request_mutex.synchronize do
            cleanup_login_requests
            now = Time.now.to_i
            minimum_expiry = now + Authentication::LOGIN_REQUEST_RESEND_SECONDS
            existing_params = {
                :email => email,
                :minimum_expiry => minimum_expiry,
                :max_attempts => Authentication::LOGIN_REQUEST_MAX_ATTEMPTS,
            }
            existing = neo4j_query(<<~END_OF_QUERY, existing_params).to_a.first
                MATCH (r:LoginRequest)-[:FOR]->(u:User {email: $email})
                WHERE r.expires_at > $minimum_expiry
                  AND COALESCE(r.attempts, 0) < $max_attempts
                RETURN r
                ORDER BY r.created_at DESC
                LIMIT 1;
            END_OF_QUERY

            rate_state = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email)
                MATCH (u:User {email: $email})
                RETURN COALESCE(u.login_request_created_at, 0) AS last_created_at;
            END_OF_QUERY

            if existing
                request_node = existing['r']
                resend = now - request_node[:last_sent_at].to_i >= Authentication::LOGIN_REQUEST_RESEND_SECONDS
                if resend
                    neo4j_query(<<~END_OF_QUERY, {:tag => request_node[:tag], :now => now})
                        MATCH (r:LoginRequest {tag: $tag})
                        SET r.last_sent_at = $now;
                    END_OF_QUERY
                end
                {
                    :tag => request_node[:tag],
                    :code => request_node[:code],
                    :send_mail => resend,
                }
            else
                last_created_at = rate_state['last_created_at'].to_i
                if now - last_created_at < Authentication::LOGIN_REQUEST_RESEND_SECONDS
                    next { :rate_limited => true }
                end
                next { :rate_limited => true } unless DEVELOPMENT || LOGIN_CODES_PER_ADDRESS.allow?(email)

                # Replace a nearly-expired request rather than handing the browser
                # a code that may stop working a few seconds later.
                neo4j_query(<<~END_OF_QUERY, {:email => email})
                    MATCH (r:LoginRequest)-[:FOR]->(u:User {email: $email})
                    DETACH DELETE r;
                END_OF_QUERY

                tag = RandomTag::generate(12)
                code = Authentication.generate_login_code(:development => DEVELOPMENT)
                expires_at = Authentication.login_request_expires_at(:now => now)
                create_request_params = {
                    :email => email,
                    :tag => tag,
                    :code => code,
                    :created_at => now,
                    :expires_at => expires_at,
                }
                neo4j_query_expect_one(<<~END_OF_QUERY, create_request_params)
                    MATCH (u:User {email: $email})
                    SET u.login_request_created_at = $created_at
                    CREATE (r:LoginRequest)-[:FOR]->(u)
                    SET r.tag = $tag,
                        r.code = $code,
                        r.created_at = $created_at,
                        r.last_sent_at = $created_at,
                        r.expires_at = $expires_at,
                        r.attempts = 0
                    RETURN u.email;
                END_OF_QUERY
                { :tag => tag, :code => code, :send_mail => true }
            end
        end

        if login_request[:rate_limited]
            content_type :json
            halt 429, { :error => 'login_request_rate_limited' }.to_json
        end

        broadcast_login_codes()

        if login_request[:send_mail] && !email.include?('@example.com')
            if DEVELOPMENT
                STDERR.puts 'Not sending login code email in development mode.'
            else
                # A test account's code goes to its teacher, at the address
                # without +test: plus addresses do not reach every inbox.
                recipient = @@test_accounts[email] || email
                for_test = recipient != email
                STDERR.puts "Sending login code email for invited user #{email} to #{recipient}"
                signature = mail_signature
                deliver_mail do
                    to recipient
                    # bcc SMTP_FROM
                    from SMTP_FROM

                    subject for_test ? "Anmeldecode für dein Testkonto: #{login_request[:code]}" : "Dein Anmeldecode lautet #{login_request[:code]}"

                    StringIO.open do |io|
                        io.puts "<p>Hallo!</p>"
                        io.puts for_test ? "<p>Der Anmeldecode für dein Testkonto #{CGI.escapeHTML(email)} lautet:</p>" : "<p>Dein Anmeldecode lautet:</p>"
                        io.puts "<p style='font-size: 200%;'>#{login_request[:code]}</p>"
                        io.puts "<p>Der Code ist für zehn Minuten gültig. Nachdem du dich angemeldet hast, bleibst du für ein ganzes Jahr angemeldet (falls du dich nicht wieder abmeldest).</p>"
                        io.puts "<p>Falls du diese E-Mail nicht angefordert hast, hat jemand versucht, sich mit deiner E-Mail-Adresse auf <a href='https://#{WEBSITE_HOST}/'>https://#{WEBSITE_HOST}/</a> anzumelden. In diesem Fall musst du nichts weiter tun (es sei denn, du befürchtest, dass jemand anderes Zugriff auf dein E-Mail-Konto hat – dann solltest du dein E-Mail-Passwort ändern).</p>"
                        io.puts "<p>Viele Grüße,<br />#{CGI.escapeHTML(signature)}</p>"
                        io.string
                    end
                end
            end
        end
        respond(:ok => 'yay', :tag => login_request[:tag])
    end
    post '/api/complete_login' do
        unless DEVELOPMENT || LOGIN_TRIES_PER_CLIENT.allow?(client_ip)
            content_type :json
            halt 429, { :error => 'login_rate_limited' }.to_json
        end
        data = parse_request_data(:required_keys => [:tag, :code])
        tag = data[:tag].downcase.strip
        code = data[:code].strip

        unless Authentication.valid_login_tag?(tag) && Authentication.valid_login_code?(code)
            content_type :json
            halt 401, { :error => 'invalid_login_code' }.to_json
        end

        # the cookie gets the sid, the database its hash (Authentication.session_key)
        sid = RandomTag::generate(24)
        login_result = @@login_request_mutex.synchronize do
            now = Time.now.to_i
            row = neo4j_query(<<~END_OF_QUERY, :tag => tag).to_a.first
                MATCH (r:LoginRequest {tag: $tag})-[:FOR]->(u:User)
                RETURN r, u;
            END_OF_QUERY

            next nil unless row

            request_node = row['r']
            unless Authentication.login_request_active?(
                :expires_at => request_node[:expires_at],
                :attempts => request_node[:attempts],
                :now => now,
            )
                neo4j_query(<<~END_OF_QUERY, :tag => tag)
                    MATCH (r:LoginRequest {tag: $tag})
                    DETACH DELETE r;
                END_OF_QUERY
                next nil
            end

            unless request_node[:code].to_s == code
                attempts = request_node[:attempts].to_i + 1
                if attempts >= Authentication::LOGIN_REQUEST_MAX_ATTEMPTS
                    neo4j_query(<<~END_OF_QUERY, :tag => tag)
                        MATCH (r:LoginRequest {tag: $tag})
                        DETACH DELETE r;
                    END_OF_QUERY
                else
                    neo4j_query(<<~END_OF_QUERY, {:tag => tag, :attempts => attempts})
                        MATCH (r:LoginRequest {tag: $tag})
                        SET r.attempts = $attempts;
                    END_OF_QUERY
                end
                next nil
            end

            session_expires = (DateTime.now + Authentication::SESSION_LIFETIME_DAYS).to_s
            complete_params = {
                :tag => tag,
                :code => code,
                :now => now,
                :max_attempts => Authentication::LOGIN_REQUEST_MAX_ATTEMPTS,
                :sid => Authentication.session_key(sid),
                :session_expires => session_expires,
                :session_info => new_session_fields,
            }
            neo4j_query_expect_one(<<~END_OF_QUERY, complete_params)
                MATCH (r:LoginRequest {tag: $tag, code: $code})-[:FOR]->(u:User)
                WHERE r.expires_at > $now
                  AND COALESCE(r.attempts, 0) < $max_attempts
                DETACH DELETE r
                WITH u
                REMOVE u.login_request_created_at
                CREATE (s:Session {sid: $sid, expires: $session_expires})-[:FOR]->(u)
                SET s += $session_info
                RETURN u.email AS email, u.server_sid AS server_sid;
            END_OF_QUERY
        end

        broadcast_login_codes()

        unless login_result
            content_type :json
            halt 401, { :error => 'invalid_login_code' }.to_json
        end

        expires = session_cookie_expiry
        set_session_cookie(sid, expires)
        set_server_sid_cookie(login_result['server_sid'], expires) if login_result['server_sid']
        respond(:ok => 'yay')
    end
    def terms_bar_html
        return '' unless terms_required?

        "<div class='look-in-bar terms-bar'><i class='bi bi-file-earmark-check'></i>" \
            "<span>Du nutzt den Workspace als Gast. Dafür gilt die <a href='/datenschutz/nutzungsordnung' target='_blank'>Nutzungsordnung</a>; " \
            'bitte lies sie und stimme zu, dann startet auch dein Workspace.</span>' \
            "<button type='button' class='btn btn-sm btn-light bu-accept-terms'><i class='bi bi-check-lg'></i>Ich bin einverstanden</button></div>"
    end
    post '/api/accept_terms' do
        assert(user_logged_in?)
        halt 403, { :error => 'impersonating' }.to_json if @impersonated_by
        neo4j_query('MATCH (u:User {email: $email}) SET u.terms_accepted_at = $now, u.terms_version = $version;',
            :email => @session_user[:email], :now => Time.now.to_i, :version => Privacy::CHECKED.to_s)
        STDERR.puts "Nutzungsordnung accepted by #{@session_user[:email]}"
        respond(:ok => true)
    end
    # Other browsers in which this account is logged in (sessions last a
    # year; a forgotten login on a school computer is ended from here).
    def other_sessions_count
        return 0 unless user_logged_in?

        neo4j_query('MATCH (s:Session)-[:FOR]->(:User {email: $email}) WHERE s.sid <> $sid AND s.impersonated_by IS NULL RETURN count(s) AS n;',
            :email => @session_user[:email], :sid => current_session_key.to_s).first['n'].to_i
    end
    post '/api/sign_out_elsewhere' do
        assert(user_logged_in?)
        halt 403 if @impersonated_by
        n = other_sessions_count
        neo4j_query('MATCH (s:Session)-[:FOR]->(:User {email: $email}) WHERE s.sid <> $sid AND s.impersonated_by IS NULL DETACH DELETE s;',
            :email => @session_user[:email], :sid => current_session_key.to_s)
        STDERR.puts "#{@session_user[:email]} signed out #{n} other sessions"
        respond(:ended => n)
    end
end
